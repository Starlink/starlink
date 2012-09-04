/*
*+
*  Name:
*     smf_fix_pol2

*  Purpose:
*     Correct data for the POL-2 triggering issue.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_fix_pol2( ThrWorkForce *wf,  smfArray *array, int *status )

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads.
*     array = smfArray * (Given and Returned)
*        The data that will be fixed.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     According to Ryan Berthold, early data from POL-2 suffered from a
*     problem that caused it to "trigger on nothing". This causes an
*     extra spurious value to be inserted into the POL_ANG array in the
*     JCMTSTATE extension, causing later valid values to appear to be
*     delayed.
*
*     This function corrects for these by first finding candidate "bonus"
*     POL_ANG values, and then seeing if the removal of such values would
*     result in a reduction in the residuals between the (rts_end,pol_ang)
*     points and the best fitting straight line. If so, the point is
*     removed, and later POL_ANG values are shuffled down to fill the gap
*     (a VAL__BADD value is pushed onto the end of the array).
*
*     Candidate bonus points are identified by the fact that there is
*     an unusually low angular speed in the time step leading up to the
*     point.

*  Authors:
*     David Berry (JAC)
*     {enter_new_authors_here}

*  History:
*     4-SEP_2012 (DSB):
*        Original version.

*  Copyright:
*     Copyright (C) 2012 Science and Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* Starlink includes */
#include "mers.h"
#include "sae_par.h"
#include "prm_par.h"
#include "star/thr.h"

/* SMURF includes */
#include "libsmf/smf.h"

/* POL_ANG is given in integer units where 944000 is equivalent to 2*PI */
#define MAXANG 944000

/* The length of the box over which a linear fit is performed to the
   (rts_end,pol_ang) positions around each potential binus point. */
#define BOX 50

/* The number of sigma below the mean angular speed at which POL_ANG
   values are considered as candidate bonus points. */
#define NSIGMA 10



void smf_fix_pol2( ThrWorkForce *wf,  smfArray *array, int *status ){

/* Local Variables: */
   JCMTState *state;
   JCMTState *wstate;
   dim_t *jumps;
   dim_t idx;
   dim_t iframe;
   dim_t ijump;
   dim_t j;
   dim_t jmin;
   dim_t njump;
   double *angles;
   double *pa;
   double *pc;
   double *ps;
   double *pt;
   double *speeds;
   double *times;
   double ang_change;
   double ang_offset;
   double c;
   double m;
   double mean_speed;
   double minspeed;
   double rts_origin;
   double s1;
   double s2;
   double sigma;
   double work[ 2*BOX ];
   int lbnd;
   int ubnd;
   smfHead *hdr;

/* Check inherited status. */
   if( *status != SAI__OK ) return;

/* Loop over subarray */
   for( idx = 0; idx < array->ndat && *status == SAI__OK; idx++ ) {
      hdr = array->sdata[ idx ]->hdr;

/* Store all the POL_ANG and RTS_END values in local arrays, and also
   store the angular speed between each pair of values. Take accout of
   the wrap-around in POL_ANG from MAXANG back to zero at the end of each
   revolution. */
      pt = times = astMalloc( hdr->nframes*sizeof( *times ) );
      pa = angles = astMalloc( hdr->nframes*sizeof( *angles ) );
      ps = speeds = astMalloc( hdr->nframes*sizeof( *speeds ) );
      rts_origin = VAL__BADD;
      ang_offset = 0;
      state = hdr->allState;
      for( iframe = 0; iframe < hdr->nframes; iframe++,state++,pa++,pt++,ps++ ) {
         if( rts_origin == VAL__BADD ) rts_origin = state->rts_end;
         if( state->rts_end != VAL__BADD && state->pol_ang != VAL__BADD ) {
            *pt = state->rts_end - rts_origin;
            *pa = state->pol_ang + ang_offset;

            if( iframe > 0 && pa[-1] != VAL__BADD ) {
               ang_change = ( pa[0] - pa[-1] );
               if( ang_change < -MAXANG/2 ) {
                  ang_offset += MAXANG;
                  pa[0] += MAXANG;
                  ang_change += MAXANG;
               }
               *ps = ang_change/( pt[0] - pt[-1] );
            } else {
               *ps = VAL__BADD;
            }
         } else {
            *pt = VAL__BADD;
            *pa = VAL__BADD;
            *ps = VAL__BADD;
         }
      }

/* Work out the sigma clipped mean speed. This should ignore the frames that
   involve one of the extra "bonus" POL_ANG values. */
      mean_speed = smf_sigmaclip( hdr->nframes, speeds, NULL, 2.0, 3, &sigma,
                                  status );

/* Now go through the values again looking for samples that have an
   unusually low speed (i.e. more than NSIGMA standard deviations below the
   mean speed). */
      jumps = NULL;
      njump = 0;
      pt = times;
      pa = angles;
      ps = speeds;
      for( iframe = 0; iframe < hdr->nframes; iframe++,pa++,pt++,ps++ ) {
         if( *ps != VAL__BADD ) {
            if( *ps < mean_speed - NSIGMA*sigma ) {

/* Find the upper and lower bounds of a small box centered on the current
   candidate bonus point. */
               lbnd = iframe - BOX/2;
               if( lbnd < 0 ) lbnd = 0;
               ubnd = iframe + BOX/2;
               if( ubnd >= (int) hdr->nframes ) ubnd = hdr->nframes - 1;

/* Find the point with the lowest speed in this box. We do not need to
   check the lower half of the box as this will have been done on previous
   passes through the "iframe" loop. */
               minspeed = *ps;
               jmin = iframe;
               for( j = iframe + 1; j < (dim_t) ubnd; j++ ) {
                  if( speeds[ j ] != VAL__BADD && speeds[ j ] < minspeed ) {
                     minspeed = speeds[ j ];
                     jmin = j;
                  }
               }

/* Move on so we are centred on the lowest speed in the box. */
               if( jmin != iframe ) {
                  iframe = jmin;
                  pa = angles + iframe;
                  pt = times + iframe;
                  ps = speeds + iframe;
                  lbnd = iframe - BOX/2;
                  if( lbnd < 0 ) lbnd = 0;
                  ubnd = iframe + BOX/2;
                  if( ubnd >= (int) hdr->nframes ) ubnd = hdr->nframes - 1;
               }

/* Do a least squares linear fit to the (time,angle) points in the box, and
   note the RMS residuals ("s1"). */
               lbnd = iframe - BOX/2;
               if( lbnd < 0 ) lbnd = 0;
               ubnd = iframe + BOX/2;
               if( ubnd >= (int) hdr->nframes ) ubnd = hdr->nframes - 1;
               kpg1Fit1d( lbnd, ubnd, angles + lbnd, times + lbnd, &m, &c,
                          &s1, status );

/* Now try removing the current point to see if that produces a better
   linear fit. Copy the angle (except the candidate bonus point) into the
   work array. */
               pc = work;
               for( j = lbnd; j < iframe; j++ ) {
                  *(pc++) = angles[ j ];
               }
               for( j = iframe + 1; j < (dim_t) ubnd; j++ ) {
                  *(pc++) = angles[ j ];
               }

/* Now do the fit. The RMS of the residuals is put into "s2". */
               kpg1Fit1d( lbnd, ubnd, work, times + lbnd, &m, &c, &s2,
                          status );

/* Tell the user about the test if in debug mode. */
               msgOutiff( MSG__DEBUG, "", "smf_fix_pol2: Testing "
                          "POL_ANG[%d]: RMS with: %g without: %g", status,
                          (int) iframe, s1, s2 );

/* If the linear fit is significantly improved by removing the current
   candidate bonus point, add the current sample index to the list of steps
   to be removed. */
               if( s2 < 0.95*s1 ) {
                  jumps = astGrow( jumps, ++njump, sizeof( *jumps ) );
                  jumps[ njump - 1 ] = iframe;
               }
            }
         }
      }

/* Now shuffle the POL_ANG values down to remove the accepted bonus values. */
      if( njump > 0 ) {
         state = wstate = hdr->allState;
         ijump = 0;
         for( iframe = 0; iframe < hdr->nframes; iframe++,state++ ) {
            if( iframe < jumps[ ijump ] ) {
               (wstate++)->pol_ang = state->pol_ang;
            } else {
               msgOutiff( MSG__VERB, "", "smf_fix_pol2: Removing "
                          "POL_ANG[%d]", status, (int) iframe );
               ijump++;
            }
         }

/* Fill the end of the POL_ANG array with bad values. */
         while( wstate < state ) (wstate++)->pol_ang = VAL__BADD;
      }

/* Release temporary work space */
      jumps = astFree( jumps );
      times = astFree( times );
      angles = astFree( angles );
      speeds = astFree( speeds );
   }

}

