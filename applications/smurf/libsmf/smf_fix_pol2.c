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

*  Authors:
*     David Berry (JAC)
*     {enter_new_authors_here}

*  History:
*     4-SEP_2012 (DSB):
*        Original version.
*     7-SEP-2012 (DSB):
*        Complete re-write.
*     11-SEP-2012 (DSB):
*        Put VAL__BADD padding at the end of the POL_ANG array rather
*        than immediately before the last bonus point.
*     21-SEP-2012 (DSB):
*        Recent data contains POL_ANG values in radians rather than
*        arbitrary encoder units.
*     11-SEP-2015 (DSB):
*        Re-written because the old algorithm found 100 bonus points in
*        20150716/00021 when in fact there were none.
*     3-FEB-2016 (DSB):
*        Report error if HWP is not rotating.
*     13-SEP-2016 (DSB):
*        If unexpected featurs are found, issue a warning rather than
*        reporting an error. Also set POLANG values bad that occur after the
*        unexpected feature, so that the data prior to the unexpected
*        feature can be used.

*  Copyright:
*     Copyright (C) 2012 Science and Technology Facilities Council.
*     Copyright (C) 2016 East Asian Observatory
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

/* Prototypes for local functions. */
static int smf1_findlag( dim_t iframe, int curlag, dim_t nframe, const double *agaps,
                         const double *tgaps, int *status );

/* Main entry point . */
void smf_fix_pol2( ThrWorkForce *wf,  smfArray *array, int *status ){

/* Local Variables: */
   JCMTState *state;
   JCMTState *wstate;
   dim_t *jumps;
   dim_t idx;
   dim_t iframe;
   dim_t ijump;
   dim_t next_jump;
   dim_t njump;
   dim_t ntgood;
   double *agaps;
   double *angles;
   double *pa;
   double *pga;
   double *pgt;
   double *pt;
   double *tgaps;
   double *times;
   double ang_change;
   double ang_offset;
   double angle;
   double langle;
   double ltime;
   double maxang;
   double rotafreq;
   double rts_origin;
   double time;
   int curlag;
   int newlag;
   smfHead *hdr;

/* Check inherited status. */
   if( *status != SAI__OK ) return;

/* Set the maximum value of POL_ANG prior to wrapping back to zero. Old
   data had POL_ANG in arbitrary encoder units, with SMF__MAXPOLANG being
   equivalent to 360 degrees. New data stored OL_ANG in radians. Assume
   new data until we find a POL_ANG value greater than 2*PI. */
   maxang = 2*AST__DPI;

/* Check the HWP was spinning. */
   smf_getfitsd( array->sdata[ 0 ]->hdr, "ROTAFREQ", &rotafreq, status );
   if( rotafreq == 0.0 && *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRep( " ", "Unusable observation: half-waveplate was not spinning.",
              status );
   }

/* Loop over subarray. They probably all share the same header, but is it
   guaranteed? */
   for( idx = 0; idx < array->ndat && *status == SAI__OK; idx++ ) {
      hdr = array->sdata[ idx ]->hdr;

/* Allocate work arrays to hold the offset from the start of the chunk in
   days, and the rotation since the start of the chunk, in arbitrary encoder
   units. */
      times = astMalloc( hdr->nframes*sizeof(*times) );
      angles = astMalloc( hdr->nframes*sizeof(*angles) );
      tgaps = astMalloc( hdr->nframes*sizeof(*tgaps) );
      agaps = astMalloc( hdr->nframes*sizeof(*agaps) );

/* Initially, assume all time slices are good. */
      ntgood = hdr->nframes;

/* Loop round all frames. The "langle" variable is used to record the angle
   for the previous frame. The "rts_origin" value is the RTS_END value at
   the first valid frame - it is removed from every subsequent RTS_END
   value in order to reduced the dynamic range of the time values. The
   "ang_offset" value is added to the POL_ANG values in order to remove
   the downard jumps from 360 degs to zero degs at the end of each
   revolution. The "pa" and "pt" pointers point to the element of
   "angles" and "times" to which the angle and time at the next Frame
   should be written. */
      langle = VAL__BADD;
      ltime = VAL__BADD;
      rts_origin = VAL__BADD;
      ang_offset = VAL__BADD;

      pa = angles;
      pt = times;

      pga = agaps;
      pgt = tgaps;

      state = hdr->allState;
      for( iframe = 0; iframe < hdr->nframes; iframe++,state++,pa++,pt++,pga++,pgt++ ) {

/* Not sure if there ever will be bad RTS_END or POL_ANG values, but just
   in case... Also ignore slices that have a non-zero JOS_DRCONTROL value. */
         if( state->jos_drcontrol == 0 && state->rts_end != VAL__BADD &&
            state->pol_ang != VAL__BADD ) {

/* If the POL_ANG value is a lot more than 2.PI we must be dealing with
   old data in which the POL_ANG value was given in arbitrary encoder units.
   Change the maximum POL_ANG value appropriately. Since the old-style
   POL_ANG values increased by several 1000 between steps, checking for a
   value above 20 should be safe. */
            if( state->pol_ang > 20 ) maxang = SMF__MAXPOLANG;

/* If this is the first valid frame, record the initial RTS_END and
   POL_ANG values so that "time" and "angle" values are zero for the
   first frame. */
            if( rts_origin == VAL__BADD ) rts_origin = state->rts_end;
            if( ang_offset == VAL__BADD ) ang_offset = -state->pol_ang;

/* Store the time since the first valid frame, in days */
            time = state->rts_end - rts_origin;

/* Store the POL_ANG rotation since the first valid frame. */
            angle = state->pol_ang + ang_offset;

/* If the previous frame was also valid, find the increment in POL_ANG. */
            if( langle != VAL__BADD ) {
               ang_change = angle - langle;

/* If there has been a huge drop in angle, we must have passed through a
   360->zero degs discontinuity. Increase the angle offset by 360 degs to
   remove this discontinity, and adjust the current angle and angle
   increment to use this new angle offset. */
               if( ang_change < -maxang/2 ) {
                  ang_offset += maxang;
                  angle += maxang;
                  ang_change += maxang;
               }
            }

/* The angle, time and speed are unknown if the current frame is not valid. */
         } else {
            angle = VAL__BADD;
            time = VAL__BADD;
         }

/* Store the final time and angle in the work arrays. */
         if( time != VAL__BADD && ltime != VAL__BADD ) {
            *pgt = time - ltime;
         } else {
            *pgt = VAL__BADD;
         }

         if( angle != VAL__BADD && langle != VAL__BADD ) {
            *pga = angle - langle;
         } else {
            *pga = VAL__BADD;
         }

         *pt = time;
         *pa = angle;

/* Record the angle of the current frame for use on the next pass round this
   loop. */
         langle = angle;
         ltime = time;
      }

/* Initialise the lag (i.e. change in index) that produces the best
   correlation between the angle gaps and the time gaps. */
      curlag = 0;

/* Loop over all time slices. */
      njump = 0;
      jumps = NULL;
      for( iframe = 0; iframe < hdr->nframes; iframe++ ) {

/* Find the lag that produces thes best correlation between angle gaps
   and time gaps within a small box centred on the current time slice. */
         newlag = smf1_findlag( iframe, curlag, hdr->nframes, agaps,
                                tgaps, status );

/* If the lag has dropped, indicate that the corresponding angle value is
   a bonus point. */
         if( newlag == curlag - 1 ) {
            curlag = newlag;
            jumps = astGrow( jumps, ++njump, sizeof( *jumps ) );
            jumps[ njump - 1 ] = iframe + njump - 1;

/* The lag should only ever drop by one, or remain the same. If anything
   else happens, it is either a bug in this function, or something very
   strange in the data, so report an error. */
         } else if( newlag != curlag && *status == SAI__OK ) {
            ntgood = iframe - 1;

            msgOutf( "", "WARNING: Unexpected feature found in the JCMTSTATE.POL_ANG "
                     "array at RTS_NUM=%d. The remaining %zu time-slices will be ignored",
                     status, (int) hdr->allState[iframe].rts_num, hdr->nframes - iframe );
            break;
         }
      }

/* Now shuffle the POL_ANG values down to remove the accepted bonus values. */
      if( njump > 0 || ntgood < hdr->nframes ) {
         state = wstate = hdr->allState;
         ijump = 0;
         next_jump = jumps ? jumps[ ijump ] : hdr->nframes + 1;
         for( iframe = 0; iframe < ntgood; iframe++,state++ ) {
            if( iframe < next_jump ) {
               (wstate++)->pol_ang = state->pol_ang;
            } else {
               msgOutf( " ", "WARNING: Removing spurious POL_ANG value at time "
                        "slice %d", status, (int) iframe );
               ijump++;
               if( ijump == njump ) {
                  next_jump = hdr->nframes + 1;
               } else {
                  next_jump = jumps[ ijump ];
               }
            }
         }

/* Fill the end of the POL_ANG array with bad values. */
         while( wstate < hdr->allState + hdr->nframes ) {
            (wstate++)->pol_ang = VAL__BADD;
         }
      }

/* Free work space. */
      jumps = astFree( jumps );
      times = astFree( times );
      angles = astFree( angles );
      agaps = astFree( agaps );
      tgaps = astFree( tgaps );
   }

}


static int smf1_findlag( dim_t iframe, int curlag, dim_t nframe, const double *agaps,
                         const double *tgaps, int *status ){
/*
*  Name:
*     smf1_findlag

*  Purpose:
*     Find the shift in frame index that gives the best correlation between
*     changes in angle and changes in time.

*  Invocation:
*     int smf1_findlag( dim_t iframe, int curlag, dim_t nframe, const double *agaps,
*                       const double *tgaps, int *status )

*  Arguments:
*     iframe = dim_t * (Given)
*        The index of the central tgaps value within the box.
*     curlag = int * (Given)
*        The lag at which to start searching. A range of lags between
*        curlag - 5 and curlag + 5 are checked and the lag that gives the
*        highest correlation between agaps and tagps values is returned.
*     nframe = dim_t * (Given)
*        The number of values in the agaps and tgaps arrays.
*     agaps = cont double * (Given)
*        An array holding the changes in POL_ANG value between adjacent
*        time slices. The value in agaps[i] is the change in POL_ANG between
*        time slice i and time slice i+1. May contain VAL__BADD values.
*     tgaps = cont double * (Given)
*        An array holding the time intervals between adjacent time slices.
*        The value in tgaps[i] is the interval between time slice i and time
*        slice i+1. May contain VAL__BADD values.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Returned Value:
*     The integer lag between the agaps and tgaps arrays that produces
*     the highest correlation, within a box centred on "iframe". The
*     supplied curlag value is returned if the lag cannot be calculated
*     reliably.

*  Description:
*     This function shifts the "agaps" array backwards and forwards,
*     looking for the shift (lag) that gives the best correlation between
*     the agaps values and the tgaps values within a small box centred on
*     a specified index. This shift is returned.


*/

/* Local Constants: */
#define CBOX 40
#define LBOX 5

/* Local Variables: */
   double cor;
   double cormax;
   double sa2;
   double sa;
   double sat;
   double st2;
   double st;
   int ia;
   int ihit;
   int ilot;
   int it;
   int lag;
   int laghi;
   int laglo;
   int ns;
   int result;

/* Initialise the returned value */
   result = curlag;

/* Check inherited status */
   if( *status != SAI__OK ) return result;

/* Initialise the smallest and greatest lags to test. */
   laglo = curlag - LBOX;
   laghi = curlag + LBOX;

/* Initialise the greatest correlation found so far. */
   cormax = VAL__MIND;

/* Test each lag in turn. */
   for( lag = laglo; lag <= laghi; lag++ ) {

/* Set up the indices of the first and last tgaps values to include in
   the calculation of the correlation produced by the current lag. */
      ilot = iframe - CBOX;
      ihit = iframe + CBOX;

/* Initialise the runing sums> */
      sa = 0.0;
      st = 0.0;
      sa2 = 0.0;
      st2 = 0.0;
      sat = 0.0;
      ns = 0;

/* Loop over all pairs of tgaps/agaps values to be included in
   the calculation of the correlation produced by the current lag. */
      for( it = ilot; it <= ihit; it++ ) {

/* "it" is the index of the tgaps value. FInd the index of the
   corresponding agaps value, taking into account the current lag. */
         ia = it - lag;

/* Check they are both within the bounds of the arrays. */
         if( ia > 0 && ia < (int) nframe &&
             it > 0 && it < (int) nframe ) {

/* If neither is bad, update the running sums needed to find the
   correlation. */
            if( agaps[ia] != VAL__BADD && tgaps[it] != VAL__BADD ) {
               sa += agaps[ia];
               st += tgaps[it];
               sa2 += agaps[ia]*agaps[ia];
               st2 += tgaps[it]*tgaps[it];
               sat += agaps[ia]*tgaps[it];
               ns++;
            }
         }
      }

/* We require at least 50% of the total box (i.e. one CBOX) to have good
   values to produce a reliable correlation. */
      if( ns > CBOX ) {

/* Find the correlation coefficient between the tgaps values and the
   lagged agaps values. */
         cor = ( ns*sat - sa*st )/sqrt( ( ns*st2 - st*st )*( ns*sa2 - sa*sa ));

/* If this is the largest correlation found so far, record it together
   with the current lag. */
         if( cor > cormax ) {
            cormax = cor;
            result = lag;
         }
      }
   }

/* Return the best lag. */
   return result;
}

