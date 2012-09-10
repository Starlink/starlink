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

/* The half-width of the box over which a linear fit is done to the
   POL_ANG values. */
#define HALF_BOX 25

void smf_fix_pol2( ThrWorkForce *wf,  smfArray *array, int *status ){

/* Local Variables: */
   JCMTState *state;
   JCMTState *wstate;
   dim_t *jumps;
   dim_t idx;
   dim_t iframe;
   dim_t ijump;
   dim_t j;
   dim_t jtop;
   dim_t njump;
   double *angles;
   double *pa;
   double *pcenx;
   double *pceny;
   double *pnewx;
   double *pnewy;
   double *pnewytop;
   double *poldx;
   double *poldy;
   double *pt;
   double *times;
   double ang_change;
   double ang_offset;
   double angle;
   double c;
   double denom;
   double langle;
   double limit;
   double lresid;
   double m;
   double pop;
   double resid;
   double rts_origin;
   double sx;
   double sxx;
   double sxy;
   double sy;
   double syy;
   double time;
   smfHead *hdr;

/* Check inherited status. */
   if( *status != SAI__OK ) return;

/* Loop over subarray. They probably all shre the same header, but is it
   guaranteed? */
   for( idx = 0; idx < array->ndat && *status == SAI__OK; idx++ ) {
      hdr = array->sdata[ idx ]->hdr;

/* Allocate work arrays to hold the offset from the start of the chunk in
   days, and the rotation since the start of the chunk, in arbitrary encoder
   units. */
      times = astMalloc( hdr->nframes*sizeof(*pt) );
      angles = astMalloc( hdr->nframes*sizeof(*pt) );

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
      rts_origin = VAL__BADD;
      ang_offset = VAL__BADD;

      pa = angles;
      pt = times;

      state = hdr->allState;
      for( iframe = 0; iframe < hdr->nframes; iframe++,state++,pa++,pt++ ) {

/* Not sure if there ever will be bad RTS_END or POL_ANG values, but just
   in case... */
         if( state->rts_end != VAL__BADD && state->pol_ang != VAL__BADD ) {

/* If this is the first valid frame, record the initial RTS_END and
   POL_ANG values so that "time" and "angle" values are zero for the
   first frame. */
            if( rts_origin == VAL__BADD ) rts_origin = state->rts_end;
            if( ang_offset == VAL__BADD ) ang_offset = -state->pol_ang;

/* Store the time since the first valid frame, in days */
            time = state->rts_end - rts_origin;

/* Store the POL_ANG rotation since the first valid frame, in arbitrary
   encoder counts ( MAXANG corresponds to 360 degs). */
            angle = state->pol_ang + ang_offset;

/* If the previous frame was also valid, find the increment in POL_ANG. */
            if( langle != VAL__BADD ) {
               ang_change = angle - langle;

/* If there has been a hugedrop in angle, we must have passed through a
   360->zero degs discontinuity. Increase the angle offset by 360 degs to
   remove this discontinity, and adjust the current angle and angle
   increment to use this new angle offset. */
               if( ang_change < -MAXANG/2 ) {
                  ang_offset += MAXANG;
                  angle += MAXANG;
                  ang_change += MAXANG;
               }
            }

/* The angle, time and speed are unknown if the current frame is not valid. */
         } else {
            angle = VAL__BADD;
            time = VAL__BADD;
         }

/* Store the final time and angle in the work arrays. */
         *pt = time;
         *pa = angle;

/* Record the angle of the current frame for use on the next pass round this
   loop. */
         langle = angle;
      }

/* Now we have the angles and time, we step through the array, and at
   each frame perform a least squares linear fit to produce the gradient of
   offset of the line through a box of values centred on the current frame.
   We find the residual between this line and the POL_ANG value at the
   central time. As a bonus point is approached, the residual will get
   more and more negative until it suddenly switches sign at the bonus
   point itself. We then remove the bonus point by shuffling down later
   POL_ANG values to replace it, and continue to look for the next bonus
   point. The least squares fitting is done by maining the required
   statistics for a box of values centred on the required frame. As we
   move to a new frame, a new point is added to the statistics, and the
   oldest point is removed..

   First initialise the required running sums to hold the sums of the data
   in the first HALF_BOX+1 data values. The independent variable in the fit
   is time. This means the "x*y" product summed in "sxy" does not depend
   on the current centre of the box, which is good. */
      sy = 0.0;
      sx = 0.0;
      sxy = 0.0;
      sxx = 0.0;
      syy = 0.0;
      pop = 0;

      pnewy = angles;
      pnewx = times;
      for( iframe = 0; iframe <= HALF_BOX; iframe++,pnewy++,pnewx++ ) {
         if( *pnewy != VAL__BADD && *pnewx != VAL__BADD ) {
            sy += *pnewy;
            sx += *pnewx;
            sxy += ( *pnewx )*( *pnewy );
            sxx += ( *pnewx )*( *pnewx );
            syy += ( *pnewy )*( *pnewy );
            pop++;
         }
      }

/* The "pnewy" and "pnewx" pointers now point to the input values that are
   about to be added to the fit box. Initialise another pair of pointers
   "poldx" and "poldy" to point to the input values that are about to leave
   the fit box. These will initially point to elements before the start of
   the input arrays. */
      poldy = angles - HALF_BOX;
      poldx = times - HALF_BOX;

/* Initialise another pair of pointers to point to the central values
   in the fit box. */
      pceny = angles;
      pcenx = times;

/* Store a pointer to the fist value after the end of the angles array. */
      pnewytop = angles + hdr->nframes;

/* Initialise the list of POL_ANG values to remove. */
      jumps = NULL;
      njump = 0;

/* Do each frame in turn. */
      lresid = VAL__BADD;
      for( iframe = 0; iframe < hdr->nframes; iframe++,poldx++,pcenx++,pnewx++,poldy++,pceny++,pnewy++ ) {
         resid = VAL__BADD;

/* Calculate the gradient  and offset of the least squares fit to the data
   currently in the fittting box. The offset here ("c") is the value of the
   fitted line at time == 0*/
         denom =  pop*sxx - sx*sx;
         if( denom > 0 && pop > 0 ) {
            m =  ( pop*sxy - sx*sy )/denom;
            c =  ( sxx*sy - sx*sxy )/denom;

/* Correct the offset to be the value of the line at the centre of the
   box. All this least square sfitting would be unnecessary if we knew
   that the mean time in the box was equal to the central time. If that
   were the case we could use the mean angle in the box as "c", but alas
   it's not the case. */
            c += m*( *pcenx );

/* Get the residual between the central fit value and the central angle
   value. */
            if( *pceny != VAL__BADD ) {
               resid = c - *pceny;

/* If the previous residual was large and negative, and the current
   residual is large and positive, then we have found a bonus point.
   Here, "large" is defined as more than 25% of the angular step since the
   last sample. */
               if( pceny[ -1 ] != VAL__BADD ) {
                  limit = 0.25*( pceny[0] - pceny[-1] );
                  if( lresid != VAL__BADD && lresid < -limit && resid > limit ) {

/* Remove the central value and the upper half of the box from the stats. */
                     jtop = iframe + HALF_BOX + 1;
                     if( jtop > hdr->nframes ) jtop = hdr->nframes;
                     for( j = iframe; j < jtop; j++ ) {
                        if( angles[ j ] != VAL__BADD && times[ j ] != VAL__BADD ) {
                           sy -= angles[ j ];
                           sx -= times[ j ];
                           sxy -= times[ j ]*angles[ j ];
                           sxx -= times[ j ]*times[ j ];
                           syy -= angles[ j ]*angles[ j ];
                           pop--;
                        }
                     }

/* Shuffle down the remaining angle values to replace the hole left by
   removing the central value. Pad the end with a blank value. */
                     for( j = iframe + 1; j < hdr->nframes; j++ ) {
                        angles[ j - 1 ] = angles[ j ];
                     }
                     angles[ j - 1 ] = VAL__BADD;

/* Add back the new central value and the upper half of the box into the
   stats. */
                     for( j = iframe; j < jtop; j++ ) {
                        if( angles[ j ] != VAL__BADD && times[ j ] != VAL__BADD ) {
                           sy += angles[ j ];
                           sx += times[ j ];
                           sxy += times[ j ]*angles[ j ];
                           sxx += times[ j ]*times[ j ];
                           syy += angles[ j ]*angles[ j ];
                           pop++;
                        }
                     }

/* Ensure we use a bad value for "lresid" next time. */
                     resid = VAL__BADD;

/* Add the *original* index of the current frame (i.e. assuming no points
   have been removed) to the list of POL_ANG values to remove. */
                     jumps = astGrow( jumps, ++njump, sizeof( *jumps ) );
                     jumps[ njump - 1 ] = iframe + njump - 1;
                  }
               }
            }
         }

/* If there will be another pass through the "iframe" loop... */
         if( iframe < hdr->nframes ) {

/* Record the residual */
            lresid = resid;

/* Add the next input value into the running sums. */
            if( pnewy < pnewytop && *pnewy != VAL__BADD && *pnewx != VAL__BADD ) {
               sy += *pnewy;
               sx += *pnewx;
               sxy += ( *pnewx )*( *pnewy );
               sxx += ( *pnewx )*( *pnewx );
               syy += ( *pnewy )*( *pnewy );
               pop++;
            }

/* Remove the oldest input value from the running sums. There is nothing
   to add in on the last pass through the "i" loop. */
            if( poldy >= angles && *poldy != VAL__BADD && *poldx != VAL__BADD ) {
               sy -= *poldy;
               sx -= *poldx;
               sxy -= ( *poldx )*( *poldy );
               sxx -= ( *poldx )*( *poldx );
               syy -= ( *poldy )*( *poldy );
               pop--;
            }
         }
      }

/* Now shuffle the POL_ANG values down to remove the accepted bonus values. */
      if( njump > 0 ) {
         state = wstate = hdr->allState;
         ijump = 0;
         for( iframe = 0; iframe < hdr->nframes && ijump < njump;
              iframe++,state++ ) {
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

/* Free work space. */
      jumps = astFree( jumps );
      times = astFree( times );
      angles = astFree( times );
   }

}

