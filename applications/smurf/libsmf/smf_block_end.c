/*
*+
*  Name:
*     smf_block_end

*  Purpose:
*     Find how many time slices pass before there is a significant shift
*     in map position.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     result = smf_block_end( smfData *data, int block_start, int ipolcrd,
*                             float arcerror, int *status );

*  Arguments:
*     data = smfData * (Given)
*        Pointer to the time series data.
*     block_start
*        Index of the time slice at start of block.
*     ipolcrd
*        Indicates the reference direction for half-waveplate angles:
*        0 = FPLANE, 1 = AZEL, 2 = TRACKING. In all case, the reference
*        direction is the positive direction of the second axis.
*     arcerror = float (Given)
*        The maximum shift in position (in arc-seconds) allowed within a
*        single block of time slices.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Returned Value:
*     The index of the last time slice in the block, or -1 if there are
*     no remaining time slices (or an error occurs).

*  Description:
*     This function searches through time slices in the supplied data,
*     starting at the time slice with index "block_start", until a time
*     slice is found for which the spatial position at one or more of
*     the corners of the focal plane has moved by more than "arcerror"
*     arc-seconds from its position at the start of the block. The index
*     of the final time slice is then reduced in order to ensure that the
*     whole block spans an integral number of quarter revolutions of the
*     half-waveplate, relative to north in the tracking system. The index
*     of this final time slice is returned as the function value.

*  Authors:
*     DSB: David Berry (JAC, Hawaii)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1-FEB-2011 (DSB):
*        Original version.
*     2012-03-06 (TIMJ):
*        Use SOFA instead of SLA.
*     21-SEP-2012 (DSB):
*        Take 360->zero wrap-around in POL_ANG into account when adjusting the
*        end time slice to a quarter revolution boundary.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2011-2012 Science and Technology Facilities Council.
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

#include <stdio.h>

/* Starlink includes */
#include "sofa.h"
#include "sae_par.h"

/* SMURF includes */
#include "libsmf/smf.h"

/* A macro to test if the focal plane has moved too far from its position
   at the start of the block. Check the angular separation between the
   centre of the focal plane at the start and at the current time slice
   is not more than "arcerror", and check that he focal plane has not rotated
   by more than "angle". */
#define FPMOVED ( \
     iauSeps( ac1_start, ac2_start, state->tcs_tr_ac1, state->tcs_tr_ac2 ) \
     > arcerror || \
       fabs( ang_start - state->tcs_tr_ang ) > angle )



int smf_block_end( smfData *data, int block_start, int ipolcrd, float arcerror,
                   int *status ){

/* Local Variables: */
   const JCMTState *state;    /* JCMTState info for current time slice */
   dim_t ntslice;             /* Number of time-slices in data */
   double ac1_start;          /* Tracking longitude at start of block (rads) */
   double ac2_start;          /* Tracking latitude at start of block (rads) */
   double ang_start;          /* Tracking orientaion at start of block (rads) */
   double angle;              /* Rotation that gives arcerror shift at corners */
   double end_wang;           /* Half-waveplate angle at end of block */
   double start_wang;         /* Half-waveplate angle at start of block */
   double wang;               /* Half-waveplate angle */
   int ifail;                 /* Index of last time slice to fail the test */
   int inc;                   /* No. of time slices between tests */
   int ipass;                 /* Index of last time slice to pass the test */
   int itime;                 /* Time slice index at next test */
   int result;                /* The returned time slice index at block end */
   smfHead *hdr;              /* Pointer to data header this time slice */

/* Initialise */
   result = -1;

/* Check the inherited status. */
   if( *status != SAI__OK ) return result;

/* Convenience pointers. */
   hdr = data->hdr;

/* Obtain number of time slices - will also check for 3d-ness. */
   smf_get_dims( data, NULL, NULL, NULL, &ntslice, NULL, NULL, NULL,
                 status );

/* Check that we have not used all time slices. */
   if( block_start >= 0 && block_start < (int) ntslice ) {

/* Determine the critical angle (in radians) that produces a shift of
   "arcerror" arc-seconds at a distance of 4 arc-minutes (the rough radius
   of the SCUBA-2 field of view) from the focal plane centre. */
      angle = arcerror/( 4.0 * 60 );

/* Convert arcerror from arc-seconds to radians. */
      arcerror *= AST__DD2R/3600.0;

/* Note the actual boresight position and focal plane orientation within
   the tracking system at the block start, all three in radians. */
      state = (hdr->allState) + block_start;
      ac1_start = state->tcs_tr_ac1;
      ac2_start = state->tcs_tr_ac2;
      ang_start = state->tcs_tr_ang;

/* For speed, we do not test every time slice. Instead we first check the
   time slice following the start, but then accelarate through subsequent
   time slices, doubling the gap between tests each time. When we find
   a test that fails (i.e. the focal plane has moved further than
   "arcerror" from the starting time slice), we do a binary chop between
   that test and the previous successful test. */
      ipass = 0;
      ifail = -1;
      inc = 1;
      itime = block_start + inc;
      while( itime < (int) ntslice ) {
         state = (hdr->allState) + itime;

/* The test fails if the actual telescope position in the tracking system
   has moved by more than arcerror arc-seconds since the block start, or
   if the focal plane has rotated on the sky (in the tracking system) by
   more than the critical angle. Leave the loop if the test fails. */
         if( FPMOVED ) {
            ifail = itime;
            break;
         }

/* The focal plane has not yet moved significantly away from its position
   at the starting time slice. Record the index of the last known unmoved
   time slice. */
         ipass = itime;

/* Get the index of the time slice to test next. We double the gap
   between tests. */
         inc *= 2;
         itime += inc;
      }

/* If no test failed, test the last time slice. If the last time slice
   passes the test then all time slices are in the block. So set the result
   to the index of the last time slice. */
      if( ifail == -1 ) {
         itime = ntslice - 1;
         state = (hdr->allState) + itime;
         if( FPMOVED ) {
            ifail = itime;
         } else {
            result = itime;
         }
      }

/* If we have not yet found the result, do a binary chop between the last
   successful test and the first unsuccessful test. */
      if( result == -1 ) {

/* Loop until the failed and successful tests are adjacent to each other. */
         while( ifail > ipass + 1 ) {

/* Perform a test at the central time slice between the failed and
   successful tests. If the time slice fails the test, use it to replace
   "ifail". otherwise use it to replace "ipass". */
            itime = ( ifail + ipass )/2;
            state = (hdr->allState) + itime;
            if( FPMOVED ) {
               ifail = itime;
            } else {
               ipass = itime;
            }
         }

/* The returned index is the index of the last time slice to pass the test. */
         result = ipass;
      }

/* In order to reduce inaccuracies when finding the required Fourier
   component of the time series, we now shorten the block until it spans an
   integral number of quarter revolutions of the half-waveplate relative to
   tracking north. We use quarter revolutions rather than whole
   revolutions because analysed intensity varies four times faster than
   the half-waveplate position. First find the half-waveplate angle with
   respect to tracking north, at the start of the block. */
      state = (hdr->allState) + block_start;
      start_wang = state->pol_ang;
      if( ipolcrd == 0 ) {
         start_wang -= state->tcs_tr_ang;
      } else if( ipolcrd == 1 ) {
         start_wang -= state->tcs_tr_ang - state->tcs_az_ang ;
      }

/* Now find the half-waveplate angle with respect to tracking north, at
   the current end of the block. */
      state = (hdr->allState) + result;
      end_wang = state->pol_ang;
      if( ipolcrd == 0 ) {
         end_wang -= state->tcs_tr_ang;
      } else if( ipolcrd == 1 ) {
         end_wang -= state->tcs_tr_ang - state->tcs_az_ang ;
      }

/* On the assumption that POL_ANG increases with time, if the
   half-waveplate angle at the end of the block is less than at the start of
   the block, it must have reached 2*PI and wrapped back round to zero. So
   add on 2*PI to the end value. */
      if( end_wang < start_wang ) end_wang += 2*AST__DPI;

/* Reduce the end angle so that it is an integral number of quarter
   revolutions in front of the start angle. We are assuming here that
   POL_ANG increases (rather than decreasing) with time. */
      end_wang = start_wang +
                  AST__DPIBY2*( (int) ( ( end_wang - start_wang )/AST__DPIBY2 ) );

/* If the end angle is greater than 2*PI, reduce it by 2.PI. */
      if( end_wang > 2*AST__DPI ) end_wang -= 2*AST__DPI;

/* Work backwards through the time slices, starting at the current end
   time slice, until a time slice is found which has an angle less than the
   end angle found above. */
      for( ; result >= block_start; result-- ) {
         state = (hdr->allState) + result;
         wang = state->pol_ang;
         if( ipolcrd == 0 ) {
            wang -= state->tcs_tr_ang;
         } else if( ipolcrd == 1 ) {
            wang -= state->tcs_tr_ang - state->tcs_az_ang ;
         }
         if( wang < end_wang ) break;
      }

   }

/* Return the index of the last time slice in the block, or -1 if an
   error has occurred. */
   return ( *status == SAI__OK && result >= block_start ) ? result : -1 ;
}


