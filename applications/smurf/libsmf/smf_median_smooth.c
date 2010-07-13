/*
*+
*  Name:
*     smf_median_smooth

*  Purpose:
*     Smooth a 1D data array using a fast median, minimum or maximum box filter.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf_median_smooth( dim_t box, smf_filt_t filter_type,
*                             float wlim, dim_t el, const double *dat,
*                             const smf_qual_t *qua, size_t stride,
*                             smf_qual_t mask, double *out, double *w1,
*                             double *w2, double *mean, double *sigma,
*                             int *status )

*  Arguments:
*     box = dim_t (Given)
*        The width of the box.
*     filter_type = smf_filt_t (Given)
*        The type of filter to use:
*        SMF__FILT_MEDIAN: Replace each value with the median of the local values
*        SMF__FILT_MIN: Replace each value with the minimum of the local values
*        SMF__FILT_MAX: Replace each value with the maximum of the local values
*     wlim = double (Given)
*        A value in the range 0.0 to 1.0 that gives the minimum fraction of
*        valid input values that a filter box must contain in order to
*        produce a valid output value. If a value smaller than 0.0 or
*        larger than 1.0 is supplied, then an output value will be bad if
*        and only if the corresponding input value is bad.
*     el = dim_t (Given)
*        The number of elements to used from the data array (each
*        separated by a step of "stride").
*     dat = const double * (Given)
*        The data array. Any VAL__BADD values are ignored.
*     qua = const smf_qual_t * (given)
*        The quality array associated with the data array. May be NULL.
*     stride = size_t (Given)
*        The step between samples to use in the data and quality arrays.
*     mask = smf_qual_t (Given)
*        A mask specifying the samples that are to be includedin the
*        median filtering.
*     out = double * (Returned)
*        The output array. The array will start and end with a section of
*        VAL__BADD values, each of length "box/". VAL__BADD is used to
*        flag values for which no median value could be calculated.
*     w1 = double * (Given and Returned)
*        A work array of length "box".
*     w2 = double * (Given and Returned)
*        A work array of length "box".
*     mean = double * (Returned)
*        An optional array to recieve the mean value in each filter box.
*        If not NULL, it should have "el" elements.
*     sigma = double * (Returned)
*        An optional array to recieve the standard deviation in each filter
*        box. If supplied, it should have "el" elements.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine creates a 1D output array in which each value is the
*     median, minimum or maximum (as selected via "filter_type") of the
*     input values in a box of width "box", centred on the output value.
*
*     It optionally also returns the mean and standard deviation of the
*     data values in each filter box.
*
*     The method attempts to be efficient in that it avoids sorting the
*     list of values in the filter box for every output value. Instead, it
*     modifies the filter box for the previous output value - which is
*     known already to be sorted - by removing the oldest value in the box
*     and inserting a new value value at the correct place in the list.

*  Authors:
*     David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     25-MAR-2010 (DSB):
*        Original version.
*     18-MAY-2010 (DSB):
*        Fix buffer over-run caused by incorrect value for ihi that sets
*        the end of the main iout loop.
*     28-JUN-2010 (DSB):
*        Added argument "filter_type".
*     2010-06-29 (TIMJ):
*        Use an enum for filter_type
*     2010-07-13 (DSB):
*        Added arguments wlim, mean and sigma.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2010 Science & Technology Facilities Council.
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
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* Starlink includes */
#include "ast.h"
#include "mers.h"
#include "sae_par.h"
#include "prm_par.h"

/* SMURF includes */
#include "libsmf/smf.h"

/* Other includes */
#include <gsl/gsl_sort.h>

void smf_median_smooth( dim_t box, smf_filt_t filter_type, float wlim,
                        dim_t el, const double *dat, const smf_qual_t *qua,
                        size_t stride, smf_qual_t mask, double *out,
                        double *w1, double *w2, double *mean, double *sigma,
                        int *status ){

/* Local Variables: */
   const double *pdat;         /* Pointer to next bolo data value */
   const smf_qual_t *pqua;     /* Pointer to next quality flag */
   dim_t ibox;                 /* Index within box */
   dim_t ihi;                  /* Upper limit for which median can be found */
   dim_t inbox;                /* No. of values currently in the box */
   dim_t iold;                 /* Index of oldest value in "w2" */
   dim_t iout;                 /* Index within out array */
   dim_t minin;                /* Min no of valid i/p values for a valid o/p value */
   double *mout;               /* Pointer to next output mean value */
   double *pout;               /* Pointer to next output median value */
   double *sout;               /* Pointer to next output sigma value */
   double *pw1;                /* Pointer to next "w1" value */
   double *pw2;                /* Pointer to next "w2" value */
   double dnew;                /* Data value being added into the filter box */
   double dold;                /* Data value being removed from the filter box */
   double meanval;             /* Mean of values in filter box */
   double outval;              /* Main output filter value */
   double sum;                 /* Sum of values in filter box */
   double sum2;                /* Sum of squared values in filter box */
   int iadd;                   /* Index within box at which to store new value */
   int iremove;                /* Index within box of element to be removed */
   int offset;                 /* Offset from next new value to central value */

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Check the filter type. */
   if (filter_type != SMF__FILT_MEDIAN &&
       filter_type != SMF__FILT_MIN &&
       filter_type != SMF__FILT_MAX ) {
      *status = SAI__ERROR;
      msgSeti( "T", filter_type );
      errRep( " ", "smf_median_smooth: Illegal filter type value ^T "
              "(internal SMURF programming error).", status );
   }

/* Store the minimum number of valid input values that must be present in
   a filter box to create a valid output value. */
   if( wlim >= 0 && wlim <= 1.0 ) {
      minin = wlim*box + 0.5;
      if( minin == 0 ) {
         minin = 1;
      } else if( minin > box ) {
         minin = box;
      }
   } else {
      minin = 0;
   }

/* First initialise the filter box to contain the first "box" values from the
   supplied array. Do not store bad or flagged values in the filter box. The
   good values are stored at the start of the "w1" array, with no gaps.
   The "w2" array holds all values in the box, good or bad, in the order
   they occur in the time-series (i.e. un-sorted). */
   pdat = dat;
   pqua = qua;
   pw1 = w1;
   pw2 = w2;
   sum = 0.0;
   sum2 = 0.0;
   for( ibox = 0; ibox < box; ibox++ ) {

      if( ( !qua || !( *pqua & mask ) ) && *pdat != VAL__BADD ) {
         *(pw2++) = *(pw1++) = *pdat;
         sum += *pdat;
         sum2 += ( *pdat )*( *pdat );
      } else {
         *(pw2++) = VAL__BADD;
      }

/* Get pointers to the next data and quality values. */
      pdat += stride;
      pqua += stride;
   }

/* Initialise the index at which to store the next data value in the
   "w2" array. The first new value added to the box will over-write element
   zero - the oldest value in the box. */
   iold = 0;

/* Note the number of good values stored in the filter box. */
   inbox = (int)( pw1 - w1 );

/* If there are any bad data values, pad out the w1 array with bad
   values. */
   for( ibox = inbox; ibox < box; ibox++ ) w1[ ibox ] = VAL__BADD;

/* If any good values are stored in the filter box, we now sort them. */
   if( inbox > 0 ) gsl_sort( w1, 1, inbox );

/* Fill the first half-box of the output array(s) with bad values. */
   ihi = box/2;
   mout = mean;
   sout = sigma;
   pout = out;
   for( iout = 0; iout < ihi; iout++ ) {
      *(pout++) = VAL__BADD;
      if( sigma ) *(sout++) = VAL__BADD;
      if( mean ) *(mout++) = VAL__BADD;
   }

/* Note the offset from the input value that is to be added into the filter
   box next, and the current central input value. */
   offset = -stride*(int)( ( box + 1 )/2 );

/* Loop round the rest of the output array, stopping just before the
   final half box. "iout" gives the index of the centre element in the box,
   or the element just above centre if "box" is even. */
   ihi = el - ( box + 1 )/2;
   for( ; iout < ihi; iout++ ) {

/* Store the median value of the current box in the output array. If the
   current box contains an odd number of good values, use the central good
   value as the median value. If the box contains an even number of good
   values, use the mean of the two central values as the median value. If
   the box contains insufficient good values use VAL__BADD. If the
   central input value is bad, use VAL__BADD. */
      if( inbox == 0 ) {
         outval = VAL__BADD;

      } else if( minin == 0 && ( pdat[ offset ] == VAL__BADD ||
                          ( qua && ( pqua[ offset ] & mask ) ) ) ){
         outval = VAL__BADD;

      } else if( inbox < minin ) {
         outval = VAL__BADD;

      } else if( filter_type == SMF__FILT_MIN ) {
         outval = w1[ 0 ];

      } else if( filter_type == SMF__FILT_MAX ) {
         outval = w1[ inbox - 1 ];

      } else if( inbox % 2 == 1 ) {
         outval = w1[ inbox/2 ];

      } else {
         ibox = inbox/2;
         outval = 0.5*( w1[ ibox ] + w1[ ibox - 1 ] );
      }

      *(pout++) = outval;

/* Store the optional mean value. */
      meanval = ( inbox > 0 ) ? sum/inbox : VAL__BADD;

      if( mean ) {
         if( outval != VAL__BADD ) {
            *(mout++) = meanval;
         } else {
            *(mout++) = VAL__BADD;
         }
      }

/* Store the optional standard deviation value. */
      if( sigma ) {
         if( outval != VAL__BADD ) {
            *(sout++) = sqrt( sum2/inbox - meanval*meanval );
         } else {
            *(sout++) = VAL__BADD;
         }
      }

/* Now advance the box by one sample and update the supplied values
   accordingly. Get the data value for the time slice that is about to
   enter the filter box. Set it bad if it is flagged in the quality array. */
      dnew = *pdat;
      if( qua && ( *pqua & mask ) ) dnew = VAL__BADD;

/* Get the data value for the time slice that is about to leave the filter
   box. */
      dold = w2[ iold ];

/* Store the new value in "w2" in place of the old value, and then
   increment the index of the next "w2" value to be removed, wrapping back
   to the start when the end of the array is reached. */
      w2[ (iold)++ ] = dnew;
      if( iold == box ) iold = 0;

/* If the new value to be added into the box is good... */
      if( dnew != VAL__BADD ) {

/* Increment sums. */
         sum += dnew;
         sum2 += dnew*dnew;

/* Find the index (iadd) within the w1 box at which to store the new
   value so as to maintain the ordering of the values in the box. Could do
   a binary chop here, but the box size is presumably going to be very
   small and so it's probably not worth it. At the same time, look for
   the value that is leaving the box (if it is not bad). */
         iremove = -1;
         iadd = -1;

         if( dold != VAL__BADD ) {
            for( ibox = 0; ibox < inbox; ibox++ ) {
               if( iremove == -1 && w1[ ibox ] == dold ) {
                  iremove = ibox;
                  if( iadd != -1 ) break;
               }
               if( iadd == -1 && w1[ ibox ] >= dnew ) {
                  iadd = ibox;
                  if( iremove != -1 ) break;
               }
            }

/* Decrement sums. */
            sum -= dold;
            sum2 -= dold*dold;

         } else {
            for( iadd = 0; iadd < (int) inbox; iadd++ ) {
               if( w1[ iadd ] >= dnew ) break;
            }
         }

/* If the new value is larger than any value currently in w1, we add it
   to the end. */
         if( iadd == -1 ) iadd = inbox;

/* If the value being removed is bad, shuffle all the good values greater
   than the new value up one element, and increment the number of good
   values for this bolometer box. */
         if( iremove == -1 ) {
            for( ibox = inbox; (int) ibox > iadd; ibox-- ) {
               w1[ ibox ] = w1[ ibox - 1 ];
            }
            (inbox)++;

/* If the value being removed is good and the value being added is greater
   than the value being removed, shuffle all the intermediate values down
   one element within the box. */
         } else if( (int) iadd > iremove ) {
            for( ibox = iremove; (int) ibox < iadd - 1; ibox++ ) {
               w1[ ibox ] = w1[ ibox + 1 ];
            }
            iadd--;

/* If the value being removed is good and the value being added is less
   than or equal to the value being removed, shuffle all the intermediate
   values up one element within the box. */
         } else {
            for( ibox = iremove; (int) ibox > iadd; ibox-- ) {
               w1[ ibox ] = w1[ ibox - 1 ];
            }
         }

/* Store the new value in the box. */
         w1[ iadd ] = dnew;

/* If the value being added is bad but the value being removed is good... */
      } else if( dold != VAL__BADD ){

/* Decrement sums. */
         sum -= dold;
         sum2 -= dold*dold;

/* Find the index (iremove) within the w1 box at which is stored the value
   that is leaving the box. */
         iremove = -1;
         for( iremove = 0; iremove < (int) inbox; iremove++ ) {
            if( w1[ iremove ] == dold ) break;
         }

/* Move all the larger values down one element in "w1" to fill the gap
   left by the removal. */
         for( iremove++; iremove < (int) inbox; iremove++ ) {
            w1[ iremove - 1 ] = w1[ iremove ];
         }

/* Over-write the un-used last element with a bad value, and decrement
   the number of values in "w1". */
         w1[ iremove - 1 ] = VAL__BADD;
         (inbox)--;
      }

/* Increment the pointers. */
      pdat += stride;
      pqua += stride;
   }

/* Fill the last half-box of the output array(s) with bad values. */
   for( ; iout < el; iout++ ) {
      *(pout++) = VAL__BADD;
      if( sigma ) *(sout++) = VAL__BADD;
      if( mean ) *(mout++) = VAL__BADD;
   }

}

