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
*                             const smf_qual_t *qua, dim_t stride,
*                             smf_qual_t mask, double *out, double *w1,
*                             dim_t *w2, dim_t *w3, int *status )

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
*     stride = dim_t (Given)
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
*     w2 = dim_t * (Given and Returned)
*        A work array of length "box".
*     w3 = dim_t * (Given and Returned)
*        A work array of length "box".
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
*     2010-07-19 (DSB):
*        Re-implement using a binary chop to search the sorted list.
*        Remove arguments mean and sigma which turned out not to be
*        needed.
*     22-NOV-2013 (DSB):
*        Ensure the box is no larger than the size of the array.
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
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

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
                        dim_t stride, smf_qual_t mask, double *out,
                        double *w1, dim_t *w2, dim_t *w3, int *status ){

/* Local Variables: */
   const double *pdat;         /* Pointer to next bolo data value */
   const smf_qual_t *pqua;     /* Pointer to next quality flag */
   dim_t *pw2;                 /* Pointer to next sorted data index */
   dim_t ibox;                 /* Index within box */
   dim_t ihi;                  /* Upper limit for which median can be found */
   dim_t inbox;                /* No. of values currently in the box */
   dim_t iold;                 /* Index of oldest value in "w2" */
   dim_t iout;                 /* Index within out array */
   dim_t jbox;                 /* Index within box */
   dim_t jhi;                  /* Index of last element in search box */
   dim_t jlo;                  /* Index of first element in search box */
   dim_t jtest;                /* Index of element at centre of search box */
   dim_t minin;                /* Min no of valid i/p values for a valid o/p value */
   dim_t newi;                 /* Index within "w1" at which to store new value */
   dim_t off;                  /* Vector index of ne wvalue */
   dim_t offset;               /* Offset from next new value to central value */
   dim_t oldi;                 /* Index within "w1" of the old value */
   double *pout;               /* Pointer to next output median value */
   double *pw1;                /* Pointer to next sorted data value */
   double dnew;                /* Data value being added into the filter box */
   double outval;              /* Main output filter value */
   size_t *perm;               /* Initial permutation array */

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

/* Check the box size. */
   if ( box == 0 && *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRep( " ", "smf_median_smooth: Box is zero.", status );
   }

/* Limit the box to the size of the data array. */
   if( box > el ) box = el;

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

/* Initialise the number of good values in the initial filter box. */
   inbox = 0;

/* Get an index that sorts the first "box" data values into ascending
   order. This includes bad values, and flagged values. Returned "perm"
   values are in the range 0 to "box". */
   perm = astMalloc( box*sizeof( *perm ) );
   if( *status == SAI__OK ) {
      gsl_sort_index( perm, dat, stride, (dim_t) box );

/* Loop round each element in the first filter box. */
      for( ibox = 0; ibox < box; ibox++ ) {

/* Get the vector index of the i'th largest value in the filter box
   (which may be a bad or flagged value since GSL does not recognise
   these). */
         jbox = perm[ ibox ];
         off = stride*jbox;

/* Get the value. Use bad if the element is flagged. */
         if( !qua || !( qua[ off ] & mask ) ) {
            dnew = dat[ off ];
         } else {
            dnew = VAL__BADD;
         }

/* If the value is not bad, store at the next free element of the w1 array.
   Also, store its original index (i.e. its index within the box) in "w2",
   and store the inverse look-up in "w3". That is, "w2" gives box index
   as a function of sorted index, and "w3" gives sorted index as a
   function of box index. */
         if( dnew != VAL__BADD ) {
            w1[ inbox ] = dnew;
            w2[ inbox ] = jbox;
            w3[ jbox ] = inbox;
            inbox++;
         } else {
            w3[ jbox ] = -1;
         }

      }

/* Free the perm array since we will not be sorting explicitly again. */
      perm = astFree( perm );
   }

/* Initialise the box index of the oldest value in the filter box. */
   iold = 0;

/* If there are any bad data values, pad out the w1 array with bad
   values, and w2 array with -1 values. */
   for( ibox = inbox; ibox < box; ibox++ ) {
      w1[ ibox ] = VAL__BADD;
      w2[ ibox ] = -1;
   }

/* Fill the first half-box of the output array with bad values. */
   ihi = box/2;
   pout = out;
   for( iout = 0; iout < ihi; iout++ ) *(pout++) = VAL__BADD;

/* Note the offset from the input value that is to be added into the filter
   box next, and the current central input value. */
   offset = -stride*( ( box + 1 )/2 );

/* Initialise pointers to the next data and quality value to enter the
   filter box. */
   pdat = dat + stride*box;
   pqua = qua + stride*box;

/* Loop round the rest of the output array, stopping just before the
   final half box. "iout" gives the index of the centre element in the box,
   or the element just above centre if "box" is even. */
   ihi = el - ( box + 1 )/2;
   for( ; iout < ihi; iout++ ) {

/* Store the median or other required value of the current box in the
   output array. If the current box contains an odd number of good values,
   use the central good value as the median value. If the box contains an
   even number of good values, use the mean of the two central values as
   the median value. If the box contains insufficient good values use
   VAL__BADD. If the central input value is bad, use VAL__BADD. */
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

/* Now advance the box by one sample. Get the data value for the time
   slice that is about to enter the filter box. Set it bad if it is
   flagged in the quality array. */
      dnew = *pdat;
      if( qua && ( *pqua & mask ) ) dnew = VAL__BADD;

/* Find the index at which the new value should be stored within the
   sorted filter box (w1). If the new value is bad, we do not put it into
   the filter box. Just store a new index of -1 to indicate this. */
      if( dnew == VAL__BADD ) {
         newi = -1;

/* If the new value is larger than the largest value currently in the
   filter box, it goes at the end of w1. */
      } else if( inbox == 0 || dnew >= w1[ inbox - 1 ] ) {
         newi = inbox;

/* If the new value is smaller than the smallest value currently in the
   filter box, it goes at the start of w1. */
      } else if( dnew <= w1[ 0 ] ) {
         newi = 0;

/* Otherwise do a binary chop on the values in the "w2" array in order to
   find the index at which the new value should be stored. */
      } else {

/* Initialise index of the first element that is still within the search
   range. */
         jlo = 0;

/* Initialise index of the last element that is still within the search
   range. */
         jhi = inbox - 1;

/* Loop until the search range contains only a single element. */
         while( jhi - jlo > 1 ) {

/* Get the index of the mid value to test. */
            jtest = ( jlo + jhi ) /2;

/* If the mid value in the current search range is greater than or equal
   to the new value, the new value must lie in the lower half (including
   the test point itself in the upper half). Restrict the search range
   to the lower half. */
            if( w1[ jtest ] >= dnew ) {
               jhi = jtest;

/* If the mid value in the current search range is less than the new
   value, the new value must lie in the upper half. Restrict the search range
   to the upper half. */
            } else {
               jlo = jtest;
            }
         }

/* The new value goes in in front of the "jhi" value. */
         newi = jhi;
      }

/* Get the index within w1/w2 of the oldest value in the filter box. */
      oldi = w3[ iold ];

/* If the new value being added into the box is good... */
      if( newi >= 0 ) {

/* If the old value being removed from the box is good... */
         if( oldi >= 0 ) {

/* If the value being removed is larger than the value being added, we
   need to shunt the intermediate values up, over-writing the old value,
   to make room for the new value. */
            if( oldi > newi ) {
               pw1 = w1 + oldi - 1;
               pw2 = w2 + oldi - 1;
               for( ibox = oldi; ibox > newi; ibox--,pw1--,pw2--) {
                  pw1[ 1 ] = *pw1;
                  w3[ ( pw2[ 1 ] = *pw2 ) ] = ibox;
               }

/* If the value being removed is smaller than the value being added, we
   need to shunt the intermediate values down, over-writing the old value,
   to make room for the new value. */
            } else if( newi > oldi ) {
               newi--;
               pw1 = w1 + oldi;
               pw2 = w2 + oldi;
               for( ibox = oldi; ibox < newi; ibox++,pw1++,pw2++ ) {
                  *pw1 = pw1[ 1 ];
                  w3[ ( *pw2 = pw2[ 1 ] ) ] = ibox;
               }
            }

/* Then store the new value, and its associated indices. */
            w1[ newi ] = dnew;
            w2[ newi ] = iold;
            w3[ iold ] = newi;

/* If the new value is good but the old value is bad, shunt the higher
   values up to open up a slot for the new value. */
         } else {
            for( ibox = inbox; ibox > newi; ibox-- ) {
               w1[ ibox ] = w1[ ibox - 1 ];
               w2[ ibox ] = w2[ ibox - 1 ];
               w3[ w2[ ibox ] ] = ibox;
            }

/* Then store the new value, and its associated indices. */
            w1[ newi ] = dnew;
            w2[ newi ] = iold;
            w3[ iold ] = newi;

/* Increment the number of good values in the filter box. */
            inbox++;
         }

/* If the new value is bad but the old value is good, shunt the higher
   values down to over-write the old value. */
      } else if( oldi >= 0 ) {
         for( ibox = oldi + 1; ibox < inbox; ibox++ ) {
            w1[ ibox - 1 ] = w1[ ibox ];
            w2[ ibox - 1 ] = w2[ ibox ];
            w3[ w2[ ibox - 1 ] ] = ibox - 1;
         }

/* Decrement the number of good values in the filter box. */
         inbox--;

/* Then store the new value, and its associated indices. */
         w1[ inbox ] = dnew;
         w2[ inbox ] = -1;
         w3[ iold ] = -1;

/* If the new value is bad but the old value is also bad, do nothing. */
      }

/* Increment the index of the oldest element in the filter box. If we hit
   the end of the "w3" array, start again at the beginning. */
      if( ++iold == box ) iold = 0;

/* Increment the pointers. */
      pdat += stride;
      pqua += stride;
   }

/* Fill the last half-box of the output array with bad values. */
   for( ; iout < el; iout++ ) *(pout++) = VAL__BADD;

}

