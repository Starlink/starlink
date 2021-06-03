/*
*+
*  Name:
*     smf_median

*  Purpose:
*     Find the approximate median in an array of values between -1.0 and +1.0

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf_median( const double *array, dim_t nel, size_t stride, int bad,
*                      double *median, int *status )

*  Arguments:
*     array = const double * (Given)
*        Pointer to an array of double precision data for which the
*        median is to be returned. It is assumed that all values are in the
*        range -1.0 to +1.0. Any that are not are ignored (i.e. treated
*        as bad).
*     nel = dim_t (Given)
*        The number of elements in array.
*     stride = size_t (Given)
*        The stride between elements of the array.
*     bad = int (Given)
*        Indicates if the supplied array may contain bad values.
*     median = double * (Returned)
*        Pointer to a double in which to returned the median value in array.
*        Will be set to VAL__BADD on error.
*     status = int * (Given and Returned)
*        Inherited status value.

*  Description:
*     This function finds the median value in a given array of values that
*     fall between -1.0 and +1.0. The absolute error in the returned median
*     will be less than 1E-8.

*  Authors:
*     DSB: David S Berry (EAO)

*  History:
*     3-JUN-2021 (DSB):
*        Initial version.

*  Copyright:
*     Copyright (C) 2021 East Asian Observatory
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
*-
*/

/* Starlink includes */
#include "sae_par.h"
#include "prm_par.h"
#include "mers.h"
#include "smf.h"

#define NHIST 10000

void smf_median( const double *array, dim_t nel, size_t stride, int bad,
                 double *median, int *status ){

/* Local Variables */
   const double *pa;
   const double *pend;
   dim_t hist[ NHIST ];
   dim_t i;
   dim_t pop0;
   dim_t pop;
   dim_t target;
   dim_t totpop;
   double delta;
   double hi;
   double lo;

/* Initialise returned values. */
   *median = VAL__BADD;

/* Check the inherited status */
   if( *status != SAI__OK ) return;

/* Sanity check */
   if( !array ) {
     *status = SAI__ERROR;
     errRep( " ", "smf_median called with array NULL (possible programming"
             " error)", status);
     return;
   }

/* Special cases. */
   if( nel == 1 ) {
      *median = array[ 0 ];
      return;

   } if( nel == 2 ) {
      if( bad ) {
         if( array[ 0 ] == VAL__BADD ) {
            *median = array[ 1 ];
         } else if( array[ 1 ] == VAL__BADD ) {
            *median = array[ 0 ];
         } else {
            *median = 0.5*( array[ 0 ] + array[ 1 ] );
         }
      } else {
         *median = 0.5*( array[ 0 ] + array[ 1 ] );
      }
      return;
   }

/* Form an initial histogram with limits -1 and +1 */
   memset( hist, 0, sizeof(hist) );
   lo = -1.0;
   hi = 1.0;
   delta = ( hi - lo )/NHIST;
   pa = array;
   pend = array + nel;
   if( bad ) {
      while( pa < pend ){
         if( *pa != VAL__BADD && *pa >= lo ){
            i = ( *pa - lo )/delta;
            if( i < NHIST ) hist[ i ]++;
         }
         pa += stride;
      }
   } else {
      while( pa < pend ){
         if( *pa >= lo ){
            i = ( *pa - lo )/delta;
            if( i < NHIST ) hist[ i ]++;
         }
         pa += stride;
      }
   }

/* Find the total number of values in the histogram and then divide by
   two to get the cumulative population at the median value. */
   totpop = 0;
   for( i = 0; i < NHIST; i++ ) totpop += hist[ i ];
   if( totpop > 0 ){
      target = totpop/2;

/* Find the cell containing the median (the lower boundary of each cell
   is considered to be part of the cell). */
      pop = 0;
      for( i = 0; i < NHIST; i++ ){
         pop0 = pop;
         pop += hist[ i ];
         if( pop > target ) break;
      }

/* Form a new histogram covering just the cell that contains the median in
   the first histogram. The bin size in this histogram should be about 2E-8,
   good enough for SCUBA-2 work (assuming units of pW). */
      memset( hist, 0, sizeof(hist) );
      lo += i*delta;
      hi = lo + delta;
      delta /= NHIST;
      pa = array;
      pend = array + nel;
      if( bad ) {
         while( pa < pend ){
            if( *pa != VAL__BADD && *pa >= lo ){
               i = ( *pa - lo )/delta;
               if( i < NHIST ) hist[ i ]++;
            }
            pa += stride;
         }
      } else {
         while( pa < pend ){
            if( *pa >= lo ){
               i = ( *pa - lo )/delta;
               if( i < NHIST ) hist[ i ]++;
            }
            pa += stride;
         }
      }

/* Find the cell containing the median. Note, we know that there are "pop0"
   values below the value of the first histogram cell. */
      pop = pop0;
      for( i = 0; i < NHIST; i++ ){
         pop0 = pop;
         pop += hist[ i ];
         if( pop > target ) break;
      }

/* Return the median using linear interpolation. */
      if( i > 0 && i < NHIST - 1 ){
         *median = ( ((double)(target-pop0)/hist[i]) + i )*delta + lo;
      } else {
         *median = ( i + 0.5 )*delta + lo;
      }
   }
}
