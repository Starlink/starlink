#include <stdlib.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"

void ndf1Qmlog( unsigned char badbit, size_t el, const unsigned char
                qual[], int larray[], int *bad, int *status ){
/*
*+
*  Name:
*     ndf1Qmlog

*  Purpose:
*     Convert a vectorised quality mask into a logical array.

*  Synopsis:
*     void ndf1Qmlog( unsigned char badbit, size_t el, const unsigned char
*                     qual[], int larray[], int *bad, int *status )

*  Description:
*     This function converts a vectorised array holding an unsigned byte
*     quality mask into a logical array. The logical values are derived by
*     performing a bit-wise "AND" operation between each quality value and
*     an unsigned byte bad-bits mask and then testing if the result is
*     equal to zero. The resulting logical values are assigned to the
*     output array; non-zero means that the corresponding NDF pixel is to
*     be accepted for processing by subsequent algorithms and zero means
*     that it should be rejected.

*  Parameters:
*     badbit
*        The unsigned byte bad-bits mask.
*     el
*        Number of array elements to process.
*     qual
*        Array of quality values. The supplied "qual" array should have at
*        least "el" elements.
*     larray
*        Returned holding the array of logical values. The supplied
*        "larray" array should have at least "el" elements.
*     *bad
*        Whether the quality mask resulted in the rejection of any pixels.
*        This value is set to non-zero if any of the "larray" values
*        returned are set to zero. Otherwise it is set to zero.
*     *status
*        The global status.

*  Notes:
*     -  This function loops through the arrays in a backward direction.
*     This is to minimise paging on a virtual memory machine, since this
*     function will usually be followed by a processing loop which passes
*     through the same arrays in the forward direction.

*  Copyright:
*     Copyright (C) 2018 East Asian Observatory
*     All rights reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or modify
*     it under the terms of the GNU General Public License as published by
*     the Free Software Foundation; either version 2 of the License, or (at
*     your option) any later version.
*
*     This program is distributed in the hope that it will be useful,but
*     WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*     General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     DSB: David S. Berry (EAO)

*  History:
*     3-APR-2019 (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   int i;                /* 1st loop counter for array elements */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Initialise. */
   *bad = 0;

/* If the mask is zero, then fill the logical array with non-zero values. */
   if( badbit == 0 ) {
      ndf1True( el, larray, status );

/* Loop to process each array element. */
   } else {
      i = el;
      while( i > 0 ) {
         i--;

/* Evaluate the quality masking function and assign the result to the
   logical array. */
         if( NDF_QMASK( qual[ i ], badbit ) ) {
            larray[ i ] = 1;
         } else {
            larray[ i ] = 0;

/* Note if any zero values are generated. */
            *bad = 1;

/* Having detected a zero value, further assignments to the "bad"
   parameter can be eliminated, so quit this loop to process the
   remaining array elements without further assignments. */
            break;
         }
      }

/* If a zero value has been produced, then process any remaining
   array elements without making further assignments to the "bad"
   parameter. */
      if( *bad ) {
         while( i > 0 ) {
            i--;
            if( NDF_QMASK( qual[ i ], badbit ) ) {
               larray[ i ] = 1;
            } else {
               larray[ i ] = 0;
            }
         }
      }
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Qmlog", status );

}

