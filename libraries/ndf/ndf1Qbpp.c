#include "sae_par.h"
#include "ndf1.h"

void ndf1Qbpp( unsigned char badbit, size_t el, const unsigned char qual[],
               int *bad, int *status ){
/*
*+
*  Name:
*     ndf1Qbpp

*  Purpose:
*     Determine if a vectorised quality array contains bad pixels.

*  Synopsis:
*     void ndf1Qbpp( unsigned char badbit, size_t el, const unsigned char
*                    qual[], int *bad, int *status )

*  Description:
*     This function examines a vectorised array of unsigned byte quality
*     values using a bad-bits mask and determines if any of the quality
*     values give a non-zero result when a bit-wise "AND" is performed with
*     the mask.

*  Parameters:
*     badbit
*        Unsigned byte bad-bits mask.
*     el
*        Number of quality array elements to examine.
*     qual
*        Array of unsigned byte quality values. The supplied "qual" array
*        should have at least "el" elements.
*     *bad
*        Returned holding the whether any non-zero result is obtained when
*        performing a bit-wise "AND" of the quality values with the bad-
*        bits mask.
*     *status
*        The global status.

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
   size_t i;                /* Loop counter for array elements */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Initialise. */
   *bad = 0;

/* If the bad-bits value is non-zero, loop to check each array element. */
   if( badbit != 0 ) {
      for( i = 0; i < el; i++ ){

/* Evaluate the quality masking function and set "bad" if appropriate.
   Quit checking once "bad" is set. */
         if( !NDF_QMASK( qual[ i ], badbit ) ) {
            *bad = 1;
            break;
         }
      }
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Qbpp", status );

}

