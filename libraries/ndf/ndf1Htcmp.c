#include "sae_par.h"
#include "ndf1.h"

void ndf1Htcmp( const int ymdhm1[], float sec1, const int ymdhm2[],
                float sec2, int *order, int *status ){
/*
*+
*  Name:
*     ndf1Htcmp

*  Purpose:
*     Compare two history times to determine their order.

*  Synopsis:
*     void ndf1Htcmp( const int ymdhm1[], float sec1, const int ymdhm2[],
*                     float sec2, int *order, int *status )

*  Description:
*     This function compares two times and returns an indication of which
*     is earlier or later.

*  Parameters:
*     ymdhm1
*        The years, months, days, hours and minutes fields of the first
*        time, stored in that order. The supplied "ymdhm1" array should
*        have at least "5" elements.
*     sec1
*        The seconds field of the first time.
*     ymdhm2
*        The years, months, days, hours and minutes fields of the second
*        time, stored in that order. The supplied "ymdhm2" array should
*        have at least "5" elements.
*     sec2
*        The seconds field of the second time.
*     *order
*        Returned holding the returns +1 if the second time is later than
*        the first one, -1 if the reverse is true, and 0 if both times are
*        the same.
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
   int i;                /* Loop counter for YMDHM fields */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Initialise. */
   *order = 0;

/* Inspect each field in the YMDHM arrays starting at the most
   significant (years) end. */
   for( i = 0; i < 5; i++ ){

/* Detect if the first date/time is larger. */
      if( ymdhm1[ i ] > ymdhm2[ i ] ) {
         *order = -1;
         break;

/* Detect if the second date/time is larger. */
      } else if( ymdhm1[ i ] < ymdhm2[ i ] ) {
         *order = 1;
         break;
      }
   }

/* If all the YMDHM fields are the same, then compare the seconds
   fields in the same way. */
   if( *order == 0 ) {
      if( sec1 > sec2 ) {
         *order = -1;
      } else if( sec1 < sec2 ) {
         *order = 1;
      }
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Htcmp", status );

}

