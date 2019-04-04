#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"

void ndf1True( size_t el, int larray[], int *status ){
/*
*+
*  Name:
*     ndf1True

*  Purpose:
*     Set all elements of a vectorised logical array to non-zero.

*  Synopsis:
*     void ndf1True( size_t el, int larray[], int *status )

*  Description:
*     This function sets all elements of the vectorised logical array
*     supplied to the value non-zero.

*  Parameters:
*     el
*        Number of array elements to be set.
*     larray
*        Returned holding the logical array. The supplied "larray" array
*        should have at least "el" elements.
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

/* Loop through the array, setting all elements to non-zero. */
   for( i = 0; i < el; i++ ) larray[ i ] = 1;

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1True", status );

}

