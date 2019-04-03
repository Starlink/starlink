#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"

void ndf1Vsta( NdfACB *acb, int *state, int *status ){
/*
*+
*  Name:
*     ndf1Vsta

*  Purpose:
*     Determine the state of the variance component of an NDF.

*  Synopsis:
*     void ndf1Vsta( NdfACB *acb, int *state, int *status )

*  Description:
*     This function returns a logical value indicating if the variance
*     component of an NDF is defined. The NDF is identified by its ACB
*     entry.

*  Parameters:
*     acb
*        Pointer to the NDF's ACB entry.
*     *state
*        Returned holding the whether the variance component's values are
*        defined (non-zero for defined, zero for undefined).
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
*     xxx (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   NdfDCB *dcb;          /* Pointer to data object entry in the DCB */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure that variance information is available in the DCB and ACB. */
   ndf1Vimp( acb, status );

/* Obtain an index to the data object entry in the DCB. */
   dcb = acb->dcb;

/* See if the variance array identifier is valid. If not, then the
   component is undefined. */
   *state = aryValid( dcb->vid, status );
   if( *status == SAI__OK ) {

/* If the identifier is valid, then see if the array has defined
   values. */
      if( *state ) aryState( dcb->vid, state, status );
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Vsta", status );

}

