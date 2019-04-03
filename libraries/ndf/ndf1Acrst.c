#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"

void ndf1Acrst( int iax, int iccomp, NdfACB *acb, int *status ){
/*
*+
*  Name:
*     ndf1Acrst

*  Purpose:
*     Reset an axis character component to an undefined state.

*  Synopsis:
*     void ndf1Acrst( int iax, int iccomp, NdfACB *acb, int *status )

*  Description:
*     This function resets an axis character component of an NDF to an
*     undefined state by erasing the associated data object.

*  Parameters:
*     iax
*        Axis number.
*     iccomp
*        Axis character component identifier: ndfAlab or ndfAuni (as
*        defined in the header file "ndf1.h").
*     acb
*        Pointer to the NDF entry in the ACB.
*     *status
*        The global status.

*  Notes:
*     -  This function can only be used to reset an axis character
*     component via a base NDF. If an NDF section is supplied, then it will
*     return without action. No error will result.

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

/* Check if the NDF is a section. There is nothing to do if it is. */
   if( !acb->cut ) {

/* Obtain an index to the data object entry in the DCB. */
      dcb = acb->dcb;

/* Ensure that axis character component information is available in the
   DCB. */
      ndf1Dac( iax, iccomp, dcb, status );
      if( *status == SAI__OK ) {

/* If the character component exists, then annul its locator and erase
   it. */
         if( dcb->acloc[ iax ][ iccomp ] ) {
            datAnnul( dcb->acloc[ iax ] + iccomp, status );
            datErase( dcb->aloc[ iax ], Ndf_DCB_accn[ iccomp ], status );
         }
      }
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Acrst", status );

}

