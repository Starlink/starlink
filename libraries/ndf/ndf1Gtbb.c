#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"

void ndf1Gtbb( NdfACB *acb, unsigned char *badbit, int *status ){
/*
*+
*  Name:
*     ndf1Gtbb

*  Purpose:
*     Get the effective bad-bits mask value for an ACB entry.

*  Synopsis:
*     void ndf1Gtbb( NdfACB *acb, unsigned char *badbit, int *status )

*  Description:
*     This function returns the effective bad-bits mask value to be applied
*     to the quality component of an ACB entry. It takes account of any
*     override value which may have been applied.

*  Parameters:
*     acb
*        Pointer to the NDF entry in the ACB.
*     *badbit
*        Returned holding the unsigned byte bad-bits value.
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

/* If an override bad-bits value has been set for the ACB entry, then
   return that value. */
   if( acb->isqbb ) {
      *badbit = acb->qbb;
   } else {

/* Otherwise, ensure that quality information is available in the DCB
   and ACB. */
      ndf1Qimp( acb, status );
      if( *status == SAI__OK ) {

/* Return the DCB bad-bits value. */
         dcb = acb->dcb;
         *badbit = dcb->qbb;
      }
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Gtbb", status );

}

