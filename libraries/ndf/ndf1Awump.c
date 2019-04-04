#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "mers.h"

void ndf1Awump( int iax, NdfACB *acb, int *status ){
/*
*+
*  Name:
*     ndf1Awump

*  Purpose:
*     Unmap an axis width array.

*  Synopsis:
*     void ndf1Awump( int iax, NdfACB *acb, int *status )

*  Description:
*     This function unmaps an NDF axis width array which has previously
*     been mapped for access with the ndf1Awmap function. An error will be
*     reported if the specified axis array is not currently mapped for
*     access. The NDF is identified by its index in the ACB.

*  Parameters:
*     iax
*        Zero-based index of the axis whose width array is to be unmapped.
*     acb
*        Pointer to the NDF entry in the ACB.
*     *status
*        The global status.

*  Notes:
*     -  This function will attempt to execute even if "status" is set on
*     entry, although no further error report will be made if it
*     subsequently fails under these circumstances.

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
   NdfDCB *dcb;          /* Pointer to data object entry in the DCB */
   int tstat;            /* Temporary status variable */

/* Save the "status" value and mark the error stack. */
   tstat = *status;
   errMark();

/* Obtain an index to the data object entry in the DCB. */
   *status = SAI__OK;
   dcb = acb->dcb;

/* Check that the specified axis width array is mapped for access.
   Report an error if it is not. */
   if( !acb->awmap[ iax ] ) {
      *status = NDF__NTMAP;
      msgSeti( "AXIS", iax );
      ndf1Amsg( "NDF", acb );
      errRep( " ", "The width array for axis ^AXIS of the NDF structure "
              "^NDF is not mapped for access through the specified "
              "identifier (possible programming error).", status );

/* Unmap the axis width array by annulling its ARY_ system identifier
   in the ACB (if a temporary array has been mapped, then it will be
   deleted at this point). */
   } else {
      aryAnnul( acb->awmid + iax, status );

/* If successful, then note the array is no longer mapped. */
      if( *status == SAI__OK ) {
         acb->awmap[ iax ] = 0;

/* Decrement the DCB mapping counts. */
         dcb->nawmp[ iax ]--;
         dcb->nmap--;
      }
   }

/* Annul any error if "status" was previously bad, otherwise let the new
   error report stand. */
   if( *status != SAI__OK ) {
      if( tstat != SAI__OK ) {
         errAnnul( status );
         *status = tstat;

/* Call the error tracing function if appropriate. */
      } else {
         ndf1Trace( "ndf1Awump", status );
      }
   } else {
      *status = tstat;
   }

/* Release error stack. */
   errRlse();

}

