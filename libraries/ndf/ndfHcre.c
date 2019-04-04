#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf.h"
#include "mers.h"

void ndfHcre_( int indf, int *status ){
/*
*+
*  Name:
*     ndfHcre

*  Purpose:
*     Ensure that a history component exists for an NDF.

*  Synopsis:
*     void ndfHcre( int indf, int *status )

*  Description:
*     This function ensures that an NDF has a history component, creating a
*     new one if necessary. No action is taken if a history component
*     already exists.

*  Parameters:
*     indf
*        NDF identifier.
*     *status
*        The global status.

*  Notes:
*     A history component may be removed from an NDF by calling ndfReset
*     with a component name of "History".

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
   NdfACB *acb;          /* Pointer to the NDF entry in the ACB */
   NdfDCB *dcb;          /* Pointer to the data object in the DCB */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Import the NDF identifier. */
   ndf1Impid( indf, &acb, status );
   if( *status == SAI__OK ) {

/* Check that WRITE access to the NDF is available. */
      ndf1Chacc( acb, "WRITE", status );

/* Obtain an index to the NDF entry in the DCB and ensure that a
   history component exists. */
      dcb = acb->dcb;
      ndf1Hdcre( dcb, status );
   }

/* If an error occurred, then report context information and call the
   error tracing function. */
   if( *status != SAI__OK ) {
      errRep( " ", "ndfHcre: Error ensuring that a history component "
              "exists for an NDF.", status );
      ndf1Trace( "ndfHcre", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}

