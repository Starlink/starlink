#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf.h"
#include "mers.h"

void ndfHcopy_( int indf1, int indf2, int *status ){
/*
*+
*  Name:
*     ndfHcopy

*  Purpose:
*     Copy history information from one NDF to another.

*  Synopsis:
*     void ndfHcopy( int indf1, int indf2, int *status )

*  Description:
*     This function copies history information from one NDF to another,
*     replacing any that already exists in the destination NDF.

*  Parameters:
*     indf1
*        Identifier for the NDF (or NDF section) containing the history
*        information to be copied.
*     indf2
*        Identifier for the NDF to receive the copied history information.
*     *status
*        The global status.

*  Notes:
*     - This function returns without action leaving the destination NDF
*     unchanged if no History component exists in the input NDF.
*     - If the input NDF contains a History component, then a History
*     component is added to the destination NDF automatically, if one does
*     not already exist.

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
   NdfACB *acb1;         /* ACB for 1st NDF */
   NdfACB *acb2;         /* ACB for 2nd NDF */
   NdfDCB *dcb1;         /* DCB for 1st data object */
   NdfDCB *dcb2;         /* DCB for 2nd data object */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Import the two identifiers. */
   ndf1Impid( indf1, &acb1, status );
   ndf1Impid( indf2, &acb2, status );

/* Obtain indices to the DCB entries of the two data object. */
   dcb1 = acb1->dcb;
   dcb2 = acb2->dcb;

/* Ensure history structure information is available for the input
   NDF. */
   ndf1Dh( dcb1, status );

/* Use the component locator to determine the state of the History
   component in the input NDF. Only proceed if it is defined. */
   if( *status == SAI__OK ) {
      if( dcb1->hloc ) {

/* Reset any pre-existing History component in the destination NDF. */
         ndf1Rst( acb2, "HISTORY", status );

/* Copy the History component from input to output. */
         ndf1Hprp( dcb1, 1, dcb2, status );
      }
   }

/* If an error occurred, then report context information and call the
   error tracing function. */
   if( *status != SAI__OK ) {
      errRep( " ", "ndfHcopy: Error copying history information from one "
              "NDF to another.", status );
      ndf1Trace( "ndfHcopy", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}

