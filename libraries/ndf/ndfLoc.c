#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf.h"
#include "mers.h"

void ndfLoc_( int indf, const char *mode, HDSLoc **loc, int *status ){
/*
*+
*  Name:
*     ndfLoc

*  Purpose:
*     Obtain an HDS locator for an NDF.

*  Synopsis:
*     void ndfLoc( int indf, const char *mode, HDSLoc **loc, int *status )

*  Description:
*     This function returns an HDS locator for an NDF whose identifier is
*     supplied.

*  Parameters:
*     indf
*        NDF identifier.
*     mode
*        Pointer to a null terminated string holding the mode of access
*        required to the NDF: "READ", "UPDATE" or "WRITE".
*     *loc
*        Returned holding the HDS locator to the NDF data structure.
*     *status
*        The global status.

*  Notes:
*     -  If an identifier for an NDF section is supplied to this function,
*     then the returned locator will refer to the associated base NDF.
*     -  It is the caller's responsibility to annul the locator returned by
*     this function (e.g. by calling the HDS function "datAnnul") when it
*     is no longer required. The NDF_ system will not perform this task
*     itself.
*     -  If this function is called with "status" set, then an invalid
*     locator value of DAT__NOLOC will be returned for the "loc" parameter,
*     although no further processing will occur. The same value will also
*     be returned if the function should fail for any reason. The constant
*     DAT__NOLOC is defined in the header file "dat_par.h".
*     -  Although this function will check the access mode value supplied
*     against the available access to the NDF, HDS does not allow the
*     returned locator to be protected against write access in the case
*     where WRITE access to an NDF is available, but only READ access was
*     requested. In this case it is the responsibility of the caller to
*     respect the locator access restriction.
*     -  The locator returned by this function should not be used to make
*     alterations to any part of a data structure which is simultaneously
*     being used by the NDF_ system, otherwise there is the possibility of
*     serious internal errors and data corruption.

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
   NdfACB *acb;          /* Pointer to NDF entry in the ACB */
   NdfDCB *dcb;          /* Pointer to data object entry in the DCB */
   char vmode[ NDF__SZMOD + 1 ];   /* Validated access mode */

/* Set an initial invalid value for the locator. */
   *loc = NULL;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Import the NDF identifier. */
   ndf1Impid( indf, &acb, status );

/* Validate the requested access mode string. */
   ndf1Vmod( mode, vmode, sizeof( vmode ), status );

/* Check that the requested mode of access is available. */
   ndf1Chmod( acb, vmode, status );

/* Obtain an index to the data object entry in the DCB and clone a
   locator for it. */
   if( *status == SAI__OK ) {
      dcb = acb->dcb;
      datClone( dcb->loc, loc, status );
   }

/* If an error occurred, then return an invalid locator. */
   if( *status != SAI__OK ) {
      *loc = NULL;

/* Report context information and call the error tracing function. */
      errRep( " ", "ndfLoc: Error obtaining an HDS locator for an NDF.",
              status );
      ndf1Trace( "ndfLoc", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}

