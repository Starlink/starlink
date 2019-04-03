#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ndf.h"
#include "mers.h"
#include <string.h>

void ndfXloc_( int indf, const char *xname, const char *mode, HDSLoc **loc,
              int *status ){
/*
*+
*  Name:
*     ndfXloc

*  Purpose:
*     Obtain access to a named NDF extension via an HDS locator.

*  Synopsis:
*     void ndfXloc( int indf, const char *xname, const char *mode,
*                   HDSLoc **loc, int *status )

*  Description:
*     This function returns an HDS locator to a named extension (if
*     present) in an NDF. An error results if the specified extension is
*     not present.

*  Parameters:
*     indf
*        NDF identifier.
*     xname
*        Pointer to a null terminated string holding the name of the
*        required extension.
*     mode
*        Pointer to a null terminated string holding the mode of access
*        required to the extension: "READ", "UPDATE" or "WRITE".
*     *loc
*        Returned holding the extension locator.
*     *status
*        The global status.

*  Notes:
*     -  If WRITE access is specified, then any existing extension contents
*     or values will be erased or reset, so that the extension is ready to
*     receive new values. If UPDATE access is specified, then existing
*     values will be retained so that they may be modified.
*     -  It is the caller's responsibility to annul the HDS locator issued
*     by this function (e.g. by calling "datAnnul") when it is no longer
*     required. The NDF_ system will not perform this task itself.
*     -  Although this function will check the access mode value supplied
*     against the available access to the NDF, HDS does not allow the
*     returned locator to be protected against write access in the case
*     where WRITE access to the NDF is available, but only READ access was
*     requested.  In this case it is the responsibility of the caller to
*     respect the locator access restriction.
*     -  If this function is called with "status" set, then an invalid
*     locator will be returned for the "loc" parameter, although no further
*     processing will occur. The same value will also be returned if the
*     function should fail for any reason.

*  Implementation Deficiencies:
*     -  At present, although the requested access mode will be checked
*     against the available access to the NDF, the returned locator may
*     allow WRITE access, even when only READ access was requested. This is
*     because HDS does not currently provide a means of explicitly
*     protecting a locator against write operations.

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
   NdfACB *acb;          /* Pointer to NDF entry in the ACB */
   NdfDCB *dcb;          /* Pointer to data object entry in the DCB */
   char vmode[ NDF__SZMOD + 1 ];   /* Validated access mode */
   hdsbool_t there;      /* Whether the extension is there */

/* Set an initial value for the "loc" parameter. */
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

/* Check the extension name. */
   ndf1Chxnm( xname, 1, 0, status );
   if( *status == SAI__OK ) {

/* Obtain an index to the data object entry in the DCB. */
      dcb = acb->dcb;

/* Ensure that extension information is available in the DCB. */
      ndf1Dx( dcb, status );
      if( *status == SAI__OK ) {

/* If there is no extension (MORE) structure, then the requested
   extension component cannot be there, so report an error. */
         if( !dcb->xloc ) {
            *status = NDF__NOEXT;
            msgSetc( "XNAME", xname );
            ndf1Amsg( "NDF", acb );
            errRep( " ", "There is no '^XNAME' extension in the NDF "
                    "structure ^NDF", status );

/* Otherwise, see if the requested extension component is present. */
         } else {
            datThere( dcb->xloc, xname, &there, status );
            if( *status == SAI__OK ) {

/* If present, obtain a locator to it. */
               if( there ) {
                  datFind( dcb->xloc, xname, loc, status );

/* If write access was requested, then reset the extension value(s). */
                  if( !strcmp( vmode, "WRITE" ) ) ndf1Hrst( *loc, status );

/* If absent, report an error. */
               } else {
                  *status = NDF__NOEXT;
                  msgSetc( "XNAME", xname );
                  ndf1Amsg( "NDF", acb );
                  errRep( " ", "There is no '^XNAME' extension in the NDF "
                          "structure ^NDF", status );
               }
            }
         }
      }
   }

/* If an error occurred, then report context information and call the
   error tracing function. */
   if( *status != SAI__OK ) {
      errRep( " ", "ndfXloc: Error obtaining access to a named NDF "
              "extension via an HDS locator.", status );
      ndf1Trace( "ndfXloc", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}

