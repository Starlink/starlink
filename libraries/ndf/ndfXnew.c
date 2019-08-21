#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ndf.h"
#include "mers.h"

void ndfXnew_( int indf, const char *xname, const char *type, int ndim,
              const hdsdim dim[], HDSLoc **loc, int *status ){
/*
*+
*  Name:
*     ndfXnew

*  Purpose:
*     Create a new extension in an NDF.

*  Synopsis:
*     void ndfXnew( int indf, const char *xname, const char *type,
*                   int ndim, const hdsdim dim[], HDSLoc **loc, int *status )

*  Description:
*     This function creates a new named extension of specified type and
*     shape in an NDF structure, and returns an HDS locator to it.

*  Parameters:
*     indf
*        NDF identifier.
*     xname
*        Pointer to a null terminated string holding the extension name.
*     type
*        Pointer to a null terminated string holding the HDS data type of
*        the extension.
*     ndim
*        Number of extension dimensions.
*     dim
*        Extension dimension sizes.
*     *loc
*        Returned holding the locator to the newly created extension.
*     *status
*        The global status.

*  Notes:
*     -  If this function is called with "status" set, then an invalid
*     locator will be returned for the "loc" parameter, although no further
*     processing will occur. The same value will also be returned if the
*     function should fail for any reason.

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
   hdsbool_t there;      /* Whether extension component exists */

/* Set an initial value for the "loc" parameter. */
   *loc = NULL;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Import the NDF identifier. */
   ndf1Impid( indf, &acb, status );

/* Check that WRITE access to the NDF is available. */
   ndf1Chacc( acb, "WRITE", status );

/* Check the extension name. */
   ndf1Chxnm( xname, 1, 0, status );
   if( *status == SAI__OK ) {

/* Obtain an index to the data object entry in the DCB. */
      dcb = acb->dcb;

/* Ensure that extension information is available in the DCB. */
      ndf1Dx( dcb, status );
      if( *status == SAI__OK ) {

/* If an extension (MORE) structure does not exist, then create one and
   obtain a locator to it, storing this in the DCB. */
         if( !dcb->xloc ) {
            datNew( dcb->loc, "MORE", "EXT", 0, NULL, status );
            datFind( dcb->loc, "MORE", &dcb->xloc, status );

/* Note whether the required extension component already exists,
   enquiring this information if necessary. */
            there = 0;
         } else {
            datThere( dcb->xloc, xname, &there, status );
         }

/* If it already exists, then report an error. */
         if( *status == SAI__OK ) {
            if( there ) {
               *status = NDF__XISTS;
               msgSetc( "XNAME", xname );
               ndf1Amsg( "NDF", acb );
               errRep( " ", "A '^XNAME' extension already exists in the "
                       "NDF structure ^NDF", status );

/* Otherwise, create the extension component and obtain a locator to it. */
            } else {
               datNew( dcb->xloc, xname, type, ndim, dim, status );
               datFind( dcb->xloc, xname, loc, status );
            }
         }
      }
   }

/* If an error occurred, then report context information and call the
   error tracing function. */
   if( *status != SAI__OK ) {
      msgSetc( "XNAME", xname );
      errRep( " ", "ndfXnew: Error creating a new extension named ^XNAME "
              "in an NDF.", status );
      ndf1Trace( "ndfXnew", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}

