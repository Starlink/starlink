#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf.h"
#include "mers.h"

void ndfXdel_( int indf, const char *xname, int *status ){
/*
*+
*  Name:
*     ndfXdel

*  Purpose:
*     Delete a specified NDF extension.

*  Synopsis:
*     void ndfXdel( int indf, const char *xname, int *status )

*  Description:
*     This function deletes a named extension in an NDF together with its
*     contents, if any. No error results if the specified extension does
*     not exist.

*  Parameters:
*     indf
*        NDF identifier.
*     xname
*        Pointer to a null terminated string holding the name of the
*        extension to be deleted.
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
   NdfACB *acb;          /* Pointer to NDF entry in the ACB */
   NdfDCB *dcb;          /* Pointer to data object entry in the DCB */
   hdsbool_t there;      /* Whether the extension exists */
   int ncomp;            /* No. extension structure components */

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

/* Obtain an index to the data object in the DCB. */
      dcb = acb->dcb;

/* Ensure that extension information is available in the DCB. */
      ndf1Dx( dcb, status );
      if( *status == SAI__OK ) {

/* Check there is an extension (MORE) structure, otherwise the
   specified extension component cannot exist. */
         if( dcb->xloc ) {

/* Determine if the extension component exists. */
            datThere( dcb->xloc, xname, &there, status );
            if( *status == SAI__OK ) {

/* If it does, then erase it. */
               if( there ) {
                  datErase( dcb->xloc, xname, status );

/* See how many extension components remain. */
                  datNcomp( dcb->xloc, &ncomp, status );
                  if( *status == SAI__OK ) {

/* If there are none left, then annul the extension (MORE) structure
   locator and erase the structure. */
                     if( ncomp == 0 ) {
                        datAnnul( &dcb->xloc, status );
                        datErase( dcb->loc, "MORE", status );
                     }
                  }
               }
            }
         }
      }
   }

/* If an error occurred, then report context information and call the
   error tracing function. */
   if( *status != SAI__OK ) {
      errRep( " ", "ndfXdel: Error deleting a specified NDF extension.",
              status );
      ndf1Trace( "ndfXdel", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}

