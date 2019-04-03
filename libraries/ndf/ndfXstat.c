#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf.h"
#include "mers.h"

void ndfXstat_( int indf, const char *xname, hdsbool_t *there, int *status ){
/*
*+
*  Name:
*     ndfXstat

*  Purpose:
*     Determine if a named NDF extension exists.

*  Synopsis:
*     void ndfXstat( int indf, const char *xname, hdsbool_t *there,
*                    int *status )

*  Description:
*     This function returns a logical value indicating whether a named
*     extension is present in an NDF.

*  Parameters:
*     indf
*        NDF identifier.
*     xname
*        Pointer to a null terminated string holding the name of the
*        extension.
*     *there
*        Returned holding the whether the extension is present in the NDF.
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

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Import the NDF identifier. */
   ndf1Impid( indf, &acb, status );

/* Check the extension name. */
   ndf1Chxnm( xname, 1, 0, status );
   if( *status == SAI__OK ) {

/* Obtain an index to the data object entry in the DCB. */
      dcb = acb->dcb;

/* Ensure that extension information is available in the DCB. */
      ndf1Dx( dcb, status );
      if( *status == SAI__OK ) {

/* If an extension structure does not exist, then neither does the
   extension component. */
         if( !dcb->xloc ) {
            *there = 0;

/* Otherwise, see if the specified component is present. */
         } else {
            datThere( dcb->xloc, xname, there, status );
         }
      }
   }

/* If an error occurred, then report context information and call the
   error tracing function. */
   if( *status != SAI__OK ) {
      msgSetc( "NAM", xname );
      errRep( " ", "ndfXstat: Error determining if an NDF extension named "
              "\"^NAM\" exists.", status );
      ndf1Trace( "ndfXstat", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}

