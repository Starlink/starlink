#include <stdlib.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf.h"
#include "mers.h"

void ndfCmsg_( const char *token, int indf, const char *comp, int *status ){
/*
*+
*  Name:
*     ndfCmsg

*  Purpose:
*     Assign the value of an NDF character component to a message token.

*  Synopsis:
*     void ndfCmsg( const char *token, int indf, const char *comp, int *status )

*  Description:
*     This function assigns the value of the specified character component
*     of an NDF to a message token, for use in constructing messages using
*     the MSG_ or ERR_ functions (see SUN/104).

*  Parameters:
*     token
*        Pointer to a null terminated string holding the name of the
*        message token.
*     indf
*        NDF identifier.
*     comp
*        Pointer to a null terminated string holding the name of the
*        character component whose value is to be used: "LABEL", "TITLE" or
*        "UNITS".
*     *status
*        The global status.

*  Notes:
*     -  If the specified NDF component does not have a defined value, then
*     the string "<undefined>" is assigned to the token instead.

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
   NdfACB *acb;          /* Pointer to NDF entry in the DCB */
   NdfDCB *dcb;          /* Pointer to data object entry in the DCB */
   int iccomp;           /* Indentifier for character component */
   char *pntr;           /* Pointer to mapped data */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Import the NDF identifier. */
   ndf1Impid( indf, &acb, status );

/* Validate the component name. */
   ndf1Vccn( comp, &iccomp, status );
   if( *status == SAI__OK ) {

/* Obtain an index to the data object entry in the DCB. */
      dcb = acb->dcb;

/* Ensure that information about the required character component is
   available in the DCB. */
      ndf1Dc( dcb, iccomp, status );
      if( *status == SAI__OK ) {

/* If the component is not present in the NDF, then assign an
   appropriate value to the message token. */
         if( !dcb->cloc[ iccomp ] ) {
            msgSetc( token, "<undefined>" );

/* If it is present, then map it and determine its length. */
         } else {
            pntr = ndf1Hmp0C( dcb->cloc[ iccomp ], status );

/* Assign the component's value to the message token and then unmap it. */
            if( pntr ) {
               msgSetc( token, pntr );
               pntr = astFree( pntr );
            }
            ndf1Hunmp( dcb->cloc[ iccomp ], status );
         }
      }
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndfCmsg", status );

/* Restablish the original AST status pointer */
   NDF_FINAL

}
