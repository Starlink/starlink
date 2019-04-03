#include <stdlib.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ndf.h"
#include "ndf_ast.h"
#include "mers.h"
#include "star/util.h"
#include "star/cmp.h"

void ndfHgmod_( int indf, char *hmode, size_t hmode_length, int *status ){
/*
*+
*  Name:
*     ndfHgmod

*  Purpose:
*     Get the history update mode for an NDF.

*  Synopsis:
*     void ndfHgmod( int indf, char *hmode, size_t hmode_length, int *status )

*  Description:
*     This function returns the current history component update mode of an
*     NDF. See ndfHsmod.

*  Parameters:
*     indf
*        NDF identifier.
*     hmode
*        Pointer to an array in which to return a null terminated string
*        holding the history update mode: "DISABLED", "QUIET", "NORMAL" or
*        "VERBOSE".
*     hmode_length
*        The length of the supplied 'hmode' array. This should include
*        room for the terminating null.
*     *status
*        The global status.

*  Notes:
*     - An error is reported if the NDF has no HISTORY component.

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
   hdsbool_t there;      /* Does HISTORY component exist? */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Import the NDF identifier. */
   ndf1Impid( indf, &acb, status );
   if( *status == SAI__OK ) {

/* Obtain an index to the data object entry in the DCB and ensure that
   DCB history information is available. */
      dcb = acb->dcb;
      ndf1Dh( dcb, status );
      if( *status == SAI__OK ) {

/* Check that a history component is present and report an error if it
   is not. */
         if( !dcb->hloc ) {
            *status = NDF__NOHIS;
            ndf1Dmsg( "NDF", dcb );
            errRep( " ", "There is no history component present in the NDF "
                    "structure ^NDF (possible programming error).", status );

/* Otherwise, if the UPDATE_MODE component exists, get its value. If not,
   use a default value of NORMAL. */
         } else {
            datThere( dcb->hloc, "UPDATE_MODE", &there, status );
            if( there ) {
               cmpGet0C( dcb->hloc, "UPDATE_MODE", hmode, hmode_length,
                         status );
               astChrRemoveBlanks( hmode );
            } else {
               star_strlcpy( hmode, "NORMAL", hmode_length );
            }
         }
      }
   }

/* If an error occurred, then report context information and call the
   error tracing function. */
   if( *status != SAI__OK ) {
      errRep( " ", "ndfHgmod: Error getting the history update mode for an "
              "NDF.", status );
      ndf1Trace( "ndfHgmod", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}

