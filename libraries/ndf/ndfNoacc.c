#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ndf.h"
#include "ndf_ast.h"
#include "mers.h"

void ndfNoacc_( const char *access, int indf, int *status ){
/*
*+
*  Name:
*     ndfNoacc

*  Purpose:
*     Disable a specified type of access to an NDF.

*  Synopsis:
*     void ndfNoacc( const char *access, int indf, int *status )

*  Description:
*     This function disables the specified type of access to an NDF, so
*     that any subsequent attempt to access it in that way will fail.
*     Access restrictions imposed on an NDF identifier by this function
*     will be propagated to any new identifiers derived from it, and cannot
*     be revoked.

*  Parameters:
*     access
*        Pointer to a null terminated string holding the type of access to
*        be disabled: "BOUNDS", "DELETE", "MODIFY", "SHIFT", "TYPE" or
*        "WRITE".
*     indf
*        NDF identifier.
*     *status
*        The global status.

*  Notes:
*     Disabling each type of access imposes the following restrictions on
*     an NDF:
*     -  "BOUNDS" prevents the pixel-index bounds of a base NDF from being
*     altered.
*     -  "DELETE" prevents an NDF from being deleted.
*     -  "MODIFY" prevents any form of modification to the NDF (i.e. it
*     disables all the other access types).
*     -  "SHIFT" prevents pixel-index shifts from being applied to a base
*     NDF.
*     -  "TYPE" prevents the data type of any NDF components from being
*     altered.
*     -  "WRITE" prevents new values from being written to the NDF, or the
*     state of any of its components from being reset.

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

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Import the NDF identifier. */
   ndf1Impid( indf, &acb, status );
   if( *status == SAI__OK ) {

/* Check for each type of access in turn and reset the appropriate
   access control flag. */

/* ...BOUNDS access; prevents the NDF's bounds from being altered. */
      if( astChrMatch( access, "BOUNDS" ) ) {
         acb->access &= ~NDF__ACC_BOUND;

/* ...DELETE access; prevents the NDF being deleted. */
      } else if( astChrMatch( access, "DELETE" ) ) {
         acb->access &= ~NDF__ACC_DELET;

/* ...MODIFY access; prevents any form of modification to the NDF. */
      } else if( astChrMatch( access, "MODIFY" ) ) {
         acb->access &= ~NDF__ACC_BOUND;
         acb->access &= ~NDF__ACC_DELET;
         acb->access &= ~NDF__ACC_SHIFT;
         acb->access &= ~NDF__ACC_TYPE;
         acb->access &= ~NDF__ACC_WRITE;

/* ...SHIFT access; prevents pixel index shifts from being applied to
   the NDF. */
      } else if( astChrMatch( access, "SHIFT" ) ) {
         acb->access &= ~NDF__ACC_SHIFT;

/* ...TYPE access; prevents the NDF's data type being altered. */
      } else if( astChrMatch( access, "TYPE" ) ) {
         acb->access &= ~NDF__ACC_TYPE;

/* ...WRITE access; inhibits the writing of new data values or the
   resetting of the NDF components" state. */
      } else if( astChrMatch( access, "WRITE" ) ) {
         acb->access &= ~NDF__ACC_WRITE;

/* If the access type was not recognised, then report an error. */
      } else {
         *status = NDF__ACCIN;
         msgSetc( "BADACC", access );
         errRep( " ", "Invalid access type '^BADACC' specified (possible "
                 "programming error).", status );
      }
   }

/* If an error occurred, then report context information and call the
   error tracing function. */
   if( *status != SAI__OK ) {
      errRep( " ", "ndfNoacc: Error disabling a specified type of access "
              "to an NDF.", status );
      ndf1Trace( "ndfNoacc", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}

