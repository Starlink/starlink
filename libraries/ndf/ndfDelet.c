#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf.h"
#include "mers.h"

void ndfDelet_( int *indf, int *status ){
/*
*+
*  Name:
*     ndfDelet

*  Purpose:
*     Delete an NDF.

*  Synopsis:
*     void ndfDelet( int *indf, int *status )

*  Description:
*     This function deletes the specified NDF. If this is a base NDF, then
*     the associated data object is erased and all NDF identifiers which
*     refer to it (or to sections derived from it) become invalid. If any
*     NDF components are mapped for access, then they are first unmapped.
*     If an NDF section is specified, then this function is equivalent to
*     calling ndfAnnul, and no other identifiers are affected.

*  Parameters:
*     *indf
*        Identifier for the NDF to be deleted. A value of NDF__NOID is
*        returned (as defined in the header file "ndf.h").
*     *status
*        The global status.

*  Notes:
*     -  This function attempts to execute even if "status" is set on
*     entry, although no further error report will be made if it
*     subsequently fails under these circumstances.

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
   int tstat;            /* Temporary status variable */

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Save the "status" value and mark the error stack. */
   tstat = *status;
   errMark();

/* Import the NDF identifier. */
   *status = SAI__OK;
   ndf1Impid( *indf, &acb, status );

/* Check that DELETE access to the NDF is available. */
   ndf1Chacc( acb, "DELETE", status );

/* If access is available, then perform a deletion operation on the ACB
   entry. Reset the identifier value. */
   if( *status == SAI__OK ) ndf1Del( &acb, status );
   *indf = NDF__NOID;

/* Annul any error if "status" was previously bad, otherwise let the new
   error report stand. Release the error stack. */
   if( *status != SAI__OK ) {
      if( tstat != SAI__OK ) {
         errAnnul( status );
         *status = tstat;

/* Report context information and call error tracing function if
   appropriate. */
      } else {
         errRep( " ", "ndfDelet: Error deleting an NDF.", status );
         ndf1Trace( "ndfDelet", status );
      }
   } else {
      *status = tstat;
   }

/* Release error stack. */
   errRlse();

/* Restablish the original AST status pointer */
   NDF_FINAL

}

