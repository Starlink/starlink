#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf.h"
#include "mers.h"

void ndfLock_( int indf, int *status ){
/*
*+
*  Name:
*     ndfLock

*  Purpose:
*     Lock an NDF for use by the current thread.

*  Synopsis:
*     void ndfLock( int indf, int *status )

*  Description:
*     This function locks the base NDF associated with the supplied
*     NDF identifier for sole use by the current thread. An error will
*     be reported if another thread subsequently attempts to access the
*     base NDF through any identifier.

*  Parameters:
*     indf
*        NDF identifier.
*     *status
*        The global status.

*  Notes:
*     - This function returns without action if the NDF is already
*     locked by the currently running thread.
*     - This function will report an error if the supplied NDF has not
*     previously been unlocked by calling function ndfUnlock.
*     - The supplied NDF identifier will be imported into the current NDF
*     context within the currently running thread.
*     -  This function attempts to execute even if "status" is set on
*     entry, although no further error report will be made if it
*     subsequently fails under these circumstances.

*  Copyright:
*     Copyright (C) 2019 East Asian Observatory
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
*     DSB: David S. Berry (EAO)

*  History:
*     3-APR-2019 (DSB):
*        Original version.

*-
*/

/* Local Variables: */
   NdfACB *acb;          /* Pointer to NDF entry in the ACB */

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Begin a new error reporting context. */
   errBegin( status );

/* Import the NDF identifier. Cannot use ndf1Impid since the supplied
   identifier will usually be unlocked, causing ndf1Impid to report an
   error. So first convert the identifier to an ACB pointer. */
   acb = (NdfACB *) ndf1Id2ac( indf, 1 );

/* If a valid ACB pointer was not returned, then report an error. */
   if( acb == NULL ) {
      *status = NDF__IDINV;
      msgSeti( "INDF", indf );
      errRep( " ", "NDF identifier invalid; its value is ^INDF (possible "
              "programming error).", status );
   }

/* Attempt to lock the ACB. */
   ndf1LockACB( acb, status );

/* If an error occurred, report context information and call the error
   tracing function. */
   if( *status != SAI__OK ) {
      errRep( " ", "ndfLock: Error locking an NDF for sole use by the "
              "current thread.", status );
      ndf1Trace( "ndfLock", status );
   }

/* End the error reporting context. */
   errEnd( status );

/* Restablish the original AST status pointer */
   NDF_FINAL
}

