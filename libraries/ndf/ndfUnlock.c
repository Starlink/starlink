#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf.h"
#include "mers.h"

void ndfUnlock_( int indf, int *status ){
/*
*+
*  Name:
*     ndfUnlock

*  Purpose:
*     Unlock an NDF so it can then be locked by another thread.

*  Synopsis:
*     void ndfUnlock( int indf, int *status )

*  Description:
*     This function unlocks the base NDF associated with the supplied
*     NDF identifier so that another thread can then lock it for its
*     own use using function ndfLock. An error will be reported if the
*     supplied NDF is currently locked by another thread.
*
*     All other NDF identifiers associated with the same base NDF are
*     annulled on exit from this function.

*  Parameters:
*     indf
*        NDF identifier.
*     *status
*        The global status.

*  Notes:
*     - This function returns without action if the NDF is already
*     unlocked.
*     - This function will report an error if the supplied NDF is locked
*     by any thread other than the currently running thread.
*     - The supplied NDF identifier will be removed from the NDF context
*     associated with the currently running thread, and placed in a
*     "null" context that is ignored by the ndfEnd function. The
*     ndfReport function can be used to determine if any NDF
*     identifiers are currently in this "null" context.
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

/* Import the NDF identifier. */
   ndf1Impid( indf, &acb, status );

/* Attempt to unlock the ACB. */
   ndf1UnlockACB( acb, status );

/* If an error occurred, report context information and call the error
   tracing function. */
   if( *status != SAI__OK ) {
      errRep( " ", "ndfUnlock: Error unlocking an NDF.", status );
      ndf1Trace( "ndfUnlock", status );
   }

/* End the error reporting context. */
   errEnd( status );

/* Restablish the original AST status pointer */
   NDF_FINAL
}

