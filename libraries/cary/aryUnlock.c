#include "sae_par.h"
#include "ary1.h"
#include "mers.h"

void aryUnlock( Ary *ary, int *status ) {
/*
*+
*  Name:
*     aryUnlock

*  Purpose:
*     Unlock an array so that it can be locked by a different thread.

*  Synopsis:
*     aryUnlock( Ary *ary, int *status );

*  Description:
*     This function ensures that the current thread does not have a
*     lock of any type on the supplied array. See aryLock.
*
*     The array must be locked again, using aryLock, before it can
*     be used by any other ARY function. All arrays are initially
*     locked by the current thread when they are first created or
*     opened.

*  Parameters:
*     ary
*        Pointer to to the array to be unlocked.
*     status
*        Pointer to global status.

*  Notes:
*     - No error is reported if the supplied array is currently locked
*     for read-only or read-write access by another thread.
*     - The majority of ARY functions will report an error if the array
*     supplied to the function has not been locked for use by the calling
*     thread. The exceptions are the functions that manage these locks -
*     aryLock, aryUnlock and aryLocked.
*     - Attempting to unlock an array that is not locked by the current
*     thread has no effect, and no error is reported. The aryLocked
*     function can be used to determine if the current thread has a lock
*     on the array.

*  Copyright:
*     Copyright (C) 2017 East Asian Observatory.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S Berry (DSB)
*     {enter_new_authors_here}

*  History:
*     27-JUL-2017 (DSB):
*        Initial version
*     {enter_further_changes_here}

*-
*/

/* Local Variables; */
   AryACB *acb;

/* Check inherited status. */
   if (*status != SAI__OK) return;

/* Import and validate the array identifier, but do not include the usual
   check that the array is locked by the current thread since we'll be
   handling that as part of this function. */
   acb = (AryACB *) ary1Impid( ary, 0, 0, 1, status );

/* Attempt to unlock the specified array. Test status first so that we know
   it is safe to deference "acb". */
   if( *status == SAI__OK ) {
      (void) ary1DCBLock( acb->dcb, 3, 0, status );
   }

/* If an error occurred, then report context information and call the
   error tracing routine. */
   if( status != SAI__OK ) {
      errRep( "ARY_UNLOCK_ERR", "aryUnlock: Error unlocking an array.",
              status );
      ary1Trace( "ary1Unlock", status );
   }
}

