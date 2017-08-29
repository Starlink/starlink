#include "sae_par.h"
#include "ary1.h"
#include "ary_err.h"
#include "mers.h"

void aryLock( Ary *ary, int readonly, int *status ) {
/*
*+
*  Name:
*     aryLock

*  Purpose:
*     Lock an array for exclusive use by the current thread.

*  Synopsis:
*     aryLock( Ary *ary, int readonly, int *status );

*  Description:
*     This function locks an ARY array for use by the current thread.
*     An array can be locked for read-only access or read-write access.
*     Multiple threads can lock an array simultaneously for read-only
*     access, but only one thread can lock an array for read-write access
*     at any one time. Use of any ARY function that may modify any aspect
*     of the array - either the data values stored in the array or the
*     meta-data describing the whole array - will fail with an error unless
*     the thread has locked the array for read-write access. Use of an ARY
*     function that cannot modify the array will fail with an error unless
*     the thread has locked the array (in this case the lock can be either
*     for read-only or read-write access).
*
*     If "readonly" is zero (indicating the current thread wants to
*     modify some aspect of the array), this function will report an error
*     if any other thread currently has a lock (read-only or read-write)
*     on the array.
*
*     If "readonly" is non-zero (indicating the current thread wants
*     read-only access to the array), this function will report an error
*     only if another thread currently has a read-write lock on the array.
*
*     The current thread must unlock the array using datUnlock before it
*     can be locked for use by another thread. All arrays are initially
*     locked by the current thread when they are created or opened. The
*     type of access available to the array ("Read", "Write" or "Update")
*     determines the type of the initial lock. For pre-existing arrays,
*     this is determined by the access mode specified when it is first
*     opened. For new and temporary arrays, the initial lock is always
*     a read-write lock.

*  Parameters:
*     ary
*        Pointer to the array that is to be locked.
*     readonly
*        If non-zero, the array is locked for read-only access. Otherwise
*        it is locked for read-write access.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Notes:
*     - An error will be reported if the supplied array is currently
*     locked by another thread.
*     - The majority of ARY functions will report an error if the array
*     supplied to the function has not been locked for use by the calling
*     thread. The exceptions are the functions that manage these locks -
*     aryLock, datUnlock and aryLocked.
*     - Attempting to lock an array that is already locked by the
*     current thread will change the type of lock (read-only or
*     read-write) if the lock types differ, but will otherwise have no
*     effect.

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
*     10-JUL-2017 (DSB):
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
   acb = ary1Impid( ary, 0, 0, status );

/* Check we can de-reference "acb" safely. */
   if( *status == SAI__OK ) {

/* Attempt to lock the specified array. If the array could not be locked
   because it was already locked by another thread, report an error. */
      if( !ary1DCBLock( acb->dcb, 2, readonly, status ) ) {
         if( *status == SAI__OK ) {
            *status = ARY__THREAD;
            datMsg( "O", acb->dcb->loc );
            errRep( " ", "Array '^O' is already locked by another thread.",
                    status );
         }
      }
   }

/* If an error occurred, then report context information and call the
   error tracing routine. */
   if( status != SAI__OK ) {
      msgSetc( "U", readonly ? "read-only" : "read-write" );
      errRep( "ARY_LOCK_ERR", "aryLock: Error locking an array for ^U "
              "access by the current thread.", status );
      ary1Trace( "ary1Lock", status );
   }
}

