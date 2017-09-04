#include "sae_par.h"
#include "ary1.h"
#include "mers.h"

int aryLocked( const Ary *ary, int *status ) {
/*
*+
*  Name:
*     aryLocked

*  Purpose:
*     See if an array is locked.

*  Synopsis:
*     locked = aryLocked( const Ary *ary, int *status );

*  Description:
*     This function returns a value that indicates if the supplied ARY
*     array has been locked for use by one or more threads. A thread can
*     lock an array either for read-only access or for read-write access.
*     The lock management functions (aryLock and aryUnlock) will ensure
*     that any thread that requests and is given a read-write lock will
*     have exclusive access to the array - no other locks of either type
*     will be issued to other threads until the first thread releases the
*     read-write lock using aryUnlock. If a thread requests and is given
*     a read-only lock, the lock management functions may issue read-only
*     locks to other threads, but it will also ensure that no other thread
*     is granted a read-write lock until all read-only locks have been
*     released.

*  Parameters:
*     ary
*        Pointer to the array to be checked.
*     status
*        Pointer to global status.

*  Returned function value:
*     A value indicating the status of the supplied array:
*
*     0: the supplied array is unlocked. This is the condition that must
*        be met for the current thread to be able to lock the supplied
*        array for read-write access using function aryLock. This condition
*        can be achieved by releasing any existing locks using aryUnlock.
*
*     1: the supplied array is locked for reading and writing by the current
*        thread. This is the condition that must be met for the current
*        thread to be able to use the supplied array in any ARY function
*        that might modify the array (except for the locking and unlocking
*        functions - see below). This condition can be achieved by calling
*        aryLock.
*
*     2: the supplied array is locked for reading and writing by a different
*        thread. An error will be reported if the current thread attempts to
*        use the array in any other ARY function.
*
*     3: the supplied array is locked read-only by the current thread (and
*        maybe other threads as well). This is the condition that must be
*        met for the current thread to be able to use the supplied array
*        in any ARY function that cannot modify the array. An error will be
*        reported if the current thread attempts to use the array in any
*        ARY function that could modify the array. This condition can be
*        achieved by calling aryLock.
*
*     4: the supplied array is not locked by the current thread, but is
*        locked read-only by one or more other threads. An error will be
*        reported if the current thread attempts to use the array in any
*        other ARY function.

*  Notes:
*     - Zero is returned as the function value if an error has already
*     occurred, or if an error occurs in this function.

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
*     DSB: David S Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     27-JUL-2017 (DSB):
*        Initial version
*     {enter_further_changes_here}

*-
*/

/* Local Variables; */
   AryACB *acb;
   int result = 0;

/* Check inherited status. */
   if (*status != SAI__OK) return result;

/* Import and validate the array identifier, but do not include the usual
   check that the array is locked by the current thread since we'll be
   performing that test as part of this function. */
   acb = (AryACB *) ary1Impid( ary, 0, 0, 1, status );

/* Get the value to return. Test status first so that we know it is safe
   to deference "acb". */
   if( *status == SAI__OK ) {
      result = ary1DCBLock( acb->dcb, 1, 0, status );
   }

/* If an error occurred, then report context information and call the
   error tracing routine. */
   if( status != SAI__OK ) {
      errRep( "ARY_LOCKED_ERR", "aryLocked: Error querying the current "
              "locks on an array.", status );
      ary1Trace( "ary1Locked", status );
   }

/* Return the result. */
   return result;
}

