#include "sae_par.h"
#include "star/hds.h"
#include "mers.h"
#include "ary_err.h"
#include "ary1.h"

int ary1DCBLock( AryDCB *dcb, int oper, int rdonly, int *status ){
/*
*+
*  Name:
*     ary1DCBLock

*  Purpose:
*     Manage the lock on a DCB entry.

*  Synopsis:
*     int ary1DCBLock( AryDCB *dcb, int oper, int rdonly, int *status );

*  Description:
*     This function locks or unlocks the supplied DCB for read-only or
*     read-write access. The DCB is always in one of the following
*     three mutually exclusive states:
*
*     - Unlocked
*     - Locked for read-write access by one and only one thread.
*     - Locked for read-only access by one or more threads.
*
*     When locked for read-write access, the locking thread has exclusive
*     read-write access to the HDS object described by the supplied DCB.
*     When locked for read-only access, the locking thread shares read-only
*     access with zero or more other threads.

*  Arguments:
*     dcb
*        Pointer to the DCB structure.
*     oper = int (Given)
*        Operation to be performed:
*
*        1 - Return information about the current locks on the supplied DCB.
*
*           -1: Locking unsupported (e.g HDS is V4 or earlier).
*            0: unlocked;
*            1: locked for writing by the current thread;
*            2: locked for writing by another thread;
*            3: locked for reading by the current thread (other threads
*               may also have a read lock on the DCB);
*            4: locked for reading by one or more other threads (the
*               current thread does not have a read lock on the DCB);
*
*        2 - Lock the DCB for read-write or read-only use by the current
*            thread. Returns 0 if the requested lock conflicts with an
*            existing lock (in which case the request to lock the supplied
*            DCB is ignored) and +1 otherwise.
*
*        3 - Unlock the DCB. If the current thread has a lock - either
*            read-write or read-only - on the DCB, it is removed.
*            Otherwise the DCB is left unchanged. A value of +1 is always
*            returned.
*
*     rdonly = int (Given)
*        Only used if "oper" is 2. It indicates if the new lock is for
*        read-only or read-write access.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Returned function value:
*     The values returned for each operation are included in the
*     description of the "oper" argument above.

*  Notes:
*     - If the version of HDS being used does not support object locking,
*     this function will return without action for "oper" values 2 or 3
*     unless the HDS tuning parameter V4LOCKERROR is set to a non-zero
*     value, in which case an error will be reported.
*     - If a thread gets a read-write lock on the DCB, and
*     subsequently attempts to get a read-only lock, the existing
*     read-write lock will be demoted to a read-only lock.
*     - If a thread gets a read-only lock on the DCB, and
*     subsequently attempts to get a read-write lock, the existing
*     read-only lock will be promoted to a read-write lock only if
*     there are no other locks on the DCB.
*     - A value of zero is returned if an error has already ocurred, or
*     if this function fails for any reason.

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
*     26-JUL-2017 (DSB):
*        Initial version
*     {enter_further_changes_here}

*-
*/

/* Local Variables; */
   int result = 0;

/* Check inherited status. */
   if( *status != SAI__OK ) return result;

/* Check we have an HDS data object in the DCB. */
   if( !dcb->loc ) {
      *status = ARY__FATIN;
      errRepf( " ", "ary1DCBLock: The supplied DCB has no associated HDS "
               "data object - (internal ARY programming error).", status );

/* If required, inquire about any current locks on the HDS object. */
   } else if( oper == 1 ) {
      result = datLocked( dcb->loc, 0, status );

/* If required, request a lock on the HDS object. Should these locks be
   recursive? Making them recursive (i.e. putting a separate lock on each
   individual hds object within the file) could take a long time. But on
   the other hand, there is nothing to stop another thread getting a lock
   on a sub-component, which would cause problems if this thread ever
   attempted to access that same sub-component. Time will tell... */
   } else if( oper == 2 ) {
      result = datLock( dcb->loc, 0, rdonly, status );

/* If required, remove a lock on the HDS object. */
   } else if( oper == 3 ) {
      result = datUnlock( dcb->loc, 0, status );

/* Report an error for any other "oper" value. */
   } else if( *status == SAI__OK ) {
      *status = ARY__FATIN;
      errRepf( " ", "ary1DCBLock: Unknown 'oper' value (%d) supplied - "
               "(internal ARY programming error).", status, oper );
   }

/* Return the result. */
   return result;
}

