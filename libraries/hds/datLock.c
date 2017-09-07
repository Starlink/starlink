/*
*+
*  Name:
*     datLock

*  Purpose:
*     Lock an object for exclusive use by the current thread.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     datLock( HDSLoc *locator, int recurs, int readonly, int *status );

*  Arguments:
*     locator = HDSLoc * (Given)
*        Locator to the object that is to be locked.
*     recurs = int (Given)
*        If the supplied object is locked successfully, and "recurs" is
*        non-zero, then an attempt is made to lock any component objects
*        contained within the supplied object. No error is reported if
*        any components cannot be locked due to being locked already by
*        a different thread - such components are left unchanged. This
*        operation is recursive - any children of the child components
*        are also locked, etc.
*     readonly = int (Given)
*        If non-zero, the object (and child objects if "recurs" is non-zero)
*        is locked for read-only access. Otherwise it is locked for
*        read-write access.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This is temporary stub for a new HDS-V5 function that locks an HDS
*     object for use by the current thread. It simply reports an error if
*     called.

*  Authors:
*     DSB: David S Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     7-SEP-2017 (DSB):
*        Initial version
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2017 East Asian Observatory.
*     All Rights Reserved.

*  Licence:
*     Redistribution and use in source and binary forms, with or
*     without modification, are permitted provided that the following
*     conditions are met:
*
*     - Redistributions of source code must retain the above copyright
*       notice, this list of conditions and the following disclaimer.
*
*     - Redistributions in binary form must reproduce the above
*       copyright notice, this list of conditions and the following
*       disclaimer in the documentation and/or other materials
*       provided with the distribution.
*
*     - Neither the name of the {organization} nor the names of its
*       contributors may be used to endorse or promote products
*       derived from this software without specific prior written
*       permission.
*
*     THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
*     CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
*     INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
*     MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
*     DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
*     CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
*     SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
*     LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
*     USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
*     AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
*     LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
*     IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
*     THE POSSIBILITY OF SUCH DAMAGE.

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#include "ems.h"
#include "sae_par.h"
#include "dat_err.h"
#include "hds_types.h"

int datLock( HDSLoc *locator, int recurs, int readonly, int *status ) {

/* Check inherited status. */
   if (*status != SAI__OK) return *status;

/* Report an error to indicate this function is not available yet. */
   *status = DAT__FATAL;
   emsRep( " ", "datLock: The datLock function has not yet been "
           "implemented (programming error).", status );

/* Return the status value. */
   return *status;
}

