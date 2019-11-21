#include "ast.h"
#include "f77.h"
#include "sae_par.h"
#include "mers.h"

F77_SUBROUTINE(ndg1_alock)( LOGICAL(LOCK), INTEGER(OBJECT), LOGICAL(UNLOCKED),
                            INTEGER(STATUS) ){
/*
*+
*  Name:
*     ndg1_alock

*  Purpose:
*     Locks or unlocks an AST object

*  Invocation:
*     CALL NDG1_ALOCK( LOCK, OBJECT, LOCKED, STATUS )

*  Description:
*     This routine locks or unlocks an AST Object, returning a flag
*     indicating if the object was originally locked or not.

*  Arguments:
*     LOCK = LOGICAL (Given)
*        If .TRUE., lock the Object. Otherwise, unlock the Object.
*     OBJECT = INTEGER (Given)
*        Pointer to the AST Object.
*     UNLOCKED = LOGICAL (Returned)
*        Returned .TRUE. if the Object was unlocked on entry.
*     STATUS = INTEGER (Given and Returned)
*        The inherited status.

*  Notes;
*     - It is assumed that AST is already watching the supplied status
*     variable.

*  Copyright:
*     Copyright (C) 2019 East Asian Observatory
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public Licence as
*     published by the Free Software Foundation; either version 2 of
*     the Licence, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public Licence for more details.
*
*     You should have received a copy of the GNU General Public Licence
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S. Berry (EAO)

*  History:
*     18-NOV-2019 (DSB):
*        Original version
*/
   GENPTR_LOGICAL(LOCK)
   GENPTR_INTEGER(OBJECT)
   GENPTR_LOGICAL(UNLOCKED)
   GENPTR_INTEGER(STATUS)

/* Local Variables */
   AstObject *object;
   int plock;
   int olock;

/* Initialise */
   *UNLOCKED = F77_TRUE;

/* Check inherited status */
   if( *STATUS != SAI__OK ) return;

/* Import the Object pointer. Ignore NULL pointers.  */
   object = astI2P( *OBJECT );
   if( object ) {

/* See which thread, if any, currently has the actual Object locked. */
      olock = astThread( object, 0 );

/* See which thread, if any, currently has the supplied Object pointer
   locked. */
      plock = astThread( object, 1 );

/* Set the returned flag indicating if the object and pointer are both
   unlocked on entry. */
      *UNLOCKED = ( olock == AST__UNLOCKED && plock == AST__UNLOCKED ) ? F77_TRUE:F77_FALSE;

/* Report an error if another thread has either the object or the pointer
   locked. */
      if( olock == AST__OTHER || plock == AST__OTHER ) {
         if( astOK ) {
            *STATUS = SAI__ERROR;
            errRepf( " ", "NDG_ALOCK: Supplied AST Object is already locked by "
                    "another thread.", STATUS );
         }

/* Otherwise, if required, ensure both the Object and pointer are locked
   by the current thread. */
      } else if( *LOCK ) {
         astLock( object, 0 );

/* Otherwise, ensure both the Object and pointer are unlocked. */
      } else {
         astUnlock( object, 0 );
      }
   }
}

