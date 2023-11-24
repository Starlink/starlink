#include <pthread.h>
#include "f77.h"

static pthread_once_t ndg_mutex_initialized = PTHREAD_ONCE_INIT;
static pthread_mutexattr_t ndg_mutex_attr;
static pthread_mutex_t ndg_mutex;

static void ndg1LockInitialize( void ) {
   pthread_mutexattr_init( &ndg_mutex_attr );
   pthread_mutexattr_settype( &ndg_mutex_attr, PTHREAD_MUTEX_RECURSIVE );
   pthread_mutex_init( &ndg_mutex, &ndg_mutex_attr );
}

F77_SUBROUTINE(ndg1_glock)( LOGICAL(lock) ){
/*
*+
*  Name:
*     ndg1_glock

*  Purpose:
*     Locks or unlocks a mutex that guards access to the globals used by NDG.

*  Invocation:
*     CALL NDG1_GLOCK( LOCK )

*  Description:
*     This routine locks or unlocks a mutex that should be used to serialise
*     access to the globals used by NDG.

*  Arguments:
*     LOCK = LOGICAL (Given)
*        If .TRUE., lock the mutex. Otherwise, unlock the mutex.

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
   GENPTR_LOGICAL(lock)

   if( *lock ) {
      pthread_once( &ndg_mutex_initialized, ndg1LockInitialize );
      pthread_mutex_lock( &ndg_mutex );
   } else {
      pthread_mutex_unlock( &ndg_mutex );
   }
}

