/*
*  Name:
*     cnfLock.c

*  Purpose:
*     Manage the mutex used to synchronise calls to Fortran from C

*  Description:
*     This module implements functions which lock and unlock a global
*     mutex, allowing calls to Frotran from C to be synchronised.

*  Copyright:
*     Copyright (C) 2011 Science and Technology Facilties Council.

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
*     DSB: David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*      11-MAY-2011 (DSB):
*        Original version.
*     {enter_changes_here}
*/

/* Set XOPEN_SOURCE to 500 in order to provide the recursive mutex
   facilties. */
#define _XOPEN_SOURCE 500

/* Header files. */
/* ============= */
#if HAVE_CONFIG_H
#include <config.h>
#endif
#if USE_PTHREADS
#include <pthread.h>
#endif

/* Static variables for this module. */
/* ================================= */
/* If the pthreads library is available define a mutex that can be used
   to syncronise all calls to fortran from C. This is a recursive mutex,
   meaning that when the mutex is locked by a thread, that same thread
   can lock it again without causing a dead-lock. We need this to guard
   against cases where C calls Fortran, which then calls C, which then
   calls Fortran. With a non-recursive mutex, the first level of Fortran
   call would lock the mutex, causing the second level of Fortran call
   to freeze when it attempts to lock the same mutex. */
#if USE_PTHREADS
static pthread_once_t cnf_is_initialized = PTHREAD_ONCE_INIT;
static pthread_mutexattr_t cnf_mutex_attr;
static pthread_mutex_t cnf_mutex;
#endif

/* Function prototypes. */
/* ==================== */
static void cnf1Initialise( void );


/* Function definitions. */
/* ===================== */
void cnfLock( void ) {
/*
*+
*  Name:
*     cnfLock

*  Purpose:
*     Lock the CNF mutex

*  Invocation:
*     cnfLock();

*  Description:
*     This function locks the CNF mutex. The calling thread will block if
*     the mutex is already locked by a different thread.

*-
*/

/* Do nothing if the pthreads library is not available. */
#if USE_PTHREADS

/* Ensure the cnf recursive mutex has been initialised. */
   (void) pthread_once( &cnf_is_initialized, cnf1Initialise );

/* Lock the mutex. */
   pthread_mutex_lock( &cnf_mutex );

#endif
}

void cnfUnlock( void ) {
/*
*+
*  Name:
*     cnfUnlock

*  Purpose:
*     Lock the CNF mutex

*  Invocation:
*     cnfUnlock();

*  Description:
*     This function unlocks the CNF mutex.

*-
*/

/* Do nothing if the pthreads library is not available. */
#if USE_PTHREADS

/* Unlock the mutex */
   pthread_mutex_unlock( &cnf_mutex );

#endif
}

static void cnf1Initialise( void ) {
/*
*  Name:
*     cnf1Initialise

*  Purpose:
*     Initialise the CNF mutex

*  Invocation:
*     cnf1Initialise();

*  Description:
*     This function initialises the CNF global mutex, and sets its
*     attributes so that it is a recursive mutex.

*/
/* Do nothing if the pthreads library is not available. */
#if USE_PTHREADS

   pthread_mutexattr_init( &cnf_mutex_attr );
   pthread_mutexattr_settype( &cnf_mutex_attr, PTHREAD_MUTEX_RECURSIVE );
   pthread_mutex_init( &cnf_mutex, &cnf_mutex_attr );
#endif
}
