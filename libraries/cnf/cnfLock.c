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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     DSB: David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*      11-MAY-2011 (DSB):
*        Original version.
*     {enter_changes_here}
*/


/* Header files. */
/* ============= */
#include <config.h>
#if USE_PTHREADS
#include <pthread.h>
#endif

/* Static variables for this module. */
/* ================================= */
/* If the pthreads library is available define a mutex that can be used
   to syncronise all calls to fortran from C. */
#if USE_PTHREADS
static pthread_mutex_t cnf_mutex = PTHREAD_MUTEX_INITIALIZER;
#endif

/* Function prototypes. */
/* ==================== */


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
#if USE_PTHREADS
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
#if USE_PTHREADS
   pthread_mutex_unlock( &cnf_mutex );
#endif
}

