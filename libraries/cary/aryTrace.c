#include "sae_par.h"
#include "mers.h"
#include <pthread.h>

/* The global variable that indicates if error tracing is enabled. */
static int Ary_Trace = 0;

/* A pthread read/write lock used to serialise access to the above flag.
   This allows multiple threads to perform concurrent read, but only one
   thread can write at any one time. */
static pthread_rwlock_t  Ary_Trace_RWLock = PTHREAD_RWLOCK_INITIALIZER;



char aryTrace( char newflg ) {
/*
*+
*  Name:
*     aryTrace

*  Purpose:
*     Set the internal ARY_ system error-tracing flag.

*  Synopsis:
*     char aryTrace( char newflg )

*  Description:
*     This function sets an internal flag in the ARY_ system which
*     enables or disables error-tracing messages. If this flag is set
*     non-zero, then any error occurring within the ARY_ system will
*     be accompanied by error messages indicating which internal
*     routines have exited prematurely as a result. If the flag is set
*     to zero, this internal diagnostic information will not appear
*     and only standard error messages will be produced.

*  Parameters:
*     newflg
*        The new value to be set for the error-tracing flag. If a
*        negative value is supplied, the existing value is left unchanged.

*  Returned function value:
      The previous value of the flag.

*  Notes:
*     -  By default, the error tracing flag is set to zero, so
*     no internal diagnostic information will be produced.

*  Copyright:
*      Copyright (C) 2017 East Asian Observatory
*      All rights reserved.

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
*     RFWS: R.F. Warren-Smith (STARLINK)
*     DSB: David S. Berry (EAO)

*  History:
*     22-JUN-2017 (DSB):
*        Original version, based on equivalent Fortran routine by RFWS.

*-
*/

/* Local Variables; */
   int result;

/* If the flag value is not being changed, acquire a read-lock for the global
   variable. This blocks if another thread currently has a write lock on the
   variable, but will continue if other threads only have read locks. */
   if( newflg < 0 ) {
      pthread_rwlock_rdlock( &Ary_Trace_RWLock );

/* If the flag value is being changed, acquire a write-lock for the global
   variable. This blocks if another thread currently has a read or write
   lock on the variable. */
   } else {
      pthread_rwlock_wrlock( &Ary_Trace_RWLock );
   }

/* Get the original value of the global variable. */
   result = Ary_Trace;

/* If required, set the new value. */
   if( newflg >= 0 ) Ary_Trace = newflg;

/* Release the lock. */
   pthread_rwlock_unlock( &Ary_Trace_RWLock );

/* Return the resut. */
   return result;
}
