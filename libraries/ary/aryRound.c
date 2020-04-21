#include "sae_par.h"
#include "mers.h"
#include <pthread.h>

/* The global variable that indicates if rounding is enabled. */
static int Ary_Round = 0;

/* A pthread read/write lock used to serialise access to the above flag.
   This allows multiple threads to perform concurrent read, but only one
   thread can write at any one time. */
static pthread_rwlock_t  Ary_Round_RWLock = PTHREAD_RWLOCK_INITIALIZER;



int aryRound( int newflg ) {
/*
*+
*  Name:
*     aryRound

*  Purpose:
*     Set the internal ARY_ system rouding flag.

*  Synopsis:
*     int aryRound( int newflg )

*  Description:
*     This function sets an internal flag in the ARY_ system that
*     controls how floating point values are converted to integer
*     values when doing array type conversion. If the flag is non-zero
*     floating point values are rounded to the nearest integer value.
*     Otherwise, floating point values are truncated towards zero.

*  Parameters:
*     newflg
*        The new value to be set for the rounding flag. If a negative
*        value is supplied, the existing value is left unchanged.

*  Returned function value:
      The previous value of the flag.

*  Notes:
*     -  By default, the rounding flag is set to zero, so floating point
*     values are truncated towards zero when being converted to integers.

*  Copyright:
*      Copyright (C) 2020 East Asian Observatory
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
*     DSB: David S. Berry (EAO)

*  History:
*     21-APR-2020 (DSB):
*        Original version.

*-
*/

/* Local Variables; */
   int result;

/* If the flag value is not being changed, acquire a read-lock for the global
   variable. This blocks if another thread currently has a write lock on the
   variable, but will continue if other threads only have read locks. */
   if( newflg < 0 ) {
      pthread_rwlock_rdlock( &Ary_Round_RWLock );

/* If the flag value is being changed, acquire a write-lock for the global
   variable. This blocks if another thread currently has a read or write
   lock on the variable. */
   } else {
      pthread_rwlock_wrlock( &Ary_Round_RWLock );
   }

/* Get the original value of the global variable. */
   result = Ary_Round;

/* If required, set the new value. */
   if( newflg >= 0 ) Ary_Round = newflg;

/* Release the lock. */
   pthread_rwlock_unlock( &Ary_Round_RWLock );

/* Return the resut. */
   return result;
}
