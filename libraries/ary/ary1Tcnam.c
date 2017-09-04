#include "sae_par.h"
#include "ary1.h"
#include "star/hds.h"
#include <pthread.h>
#include <stdio.h>

/* Counter for generating names */
static int Ary_i = 0;

/* A pthread mutex is used to ensure only one thread is accessing the
   above value at any one time. */
static pthread_mutex_t  Ary_i_mutex = PTHREAD_MUTEX_INITIALIZER;


void ary1Tcnam( HDSLoc *loc, char *name, int *status ) {
/*
*+
*  Name:
*     ary1Tcnam

*  Purpose:
*     Generate a temporary data component name.

*  Synopsis:
*     void ary1Tcnam( HDSLoc *loc, char *name, int *status )

*  Description:
*     The routine generates a name which may be used to create a
*     temporary component in a specified data structure. The name is
*     chosen so that it does not clash with any existing component
*     name.

*  Parameters:
*     loc
*        Locator to data structure in which a temporary component is
*        required.
*     name
*        Temporary component name.
*     status
*        The global status.

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
*     03-JUL-2017 (DSB):
*        Original version, based on equivalent Fortran routine by RFWS.

*-
*/

/* Local variables: */
   char tname[DAT__SZNAM + 1];/* Possible name to test */
   int there;                 /* Whether a component exists */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Lock the mutex so that this thread has exclusive access to "Ary_i". */
   pthread_mutex_lock( &Ary_i_mutex );

/* Generate a name from the counter. */
   sprintf( tname, "TEMP_%d", Ary_i );

/* See if a component with the current name already exists. */
   there = 0;
   datThere( loc, tname, &there, status );

/* If so, then increment the counter and use it to generate a new name. */
   while( ( *status == SAI__OK ) && there ){
      Ary_i++;
      sprintf( tname, "TEMP_%d", Ary_i );
      datThere( loc, tname, &there, status );
   }

/* Unlock the mutex so that other threads can access "Ary_i". */
   pthread_mutex_unlock( &Ary_i_mutex );

/* Return the name. */
   ary1Ccpy( tname, sizeof(name), name, status );

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) ary1Trace( "ary1Tcnam", status );

}
