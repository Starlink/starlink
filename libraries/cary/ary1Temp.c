#include <pthread.h>
#include <stdio.h>
#include "sae_par.h"
#include "ary1.h"
#include "ary_err.h"
#include "star/hds.h"
#include "mers.h"

/* Mutex used to serialise access to the following global variables */
static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
static HDSLoc *tmploc = NULL;
static size_t count = 0;



void ary1Temp( const char *type, int ndim, const hdsdim *dim, HDSLoc **loc,
               int *status ) {
/*
*+
*  Name:
*     ary1Temp

*  Purpose:
*     Create a temporary HDS object.

*  Synopsis:
*     void ary1Temp( const char *type, int ndim, const hdsdim *dim,
*                    HDSLoc **loc, int *status )

*  Description:
*     The routine creates a temporary HDS object with the specified
*     type and shape. On the first invocation a temporary structure is
*     created to contain such objects. Subsequently, temporary objects
*     are created within this enclosing structure.

*  Parameters:
*     type
*        HDS type of object to be created.
*     ndim
*        Number of object dimensions.
*     dim
*        Pointer to array of object dimensions.
*     loc
*        Address at which to return a locator to the temporary object.
*     status
*        The global status.

*  Notes:
*     -  This routine is a work-around to avoid the problems associated
*     with calling datTemp if the objects created must subsequently be
*     erased.

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
   char name[10*DAT__SZNAM];  /* Temporary object name */

/* Set an initial value for the LOC argument. */
   *loc = NULL;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Since this function uses global variables, we serialise access to it
   by requiring each thread to acquire a mutex lock before proceeding. */
   pthread_mutex_lock( &mutex );

/* Increment the count of temporary objects created. */
   count++;

/* Before creating the first object, create a temporary enclosing structure
   and tune HDS to expect a large number of components in it. */
   if( count == 1 ){
      datTemp( "ARY_TEMP", 0, NULL, &tmploc, status );
      hdsTune( "NCOMP", 20, status );
   }

/* Form a unique name for the temporary object. */
   if( *status == SAI__OK ){
      if( sprintf( name, "ARY_%zu", count ) > DAT__SZNAM ) {
         *status = ARY__FATIN;
         errRep( " ", "ary1Temp: Too many temporary objects created",
                 status );
      }

/* Create an object inside the enclosing structure and obtain a locator to
   it. */
      datNew( tmploc, name, type, ndim, dim, status );
      datFind( tmploc, name, loc, status );
   }

/* Unlock the mutex. */
   pthread_mutex_unlock( &mutex );

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) ary1Trace( "ary1Temp", status );

}
