#include <stdlib.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"

void ndf1Temp( const char *type, int ndim, const hdsdim dim[],
               HDSLoc **loc, int *status ){
/*
*+
*  Name:
*     ndf1Temp

*  Purpose:
*     Create a temporary HDS object.

*  Synopsis:
*     void ndf1Temp( const char *type, int ndim, const hdsdim dim[],
*                    HDSLoc **loc, int *status )

*  Description:
*     This function creates a temporary HDS object with the specified type
*     and shape. On the first invocation a temporary structure is created
*     to contain such objects. Subsequently, temporary objects are created
*     within this enclosing structure.

*  Parameters:
*     type
*        Pointer to a null terminated string holding the HDS type of object
*        to be created.
*     ndim
*        Number of object dimensions.
*     dim
*        Object dimensions.
*     *loc
*        Returned holding the locator to temporary object.
*     *status
*        The global status.

*  Notes:
*     -  A value of NULL will be returned for the "loc" parameter if
*     this function is called with "status" set, although no further
*     processing will occur. The same value will also be returned if the
*     function should fail for any reason.
*     -  This function is a work-around to avoid the problems associated
*     with calling "datTemp" if the objects created must subsequently be
*     erased.

*  Copyright:
*     Copyright (C) 2018 East Asian Observatory
*     All rights reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or modify
*     it under the terms of the GNU General Public License as published by
*     the Free Software Foundation; either version 2 of the License, or (at
*     your option) any later version.
*
*     This program is distributed in the hope that it will be useful,but
*     WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*     General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     DSB: David S. Berry (EAO)

*  History:
*     3-APR-2019 (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local variables: */
   char name[ DAT__SZNAM + 1 ];    /* Temporary object name */
   hdsdim dummy;                   /* Dummy dimensions array */

/* Initialise the "loc" parameter. */
   *loc = NULL;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Lock a mutex to ensure we have sole access to the global variables used
   by this function */
   NDF__TMP_LOCK_MUTEX;

/* Increment the count of temporary objects created. */
   Ndf_TMP_count++;

/* Before creating the first object, create a temporary enclosing
   structure and tune HDS to expect a large number of components in it. */
   if( Ndf_TMP_count == 1 ) {
      datTemp( "ndfTemp", 0, &dummy, &Ndf_TMP_tmploc, status );
      hdsTune( "NCOMP", 20, status );
   }

/* Form a unique name for the temporary object. */
   if( *status == SAI__OK ) {
      sprintf( name, "NDF_%d", Ndf_TMP_count );

/* Create an object inside the enclosing structure and obtain a locator
   to it. */
      datNew( Ndf_TMP_tmploc, name, type, ndim, dim, status );
      datFind( Ndf_TMP_tmploc, name, loc, status );

/* If an error occurred, then reset the "loc" parameter. */
      if( *status != SAI__OK ) *loc = NULL;
   }

/* Unlock the mutex. */
   NDF__TMP_UNLOCK_MUTEX;

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Temp", status );

}

