#include <stdlib.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "star/util.h"

void ndf1Tcnam( HDSLoc *loc, char *name, size_t name_length, int *status ){
/*
*+
*  Name:
*     ndf1Tcnam

*  Purpose:
*     Generate a temporary data component name.

*  Synopsis:
*     void ndf1Tcnam( HDSLoc *loc, char *name, size_t name_length, int *status )

*  Description:
*     This function generates a name which may be used to create a
*     temporary component in a specified data structure. The name is chosen
*     so that it does not clash with any existing component name.

*  Parameters:
*     loc
*        Locator to data structure in which a temporary component is
*        required.
*     name
*        Pointer to an array in which to return a null terminated string
*        holding the temporary component name.
*     name_length
*        The length of the supplied 'name' array. This should include
*        room for the terminating null.
*     *status
*        The global status.

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

/* Local Variables: */
   char tname[ DAT__SZNAM + 1 ];   /* Possible name to test */
   hdsbool_t there;      /* Whether a component exists */
   int i;                /* Counter for generating names */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Initialise. */
   there = 0;

/* Generate a name from the counter. */
   i = 1;
   sprintf( tname, "TEMP_%d", i );
   datThere( loc, tname, &there, status );

/* See if a component with the current name already exists. */
   while( ( *status == SAI__OK ) && there ){

/* If so, then increment the counter and use it to generate a new name. */
      i++;
      sprintf( tname, "TEMP_%d", i );
      datThere( loc, tname, &there, status );

   }

/* Return the name. */
   ndf1Ccpy( tname, name, name_length, status );

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Tcnam", status );

}

