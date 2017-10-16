#include "star/hds.h"
#include "star/cmp.h"
#include "sae_par.h"
#include "ary1.h"

void ary1Dfppl( HDSLoc *locp, const char *name, HDSLoc **loc, int *status ) {
/*
*+
*  Name:
*     ary1Dfppl

*  Purpose:
*     Create a primitive array with an entry in the DCB.

*  Synopsis:
*     void ary1Dfppl( HDSLoc *locp, const char *name, HDSLoc **loc, int *status )

*  Description:
*     This function creates an placeholder object for a deferred primitive
*     array.

*  Parameters:
*     locp
*        Locator for the parent object in which the primitive array is to
*        be created.
*     name
*        The name for the primitive array.
*     loc
*        Returned holding a locator to the new placeholder object.
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
   int there;                /* Does component already exist? */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Check the component does not already exist. */
   datThere( locp, name, &there, status );
   if( !there ){

/* Create an ARRAY structure in the given parent object, and get a locator
   to it. */
      datNew( locp, name, "ARRAY", 0, 0, status );
      datFind( locp, name, loc, status );

/* Put a VARIANT component in here indicating that the ARRAY is a deferred
   primitive array. */
      datNew0C( *loc, "VARIANT", 9, status );
      cmpPut0C( *loc, "VARIANT", "PRIMITIVE", status );
   }

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) ary1Trace( "ary1Dfppl", status );

}
