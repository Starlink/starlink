#include "sae_par.h"
#include "ary1.h"
#include "star/hds.h"
#include <string.h>

void ary1Dp2s( AryDCB *dcb, int *status ) {
/*
*+
*  Name:
*     ary1Dp2s

*  Purpose:
*     Convert a data object from primitive to simple form.

*  Synopsis:
*     void ary1Dp2s( AryDCB *dcb, int *status )

*  Description:
*     The routine converts a data object, identified by its DCB entry,
*     from primitive to simple form. The DCB entry is updated to
*     reflect the change.

*  Notes:
*     -  This routine does not create an ORIGIN component in the array
*     structure.

*  Parameters:
*     dcb
*        The DCB object to be converted.
*     status
*        The global status.

* Prior Requirements:
*     - The DCB mutex must be locked.

*  Implementation Deficiencies:
*     -  This routine requires the data type of the HDS object to
*     change. Therefore, it cannot be used if the object is a top level
*     object.

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
   HDSLoc *loc = NULL;        /* New object locator */
   HDSLoc *locp = NULL;       /* Parent structure locator */
   char defer;                /* Defer creation of data arrys(s)? */
   char name[DAT__SZNAM+1];   /* Data object name */
   char tname[DAT__SZNAM+1];  /* Temporary component name */
   hdsdim dummy;              /* Dummy dimension array */

   ARY__DCB_ASSERT_MUTEX;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure that data type information and component locators are available
   for the data object in the DCB. */
   ary1Dtyp( dcb, status );

/* Annul the non-imaginary data component locator (for primitive objects,
   the main data object locator will point to the same object). */
   if( dcb->dloc ){
      datAnnul( &dcb->dloc, status );
      defer = 0;
   } else {
      defer = 1;
   }

/* Obtain the data object name within this structure. */
   datName( dcb->loc, name, status );

/* Obtain a locator to the parent structure which contains the data object. */
   datParen( dcb->loc, &locp, status );

/* Generate a temporary component name which will not clash with any
   existing components in the parent structure and rename the primitive
   data object. */
   ary1Tcnam( locp, tname, status );
   datRenam( dcb->loc, tname, status );

/* Create an empty ARRAY component with the original name. */
   dummy = 0;
   datNew( locp, name, "ARRAY", 0, &dummy, status );

/* Obtain a locator to the new component. */
   datFind( locp, name, &loc, status );

/* Annul the parent structure locator. */
   datAnnul( &locp, status );

/* Move the original primitive array into the DATA component within the new
   ARRAY structure. Store the locator to the resulting new "simple" array
   in the DCB. */
   if( defer ){
      datAnnul( &dcb->loc, status );
   } else {
      datMove( &dcb->loc, loc, "DATA", status );
   }
   dcb->loc = loc;

/* Obtain a locator to the non-imaginary data component and store it in the
   DCB. */
   if( !defer ){
      datFind( dcb->loc, "DATA", &dcb->dloc, status );
   }

/* Note the array form is now 'SIMPLE'. */
   strcpy( dcb->form, "SIMPLE" );

/* Note whether data type and form information are now available in the
   DCB. */
   dcb->kform = dcb->ktype = (*status == SAI__OK);

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) ary1Trace( "ary1Dp2s", status );

}
