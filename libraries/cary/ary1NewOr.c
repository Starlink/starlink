#include "sae_par.h"
#include "ary1.h"
#include "star/hds.h"
#include "ary_ast.h"
#include <stdlib.h>

void ary1NewOr( HDSLoc *loc, int ndim, HDSLoc **locor, int *status ) {
/*
*+
*  Name:
*     ary1NewOr

*  Purpose:
*     Create a new ORIGIN component.

*  Synopsis:
*     void ary1NewOr( HDSLoc *loc, int ndim, HDSLoc **locor, int *status )

*  Description:
*     The routine creates a new ORIGIN component within the supplied
*     object. The data type will be _INTEGER or _INT64, chosen to match
*     the size of the "hdsdim" data type defined by the HDS library. The
*     environment variable NDF_SHORTORIGIN is defined, the above rule is
*     over-ridden and a 4 byte _INTEGER array is created regardless of
*     the type of hdsdim. This provides a means of forcing the NDF
*     library to create NDFs that can be read by old versions of
*     starlink that expect _INTEGER origins.

*  Parameters:
*     loc
*        Locator for the object in which to create the ORIGIN component.
*     ndim
*        Number of array dimensions.
*     locor
*        Ignored if NULL. Otherwise, a locator to the new ORIGIN
*        component is returned.
*     status
*        The global status.

*  Copyright:
*      Copyright (C) 2018 East Asian Observatory
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
*     1-FEB-2018 (DSB):
*        Original version.

*-
*/

/* Initialise returned values */
   if( locor ) *locor = NULL;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* If the environment variable is defined, always create an _INTEGER
   array. */
   if( getenv("NDF_SHORTORIGIN" ) ) {
      datNew1I( loc, "ORIGIN", ndim, status );

/* Otherwise create an _INTEGER or _INT64 array to match the type of
   hdsdim. */
   } else {
      HDSDIM_CODE(datNew1)( loc, "ORIGIN", ndim, status );
   }

/* If required, return a locator to the new ORIGIN component. */
   if( locor ) datFind( loc, "ORIGIN", locor, status );

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) ary1Trace( "ary1NewOr", status );

}
