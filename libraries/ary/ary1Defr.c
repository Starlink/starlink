#include <string.h>
#include "sae_par.h"
#include "ary1.h"
#include "star/hds.h"

int ary1Defr( const AryDCB *dcb, int *status ) {
/*
*+
*  Name:
*     ary1Defr

*  Purpose:
*     See if the creation of the arrays has been deferred.

*  Synopsis:
*     int ary1Defr( const AryDCB *dcb, int *status )

*  Description:
*     Arrays created via aryDupe initially have no HDS data objects to
*     contain the real and imaginary array values. The creation of these
*     arrays is deferred until they are mapped. This is done so that
*     any changes made to the properties (e.g. type, bounds, etc) of
*     the deferred array (before it is mapped) are reflected in the size
*     of the corresponding HDS container file. If the array creation is
*     not deferred, then any changes which should produce a reduction in
*     the container file size do not in fact do so because HDS never
*     shrinks the size of a container file (it is just padded with unused
*     space).

*  Parameters:
*     dcb
*        Pointer to the DCB.
*     status
*        The global status.

* Prior Requirements:
*     -  The DCB mutex must be locked.

*  Returned function value:
*     If non-zero then the HDS arrays holding the real and imaginary
*     array values have not yet been created.

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
*     23-JUN-2017 (DSB):
*        Original version, based on equivalent Fortran routine by RFWS.

*-
*/

/* Local variables: */
   HDSLoc *loc=NULL;          /* Locator to first component */
   char name[ DAT__SZNAM+1 ]; /* Name of first component */
   int i;                     /* Component index */
   int ncomp;                 /* Number of components */
   int prim;                  /* Is the locator primitive? */
   int result;                /* Returned value */

   ARY__DCB_ASSERT_MUTEX;

/* Initialise */
   result = 0;

/* Check inherited global status. */
   if( *status != SAI__OK ) return result;

/* The array is deferred if the dcb->dloc locator (for the real component)
   is NULL and the data object is either empty or contains a single
   component called VARIANT. */
   if( !dcb->dloc ){
      datPrim( dcb->loc, &prim, status );
      if( !prim ){
         result = 1;
         datNcomp( dcb->loc, &ncomp, status );
         for( i = 1; i < ncomp+1; i++ ){
            datIndex( dcb->loc, i, &loc, status );
            datName( loc, name, status );
            if( !strcmp( name, "DATA" ) ) result = 0;
            datAnnul( &loc, status );
         }
      }
   }

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) ary1Trace( "ary1Defr", status );

/* Return the result */
   return result;
}
