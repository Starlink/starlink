#include "sae_par.h"
#include "ary1.h"
#include "ary_ast.h"

int ary1Intyp( const char *type, int *status ) {
/*
*+
*  Name:
*     ary1Intyp

*  Purpose:
*     Is an HDS type numeric?

*  Synopsis:
*     int ary1Intyp( const char *type, int *status )

*  Description:
*     The routine returns a logical value indicating if the HDS data
*     type string supplied represents a primitive numeric type.

*  Parameters:
*     type
*        The data type string to be tested (case insensitive).
*     status
*        The global status.

*  Returned function value:
*     Whether the data type is numeric. Zero is returned unless 'type'
*     represents a primitive numeric HDS data type.

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
   int result = 0;

/* Check inherited global status. */
   if( *status != SAI__OK ) return result;

/* Test against each of the primitive numerical data types, setting result
   to 1 if a match is found. */
   if( astChrMatch( type, "_BYTE" ) ||
       astChrMatch( type, "_UBYTE" ) ||
       astChrMatch( type, "_DOUBLE" ) ||
       astChrMatch( type, "_INTEGER" ) ||
       astChrMatch( type, "_REAL" ) ||
       astChrMatch( type, "_WORD" ) ||
       astChrMatch( type, "_UWORD" ) ||
       astChrMatch( type, "_INT64" ) ){
      result = 1;
   }

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) ary1Trace( "ary1Intyp", status );

   return result;
}
