#include <dx/dx.h>

Error m_SXMerge( Object *in, Object*out){
/*
*+
*  Name:
*     SXMerge

*  Purpose:
*     passes the first non-NULL input to the output

*  Language:
*     ANSI C

*  Syntax:
*     output = SXMerge( def, input1, input2, ... );

*  Classification:
*     Flow Control

*  Description:
*     The SXMerge module passes the first non-null input to the output, or
*     "NULL" if all of the inputs are NULL.

*  Parameters:
*     n = integer (Given)
*        number of inputs to consider [none]
*     input1 = Object (Given)
*        First input object [none]
*     input1 = Object (Given)
*        Second input object [none]
*     output = Object (Returned)
*        output object

*  Returned Value:
*     OK, unless an error occurs in which case ERROR is returned and the
*     DX error code is set.

*  Copyright:
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
*     All Rights Reserved.

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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     23-OCT-1995 (DSB):
*        Original version
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/*  Local Variables. */

      int i,n;


/*  Initialise the returned object */

      out[0] = NULL;


/*  Check that the "n" object has been supplied. */


      if( !in[0] ){
         DXSetError( ERROR_BAD_PARAMETER, "missing parameter \"n\".");
         goto error;
      }


/*  Get a value for the "n" object. */

      if( !SXGet0is( 'n', in[0], 100, 0, 0, NULL, &n, NULL ) ) goto error;


/* Check each of the first "n" inputs */

      for( i=1; i<=n; i++ ){

         if( in[i] ) {
            out[0] = in[i];
            break;
         }

      }


/*  Return the output object. */

      return( OK );

error:
      return( ERROR );

}
