#include <dx/dx.h>

Error m_SXError( Object *in, Object*out){
/*
*+
*  Name:
*     SXError

*  Purpose:
*     reports an error message

*  Language:
*     ANSI C

*  Syntax:
*     SXError( message );

*  Classification:
*     Debugging

*  Description:
*     The SXError module reports an error, issuing the supplied "message".
*     If a null value is supplied for "message" then no errror is reported.

*  Parameters:
*     message = string (Given)
*        the error message [none]

*  Returned Value:
*     ERROR, unless a null value was supplied for "message" in which case
*     OK.

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
*     5-OCT-1995 (DSB):
*        Original version
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/*  Local Variables: */

      char *message;


/*  Get the error message to report, and report it */

      if( in[0] ){
         message = DXGetString( (String) in[0] );
         DXSetError( ERROR_UNEXPECTED, "%s", message );
         return( ERROR );
      }

      return( OK );

}
