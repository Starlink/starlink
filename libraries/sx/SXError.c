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
