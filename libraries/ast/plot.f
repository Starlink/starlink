      CHARACTER * ( * ) FUNCTION AST_STRIPESCAPES( THIS, ATTRIB )
*+
* Name:
*     AST_STRIPESCAPES

*  Purpose:
*     Wrap up the C AST_STRIPESCAPES_A function.

*  Language:
*     Fortran 77

*  Invocation:
*     RESULT = AST_STRIPESCAPES( THIS, ATTRIB )

*  Description:
*     This function is a wrap-up of the C ast_stripescapes_a function. It
*     will rarely need to be used, but is provided for use on platforms
*     where the normal mechanism for returning character strings as
*     function results from C to Fortran (as defined in the f77.h
*     include file) doesn't work.
*
*     If this problem is encountered, the C macro NO_CHAR_FUNCTION
*     should be defined (in CFLAGS) during C compilation. This will
*     cause the function ast_stripescapes_a to be built (instead of
*     ast_stripescapes) and this returns its the result via an additional
*     initial argument. This Fortran function is then used simply to
*     transfer the argument value to the function result.

*  Arguments:
*     As for the C version of the Fortran-callable function ast_stripescapes.

*  Returned Value:
*     AST_STRIPESCAPES = CHARACTER * ( * )
*        The character return value required.

*  Notes:
*     - The length of the returned result is limited to 300 characters
*     by a local buffer. This length can be increased if necessary.

*  Authors:
*     DSB: David S Berry (JAC)
*     {enter_new_authors_here}

*  History:
*     10-JUL-2006 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      INTEGER THIS
      CHARACTER * ( * ) ATTRIB

*  Local Variables:
      CHARACTER * ( 300 ) BUFF   ! Local buffer for result
*.

*  Invoke the C function (with the additional argument).
      CALL AST_STRIPESCAPES_A( BUFF, THIS, ATTRIB )

*  Return the argument value as the function result.
      AST_STRIPESCAPES = BUFF

      END
