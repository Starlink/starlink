      CHARACTER * ( * ) FUNCTION AST_GETC( THIS, ATTRIB )
*+
* Name:
*    AST_GETC

*  Purpose:
*     Wrap up the C ast_getc_a function.

*  Language:
*     Fortran 77

*  Invocation:
*     RESULT = AST_GETC( THIS, ATTRIB )

*  Description:
*     This function is a wrap-up of the C ast_getc_a function. It will
*     rarely need to be used, but is provided for use on platforms
*     where the normal mechanism for returning character strings as
*     function results from C to Fortran (as defined in the f77.h
*     include file) doesn't work.
*
*     If this problem is encountered, the C macro NO_CHAR_FUNCTION
*     should be defined (in CFLAGS) during C compilation. This will
*     cause the function ast_getc_a to be built (instead of ast_getc)
*     and this returns its the result via an additional initial
*     argument. This Fortran function is then used simply to transfer
*     the argument value to the function result.

*  Arguments:
*     As for the C version of the Fortran-callable function ast_getc.

*  Returned Value:
*     AST_GETC = CHARACTER * ( * )
*        The character return value required.

*  Notes:
*     - The length of the returned result is limited to 200 characters
*     by a local buffer. This length can be increased if necessary.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     3-JUL-1996 (RFWS):
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
      CHARACTER * ( 200 ) BUFF   ! Local buffer for result
*.

*  Invoke the C function (with the additional argument).
      CALL AST_GETC_A( BUFF, THIS, ATTRIB )

*  Return the argument value as the function result.
      AST_GETC = BUFF
      END
