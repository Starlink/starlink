      SUBROUTINE CCD1_GETARG( POS, VALUE )
*+
*  Name:
*     CCD1_GETARG

*  Purpose:
*     To get the value of a command-line argument.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_GETARG( POS, VALUE )

*  Description:
*     Call the GETARG intrinsic. Use so that GETARG may be called
*     from a C routine. The GETARG intrinsic is not always available
*     to a direct call from C (when intrinsic names are mangled).

*  Arguments:
*     POS = INTEGER (Given)
*        The argument required. The first argument is 0 and is usually
*        the program name.
*     VALUE = CHARACTER * ( * ) (Returned)
*        The value of the argument. Blank if no such argument
*        is available.

*  Notes:
*     Potentially not portable. GETARG is an extension.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     20-JUL-2005 (PDRAPER):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      INTEGER POS

*  Arguments Returned:
      CHARACTER * ( * ) VALUE

*  Local Variables:

*.
      CALL GETARG( POS, VALUE )
      END
