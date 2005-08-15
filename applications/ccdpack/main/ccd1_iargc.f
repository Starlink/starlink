      INTEGER FUNCTION CCD1_IARGC()
*+
*  Name:
*     CCD1_IARGC

*  Purpose:
*     Get the number of command-line arguments.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     INTEGER = CCD1_IARG()

*  Description:
*     Invoke the IARGC intrinsic. Use so that IARGC may be called
*     from a C routine. The IARGC intrinsic is not always available
*     to a direct call from C (when intrinsic names are mangled).

*  Notes:
*     Potentially not portable. IARGC is an extension.

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

*.
      CCD1_IARGC = IARGC()
      END
