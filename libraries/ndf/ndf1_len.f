      INTEGER FUNCTION NDF1_LEN( STR )
*+
* Name:
*    NDF1_LEN

*  Purpose:
*     Return the declared length of a character string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = NDF1_LEN( STR )

*  Description:
*     The routine returns the number of characters in the string
*     supplied, as determined by the Fortran intrinsic LEN function.

*  Arguments:
*     STR = CHARACTER * ( * ) (Given)
*        The string.

*  Returned Value:
*     NDF1_LEN = INTEGER
*        The string's length.

*  Notes:
*     This routine exists purely to avoid using the intrinsic LEN
*     function in generic routines, where the compiler might otherwise
*     object to its argument having an incorrect data type (even though
*     such calls would never actually be executed).

*  Copyright:
*     Copyright (C) 1994 Particle Physics & Astronomy Research Council

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     29-JUL-1994 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER * ( * ) STR
      
*.

*  Return the string length.
      NDF1_LEN = LEN( STR )

      END
