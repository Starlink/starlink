      SUBROUTINE NDF1_ELLIP( STR )
*+
* Name:
*    NDF1_ELLIP

*  Purpose:
*     Add an ellipsis to the end of a character string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_ELLIP( STR )

*  Description:
*     The routine adds an ellipsis '...' to the end of a character
*     string, to indicate that it has been truncated.

*  Arguments:
*     STR = CHARACTER * ( * ) (Given and Returned)
*        The string to be modified.

*  Notes:
*     This routine does not perform error checking.

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

*  Arguments Given and Returned:
      CHARACTER * ( * ) STR
      
*.

*  Append the ellipsis.
      STR( MAX( 1, LEN( STR ) - 2 ) : ) = '...'

      END
