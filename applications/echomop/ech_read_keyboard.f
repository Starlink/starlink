      SUBROUTINE ECH_READ_KEYBOARD( STATUS )
*+
*  Name:
*     ECHOMOP - ECH_READ_KEYBOARD

*  Purpose:
*     Read single keypress.

*  Description :
*     This routine reads a single keypress from the keyboard.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ECH_READ_KEYBOARD( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DMILLS: Dave Mills (UCL, Starlink)
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_further_authors_here}

*  History:
*     01-SEP-1992 (DMILLS):
*       Initial release.
*     08-APR-1996 (MJC):
*       Standard prologue.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Variables:
      INCLUDE 'ECH_ENVIRONMENT.INC'

*  Status:
      INTEGER STATUS
*.

      READ ( *, '( A )' ) USER_INPUT_CHAR
      CALL CHR_UCASE( USER_INPUT_CHAR )

      END
