      SUBROUTINE SST_SKCHR( CHARS, STR, I )
*+
*  Name:
*     SST_SKCHR

*  Purpose:
*     Skip over a set of characters in a string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SST_SKCHR( CHARS, STR, I )

*  Description:
*     The routine increments a pointer to a character position in a
*     string until the character pointed at is not one of a specified
*     set of characters. If no such character position exists, the
*     pointer is set to one more then the length of the string.

*  Arguments:
*     CHARS = CHARACTER * ( * ) (Given)
*        A string consisting of the set of characters to be skipped
*        over.
*     STR = CHARACTER * ( * ) (Given)
*        The string to be searched.
*     I = INTEGER (Given and Returned)
*        The character pointer.

*  Notes:
*     If the initial value of I does not point at one of the characters
*     in the string, then the routine returns without action.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     7-SEP-1990 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER * ( * ) CHARS
      CHARACTER * ( * ) STR

*  Arguments Given and Returned:
      INTEGER I

*  Local Variables:
      INTEGER II                 ! Loop counter for character positions

*.

*  Check if the initial value of I is in range.
      IF ( ( I .GE. 1 ) .AND. ( I .LE. LEN( STR ) ) ) THEN

*  If so, then loop to inspect characters, stopping when one is found
*  which is not in CHARS.
         DO 1 II = I, LEN( STR )
            IF ( INDEX( CHARS, STR( II : II ) ) .EQ. 0 ) GO TO 2
1        CONTINUE
2        CONTINUE

*  Return the new pointer value.
         I = II
      END IF

      END
* @(#)sst_skchr.f   1.1   94/12/05 11:31:34   96/07/05 10:27:30
