      INTEGER FUNCTION ECH_WORD_LEN( WORD )
*+
*  Name:
*     ECHOMOP - ECH_WORD_LEN

*  Purpose:
*     Determine the length of a space-free string.

*  Arguments:
*     WORD = CHARACTER*( * ) (Given)
*        Word to be measured.

*  Authors:
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     20-MAR-1997 (MJC):
*       Added prologue.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Defintions:
      IMPLICIT NONE

*  Arguments Given:
      CHARACTER*( * ) WORD

*  Local Variables:
      INTEGER I
*.

      ECH_WORD_LEN = LEN( WORD )
      IF ( WORD( ECH_WORD_LEN : ECH_WORD_LEN ) .EQ. ' ' ) THEN
         DO I = 1, ECH_WORD_LEN
            IF ( WORD( I:I ) .EQ. ' ' ) THEN
               ECH_WORD_LEN = I - 1
               GO TO 10
            END IF
         END DO
   10    CONTINUE
      END IF

      END
