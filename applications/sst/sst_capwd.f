      SUBROUTINE SST_CAPWD( STR, STATUS )
*+
*  Name:
*     SST_CAPWD

*  Purpose:
*     Capitalise words in a character string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SST_CAPWD( STR, STATUS )

*  Description:
*     The routine converts the initial character of each word in the
*     character string supplied to upper case and converts all other
*     characters to lower case. In this context a word is a sequence of
*     non-blank characters following a blank (plus the first sequence
*     in the string).

*  Arguments:
*     STR = CHARACTER * ( * ) (Given and Returned)
*        The character string.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     6-MAR-1990 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given and Returned:
      CHARACTER * ( * ) STR

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop counter for characters
      LOGICAL LBLANK             ! Last character was blank?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise.
      LBLANK = .TRUE.

*  Loop to process each character.
      DO 1 I = 1, LEN( STR )

*  Note if the last character was a blank.
         IF ( STR( I : I ) .EQ. ' ' ) THEN
            LBLANK = .TRUE.

*  Otherwise, convert to upper or lower case, as required.
         ELSE
            IF ( LBLANK ) THEN
               CALL CHR_UCASE( STR( I : I ) )
            ELSE
               CALL CHR_LCASE( STR( I : I ) )
            END IF

*  Note if the last character was not a blank.
            LBLANK = .FALSE.
         END IF
1     CONTINUE

      END
* @(#)sst_capwd.f   1.1   94/12/05 11:31:22   96/07/05 10:27:31
