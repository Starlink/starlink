      SUBROUTINE SST_ZAPPL( LINE, STATUS )
*+
*  Name:
*     SST_ZAPPL

*  Purpose:
*     Remove LSE placeholders from a source code line.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SST_ZAPPL( LINE, STATUS )

*  Description:
*     The routine searches a source code line to identify those LSE
*     placeholders which are normally left in place within STARLSE
*     routine prologues and replaces them with blanks.

*  Arguments:
*     LINE = CHARACTER * ( * ) (Given and Returned)
*        The source code line to be processed.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     22-DEC-1989 (RFWS):
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
      CHARACTER * ( * ) LINE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MXPL               ! Number of recognised placeholders
      PARAMETER ( MXPL = 5 )
      INTEGER SZPL               ! Maximum length of a placeholder
      PARAMETER ( SZPL = 28 )

*  Local Variables:
      CHARACTER * ( SZPL ) PLACE( MXPL ) ! Placeholder strings
      INTEGER I                  ! Loop counter for placeholders
      INTEGER II                 ! Placeholder position in line
      INTEGER LPLACE( MXPL )     ! Length of each placeholder
      LOGICAL FOUND              ! Whether a placeholder was found

*  Local Data:
      DATA ( PLACE( I ), LPLACE( I ), I = 1, MXPL )
     :  / '{enter_new_authors_here}', 24,
     :    '{enter_changes_here}', 20,
     :    '{enter_further_changes_here}', 28,
     :    '{note_any_bugs_here}', 20,
     :    '{note_new_bugs_here}', 20 /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop to zap placeholders until no more can be found.
      FOUND = .TRUE.
1     CONTINUE                   ! Start of 'DO WHILE' loop
      IF ( FOUND ) THEN

*  Search the line for each placeholder in turn.
         FOUND = .FALSE.
         DO 2 I = 1, MXPL

*  See if the line contains the placeholder.
            II = INDEX( LINE, PLACE( I )( : LPLACE( I ) ) )

*  If so, then replace it with blanks and start agin.
            IF ( II .NE. 0 ) THEN
               LINE( II : II + LPLACE( I ) - 1 ) = ' '
               FOUND = .TRUE.
               GO TO 1
            END IF
2        CONTINUE

*  Return to search for another placeholder.
         GO TO 1
      END IF

      END
* @(#)sst_zappl.f   1.1   94/12/05 11:31:37   96/07/05 10:27:30
