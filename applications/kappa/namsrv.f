      SUBROUTINE NAMSRV( NAME, COMMNT, I, STATUS )
*+
*  Name:
*     NAMSRV

*  Purpose:
*     Display an SGS workstation name and descriptive comment.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NAMSRV( NAME, COMMNT, I, STATUS )

*  Description:
*     This is a service routine for SGS_WNAME. It displays an SGS
*     workstation name and the descriptive comment associated with it.

*  Arguments:
*     NAME = CHARACTER * ( * ) (Given)
*        The workstation name.
*     COMMNT = CHARACTER * ( * ) (Given)
*        The workstation comment.
*     I = INTEGER (Given)
*        The integer passed by SGS_WNAME (not actually used).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Implementation Deficiencies:
*     Workstation names are truncated to 15 characters for display
*     purposes.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     4-MAY-1989  (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) NAME
      CHARACTER * ( * ) COMMNT
      INTEGER I

*  Status:
      INTEGER STATUS             ! Global status

*  Local variables:

      INTEGER END                ! Last non-blank character in the
                                 ! device name
      INTEGER START              ! First non-blank character in the
                                 ! device name
      CHARACTER*15 RJNAME        ! Right-justified device name

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      RJNAME = ' '
      CALL CHR_FANDL( NAME, START, END )
      RJNAME( 15 - END + START: ) = NAME( START:END )

*  Format the workstation name and comment for display.
      CALL MSG_FMTC( 'NAME', '( A15 )', RJNAME )
      CALL MSG_SETC( 'COMMENT', COMMNT )

*  Display the information.
      CALL MSG_OUT( 'DEVICE', '^NAME - ^COMMENT', STATUS )

      END
