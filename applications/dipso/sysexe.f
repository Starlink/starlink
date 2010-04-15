      SUBROUTINE SYSEXE( COMM, OKVAL, STATUS )
*+
*  Name:
*     SYSEXE

*  Purpose:
*     Execute a Unix shell command

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SYSEXE( COMM, OKVAL, STATUS )

*  Description:
*     The Fortran RTL "SYSTEM" routine is used. If the status returned
*     by this routine is not equal to the supplied success value, an
*     error is reported.

*  Arguments:
*     COMM = CHARACTER * ( * ) (Given)
*        The system command to execute. Trailing spaces are ignored.
*     OKVAL = INTEGER (Given)
*        The system status value returned by the command to indicate
*        successfull completion (typically zero).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     20-DEC-1995 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER COMM*(*)
      INTEGER OKVAL

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER SYSTEM             ! Fortran RTL routine
      INTEGER CHR_LEN            ! Returns used length of a string


*  Local Variables:
      INTEGER SYSTAT             ! Status value from RTL routine

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Execute the system command, padding the standard output with blank
*  lines.
      CALL MSG_BLANK( STATUS )
      SYSTAT = SYSTEM( COMM( : CHR_LEN( COMM ) ) )
      CALL MSG_BLANK( STATUS )

*  Report an error if the specified success code was not returned.
      IF( SYSTAT .NE. OKVAL ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'COMM', COMM )
         CALL MSG_SETI( 'STAT', SYSTAT )
         CALL ERR_REP( 'SYSTEM_ERR1', 'Error executing system '//
     :                 'command ''^COMM'' (status = ^STAT)', STATUS )
      END IF

      END
