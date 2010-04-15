      SUBROUTINE ISTSTR( PARAM, LOG, FD, STATUS )
*+
*  Name:
*     ISTSTR

*  Purpose:
*     Display a literal string associated with an IRCAM global
*     parameter

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ISTSTR( PARAM, LOG, FD, STATUS )

*  Description:
*     An attempt is made to obtain a string using the supplied
*     parameter (which should be associated with an IRCAMPACK global
*     parameter). If the string is obtained succesfully, it is
*     displayed, and optionally logged to a file. If no string was
*     obtained, the word "undefined" is used instead.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The parameter.
*     LOG = LOGICAL (Given)
*        If .TRUE. then log the string.
*     FD = INTEGER (Given)
*        The FIO file descriptor for the log file. Ignored if LOG is
*        .FALSE.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1-OCT-1993 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_ERR'          ! PAR_ error constants

*  Arguments Given:
      CHARACTER PARAM*(*)
      LOGICAL LOG
      INTEGER FD

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER
     :        STRING*255         ! Supplied string.

      INTEGER
     :        F,                 ! Position of first non-blank character
     :        L                  ! Position of last non-blank character

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the string.
      CALL PAR_GET0C( PARAM, STRING, STATUS )

*  If an error occurred (other than a parameter abort), annul the error and
*  report a null.
      IF( STATUS .NE. SAI__OK .AND. STATUS .NE. PAR__ABORT ) THEN
         CALL ERR_ANNUL( STATUS )
         CALL MSG_OUT( 'ISTSTR_MSG1', 'undefined', STATUS )
         IF( LOG ) CALL FIO_WRITE( FD, 'undefined', STATUS )

*  Otherwise, display the string and optionally log it.
      ELSE
         CALL CHR_FANDL( STRING, F, L )
         IF( F .LE. L ) THEN
            CALL MSG_OUT( 'ISTSTR_MSG2', STRING( F : L ), STATUS )
            IF( LOG ) CALL FIO_WRITE( FD, STRING( F : L ), STATUS )
         ELSE
            CALL MSG_OUT( 'ISTSTR_MSG3', 'undefined', STATUS )
            IF( LOG ) CALL FIO_WRITE( FD, 'undefined', STATUS )
         END IF

      END IF

      END
