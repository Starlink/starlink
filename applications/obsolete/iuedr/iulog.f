      SUBROUTINE IULOG( STRING, STATUS )
*+
*  Name:
*     SUBROUTINE IULOG

*  Purpose:
*     Output message to log file and screen.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IULOG( STRING, STATUS )

*  Arguments:
*     STRING = CHARACTER*( * ) (Given)
*        The string to be logged.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     28-JAN-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Variables:
      INCLUDE 'CMPSX'

*  Arguments Given:
      CHARACTER*( * ) STRING

*  Status:
      INTEGER STATUS
*.

*   Write message text to the file.
      IF ( ISLOG ) THEN
         WRITE( LOGFILE, '( A )' ) STRING
      END IF

*   Write to user terminal.
      CALL MSG_OUT( ' ' , STRING, STATUS )

      END
