      SUBROUTINE CCD1_ERREP( ID, STRING, STATUS )
*+
*  Name:
*     CCD1_ERREP

*  Purpose:
*     To capture any error reports which are required in the log file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_ERREP( ID, STRING, STATUS )

*  Description:
*     This routine simply calls the ERR_REP routine with the error
*     ID and message string. However, if the CCDPACK logging system is
*     enabled then the string is also written to the log file. This
*     routine is intended for use in capturing simple messages, such as
*     a task exit messages which help maintain the sense of the log
*     file (an aborting task will issue other messages to the batch log
*     file or the currently interactive process).

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status. This routine tries to run if status is BAD.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-JUN-1991 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Global Variables:
      INCLUDE 'CCD1_CLOG'         ! CCDPACK log file system common block

*        CCD1_BUFF = CHARACTER * ( MSG__SZMSG ) (Write)
*           Character buffer for output strings.
*        CCD1_ILEV = INTEGER (Write)
*           Log system interaction level.
*           0 - no output from the CCDPACK logging system
*           1 - output to terminal only
*           2 - output to logfile only
*           3 - output to logfile and terminal

*  Arguments Given:
      CHARACTER * ( * ) STRING
      CHARACTER * ( * ) ID

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER STRLEN
      INTEGER TSTAT              ! Local status

*.

*  Save the STATUS value and mark the error stack.
      TSTAT = STATUS
      CALL ERR_MARK

*  Reset status in an attempt to get the next section to work.
      STATUS = SAI__OK

*  Normal section.
*  Load the string if echoing
      IF ( CCD1_ILEV .GT. 2 ) THEN
         CCD1_BUFF = ' '
         CALL MSG_LOAD( ' ', STRING, CCD1_BUFF, STRLEN, STATUS )

*  Echo this to the log file if required.
         CALL LOG_WRITE( CCD1_BUFF( :STRLEN ), STATUS )

*  And write out the error to the user. Set up error status to get
*  Error system to issue report.
         STATUS = SAI__ERROR
         CALL ERR_REP( ID, CCD1_BUFF( :STRLEN ), STATUS )
      ELSE

*  Write out the error as normal.
         STATUS = SAI__ERROR
         CALL ERR_REP( ID, STRING, STATUS )
      END IF

*  Reset the error.
      STATUS = TSTAT
      CALL ERR_RLSE

      END
* $Id$
