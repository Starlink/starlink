      SUBROUTINE CCD1_MSGL( STRING, STATUS )
*+
*  Name:
*     CCD1_MSGL

*  Purpose:
*     To write out a message string to the CCDPACK log file only.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_MSGL( STRING, STATUS )

*  Description:
*     This routine writes out the character variable string to the log
*     file.  Only if the logging flag in the communication common block
*     is set true. THIS VERSION USES FIO and PSX.
*
*     Notes:
*     -  This version is designed for use on machines which do not
*        support the ICL/ADAM logging system. It uses FIO and PSX
*        open and control the file, and to perform the date/time
*        stamping.


*  Arguments:
*     STRING = CHARACTER * ( * ) (Given)
*        The message to output to the logfile.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     31-JUL-1991 (PDRAPER):
*        Original Version.
*     19-FEB-1992 (PDRAPER):
*        Change to use FIO and PSX.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Global Variables:
      INCLUDE 'FIO1_CLOG'        ! CCDPACK log file internal common
                                 ! block.
*        CCD1_LOGFD = INTEGER (Write)
*           FIO system file descriptor for log file.
*        CCD1_ILEV = INTEGER (Write)
*           Log system interaction level.
*           0 - no output from the CCDPACK logging system
*           1 - output to terminal only
*           2 - output to logfile only
*           3 - output to logfile and terminal
*        CCD1_BUFF = CHARACTER * ( MSG__SZMSG ) (Write)
*           Character buffer for output strings.

*  Arguments Given:
      CHARACTER * ( * ) STRING

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 64 ) DATE    ! Date string
      INTEGER STRLEN             ! Length of string
      INTEGER NTICKS             ! Number of time ticks

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If echo set write the string
      IF ( CCD1_ILEV .GE. 2 ) THEN
         CCD1_BUFF = ' '

*  Load input string expanding any tokens etc.
         CALL MSG_LOAD( ' ', STRING, CCD1_BUFF, STRLEN, STATUS )

*  Get the time of failure.
         CALL PSX_TIME( NTICKS, STATUS )
         CALL PSX_CTIME( NTICKS, DATE, STATUS )

*  Set up output message tokens
         CALL MSG_SETC( 'INPUT', CCD1_BUFF )
         CALL MSG_SETC( 'DATE', DATE )
         CALL MSG_LOAD( ' ', '^DATE : ^INPUT', CCD1_BUFF, STRLEN,
     :                  STATUS )

*  Echo this to the log file if required.
         CALL FIO_WRITE( CCD1_LOGFD, CCD1_BUFF( :STRLEN ), STATUS )
      END IF

      END
* $Id$
