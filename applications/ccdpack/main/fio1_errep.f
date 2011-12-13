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
*     THIS VERSION USES FIO and PSX.
*
*     Notes:
*     -  This version is designed for use on machines which do not
*        support the ICL/ADAM logging system. It uses FIO and PSX
*        open and control the file, and to perform the date/time
*        stamping.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status. This routine tries to run if status is BAD.

*  Copyright:
*     Copyright (C) 1991-1992 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-JUN-1991 (PDRAPER):
*        Original version.
*     19-FEB-1992 (PDRAPER):
*        Changed to use FIO.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Global Variables:
      INCLUDE 'FIO1_CLOG'         ! CCDPACK log file system common block

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
      CHARACTER * ( * ) ID

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER TSTAT              ! Local status
      CHARACTER * ( 64 ) DATE    ! Date string
      INTEGER STRLEN             ! Length of string
      INTEGER NTICKS             ! Number of time ticks

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

*  Load input string expanding any tokens etc.
         CALL MSG_LOAD( ' ', STRING, CCD1_BUFF, STRLEN, STATUS )

*  Get the time of failure.
         CALL PSX_TIME( NTICKS, STATUS )
         CALL PSX_CTIME( NTICKS, DATE, STATUS )

*  Set up output message tokens
         CALL MSG_SETC( 'INPUT', CCD1_BUFF )
         CALL MSG_SETC( 'DATE', DATE )
         CALL MSG_LOAD( ' ', '^DATE : ^INPUT' , CCD1_BUFF, STRLEN,
     :                  STATUS )

*  Echo this to the log file if required.
         CALL FIO_WRITE( CCD1_LOGFD, CCD1_BUFF( :STRLEN ), STATUS )

*  And write out the error to the user. Set up error status to get
*  Error system to issue report.
         CALL MSG_RENEW
         STATUS = SAI__ERROR
         CALL ERR_REP( ID, '^INPUT', STATUS )
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
