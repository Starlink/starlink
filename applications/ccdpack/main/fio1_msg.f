      SUBROUTINE CCD1_MSG( ID, STRING, STATUS )
*+
*  Name:
*     CCD1_MSG

*  Purpose:
*     To write out a message string within CCDPACK.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_MSG( ID, STRING, STATUS )

*  Description:
*     This routine writes out the character variable string to the user.
*     It uses the MSG system to write this string out to the user. The
*     message is identified by the string ID. If the logging flag in the
*     communication common block is set true then the result is echoed
*     to the log file. THIS VERSION USES FIO and PSX.

*  Arguments:
*     ID = CHARACTER * ( * ) (Given)
*        The MSG system identifier for this string.
*     STRING = CHARACTER * ( * ) (Given)
*        The message to output to the user and optionally log to the
*        logfile.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - This version is designed for use on machines which do not
*       support the ICL/ADAM logging system. It uses FIO and PSX open
*       and control the file, and to perform the date/time stamping.

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
*        Changed to use FIO and PSX.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Global Variables:
      INCLUDE 'FIO1_CLOG'         ! CCDPACK log file internal common
                                 ! block
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
      CHARACTER * ( * ) ID
      CHARACTER * ( * ) STRING

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 64 ) DATE    ! Date string
      INTEGER STRLEN             ! Length of string
      INTEGER NTICKS             ! Number of time ticks
      INTEGER UNIT               ! Log file Fortran unit number

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
      IF ( CCD1_ILEV .GT. 0 ) THEN

*  Definitely outputing something. Load string into message buffer.
         CALL MSG_LOAD( ' ', STRING, CCD1_BUFF, STRLEN, STATUS )
         STRLEN = MAX( 1, STRLEN )
         CALL MSG_SETC( 'INPUT', CCD1_BUFF )
         IF ( CCD1_ILEV .EQ. 1 .OR. CCD1_ILEV .EQ. 3 ) THEN

*  Writing directly to user.
            CALL MSG_OUT( ID, '^INPUT', STATUS )
            IF ( CCD1_ILEV .EQ. 3 ) THEN

*  Will output to log file later. Renew message tokens.
               CALL MSG_RENEW
            END IF
         END IF

*  Do we need to output to logfile ?
         IF ( CCD1_ILEV .GE. 2 ) THEN

*  Yes we do - date stamp the output.
            CCD1_BUFF = ' '
            CALL PSX_TIME( NTICKS, STATUS )
            CALL PSX_CTIME( NTICKS, DATE, STATUS )
            CALL MSG_SETC( 'DATE', DATE )
            CALL MSG_LOAD( ' ', '^DATE : ^INPUT', CCD1_BUFF, STRLEN,
     :                     STATUS )
            STRLEN = MAX( 1, STRLEN )

*  Echo this to the log file if required.
            CALL FIO_WRITE( CCD1_LOGFD, CCD1_BUFF( :STRLEN ), STATUS )

*-----------------------------------------------------------------------
*  Fix for SUN problems with buffering (ok on unix systems)
*-----------------------------------------------------------------------
            CALL FIO_UNIT( CCD1_LOGFD, UNIT, STATUS )
            CALL FLUSH( UNIT )
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*  Not portable to non-unix systems
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

         END IF
      END IF

      END
* $Id$
