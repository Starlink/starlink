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
*     to the log file.

*  Notes:
*     - This routine uses a MSG_LOAD, before writing out the output
*       string. This expands all tokens etc. however, this has one
*       drawback (as must do similar logging systems) in that any escape
*       sequences are expanded on the first pass (MSG_LOAD) and then
*       rexpanded on the second pass. Later comment --- in attempt to
*       fix this any MSG escape characters let after MSG_LOAD are
*       duplicated.

*  Arguments:
*     ID = CHARACTER * ( * ) (Given)
*        The MSG system identifier for this string.
*     STRING = CHARACTER * ( * ) (Given)
*        The message to output to the user and optionally log to the
*        logfile.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

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
      INCLUDE 'CCD1_CLOG'         ! CCDPACK log file internal common
                                 ! block
*        CCD1_BUFF = CHARACTER * ( MSG__SZMSG )
*           Character buffer for loading the message before writting
*           out to the environment.
*        CCD1_ILEV = INTEGER (Write)
*           Log system interaction level.
*           0 - no output from the CCDPACK logging system
*           1 - output to terminal only
*           2 - output to logfile only
*           3 - output to logfile and terminal

*  Arguments Given:
      CHARACTER * ( * ) ID
      CHARACTER * ( * ) STRING

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER STRLEN             ! Length of string

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
      IF ( CCD1_ILEV .GT. 0 ) THEN

*  Load the string if echoing to log file.
         IF ( CCD1_ILEV .GE. 2 ) THEN
            CCD1_BUFF = ' '
            CALL MSG_LOAD( ' ', STRING, CCD1_BUFF, STRLEN, STATUS )

*  Echo this to the log file if required.
            CALL LOG_WRITE( CCD1_BUFF( :STRLEN ), STATUS )

*  Do not write to user if not required.
            IF ( CCD1_ILEV .EQ. 3 ) THEN

*  Trap and duplicate any MSG system escape codes that are now present
*  in the processed string. Do not use STRLEN as string is expanded
*  to the right to include new escape sequences.
               CALL CCD1_DUESC( CCD1_BUFF , STATUS )
               CALL MSG_OUT( ID, CCD1_BUFF, STATUS )
            END IF
         ELSE

*  CCD1_ILEV = 1 --- write out the string to the user as is.
            CALL MSG_OUT( ID, STRING, STATUS )
         END IF
      END IF

      END
* $Id$
