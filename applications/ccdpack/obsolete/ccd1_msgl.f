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
*     is set true.

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
*     {enter_further_changes_here}

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

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If echo set write the string
      IF ( CCD1_ILEV .GE. 2 ) THEN
         CALL LOG_WRITE( STRING, STATUS )
      END IF

      END
* $Id$
