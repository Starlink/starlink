      SUBROUTINE CCD1_CLLOG( STATUS )
*+
*  Name:
*     CCD1_CLLOG

*  Purpose:
*     To close the CCDPACK log file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_CLLOG( STATUS )

*  Description:
*     Closes the CCDPACK log file if it has been opened and flagged in
*     the log system common block. THIS VERSION USES FIO and PSX.
*
*     Notes:
*        -  This version is designed for use on machines which do not
*        support the ICL/ADAM logging system. It uses FIO and PSX open
*        and control the file, and to perform the date/time stamping.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

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
      INCLUDE 'FIO1_CLOG'         ! Log file system common block

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

*  Status:
      INTEGER STATUS             ! Global status
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Close the log file if it is opened.
      IF ( CCD1_ILEV .GE. 2  ) THEN
         CALL FIO_CLOSE( CCD1_LOGFD, STATUS )
      END IF

      END
* $Id$
