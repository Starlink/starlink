      SUBROUTINE USR_HELP( STATUS )
*+
*  Name:
*     SUBROUTINE USR_HELP

*  Description:
*     Use call to CMD_HELP to drop into prompted mode of HELP
*     system access.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL USR_HELP( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PCTR: Paul Rees (UCL)
*     DMILLS: Dave Mills (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     12-MAY-89 (PCTR):
*       IUEDR Vn. 2.1
*       Original FORTRAN version.
*     01-OCT-92 (DMILLS):
*       IUEDR Vn. 3.0
*       ADAMised version to run on multiple hardware platforms
*     18-JAN-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Status:
      INTEGER STATUS     ! Global status.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL CMD_HLP ( .FALSE., 'HELP', ' ' )

      END
