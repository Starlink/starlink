      SUBROUTINE USR_ERASE( STATUS )
*+
*  Name:
*     SUBROUTINE USR_ERASE

*  Purpose:
*     Erase the current workstation screen.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL USR_ERASE( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     01-AUG-82 (JRG):
*       IUEDR Vn. 1.0
*     22-FEB-88 (PCTR):
*       IUEDR Vn. 2.0
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

*  Global Variables:
      INCLUDE 'CMGRAF'

*  Status:
      INTEGER STATUS     ! Global status.
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   DEVICE/TERMINAL parameters.
      CALL GRF_OPZONE( STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: graphics unavailable\\', STATUS )

      ELSE
         CALL GRF_RSGRAF( STATUS )
         CALL GRF_TZONE( ZBASE, 0, ZCLEAR, STATUS )
         CALL GRF_TZONE( ZBASE, ZONE, ZCLEAR, STATUS )
      END IF

*   Set ERASED flag.
      ERASED = .TRUE.

      END
