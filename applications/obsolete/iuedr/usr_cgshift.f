      SUBROUTINE USR_CGSHIFT( STATUS )
*+
*  Name:
*     SUBROUTINE USR_CGSHIFT

*  Purpose:
*     Use the graphics cursor to locate the spectrum position.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL USR_CGSHIFT( STATUS )

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
*     03-NOV-88 (PCTR):
*       IUEDR Vn. 2.0
*     03-OCT-94 (MJC):
*       IUEDR Vn. 3.1-6
*     09-JAN-95 (MJC):
*       IUEDR Vn. 3.2
*       Changed error message to "invalid reolution".
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Global Variables:
      INCLUDE 'CMHEAD'

*  Status:
      INTEGER STATUS        ! Global status.

*  External Refernces:
      LOGICAL STR_SIMLR     ! Caseless string equality.
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Get Calibration.
      CALL DASSOC( '\\', '\\', STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: could not access dataset\\', STATUS )
         GO TO 999
      END IF

*   Branch on RESOLUTION.
      IF ( STR_SIMLR( 'HIRES\\', RESOL ) ) THEN
         CALL PICKHI( STATUS )

      ELSE IF ( STR_SIMLR( 'LORES\\', RESOL ) ) THEN
         CALL PICKLO( STATUS )

      ELSE
         CALL ERROUT( 'Error: unknown IUE Resolution\\', STATUS )
      END IF

  999 CONTINUE

      END
