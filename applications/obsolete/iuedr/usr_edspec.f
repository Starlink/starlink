      SUBROUTINE USR_EDSPEC( STATUS )
*+
*  Name:
*     SUBROUTINE USR_EDSPEC

*  Purpose:
*     Edit spectrum using a cursor on a graphics display.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL USR_EDSPEC( STATUS )

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
*     07-SEP-88 (PCTR):
*       IUEDR Vn. 2.0
*     19-OCT-94 (MJC):
*       IUEDR Vn. 3.1-9
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
      INTEGER STATUS        ! Global status.

*  External Refernces:
      LOGICAL STR_SIMLR     ! Caseless string equality.

*  Global Variables:
      INCLUDE 'CMHEAD'

*  Local Constants:
      INTEGER ERR           ! Error status.
      PARAMETER (ERR = -3)
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   DATASET.
      CALL RDSPEC( STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: accessing dataset\\', STATUS )

*   Branch on RESOLUTION.
      ELSE
         IF ( STR_SIMLR( 'HIRES\\', RESOL ) ) THEN
            CALL EDITLO( STATUS )

         ELSE IF ( STR_SIMLR( 'LORES\\', RESOL ) ) THEN
            CALL EDITLO( STATUS )

         ELSE
            CALL ERROUT( 'Error: cannot edit this dataset\\', STATUS )
         END IF
      END IF

      END
