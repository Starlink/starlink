      SUBROUTINE CALO( STATUS )
*+
*  Name:
*     SUBROUTINE CALO

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CALO( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     LORES calibration from Net.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     25-JUN-81 (JRG):
*       IUEDR Vn. 1.0
*     07-OCT-88 (PCTR):
*       IUEDR Vn. 2.0
*     16-DEC-94 (MJC):
*       IUEDR Vn. 3.2.  Converted prologue to Starlink Style.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Global Variables:
      INCLUDE 'CMWCOR'
      INCLUDE 'CMWAV'
      INCLUDE 'CMSPEC'
      INCLUDE 'CMVEL'

*  Status:
      INTEGER STATUS     ! Global status.

*  Local Variables:
      INTEGER I          ! Loop index.
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   General part.
      CALL CALGEN( ORDER, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         GO TO 999
      END IF

      IF ( .NOT. NOWCOR ) THEN
         DO I = 1, NWAV
            WAVAIR( I ) = WAVAIR( I ) + WCOR( ORDER )
         END DO
      END IF

      CALL ABSCAL( ORDER, STATUS )
      IF ( .NOT. NOVEL ) THEN
         CALL IUE_VELO( VEL( ORDER ), NWAV, WAVAIR )
      END IF
      CALL GENCAL

 999  CONTINUE

      END
