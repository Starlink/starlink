      SUBROUTINE CAHI( STATUS )
*+
*  Name:
*     SUBROUTINE CAHI

*  Purpose:
*     HIRES calibration from Net.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CAHI( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Method:
*     Assume ONLY 1 aperture, with index 1.

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
*     05-OCT-94 (MJC)
*       IUEDR Vn. 3.1-6
*     07-FEB-95 (MJC):
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
      INCLUDE 'CMHEAD'
      INCLUDE 'CMWAV'
      INCLUDE 'CMECOR'
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
      CALL CALGEN( 1, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         GO TO 999
      END IF

      IF ( .NOT. NOECOR ) THEN
         DO I = 1, NWAV
            WAVAIR( I ) = WAVAIR( I ) + ECOR( 1 ) / DBLE( ORDER )
         END DO
      END IF

      CALL RIPCAL( STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL ABSCAL( 1, STATUS )
      END IF
      IF ( .NOT. NOVEL ) THEN
         CALL IUE_VELO( VEL( 1 ), NWAV, WAVAIR )
      END IF
      CALL HALCAL
      CALL GENCAL

 999  CONTINUE

      END
