      SUBROUTINE SLCURS( STATUS )
*+
*  Name:
*     SUBROUTINE SLCURS
*
*  Description:
*     Use graphics cursor on image display.  Get image information and
*     branch on resolution.
*
*  History:
*     Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*     Paul Rees          08-SEP-88     IUEDR Vn. 2.0
*     Martin Clayton     18-OCT-94     IUEDR Vn. 3.1-8

*-

*  Implicit:
      IMPLICIT NONE

*  Starlink includes:
      INCLUDE 'SAE_PAR'

*  Export:
      INTEGER STATUS        ! status return

*  External references:
      LOGICAL STR_SIMLR     ! caseless string equality

*  Global variables:
      INCLUDE 'CMHEAD'

*  Local variables:
      INTEGER NAXIS1        ! X-axis size of image
      INTEGER NAXIS2        ! Y-axis size of image
      INTEGER D_VM          ! data VM address
      INTEGER Q_VM          ! quality VM address
*.

*  Check inherited Global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get Calibration.
      CALL DASSOC( 'I\\', 'T\\', STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: could not access dataset\\', STATUS )

      ELSE
         CALL MRDATA( NAXIS1, NAXIS2, D_VM, Q_VM, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN
            IF ( STR_SIMLR( 'HIRES\\', RESOL ) ) THEN
               CALL HISLCU( NAXIS1, NAXIS2, %VAL(D_VM), %VAL(Q_VM),
     :                      STATUS )

            ELSE IF ( STR_SIMLR( 'LORES\\', RESOL ) ) THEN
               CALL LOSLCU( NAXIS1, NAXIS2, %VAL(D_VM), %VAL(Q_VM),
     :                      STATUS )
            ELSE
               CALL ERROUT( 'Error: unknown resolution\\', STATUS )
            END IF
         END IF
      END IF

      END
