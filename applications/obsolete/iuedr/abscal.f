      SUBROUTINE ABSCAL( IAPER, STATUS )
*+
*  Name:
*     SUBROUTINE ABSCAL

*  Purpose:
*     The contents of CMFLX are scaled by multiplying by the
*     calibration curve in CMABS.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ABSCAL( IAPER, STATUS )

*  Arguments:
*     IAPER = INTEGER (Given)
*        Which aperture is used.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     The exposure time and arbitrary scale factor have already been included.

*  Method:
*     The absolute calibration table is mapped onto the spectrum
*     wavelength grid;  then the spectrum is multiplied by this.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     25-JUN-81 (JRG):
*       IUEDR Vn. 1.0
*     11-DEC-87 (PCTR):
*       IUEDR Vn. 1.4
*     07-OCT-88 (PCTR):
*       IUEDR Vn. 2.0
*     08-OCT-94 (MJC):
*       IUEDR Vn. 3.1-6
*     16-DEC-94 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      INTEGER IAPER      ! Aperture index.

*  Status:
      INTEGER STATUS     ! Global status.

*  Global Variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMWAV'
      INCLUDE 'CMABS'
      INCLUDE 'CMCAL'
      INCLUDE 'CMFLX'

*  Local Variables:
      REAL*8 FY( 1200 )  ! Mapped calibration spectrum.
      REAL*8 SCALE       ! Composite scale factor.
      REAL*8 SFAC        ! Local scaling factor.

      INTEGER I          ! Loop index.
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Map calibration onto the spectrum mesh.
      IF ( .NOT.NOABS .AND. NABS.GE.2 ) THEN
         CALL MSC_MAP1D4( NABS, XABS, YCORR, NWAV, WAVAIR, FY, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            GO TO 999
         END IF

*      Temperature sensitivity.
         IF ( THDA .NE. 0.0 ) THEN
            SCALE = 1.0 + ( THDA - TSEN( 1 ) ) * TSEN( 2 )

         ELSE
            SCALE = 1.0
         END IF

*      Modify labels.
         IF ( TSECS( IAPER ) .GT. 0.0 ) THEN
            CALL STR_MOVE( '(cgs/s/A)\\', 40, FUNITS )

         ELSE
            CALL STR_MOVE( '(cgs/A)\\', 40, FUNITS )
         END IF

*      Modify calibration.
         DO I = 1, NWAV
            IF ( QCAL( I ) .EQ. 0 ) THEN
               IF ( WAVAIR( I ).GE.XABS( 1 ) .AND.
     :              WAVAIR( I ).LE.XABS( NABS ) ) THEN
                  SFAC = FY( I ) / SCALE
                  IF ( SFAC .GT. 0.0 ) THEN
                     SCAL( I ) = SCAL( I ) / SFAC

                  ELSE
                     QCAL( I ) = 1
                  END IF

               ELSE
                  QCAL( I ) = 1
               END IF
            END IF
         END DO
      END IF

 999  CONTINUE

      END
