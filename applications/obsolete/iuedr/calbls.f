      SUBROUTINE CALBLS( IAPER, ORD )

*+
*
*   Name:
*      SUBROUTINE CALBLS
*
*   Description:
*      Calibrate LBLS wavelengths (no velocity shifts).
*
*   History:
*      Jack Giddings      25-JUN-81     IUEDR Vn. 1.0
*      Paul Rees          04-NOV-88     IUEDR Vn. 2.0
*      Martin Clayton     05-OCT-94     IUEDR Vn. 3.1-6
*
*   Method:
*      This will perform full wavelength calibration for a specified
*      "order" M. Ignore ORD for LORES.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      INTEGER IAPER     ! aperture index
      INTEGER ORD       ! nominal order

*   Global variables:
      INCLUDE 'CMLBLS'
      INCLUDE 'CMECOR'
      INCLUDE 'CMWCOR'

*   Local variables:
      INTEGER I         ! loop index

      IF ( .NOT. NOECOR ) THEN
         DO I = 1, NV
            VS(I) = VS(I) + ECOR(1) / DBLE(ORD)
         END DO
      END IF

      IF ( .NOT. NOWCOR ) THEN
         DO I = 1, NV
            VS(I) = VS(I) + WCOR(IAPER)
         END DO
      END IF

      CALL IUE_TAIR( NV, VS )

      END
