      SUBROUTINE CAWAV( IAPER, ORD, N, W )
*+
*  Name:
*     SUBROUTINE CAWAV

*  Purpose:
*     Perform full wavelength calibration for a specified order.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CAWAV( IAPER, ORD, N, W )

*  Arguments:

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     25-JUN-81 (JRG):
*       IUEDR Vn. 1.0
*     13-OCT-88 (PCTR):
*       IUEDR Vn. 2.0
*     05-OCT-94 (MJC):
*       IUEDR Vn. 3.1-6
*     17-OCT-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Arguments Given:
      INTEGER IAPER     ! Aperture index.
      INTEGER ORD       ! Nominal order.
      INTEGER N         ! Number of wavelengths.

*  Arguments Given and Returned:
      REAL*8 W( N )     ! Wavelengths to be calibrated.

*  Global Variables:
      INCLUDE 'CMECOR'
      INCLUDE 'CMWCOR'
      INCLUDE 'CMVEL'

*  Local Variables:
      INTEGER I         ! Loop index.
*.

      IF ( .NOT. NOECOR ) THEN
         DO I = 1, N
            W( I ) = W( I ) + ECOR( 1 ) / DBLE( ORD )
         END DO
      END IF

      IF ( .NOT. NOWCOR ) THEN
         DO I = 1, N
            W( I ) = W( I ) + WCOR( IAPER )
         END DO
      END IF

      IF ( .NOT. NOVEL ) THEN
         CALL IUE_VELO( VEL( IAPER ), N, W )
      END IF
      CALL IUE_TAIR( N, W )

      END
