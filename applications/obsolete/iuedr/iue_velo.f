      SUBROUTINE IUE_VELO( VKMS, NPOINT, X )
*+
*  Name:
*     SUBROUTINE IUE_VELO

*  Description:
*     Correct wavelengths for Radial Velocity.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IUE_VELO( VKMS, NPOINT, X )

*  Arguments:
*     VKMS = REAL*8 (Given)
*        Radial velocity in km/s.
*     NPOINT = INTEGER (Given)
*        Number of wavelengths to correct.
*     X = REAL*8 ( NPOINT ) (Given and Returned)
*        The wavelength array to be corrected.

*  Method:
*     If VOBJECT is non-zero, correct wavelengths appropriately.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     24-JUN-81 (JRG):
*       IUEDR Vn. 1.0
*     04-OCT-88 (PCTR):
*       IUEDR Vn. 2.0
*     19-DEC-94 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Arguments Given:
      REAL*8 VKMS        ! object radial velocity

      INTEGER NPOINT     ! number of wavelengths

*  Arguments Given and Returned:
      REAL*8 X( NPOINT ) ! Vacuum Wavelengths

*   Local variables:
      REAL*8 FW          ! 1 + Redshift

      INTEGER I          ! loop index

*.

      FW = 1.0 + VKMS / 2.997925E5

      IF ( FW.GT.0.9 .AND. ABS( VKMS ).GT.0.001 ) THEN
         DO I = 1, NPOINT
            X( I ) = X( I ) / FW
         END DO
      END IF

      END
