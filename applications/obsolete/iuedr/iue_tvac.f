      SUBROUTINE IUE_TVAC(NPOINT, X)

*+
*
*   Name:
*      SUBROUTINE IUE_TVAC
*
*   Description:
*      Correct Air wavelengths to Vacuum.
*
*   History:
*      Jack Giddings      24-JUN-81     IUEDR Vn. 1.0
*      Paul Rees          04-OCT-88     IUEDR Vn. 2.0
*
*   Method:
*      The wavelengths are converted from air to vacuum for values above
*      1999.344A.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      INTEGER NPOINT     ! number of wavelengths

*   Import-Export:
      REAL*8 X(NPOINT)     ! Vacuum Wavelengths

*   Local variables:
      INTEGER I          ! loop index

*   Internal references:
      REAL*8 CSCALE
      REAL*8 W
      CSCALE(W) = 1.0 - 1.0*2.871E-4*(1.0 + 5.67E5/W**2)

      DO 100 I = 1, NPOINT

         IF (X(I).GT.1999.344) X(I) = X(I)/CSCALE(X(I))

 100  CONTINUE

      END
