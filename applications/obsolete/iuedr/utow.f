      SUBROUTINE UTOW( U, V, R, W )
*+
*   Name:
*      SUBROUTINE UTOW
*
*   Description:
*      The coordinates (U,V) are distorted to (R,W) using the
*      distortion data in CMDISH.
*
*   History:
*      Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*      Paul Rees          20-OCT-88     IUEDR Vn. 2.0
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      REAL*8 U     ! U-value
      REAL*8 V     ! V-value

*   Export:
      REAL*8 R     ! radial distance from dispersion line
      REAL*8 W     ! wavelength

*   Local variables:
      REAL*8 S     ! S-value
      REAL*8 L     ! L-value
*.

      CALL UTOR( U, V, S, L )
      CALL RTOW( S, L, R, W )

      END
