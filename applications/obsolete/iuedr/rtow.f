      SUBROUTINE RTOW( S, L, R, W )
*+
*   Name:
*      SUBROUTINE RTOW
*
*   Description:
*      The coordinates (S,L) are distorted to (R,W).
*
*   History:
*      Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*      Paul Rees          20-OCT-88     IUEDR Vn. 2.0
*
*   Method:
*      Use RTOG and GTOW.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      REAL*8 S     ! raw S-value
      REAL*8 L     ! raw L-value

*   Export:
      REAL*8 R     ! radial distance from dispersion line
      REAL*8 W     ! wavelength

*   Local variables:
      REAL*8 X     ! geometric S-value
      REAL*8 Y     ! geometric L-value
*.

      CALL RTOG( S, L, X, Y )
      CALL GTOW( X, Y, R, W )

      END
