      SUBROUTINE RTOU( X, Y, U, V )
*+
*   Name:
*      SUBROUTINE RTOU
*
*   Description:
*      The coordinates (X,Y) are undistorted to (U,V) using the
*     distortion data in CMROTR.
*     (X,Y) are really pixel coordinates which may be RAW.
*
*   History:
*      Jack Giddings      31-DEC-81     IUEDR Vn. 1.0
*      Paul Rees          20-OCT-88     IUEDR Vn. 2.0
*
*   Method:
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      REAL*8 X     ! S-value
      REAL*8 Y     ! L-value

*   Export:
      REAL*8 U     ! U-value
      REAL*8 V     ! V-value

*   CMROTR:
      INCLUDE 'CMROTR'
*.

      U = DUDX * X + DUDY * Y + DU
      V = DVDX * X + DVDY * Y + DV

      END
