      SUBROUTINE UTOR( U, V, X, Y )
*+
*   Name:
*      SUBROUTINE UTOR
*
*   Description:
*      The coordinates (U,V) are undistorted to (X,Y) using the
*      distortion data in CMROTR.
*      (X,Y) are RAW pixel coordinates.
*
*   History:
*      Jack Giddings      31-DEC-81     IUEDR Vn. 1.0
*      Paul Rees          20-OCT-88     IUEDR Vn. 2.0
*      Martin Clayton     15-SEP-94     IUEDR Vn. 3.1-3
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      REAL*8 U     ! U-value
      REAL*8 V     ! V-value

*   Export:
      REAL*8 X     ! geometric S-value
      REAL*8 Y     ! geometric L-value

*   CMROTR:
      INCLUDE 'CMROTR'

      X = DXDU * U + DXDV * V + DX
      Y = DYDU * U + DYDV * V + DY

      END
