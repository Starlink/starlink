      SUBROUTINE GTOW( X, Y, R, W )
*+
*   Name:
*      SUBROUTINE GTOW
*
*   Description:
*      The coordinates (X,Y) are distorted to (R,W) using the
*      distortion data in CMDISH.
*
*   History:
*      Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*      Paul Rees          20-OCT-88     IUEDR Vn. 2.0
*
*   Method:
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      REAL*8 X      ! geometric S-value
      REAL*8 Y      ! geometric L-value

*   Export:
      REAL*8 R      ! radial distance from dispersion line
      REAL*8 W      ! wavelength

*   CMDISH:
      INCLUDE 'CMDISH'

*   Local variables:
      REAL*8 C0     ! constant term
      REAL*8 C1     ! linear term
      REAL*8 C2     ! parabolic term
      REAL*8 DW     ! wavelength uncertainty
      REAL*8 WM     ! minimum wavelength
      REAL*8 WP     ! maximum wavelength
*.

      C0 = DYDR * ( A0 - X ) - DXDR * ( B0 - Y )
      C1 = DYDR * A1 - DXDR * B1
      C2 = DYDR * A2 - DXDR * B2

      IF ( C2 .EQ. 0.0 ) THEN
         W = -C0 / C1

      ELSE
         DW = SQRT( C1 * C1 - 4.0 * C0 * C2 )
         WM = ( - C1 - DW ) / ( 2.0 * C2 )
         WP = ( - C1 + DW ) / ( 2.0 * C2 )

         IF ( ABS( WP - WC ) .LT. ABS( WM - WC ) ) THEN
            W = WP

         ELSE
            W = WM
         END IF
      END IF

      R = ( X - ( A0 + W * ( A1 + W * A2 ) ) +
     :      Y - ( B0 + W * ( B1 + W * B2 ) ) ) /
     :      ( DXDR + DYDR )

      END
