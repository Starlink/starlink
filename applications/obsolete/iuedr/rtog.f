      SUBROUTINE RTOG( S, L, X, Y )
*+
*   Name:
*      SUBROUTINE RTOG
*
*   Description:
*      The coordinates (S,L) are undistorted to (X,Y) using the
*      distortion data in CMGEOM.
*
*   History:
*      Jack Giddings      31-DEC-81     IUEDR Vn. 1.0
*      Paul Rees          20-OCT-88     IUEDR Vn. 2.0
*
*   Method:
*      Very slow method used!
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      REAL*8 S          ! raw S-value
      REAL*8 L          ! raw L-value

*   Export:
      REAL*8 X          ! geometric S-value
      REAL*8 Y          ! geometric L-value

*   Global variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMGEOM'

*   Local variables:
      REAL*8 T1(36)   ! Tn(axis1) values
      REAL*8 T2(36)   ! Tn(axis2) values
      REAL*8 LP       ! scaled L-value
      REAL*8 SP       ! scaled S-value
      REAL*8 TCALC

      INTEGER IQ      ! term index
      INTEGER I1      ! axis1 index
      INTEGER I2      ! axis2 index
*.

      IF ( GEOM ) THEN
         X = S
         Y = L

      ELSE
         SP = 2.0 * (S - GAXMIN(1)) / (GAXMAX(1) - GAXMIN(1)) - 1.0
         LP = 2.0 * (L - GAXMIN(2)) / (GAXMAX(2) - GAXMIN(2)) - 1.0
         CALL MSC_CHEP( SP, NGTERM( 1 ), T1 )
         CALL MSC_CHEP( LP, NGTERM( 2 ), T2 )
         X = 0.0
         Y = 0.0
         IQ = 0

         DO I1 = 1, NGTERM( 1 )
            DO I2 = 1, NGTERM( 2 )
               IQ = IQ + 1
               TCALC = T1( I1 ) * T2( I2 )
               X = X + GCHEBX( IQ ) * TCALC
               Y = Y + GCHEBY( IQ ) * TCALC
            END DO
         END DO
      END IF

      END
