      SUBROUTINE GTOR(X, Y, S, L)
*+
*   Name:
*      SUBROUTINE GTOR
*
*   Description:
*      The coordinates (X,Y) are distorted to (S,L) using the
*      distortion data in CMGEOM.
*
*   History:
*      Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*      Paul Rees          20-OCT-88     IUEDR Vn. 2.0
*      Martin Clayton     11-AUG-94     IUEDR Vn. 3.1-2
*
*   Method:
*      Very slow method used!
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      REAL*8 X          ! geometric S-value
      REAL*8 Y          ! geometric L-value

*   Export:
      REAL*8 S          ! raw S-value
      REAL*8 L          ! raw L-value

*   Global variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMGEOM'

*   Local variables:
      REAL*8 T1(36)     ! Tn(axis1) values
      REAL*8 T2(36)     ! Tn(axis2) values
      REAL*8 XP         ! scaled X-value
      REAL*8 YP         ! scaled Y-value
      REAL*8 DCALC      ! Temporary storage

      INTEGER IQ      ! term index
      INTEGER I1      ! axis1 index
      INTEGER I2      ! axis2 index
*.

      IF ( GEOM ) THEN
         S = X
         L = Y

      ELSE
         XP = 2.0*(X - GAXMIN(1))/(GAXMAX(1) - GAXMIN(1)) - 1.0
         YP = 2.0*(Y - GAXMIN(2))/(GAXMAX(2) - GAXMIN(2)) - 1.0
         CALL MSC_CHEP( XP, NGTERM( 1 ), T1 )
         CALL MSC_CHEP( YP, NGTERM( 2 ), T2 )
         S = 0.0
         L = 0.0
         IQ = 0

         DO I1 = 1, NGTERM( 1 )
            DO I2 = 1, NGTERM( 2 )
               IQ = IQ + 1
               DCALC = T1( I1 ) * T2( I2 )
               S = S + GCHEBS( IQ ) * DCALC
               L = L + GCHEBL( IQ ) * DCALC
            END DO
         END DO

      END IF

      END
