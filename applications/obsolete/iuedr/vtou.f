      SUBROUTINE VTOU( V, U )
*+
*   Name:
*      SUBROUTINE VTOU
*
*   Description:
*      For given (V,M) coordinate, the U coordinate is created.
*      It is assumed that SETUV and HISET have been called properly.
*
*   History:
*      Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*      Paul Rees          20-OCT-88     IUEDR Vn. 2.0
*
*   Method:
*      A necessarily iterative approach is used.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      REAL*8 V      ! V-value

*   Export:
      REAL*8 U      ! U-value for centre of order

*   CMDISH:
      INCLUDE 'CMDISH'

*   Local variables:
      REAL*8 VP     ! temporary V-value
      REAL*8 W      ! W-value
      REAL*8 R      ! R-value
*.

*   Guess W based on V
      W = WC + V / DVDW

*   Compute U(M,W)
      CALL WTOU( 0.0d0, W, U, VP )
      CALL UTOW( U, V, R, W )

*   Iterate until abs(r) < TOLERENCE
 100  CONTINUE

      IF ( .NOT.( ABS( R ) .GT. 0.05 ) ) THEN
         GO TO 999

      ELSE
         U = U - DUDR * R
         CALL UTOW( U, V, R, W )
         GO TO 100
      END IF

 999  CONTINUE

      END
