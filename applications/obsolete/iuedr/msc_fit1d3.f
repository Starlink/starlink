      SUBROUTINE MSC_FIT1D3(X, Y, C)
*+
*   Name:
*      SUBROUTINE MSC_FIT1D3
*
*   Description:
*      Parabolic interpolation coefficients.
*
*   History:
*      Jack Giddings      18-AUG-81     IUEDR Vn. 1.0
*      Paul Rees          28-OCT-88     IUEDR Vn. 2.0
*
*   Method:
*      Use analytic formula.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      REAL*8 X( 3 )     ! x-values
      REAL*8 Y( 3 )     ! y-values

*   Export:
      REAL*8 C( 3 )     ! interpolation coefficients

*   Local variables:
      REAL*8 C21      ! gradient between points 1 and 2
      REAL*8 C32      ! gradient between points 2 and 3
*.

      C32 = ( Y( 3 ) - Y( 2 ) ) / ( X( 3 ) - X( 2 ) )
      C21 = ( Y( 2 ) - Y( 1 ) ) / ( X( 2 ) - X( 1 ) )
      C( 3 ) = ( C32 - C21 ) / ( X( 3 ) - X( 1 ) )
      C( 2 ) = C32 - C( 3 ) * ( X( 3 ) + X( 2 ) )
      C( 1 ) = Y( 3 ) - ( C( 3 ) * X( 3 ) + C( 2 )) * X( 3 )

      END
