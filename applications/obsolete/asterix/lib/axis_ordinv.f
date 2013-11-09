*+  AXIS_ORDINV - Invert an axis order array
      SUBROUTINE AXIS_ORDINV( NDIM, IDIMS, ODIMS )
*
*    Description :
*
*     Inverts an axis order array.
*
*    History :
*
*      8 May 19 : Original (BHVAD::DJA)
*
*    Type definitions :
      IMPLICIT NONE
*
*    Import :
*
      INTEGER                      NDIM
      INTEGER                      IDIMS(NDIM)
*
*    Export :
*
      INTEGER                      ODIMS(NDIM)
*
*    Local variables :
*
      INTEGER                      I,J
*-

      DO I = 1, NDIM
        J = 1
        DO WHILE ( IDIMS(J) .NE. I )
          J = J + 1
        END DO
        ODIMS(I) = J
      END DO

      END
