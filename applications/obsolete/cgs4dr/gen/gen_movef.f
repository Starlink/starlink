      SUBROUTINE GEN_MOVEF( NELM, ARRAY1, ARRAY2 )
*
*    Routine to copy a FLOAT array from ARRAY1 to ARRAY2
*
      IMPLICIT NONE
      INTEGER  NELM                 ! Number of elements in array
      REAL     ARRAY1( NELM )       ! Input floar array
      REAL     ARRAY2( NELM )       ! Output float array
      INTEGER  I                    ! A counter

*    Copy the array over
      DO I = 1, NELM, 1
         ARRAY2( I ) = ARRAY1( I )
      END DO
      END
