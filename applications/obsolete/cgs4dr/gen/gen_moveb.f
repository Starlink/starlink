      SUBROUTINE GEN_MOVEB( NELM, ARRAY1, ARRAY2 )
*
*    Routine to copy a BYTE array from ARRAY1 to ARRAY2
*
      IMPLICIT NONE
      INTEGER  NELM                 ! Number of elements in array
      BYTE     ARRAY1( NELM )       ! Input byte array
      BYTE     ARRAY2( NELM )       ! Output byte array
      INTEGER  I                    ! A counter

*    Copy the array over
      DO I = 1, NELM, 1
         ARRAY2( I ) = ARRAY1( I )
      END DO
      END
