      SUBROUTINE GEN_MOVEI( NELM, ARRAY1, ARRAY2 )
*
*    Routine to copy a INTEGER array from ARRAY1 to ARRAY2
*
      IMPLICIT NONE
      INTEGER  NELM                 ! Number of elements in array
      INTEGER  ARRAY1( NELM )       ! Input integer array
      INTEGER  ARRAY2( NELM )       ! Output integer array
      INTEGER  I                    ! A counter

*    Copy the array over
      DO I = 1, NELM, 1
         ARRAY2( I ) = ARRAY1( I )
      END DO
      END
