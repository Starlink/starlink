      SUBROUTINE TEST1( STATUS )
*+
*{a_task_prologue}
*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER NX
      INTEGER NY
      INTEGER IPTR, IPTR2

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL IMG_IN( 'IN', NX, NY, IPTR, STATUS )
      WRITE(*,*)'NX, NY, IPTR', NX, NY, IPTR

      IF (STATUS .EQ. SAI__OK)
     : CALL SUMIT( NX, NY, %VAL( IPTR ), STATUS )

      CALL IMG_OUT( 'IN', 'OUT', IPTR2, STATUS )

      CALL IMG_FREE( '*', STATUS )
      END

      SUBROUTINE SUMIT( NX, NY, ARRAY, STATUS )
      IMPLICIT NONE
      INTEGER NX, NY, STATUS, I, J
      REAL ARRAY( NX, NY ), SUM

      SUM = 0.0
      DO J = 1, NY
        DO I = 1, NX
           SUM = SUM + ARRAY( I, J )
        ENDDO
      ENDDO

      WRITE(*,*)'NX, NY, SUM', NX, NY, SUM
      END
