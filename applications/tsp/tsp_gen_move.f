
      SUBROUTINE TSP_GEN_MOVE(NB,A,B)
      IMPLICIT NONE
      INTEGER NB
      INTEGER A(NB), B(NB)
      INTEGER I
      DO I=1,NB
          B(I)=A(I)
      ENDDO
      END


