C AVERAGE AND VARIANCE SUBROUTINE FROM NUMERICAL RECIPES.
C
      SUBROUTINE TIM_AVEVAR(DATA,N,AVE,VAR)
*
      IMPLICIT REAL (A-H,O-Z)
*
      REAL DATA(N)
*
      AVE=0.0
      VAR=0.0
*
      DO J=1,N
        AVE=AVE+DATA(J)
      ENDDO
*
      AVE=AVE/N
*
      DO J=1,N
        S=DATA(J)-AVE
        VAR=VAR+S*S
      ENDDO
*
      VAR=VAR/(N-1)
*
      END
