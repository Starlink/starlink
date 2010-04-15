C+
      SUBROUTINE ECH_ARFLAG(IORDERS,NI,NORDERS,F1,F2,FIT)
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NI,NORDERS,F1,F2
      INTEGER FIT(NORDERS)
      REAL IORDERS(NI)
C
C     Local variables
C
      INTEGER I,J
C
C     Initialize FIT array to 0's
C
      DO J=1,NORDERS,1
         FIT(J)=0
      END DO
C
C     Locate each of the interactive orders IORDERS and set FIT=1
C
      DO I=1,NI,1
         J=ABS(IORDERS(I)-F1)+1
         FIT(J)=1
      END DO
C
C     That's all, folks ...
C
      RETURN
      END
