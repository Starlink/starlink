C------------------------------------------------------------------------------

      SUBROUTINE CUV(ALPHA,BETA,X,Y,Z)

C     CONVERTS POLAR CO-ORDS ALPHA,BETA, TO NORMAL RECT.CO-ORDS, (X,Y,Z)
C
      IMPLICIT REAL*8(A-H,O-Z)

      COSBET=DCOS(BETA)
      X=DCOS(ALPHA)*COSBET
      Y=DSIN(ALPHA)*COSBET
      Z=DSIN(BETA)

      RETURN
      END

C------------------------------------------------------------------------------
