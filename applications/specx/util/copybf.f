C-----------------------------------------------------------------------

      SUBROUTINE COPYBF (IQ, BUF)

C  Routine to copy a single quadrant of DATA to the array BUF

      REAL*4   BUF(1)
      INCLUDE 'STACKCOMM'

      NOFF=NTOT(IQ-1)
      DO I=1,NPTS(IQ)
        BUF(I)=DATA(NOFF+I)
      END DO

      RETURN
      END


