      SUBROUTINE SETMAX (SCALE, ISTEP, SIZE)

C  Routine to set up array of ordinates (as offsets) for R.A. and Dec.
C  analogous to SETX for frequency/velocity etc.

      REAL*4 SCALE(1)

      DO I1=1,ISTEP
        SCALE(I1)=FLOAT((ISTEP+1-I1)*2-ISTEP-1)*SIZE/2.  !was /120.
      END DO

      RETURN
      END
