
C===========================================================================

      SUBROUTINE PERIOD_SETXYDATA(IPARRAY, NUMROWS, NUMCOLS,
     :                            SAMPLE, XDATA, YDATA, YRAN)

C===========================================================================
C Presets XDATA, YDATA and YRAN 1-d arrays prior to a loop through trial
C trial frequencies in PERIOD.
C All arrays can correspond to slices of dynamically-allocated memory,
C provided that the appropriate "calling" arguments are memory pointers
C being passed via the %VAL() intrinsic function.
C
C Written by Kevin P Duffey @RAL, October 2001
C===========================================================================

      IMPLICIT NONE

      INTEGER NUMROWS, NUMCOLS, SAMPLE, I
      DOUBLE PRECISION IPARRAY(NUMROWS,NUMCOLS)
      DOUBLE PRECISION XDATA(NUMROWS), YDATA(NUMROWS), YRAN(NUMROWS)

C---------------------------------------------------------------------------
C Copy real values
C---------------------------------------------------------------------------

      DO 10 I = 1, NUMROWS
         XDATA(I) = IPARRAY(I, 1)
         IF ( SAMPLE.EQ.1 ) YRAN(I) = IPARRAY(I, 2)
         YDATA(I) = YRAN(I)
   10 CONTINUE

      RETURN
      END
