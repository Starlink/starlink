
C===========================================================================

      SUBROUTINE PERIOD_SETXYEDATA(IPARRAY, NUMROWS, NUMCOLS,
     :                             SAMPLE, YERROR, XDATA, YDATA,
     :                             YRAN, YERR, ERAN)

C===========================================================================
C Presets XDATA, YDATA, YRAN, YERR and ERAN 1-d arrays prior to a loop
C through trial frequencies in PERIOD.
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
      DOUBLE PRECISION YERR(NUMROWS), ERAN(NUMROWS)
      LOGICAL YERROR

C---------------------------------------------------------------------------
C Copy real values
C---------------------------------------------------------------------------

      DO 10 I = 1, NUMROWS
         XDATA(I) = IPARRAY(I, 1)
         IF ( SAMPLE.EQ.1 ) YRAN(I) = IPARRAY(I, 2)
         YDATA(I) = YRAN(I)
         IF ( .NOT.YERROR ) THEN
            YERR(I) = 1.0D0
         ELSE
            IF ( SAMPLE.EQ.1 ) ERAN(I) = IPARRAY(I, 3)
            YERR(I) = ERAN(I)
         END IF
   10 CONTINUE

      RETURN
      END
