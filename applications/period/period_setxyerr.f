
C===========================================================================

      SUBROUTINE PERIOD_SETXYERR(IPARRAY, NUMROWS, NUMCOLS,
     :                            XDATA, YDATA, YERR)

C===========================================================================
C Presets XDATA, YDATA and YERR 1-d arrays prior to folding and sorting
C data in PHASE.
C All arrays can correspond to slices of dynamically-allocated memory,
C provided that the appropriate "calling" arguments are memory pointers
C being passed via the %VAL() intrinsic function.
C
C Written by Kevin P Duffey @RAL, October 2001
C===========================================================================

      IMPLICIT NONE

      INTEGER NUMROWS, NUMCOLS, I
      DOUBLE PRECISION IPARRAY(NUMROWS,NUMCOLS)
      DOUBLE PRECISION XDATA(NUMROWS), YDATA(NUMROWS), YERR(NUMROWS)

C---------------------------------------------------------------------------
C Copy real values
C---------------------------------------------------------------------------

      DO 10 I = 1, NUMROWS
         XDATA(I) = IPARRAY(I, 1)
         YDATA(I) = IPARRAY(I, 2)
         YERR(I) = IPARRAY(I, 3)
   10 CONTINUE

      RETURN
      END
