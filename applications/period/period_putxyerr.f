
C===========================================================================

      SUBROUTINE PERIOD_PUTXYERR(XDATA, YDATA, YERR, OPARRAY,
     :                           NUMROWS, NUMCOLS)

C===========================================================================
C Loads OPARRAY with real values from XDATA, YDATA and YERR 1-d arrays,
C following data folding and sorting in PHASE.
C All arrays can correspond to slices of dynamically-allocated memory,
C provided that the appropriate "calling" arguments are memory pointers
C being passed via the %VAL() intrinsic function.
C
C Written by Kevin P Duffey @RAL, October 2001
C===========================================================================

      IMPLICIT NONE

      INTEGER NUMROWS, NUMCOLS, I
      DOUBLE PRECISION OPARRAY(NUMROWS,NUMCOLS)
      DOUBLE PRECISION XDATA(NUMROWS), YDATA(NUMROWS), YERR(NUMROWS)

C---------------------------------------------------------------------------
C Copy real values
C---------------------------------------------------------------------------

      DO 10 I = 1, NUMROWS
         OPARRAY(I, 1) = XDATA(I)
         OPARRAY(I, 2) = YDATA(I)
         OPARRAY(I, 3) = YERR(I)
   10 CONTINUE

      RETURN
      END
