
C===========================================================================

      SUBROUTINE PERIOD_SETUPWINDOW(IPARRAY, NUMROWS, NUMCOLS,
     :                              WINDOW, EWINDOW, OPARRAY)

C===========================================================================
C Prepares data for WINDOW. Both arrays can correspond to slices of
C dynamically-allocated memory, provided that the appropriate "calling"
C arguments are memory pointers being passed via the %VAL() intrinsic
C function.
C
C Written by Kevin P Duffey @RAL, October 2001
C===========================================================================

      IMPLICIT NONE

      INTEGER NUMROWS, NUMCOLS, I
      DOUBLE PRECISION IPARRAY(NUMROWS,NUMCOLS), WINDOW, EWINDOW
      DOUBLE PRECISION OPARRAY(NUMROWS,NUMCOLS)

C---------------------------------------------------------------------------
C Copy real values
C---------------------------------------------------------------------------

      DO 10 I = 1, NUMROWS
         OPARRAY(I, 1) = IPARRAY(I, 1)
         OPARRAY(I, 2) = WINDOW
         OPARRAY(I, 3) = EWINDOW
   10 CONTINUE

      RETURN
      END
