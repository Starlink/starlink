
C===========================================================================

      SUBROUTINE PERIOD_LOADOUTONE(OPARRAY, NUMROWS, NUMCOLS,
     :                             IROW, VAL1, VAL2)

C===========================================================================
C Loads output arrays for PERIOD
C All arrays can correspond to slices of dynamically-allocated memory,
C provided that the appropriate "calling" arguments are memory pointers
C being passed via the %VAL() intrinsic function.
C
C Written by Kevin P Duffey @RAL, October 2001
C===========================================================================

      IMPLICIT NONE

      INTEGER NUMROWS, NUMCOLS, IROW
      DOUBLE PRECISION OPARRAY(NUMROWS,NUMCOLS)
      DOUBLE PRECISION VAL1, VAL2

C---------------------------------------------------------------------------
C Copy real values
C---------------------------------------------------------------------------

      OPARRAY(IROW, 1) = VAL1
      OPARRAY(IROW, 2) = VAL2
      OPARRAY(IROW, 3) = 0.0D0

      RETURN
      END
