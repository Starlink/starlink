
C===========================================================================

      SUBROUTINE PERIOD_LOADOUTALL(OPARRAY, NUMROWS, NUMCOLS,
     :                             WRK1, WRK2, NUMWRK)

C===========================================================================
C Loads output arrays for PERIOD
C All arrays can correspond to slices of dynamically-allocated memory,
C provided that the appropriate "calling" arguments are memory pointers
C being passed via the %VAL() intrinsic function.
C
C Written by Kevin P Duffey @RAL, October 2001
C===========================================================================

      IMPLICIT NONE

      INTEGER NUMROWS, NUMCOLS, NUMWRK, I
      DOUBLE PRECISION OPARRAY(NUMROWS,NUMCOLS)
      DOUBLE PRECISION WRK1(NUMWRK), WRK2(NUMWRK)

C---------------------------------------------------------------------------
C Copy real values
C---------------------------------------------------------------------------

      DO 10 I = 1, NUMROWS
         OPARRAY(I, 1) = WRK1(I)
         OPARRAY(I, 2) = WRK2(I)
         OPARRAY(I, 3) = 0.0D0
   10 CONTINUE

      RETURN
      END
