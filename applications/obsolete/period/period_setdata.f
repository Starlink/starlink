
C===========================================================================

      SUBROUTINE PERIOD_SETDATA(IPARRAY, NUMROWS, NUMCOLS,
     :                          ICOL, OPARRAY)

C===========================================================================
C Copies a column from a 2-d array IPARRAY to a 1-d array OPARRAY. Both
C arrays can correspond to slices of dynamically-allocated memory,
C provided that the appropriate "calling" arguments are memory pointers
C being passed via the %VAL() intrinsic function.
C
C Written by Kevin P Duffey @RAL, October 2001
C===========================================================================

      IMPLICIT NONE

      INTEGER NUMROWS, NUMCOLS, ICOL, I
      DOUBLE PRECISION IPARRAY(NUMROWS,NUMCOLS), OPARRAY(NUMROWS)

C---------------------------------------------------------------------------
C Copy real values
C---------------------------------------------------------------------------

      DO 10 I = 1, NUMROWS
         OPARRAY(I) = IPARRAY(I, ICOL)
   10 CONTINUE

      RETURN
      END
