
C===========================================================================

      SUBROUTINE PERIOD_SETDATASQRT(IPARRAY, NUMROWS, NUMCOLS,
     :                              NCOL, OPARRAY)

C===========================================================================
C Prepares data for NOISE.
C Both arrays can correspond to slices of dynamically-allocated memory,
C provided that the appropriate "calling" arguments are memory pointers
C being passed via the %VAL() intrinsic function.
C
C Written by Kevin P Duffey @RAL, October 2001
C===========================================================================

      IMPLICIT NONE

      INTEGER NUMROWS, NUMCOLS, NCOL, I
      DOUBLE PRECISION IPARRAY(NUMROWS,NUMCOLS), OPARRAY(NUMROWS)

C---------------------------------------------------------------------------
C Copy real values
C---------------------------------------------------------------------------

      DO 10 I = 1, NCOL
         OPARRAY(I) = DSQRT((IPARRAY(I+1, 1) - IPARRAY(I, 1))**2)
  10  CONTINUE

      RETURN
      END
