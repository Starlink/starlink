
C===========================================================================

      SUBROUTINE PERIOD_SETX(IPARRAY, NUMROWS, NUMCOLS,
     :                       YERROR, OPARRAY)

C===========================================================================
C Prepares LSQUAR data for DETREND. Both arrays can correspond to slices
C of dynamically-allocated memory, provided that the appropriate "calling"
C arguments are memory pointers being passed via the %VAL() intrinsic
C function.
C
C Written by Kevin P Duffey @RAL, October 2001
C===========================================================================

      IMPLICIT NONE

      INTEGER NUMROWS, NUMCOLS, I
      DOUBLE PRECISION IPARRAY(NUMROWS,NUMCOLS)
      DOUBLE PRECISION OPARRAY(NUMCOLS,NUMROWS)
      LOGICAL YERROR

C---------------------------------------------------------------------------
C Copy real values
C---------------------------------------------------------------------------

      DO 10 I = 1, NUMROWS
         OPARRAY(1, I) = IPARRAY(I, 1)
         OPARRAY(2, I) = IPARRAY(I, 2)
         IF ( YERROR ) THEN
            OPARRAY(3, I) = IPARRAY(I, 3)
         ELSE
            OPARRAY(3, I) = 1.0D0
         END IF
  10  CONTINUE

      RETURN
      END
