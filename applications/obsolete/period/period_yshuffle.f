
C===========================================================================

      SUBROUTINE PERIOD_YSHUFFLE(SEED, YRAN, NUMROWS)

C===========================================================================
C Randomizes a time-series for a permutation-run in PERIOD. All
C arrays can correspond to slices of dynamically-allocated memory,
C provided that the appropriate "calling" arguments are memory pointers
C being passed via the %VAL() intrinsic function.
C
C Written by Kevin P Duffey @RAL, October 2001
C===========================================================================

      IMPLICIT NONE

      INTEGER SEED, NUMROWS
      INTEGER I, EL
      DOUBLE PRECISION YRAN(NUMROWS)
      DOUBLE PRECISION PERIOD_RAN1, RANDOM, SHUFFLE1, SHUFFLE2

C---------------------------------------------------------------------------
C Randomize the Data
C---------------------------------------------------------------------------

      DO 10 I = 1, NUMROWS
         RANDOM = PERIOD_RAN1(SEED)
         EL = 1 + IDINT(RANDOM*DFLOAT(NUMROWS-1))
         SHUFFLE1 = YRAN(I)
         SHUFFLE2 = YRAN(EL)
         YRAN(I) = SHUFFLE2
         YRAN(EL) = SHUFFLE1
   10 CONTINUE

      RETURN
      END
