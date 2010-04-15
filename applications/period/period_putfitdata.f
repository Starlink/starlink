
C===========================================================================

      SUBROUTINE PERIOD_PUTFITDATA(OPARRAY, NUMROWS, MXCOL, GAMMA,
     :                             KVEL, PHASE)

C===========================================================================
C Outputs fitted data from FIT. The output array can correspond to a slice
C of dynamically-allocated memory, provided that the appropriate "calling"
C argument is a memory pointer being passed via the %VAL() intrinsic
C function.
C
C Written by Kevin P Duffey @RAL, October 2001
C===========================================================================

      IMPLICIT NONE

      INCLUDE "PIVARS"

      INTEGER NUMROWS, MXCOL, I
      DOUBLE PRECISION OPARRAY(NUMROWS,MXCOL)
      DOUBLE PRECISION GAMMA, KVEL, PHASE


C-----------------------------------------------------------------------------
C Load output slot.
C-----------------------------------------------------------------------------

      DO 10 I = 1, NUMROWS
         OPARRAY(I, 1) = DFLOAT(I)/DFLOAT(NUMROWS)
         OPARRAY(I, 2) = GAMMA +
     :                (KVEL*DSIN(TWOPI*(OPARRAY(I, 1)-PHASE)))
         OPARRAY(I, 3) = 0.0D0
  10  CONTINUE

      RETURN
      END
