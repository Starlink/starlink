
C===========================================================================

      SUBROUTINE PERIOD_PUT1D(PUTVAL, IROW, PUTARRAY, NUMROWS)

C===========================================================================
C Assigns a real value PUTVAL to 1-d array PUTARRAY; this array can
C correspond to a slice of dynamically-allocated memory, provided that
C the "calling" argument for PUTARRAY is a memory pointer that is being
C passed via the %VAL() intrinsic function.
C
C Written by Kevin P Duffey @RAL, October 2001
C===========================================================================

      IMPLICIT NONE

      INTEGER IROW, NUMROWS
      DOUBLE PRECISION PUTVAL, PUTARRAY(NUMROWS)

C---------------------------------------------------------------------------
C Assign real value
C---------------------------------------------------------------------------

      PUTARRAY(IROW) = PUTVAL

      RETURN
      END
