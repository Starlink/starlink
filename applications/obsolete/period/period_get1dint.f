
C===========================================================================

      FUNCTION PERIOD_GET1DINT(IROW, IGETARRAY, NUMROWS)

C===========================================================================
C Returns an integer value from a 1-d array IGETARRAY; this array can
C correspond to a slice of dynamically-allocated memory, provided that
C the "calling" argument for IGETARRAY is a memory pointer that is being
C passed via the %VAL() intrinsic function.
C
C Written by Kevin P Duffey @RAL, October 2001
C===========================================================================

      IMPLICIT NONE

      INTEGER IROW, NUMROWS
      INTEGER PERIOD_GET1DINT, IGETARRAY(NUMROWS)

C---------------------------------------------------------------------------
C Return integer value
C---------------------------------------------------------------------------

      PERIOD_GET1DINT = IGETARRAY(IROW)

      RETURN
      END
