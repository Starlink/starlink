
C===========================================================================

      FUNCTION PERIOD_GET2D(IROW, ICOL, GETARRAY, NUMROWS, NUMCOLS)

C===========================================================================
C Returns a real value from a 2-d array GETARRAY; this array can
C correspond to a slice of dynamically-allocated memory, provided that
C the "calling" argument for GETARRAY is a memory pointer that is being
C passed via the %VAL() intrinsic function.
C
C Written by Kevin P Duffey @RAL, October 2001
C===========================================================================

      IMPLICIT NONE

      INTEGER IROW, ICOL, NUMROWS, NUMCOLS
      DOUBLE PRECISION PERIOD_GET2D, GETARRAY(NUMROWS,NUMCOLS)

C---------------------------------------------------------------------------
C Return real value
C---------------------------------------------------------------------------

      PERIOD_GET2D = GETARRAY(IROW, ICOL)

      RETURN
      END
