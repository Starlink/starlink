
C===========================================================================

      SUBROUTINE PERIOD_OGIPSTORE(DVAL1, DVAL2, DVAL3, IROW,
     :                            JUNK2, NUMROWS, NUMCOLS)

C===========================================================================
C Assigns a real values to a work array for OGIP. The array can
C correspond to a slice of dynamically-allocated memory, provided that
C the "calling" argument for PUTARRAY is a memory pointer that is being
C passed via the %VAL() intrinsic function.
C
C Written by Kevin P Duffey @RAL, October 2001
C===========================================================================

      IMPLICIT NONE

      INTEGER IROW, NUMROWS, NUMCOLS
      DOUBLE PRECISION JUNK2(NUMROWS,NUMCOLS)
      DOUBLE PRECISION DVAL1, DVAL2, DVAL3

C---------------------------------------------------------------------------
C Assign real value
C---------------------------------------------------------------------------

      JUNK2(IROW, 1) = DVAL1
      JUNK2(IROW, 2) = DVAL2
      JUNK2(IROW, 3) = DVAL3

      RETURN
      END
