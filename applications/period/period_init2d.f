
C===========================================================================

      SUBROUTINE PERIOD_INIT2D(INITVAL, INITARRAY, NUMROWS, NUMCOLS)

C===========================================================================
C Initialises a real 2-d array INITARRAY with real value INITVAL; this
C array can correspond to a slice of dynamically-allocated memory, provided
C that the "calling" argument for PUTARRAY is a memory pointer that is
C being passed via the %VAL() intrinsic function.
C
C Written by Kevin P Duffey @RAL, October 2001
C===========================================================================

      IMPLICIT NONE

      INTEGER NUMROWS, NUMCOLS, I, J
      DOUBLE PRECISION INITVAL, INITARRAY(NUMROWS,NUMCOLS)

C---------------------------------------------------------------------------
C Assign real value
C---------------------------------------------------------------------------

      DO 20 I = 1, NUMROWS
         DO 10 J = 1, NUMCOLS
            INITARRAY(I, J) = INITVAL
   10    CONTINUE
   20 CONTINUE

      RETURN
      END
