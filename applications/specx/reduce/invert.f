C-----------------------------------------------------------------------

      SUBROUTINE INVERT (NQ)

C   Routine to swap quadrant data end for end in physical store. Useful for
C   SUBTRACT, ADD etc where DFINC is not taken into account.

      IMPLICIT  NONE

C  Formal parameters:

      INTEGER*4 NQ       ! Quadrant to be inverted

      INCLUDE 'STACKCOMM'

C  Local variables

      INTEGER*4 NST

C  Functions

      INTEGER*4 NTOT

      NST=NTOT(NQ-1)
      CALL SWAP_ARR(4,DATA(NST+1),NPTS(NQ))
      JFINC(NQ)=-JFINC(NQ)

      RETURN
      END
