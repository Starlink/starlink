C-----------------------------------------------------------------------

      LOGICAL FUNCTION INQUAD (NQ, J)

C   Routine to check that specified point J is inside range of quadrant NQ

      INCLUDE 'STACKCOMM'

      INQUAD=.FALSE.
      NST=NTOT(NQ-1)+1
      NFIN=NTOT(NQ)
      IF(J.GE.NST.AND.J.LE.NFIN)   INQUAD=.TRUE.

      RETURN
      END


