* History:
*    21 Sep 2000 (ajc)
*       Unused GEN_ILEN
*-----------------------------------------------------------------------

      INTEGER FUNCTION GEN_ICHTOT(LEV)

C  Routine to calculate amount of CLILINE taken up by command levels
C  up to and including LEV.
C  Note, first level is LEV=0, and need to allow for LEV=-1

      INCLUDE 'CLI_STACK.INC'

      GEN_ICHTOT = 0

      J = -1
      DO WHILE (J.LT.LEV)
        J = J+1
        GEN_ICHTOT = GEN_ICHTOT + ICLI(2,J)
      END DO

      RETURN
      END
