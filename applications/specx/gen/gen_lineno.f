*  History:
*     16 Nov 1993 (hme):
*        Add () to FUNCTION statement.
*-----------------------------------------------------------------------
*-----------------------------------------------------------------------

      INTEGER*4 FUNCTION GEN_LINENO()

      IMPLICIT  NONE

      INCLUDE 'CLI_STACK.INC'

      GEN_LINENO = ICLI(4,ISP)

      RETURN
      END
