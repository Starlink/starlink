
*-----------------------------------------------------------------------

      SUBROUTINE CLI_EMPTY

      IMPLICIT  NONE

      INCLUDE 'CLI_STACK.INC'

*  Ok? Go..

      ICLI (1,ISP) = 1
      ICLI (2,ISP) = 0

      RETURN
      END
