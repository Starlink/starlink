
*-----------------------------------------------------------------------

      SUBROUTINE GEN_REWIND (LEVEL, NLINES)

      IMPLICIT  NONE

      INTEGER*4 LEVEL
      INTEGER*4 NLINES

      INCLUDE 'CLI_STACK.INC'

      INTEGER*4 I

*     OK? Go..

      DO I = 1, NLINES
        BACKSPACE (ICLI(3,LEVEL))
      END DO

      ICLI(4,LEVEL) = ICLI(4,LEVEL) - NLINES

      RETURN
      END
