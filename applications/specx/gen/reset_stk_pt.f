
*-----------------------------------------------------------------------

      SUBROUTINE RESET_STK_PT

C   Routine to set ISP (i.e. the stack pointer) to the value found from
C   the highest open command file.
C   Used when stack has been unwound for input of data from a higher level,
C   so the stack pointer is less than the number of open command files.

      INTEGER*4 STACK_POINTER

      INCLUDE 'CLI_STACK.INC'

      ISP = STACK_POINTER()

      RETURN
      END
