
*-----------------------------------------------------------------------

      INTEGER*4 FUNCTION STACK_POINTER()

C   Returns the current value of the stack pointer

      INCLUDE 'CLI_STACK.INC'

      STACK_POINTER = 0

      I = 1
      DO WHILE (ICLI(3,I).NE.0. .AND. I.LE.MAXSTACK)
        STACK_POINTER = I
        I = I+1
      END DO

      RETURN
      END
