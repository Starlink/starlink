
*-----------------------------------------------------------------------

      SUBROUTINE gen_exopl (opnd1, opnd2, operator, ierr)

*  routine to perform requested binary logical operation on two
*  operands, returning the result in the first operand.

      IMPLICIT  NONE

*     Formal parameters

      LOGICAL*4 opnd1
      LOGICAL*4 opnd2
      CHARACTER operator*2
      INTEGER*4 ierr

*  OK, do it...

      IF (operator(1:1).EQ.'&') THEN
        opnd1 = opnd1 .AND. opnd2
      ELSE IF (operator(1:1).EQ.'!') THEN
        opnd1 = opnd1 .OR. opnd2
      ELSE
        ierr = 3
      END IF

      RETURN
      END
