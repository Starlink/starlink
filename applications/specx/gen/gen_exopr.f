
*-----------------------------------------------------------------------

      SUBROUTINE  gen_exopr (opnd1, opnd2, type, operator, ierr)

*  routine to evaluate relational expression. Result is returned in place
*  of first operand.

      IMPLICIT  NONE

*     Formal parameters

      REAL*8    opnd1,   opnd2
      CHARACTER type*4
      CHARACTER operator*2
      INTEGER*4 ierr

*     Local variables

      INTEGER*4 iop1, iop2
      REAL*4    rop1, rop2
      REAL*8    dop1, dop2
      EQUIVALENCE (iop1, rop1, dop1)
      EQUIVALENCE (iop2, rop2, dop2)

      LOGICAL   result
      LOGICAL   less, equal, greater
      INTEGER*4 i
      CHARACTER test*1

*     Copy the operands byte-by-byte to the equivalenced variables.

      CALL xcopy (8, opnd1, dop1)
      CALL xcopy (8, opnd2, dop2)

*     Compare the two operands

D     type *,'-- gen_exopr --'

      IF (type.EQ.'I4') THEN
D       type *,'    operands: ', iop1, iop2
        less    = iop1.LT.iop2
        equal   = iop1.EQ.iop2
        greater = iop1.GT.iop2
      ELSE IF (type.EQ.'R4') THEN
D       type *,'    operands: ', rop1, rop2
        less    = rop1.LT.rop2
        equal   = rop1.EQ.rop2
        greater = rop1.GT.rop2
      ELSE IF (type.EQ.'R8') THEN
D       type *,'    operands: ', dop1, dop2
        less    = dop1.LT.dop2
        equal   = dop1.EQ.dop2
        greater = dop1.GT.dop2
      END IF

D     type *,'    common operand type = ', type
D     type *,'    less, equal, greater = ', less, equal, greater

*     Test the specified conditions

      result = .FALSE.
      DO i = 1, 2
        test = operator(i:i)
        IF (test.EQ.'<') result = result.OR.less
        IF (test.EQ.'=') result = result.OR.equal
        IF (test.EQ.'>') result = result.OR.greater
D       type *,'    test operation: ', test
D       type *,'    cumulative result: ', result
      END DO

*     Copy the result back

      CALL xcopy (4, result, opnd1)

*     Error return

   99 CONTINUE

      RETURN
      END
