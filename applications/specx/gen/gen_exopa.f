
*-----------------------------------------------------------------------

      SUBROUTINE  gen_exopa (opnd1, type, opnd2, type2, operator, ierr)

      IMPLICIT  NONE

*     Formal parameters

      REAL*4    opnd1,   opnd2
      CHARACTER type*4,  type2*4
      CHARACTER operator*2
      INTEGER*4 ierr

*     Local variables

      INTEGER*4 iop1, iop2
      REAL*4    rop1, rop2
      REAL*8    dop1, dop2
      EQUIVALENCE (iop1, rop1, dop1)
      EQUIVALENCE (iop2, rop2, dop2)

*     Copy the operands byte-by-byte to the equivalenced variables.

      CALL xcopy (8, opnd1, dop1)
      CALL xcopy (8, opnd2, dop2)

*     Do the operation

      IF (operator(1:1).EQ.'^' .AND. type2.EQ.'I4') THEN
        IF (TYPE.EQ.'I4') IOP1 = IOP1 ** IOP2
        IF (TYPE.EQ.'R4') ROP1 = ROP1 ** IOP2
        IF (TYPE.EQ.'R8') DOP1 = DOP1 ** IOP2

      ELSE IF (OPERATOR(1:1).EQ.'^' .AND. TYPE2.NE.'I4') THEN
        IF (TYPE.EQ.'R4') THEN
          IF (ROP1.GT.0) THEN
            ROP1 = ROP1 ** ROP2
          ELSE
            IERR = 5
            GO TO 99
          END IF
        ELSE IF (TYPE.EQ.'R8') THEN
          IF (DOP1.GT.0D0) THEN
            DOP1 = DOP1 ** DOP2
          ELSE
            IERR = 5
            GO TO 99
          END IF
        END IF

      ELSE IF (OPERATOR(1:1).EQ.'*') THEN
        IF (TYPE.EQ.'I4') IOP1 = IOP1 * IOP2
        IF (TYPE.EQ.'R4') ROP1 = ROP1 * ROP2
        IF (TYPE.EQ.'R8') DOP1 = DOP1 * DOP2

      ELSE IF (OPERATOR(1:1).EQ.'/') THEN
        IF (TYPE.EQ.'I4') THEN
          IF (IOP2.EQ.0) THEN
            IERR = 4
            GO TO 99
          ELSE
            IOP1 = IOP1 / IOP2
          END IF
        ELSE IF (TYPE.EQ.'R4') THEN
          IF (ROP2.EQ.0.0) THEN
            IERR = 4
            GO TO 99
          ELSE
            ROP1 = ROP1 / ROP2
          END IF
        ELSE IF (TYPE.EQ.'R8') THEN
          IF (DOP2.EQ.0.D0) THEN
            IERR = 4
            GO TO 99
          ELSE
            DOP1 = DOP1 / DOP2
          END IF
        END IF

      ELSE IF (OPERATOR(1:1).EQ.'+') THEN
        IF (TYPE.EQ.'I4') IOP1 = IOP1 + IOP2
        IF (TYPE.EQ.'R4') ROP1 = ROP1 + ROP2
        IF (TYPE.EQ.'R8') DOP1 = DOP1 + DOP2

      ELSE IF (OPERATOR(1:1).EQ.'-') THEN
        IF (TYPE.EQ.'I4') IOP1 = IOP1 - IOP2
        IF (TYPE.EQ.'R4') ROP1 = ROP1 - ROP2
        IF (TYPE.EQ.'R8') DOP1 = DOP1 - DOP2

      END IF

*     Copy the result back

      CALL xcopy (8, dop1, opnd1)

*     Error return

   99 CONTINUE

      RETURN
      END
