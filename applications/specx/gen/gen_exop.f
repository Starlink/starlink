*  History:
*     31 July 2000 (ajc):
*        Change TYPE * to PRINT *
*        Use format I3 to read type size
*-----------------------------------------------------------------------

      SUBROUTINE GEN_EXOP (opd1, type1, nbytes1,
     &                     opd2, type2, nbytes2, operator, ierr)

*  Routine to operate on two operands of specified type with operator
*  OPERATOR, returning the result into the first operand. The TYPE of
*  operand 1 is updated to the type of the evaluated term.
*  23/12/90 - updated by RP
*  Now need to include possibility of logical/relational expression
*  evaluation.

      IMPLICIT  NONE

*     Formal parameters

      REAL*8    opd1,    opd2
      INTEGER*4 nbytes1, nbytes2
      CHARACTER type1*4, type2*4
      CHARACTER operator*2
      INTEGER*4 ierr

*     Local variables

      REAL*8    temp1, temp2

      INTEGER*4 ntypes
      PARAMETER (ntypes=4)

      INTEGER*4 nbytes
      INTEGER*4 itype
      CHARACTER type*4

      INTEGER*4 itype1,  itype2

      CHARACTER types(ntypes)*4

      LOGICAL*4 tlog1

*     Functions

      INTEGER*4 gen_ilen

      DATA TYPES   /'I4', 'R4', 'R8', 'L4'/

* ok, go..

      ierr = 0

*     Test for string variables: the only supported operations on
*     these types are the test for equality, and concatenation (+)

      IF (type1(1:1).EQ.'C' .OR. type2(1:1).EQ.'C') THEN
        IF (type1(1:1).EQ.type2(1:1)) THEN
CD        print *,'    calling gen_exops'
          CALL gen_exops (opd1, type1, nbytes1,
     &                    opd2, type2, nbytes2, operator, ierr)
CD        print *,'Result of string expression:'
CD        print *,'    type is ', type1
          call xcopy (4, opd1, tlog1)
CD        print *,'    logical value is ', tlog1
          IF (ierr.NE.0) GO TO 99
          RETURN
        ELSE
          ierr = 8
          GO TO 99
        END IF
      END IF

*     Check operand types and data element lengths (in bytes).

      itype1 = 1
      DO WHILE (type1.NE.types(itype1) .AND. itype1.LT.ntypes)
        itype1 = itype1 + 1
      END DO
      IF (ierr.NE.0) THEN
        ierr = 9
        GO TO 99
      END IF
      CALL xcopy (nbytes1, opd1, temp1)

      itype2 = 1
      DO WHILE (type2.NE.types(itype2) .AND. itype2.LT.ntypes)
        itype2 = itype2 + 1
      END DO
      IF (ierr.NE.0) THEN
        ierr = 9
        GO TO 99
      END IF
      CALL xcopy (nbytes2, opd2, temp2)

CD    Print *,' - operand types are ', types(itype1),'and',types(itype2)
CD    Print *,' - operator is "'//operator(1:2)//'"'

      IF (operator(1:1).EQ.'&' .OR. operator(1:1).EQ.'!') THEN
        IF (itype1.EQ.4 .AND. itype2.EQ.4) THEN
          CALL gen_exopl (temp1, temp2, operator, ierr)
          type  = 'L4'
          itype = 4
        ELSE
          ierr = 6
        END IF

      ELSE IF (operator(1:1).EQ.'<' .OR. operator(1:1).EQ.'='
     &                              .OR. operator(1:1).EQ.'>') THEN
        IF (itype1.LE.3 .AND. itype2.LE.3) THEN

          itype = MAX (itype1, itype2)
          type  = types(itype)
          READ (type(2:gen_ilen(type)), '(I3)') nbytes

          IF (itype1.NE.itype)
     &      CALL gen_cvt_type (temp1, type1, nbytes1,
     &                         temp1, type,  nbytes,  ierr)
          IF (itype2.NE.itype)
     &      CALL gen_cvt_type (temp2, type2, nbytes2,
     &                         temp2, type,  nbytes,  ierr)
          IF (ierr.NE.0) GO TO 99
          CALL gen_exopr (temp1, temp2, type, operator, ierr)
          itype = 4
          type  = 'L4'
        ELSE
          ierr = 7
        END IF

      ELSE IF (operator(1:1).EQ.'^' .OR. operator(1:1).EQ.'*'
     &                              .OR. operator(1:1).EQ.'/'
     &                              .OR. operator(1:1).EQ.'+'
     &                              .OR. operator(1:1).EQ.'-') THEN
        IF (itype1.LE.3 .AND. itype2.LE.3) THEN

          itype = MAX (itype1, itype2)
          type  = types(itype)
          READ (type(2:gen_ilen(type)), '(I3)') nbytes

          IF (itype1.NE.itype)
     &        CALL gen_cvt_type (temp1, type1, nbytes1,
     &                           temp1, type,  nbytes,  ierr)
          IF (itype2.NE.itype
     &        .AND. (operator(1:1).NE.'^' .OR. type2.NE.'I4'))
     &             CALL gen_cvt_type (temp2, type2, nbytes2,
     &                                 temp2, type, nbytes,   ierr)
          IF (ierr.NE.0) GO TO 99
          CALL gen_exopa (temp1, type, temp2, type2, operator, ierr)
        ELSE
          ierr = 7
        END IF

      ELSE
        ierr = 3
      END IF

      IF (ierr.NE.0) GO TO 99

*     Copy result back to OPD1 and update the operand type

      READ (type(2:gen_ilen(type2)), '(I3)', iostat=ierr) nbytes
      IF (ierr.NE.0) THEN
        ierr = 9
        GO TO 99
      END IF
      CALL xcopy (nbytes, temp1, opd1)
      type1 = type

      RETURN

   99 CONTINUE
      PRINT *, '-- gen_exop --'
      IF (ierr.EQ.1 .OR. ierr.EQ.2) THEN
        PRINT *, '     unsupported type in gen_cvt_type'
      ELSE IF (ierr.EQ.3) THEN
        PRINT *, '     unsupported operator: "'//OPERATOR(1:2)//'"'
      ELSE IF (ierr.EQ.4) THEN
        PRINT *, '     attempt to divide by zero!'
      ELSE IF (ierr.eq.5) then
        PRINT *, '     undefined exponentiation!'
      ELSE IF (ierr.EQ.6) THEN
        PRINT *, '     not a logical operand!'
      ELSE IF (ierr.EQ.7) THEN
        PRINT *, '     not an arithmetic operand!'
      ELSE IF (ierr.EQ.8) THEN
        PRINT *, '     mixed string/other expression!'
      ELSE IF (ierr.EQ.9) THEN
        PRINT *, '     error reading operator type!'
      END IF

      RETURN
      END
