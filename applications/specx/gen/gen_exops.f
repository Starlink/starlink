*  History:
*     16 Nov 1993 (hme):
*        Replace STR$UPCASE with CHR_UCASE.
*      1 Aug 2000 (ajc):
*        Change TYPE * to PRINT *
*-----------------------------------------------------------------------

      SUBROUTINE gen_exops (opd1, type1, len1,
     &                      opd2, type2, len2, operator, ierr)

*  Routine to evaluate string expression and return result into string
*  with start address at that of opd1.

      IMPLICIT  NONE

*     Formal parameters

      BYTE      opd1(*),  opd2(*)
      INTEGER*4 len1,     len2
      CHARACTER type1*4,  type2*4
      CHARACTER operator*2
      INTEGER*4 ierr

*     Functions

      LOGICAL*4 gen_sequal

*     Local variables

      LOGICAL*4 result

*  Ok, go...

      ierr = 0

CD    print *, ' --- gen_exops ---'
CD    print *, '     passed operator = ', operator

      IF (operator.EQ.'= ') THEN
        result = gen_sequal (opd1, len1, opd2, len2)
        CALL xcopy (4, result, opd1)
        type1 = 'L4'
CD      print *,'equality test? ', result

      ELSE IF (operator.EQ.'<>' .OR. operator.EQ.'><' ) THEN
        result = .NOT. gen_sequal (opd1, len1, opd2, len2)
        CALL xcopy (4, result, opd1)
        type1 = 'L4'
CD      print *,'equality test? ', result

      ELSE IF (operator(1:1).EQ.'+') THEN
        CALL xcopy (len2, opd2, opd1(len1+1))
        WRITE (type1(2:), '(I3.3)') len1+len2

CD      print *,'resulting string has type ', type1

      ELSE
        ierr = 3
      END IF

      RETURN
      END
