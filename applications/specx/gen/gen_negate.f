*  History:
*     16 Nov 1993 (hme):
*        Replace STR$UPCASE with CHR_UCASE.
*     15 Jan 1994 (rp):
*        Replace CHR_UCASE with UUCASE
*-----------------------------------------------------------------------

      SUBROUTINE gen_negate (value, type, ierr)

*  Routine to change sign on any GENLIB/SCL numeric or logical type

      IMPLICIT  none

*     Formal parameters:

      INTEGER*4 value
      CHARACTER type*4
      INTEGER*4 ierr

*     Local variables

      INTEGER*4 nb
      CHARACTER type2*4

      LOGICAL*4 lvalue
      INTEGER*4 ivalue
      REAL*4    rvalue
      REAL*8    dvalue
      EQUIVALENCE (lvalue, ivalue, rvalue, dvalue)

      INTEGER*4 gen_ilen

*  Ok, go..

      type2 = type
      CALL uucase (type2)
      READ (type2(2:gen_ilen(type2)), '(I)') nb

      IF (type2.eq.'L4') THEN
        value = NOT (value)

      ELSE
        IF (type2.eq.'I4') THEN
          ivalue = 0
        ELSE IF (type2.eq.'R4') THEN
          rvalue = 0.0
        ELSE IF (type2.eq.'R8') THEN
          dvalue = 0.D0
        END IF
        CALL gen_exop (ivalue, type2, nb, value, type2, nb, '-', ierr)
        CALL gen_cvt_type (ivalue, type2, nb, value, type2, nb, ierr)

      END IF

      RETURN
      END
