*  History:
*     16 Nov 1993 (hme):
*        Replace variable FUNCTIONS with FUNCTS. The SunOS 4.x compiler
*        otherwise thinks a FUNCTION module begins.
*        Replace STR$UPCASE with CHR_UCASE.
*     15 Jan 1994 (rp):
*        Replace CHR_UCASE with UUCASE
*-----------------------------------------------------------------------

      SUBROUTINE gen_inqfunc (string, fnc_index, type, length, ierr)

*  Routine to decide if a string is the name of a supplied function,
*  and if so to decide which one.

      IMPLICIT  none

*     Formal parameters

      CHARACTER string*(*)
      INTEGER*4 fnc_index
      CHARACTER type*(*)
      INTEGER*4 length
      INTEGER*4 ierr

*     Local variables

      INTEGER*4 i
      INTEGER*4 istr
      CHARACTER name*4

*     Function definitions

      INTEGER*4 max_func
      PARAMETER (max_func=13)
      CHARACTER functs(max_func)*4
      DATA      functs
     &          /'SQRT', 'EXP ', 'LN  ', 'LOG ',
     &           'ABS ', 'SIN ', 'COS ', 'TAN ',
     &           'ASIN', 'ACOS', 'ATAN', 'NINT',
     &           'FLT '/

*  Ok, go...

      ierr      = 0
      fnc_index = 0

      istr = MIN (LEN(name), LEN(string))
      name = string(1:istr)
      CALL uucase (name)

      i = 1
      DO WHILE (fnc_index.eq.0 .and. i.le.max_func)
        IF (name.eq.functs(i)) THEN
          fnc_index = i
        ELSE
          i = i + 1
        END IF
      END DO

      IF (fnc_index .ne. 12) THEN
        type   = 'R4'
      ELSE
        type   = 'I4'
      END IF

      length = 1

      RETURN
      END
