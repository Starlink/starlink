*  History:
*     16 Nov 1993 (hme):
*        Replace STR$UPCASE with CHR_UCASE.
*     15 Jan 1994 (rp):
*        Replace CHR_UCASE with UUCASE
*-----------------------------------------------------------------------

      SUBROUTINE gen_parseexp (string, ils, next)

      IMPLICIT  none

*     Formal parameters:

      CHARACTER string*(*)
      INTEGER*4 ils
      INTEGER*4 next

*     Local variables:

      LOGICAL*4 continue
      CHARACTER char*1

*  Ok, go..

      CALL gen_parseflt (string, ils, next)
      IF (next.gt.ils) RETURN

      char = string(next:next)
      CALL uucase (char)
      IF (char.eq.'D' .or. char.eq.'E') THEN
        next = next + 1
      ELSE
        RETURN
      END IF

      char = string(next:next)
      IF (char.eq.'+' .or. char.eq.'-') THEN
        next = next + 1
      END IF

      CALL gen_parseint (string, ils, next)

      RETURN
      END
