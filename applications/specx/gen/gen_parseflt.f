
*-----------------------------------------------------------------------

      SUBROUTINE gen_parseflt (string, ils, next)

      IMPLICIT  none

*     Formal parameters:

      CHARACTER string*(*)
      INTEGER*4 ils
      INTEGER*4 next

*     Local variables:

      LOGICAL*4 continue
      CHARACTER char*1

*  Ok, go..

      CALL gen_parseint (string, ils, next)
      IF (next.gt.ils) RETURN

      IF (string(next:next).eq.'.') THEN
        next = next + 1
      ELSE
        RETURN
      END IF
      IF (next.gt.ils) RETURN

      CALL gen_parseint (string, ils, next)

      RETURN
      END
