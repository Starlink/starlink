* History:
*    21 Sep 2000 (ajc):
*       Unused CONTINUE, CHAR
*-----------------------------------------------------------------------

      SUBROUTINE gen_parseflt (string, ils, next)

      IMPLICIT  none

*     Formal parameters:

      CHARACTER string*(*)
      INTEGER*4 ils
      INTEGER*4 next

*     Local variables:

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
