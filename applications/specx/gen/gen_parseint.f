
*-----------------------------------------------------------------------

      SUBROUTINE gen_parseint (string, ils, next)

      IMPLICIT  none

*     Formal parameters:

      CHARACTER string*(*)
      INTEGER*4 ils
      INTEGER*4 next

*     Local variables:

      LOGICAL*4 continue
      CHARACTER char*1

*  Ok, go..

      continue = .true.

      DO WHILE (continue .and. next.le.ils)
        char = string(next:next)
        IF (char.ge.'0' .and. char.le.'9') THEN
          next = next + 1
        ELSE
          continue = .false.
        END IF
      END DO

      RETURN
      END
