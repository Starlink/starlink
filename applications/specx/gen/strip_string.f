
*-----------------------------------------------------------------------

      SUBROUTINE strip_string (string, c1, c2)

*  Routine to remove leading blanks from a string expression and then
*  to remove any pairs of brackets surrounding it until the "naked"
*  expression remains.

*     Formal parameters:

      CHARACTER string*(*)
      INTEGER*4 c1, c2

*     Local variables

      LOGICAL*4 continue
      INTEGER*4 ils
      INTEGER*4 i
      INTEGER*4 ich1, ich2
      INTEGER*4 ierr

*     Functions

      INTEGER*4 gen_ilen

*  Ok, go..

      ils = gen_ilen (string)

*   Eliminate leading blanks:

      i = 1
      DO WHILE (string(i:i).eq.' ' .and. i.le.ils)
        i = i + 1
      END DO

      c1 = i
      c2 = ils

*   Remove any nesting brackets

      continue = .TRUE.
      DO WHILE (  string(c1:c1).EQ.'(' .AND. string(c2:c2).EQ.')'
     &           .AND. continue )
        CALL get_subexpr (string(c1:c2), ich1, ich2, ierr)
        IF (ich1.eq.2 .AND. ich2.eq.c2-c1) THEN
          c1 = c1 + 1
          c2 = c2 - 1
        ELSE
          continue = .false.
        END IF
      END DO

      RETURN
      END
