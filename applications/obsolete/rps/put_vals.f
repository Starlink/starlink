*+PUT_VALS         Puts Values back in database
      SUBROUTINE PUT_VALS(REFDB,LIMITS,VAL)
      IMPLICIT NONE

*   Input :
      INTEGER REFDB
      INTEGER LIMITS(2)
      CHARACTER*(*) VAL(*)

*   Variables :
      INTEGER I,J
      INTEGER IERR
*-

      I=0

      DO J=LIMITS(1),LIMITS(2)
         I=I+1
         CALL DBS_PUTC(REFDB,J,VAL(I),IERR)
      END DO

      END
