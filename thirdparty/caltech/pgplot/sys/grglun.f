C*GRGLUN -- get a Fortran logical unit number (Sun/Convex-UNIX)
C+
      SUBROUTINE GRGLUN(LUN)
      INTEGER LUN
C
C Get an unused Fortran logical unit number. 
C Returns a Logical Unit Number that is not currently opened.
C After GRGLUN is called, the unit should be opened to reserve
C the unit number for future calls.  Once a unit is closed, it
C becomes free and another call to GRGLUN could return the same
C number.  Also, GRGLUN will not return a number in the range 1-9
C as older software will often use these units without warning.
C
C Arguments:
C  LUN    : receives the logical unit number, or -1 on error.
C--
C 12-Feb-1989 [AFT/TJP].
C-----------------------------------------------------------------------
      INTEGER I
      LOGICAL QOPEN
C---
      DO 10 I=99,10,-1
          INQUIRE (UNIT=I,  OPENED=QOPEN)
          IF (.NOT.QOPEN) THEN
              LUN = I
              RETURN
          END IF
   10 CONTINUE
      CALL GRWARN('GRGLUN: out of units.')
      LUN = -1
      RETURN
      END
