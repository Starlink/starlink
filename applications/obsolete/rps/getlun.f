*+GETLUN  Returns number of a logical unit which is free for use.
      SUBROUTINE GETLUN(LUN)
      INTEGER LUN	!output	Logical unit number to use
      INTEGER STATUS	!in/out	Inherited status
*-Author	Clive Page	1991-JAN-1
* MOD MJR FOR RPS - was SYS_GETLUN
*** SEMI-PORTABLE VERSION ***
* Allocates first available unit below 99 (or stops if none free).
* Does not allocate units below 7 to avoid clash with terminal I/O
*Note: unlike VAX Fortran LIB$GET_LUN this routine is portable and does
* not require the unit to be explicitly freed after use, as the CLOSE
* statement will do this automatically.  But if used repeatedly this
* routine will return the same number unless an OPEN statement is
* actually executed after each call.
      LOGICAL OPEN, EXISTS
*
*      IF(STATUS .NE. 0) RETURN
      DO LUN = 99, 7, -1
          INQUIRE(UNIT=LUN, EXIST=EXISTS, OPENED=OPEN)
          IF(EXISTS .AND. .NOT. OPEN) GO TO 999
      END DO
      WRITE(*,*) 'SYS_GETLUN no units available'
*      STATUS = -9
999   CONTINUE
      END
