C*GRESC -- escape routine
C+
      SUBROUTINE GRESC (TEXT)
C
C GRPCKG: "Escape" routine. The specified text is sent directly to the
C selected graphics device, with no interpretation by GRPCKG. This
C routine must be used with care; e.g., the programmer needs to know
C the device type of the currently selected device, and the instructions
C that that device can accept.
C
C Arguments: none.
C  TEXT (input, character*(*)):  text to be sent to the device.
C
C 15-May-1985 - new routine [TJP].
C 26-May-1987 - add GREXEC support [TJP].
C 19-Dec-1988 - start new page if necessary [TJP].
C  4-Feb-1997 - RBUF should be an array, not a scalar [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      CHARACTER*(*) TEXT
      REAL RBUF(1)
      INTEGER NBUF
C
C If no device is currently selected, do nothing.
C
      IF (GRCIDE.GT.0) THEN
          IF (.NOT.GRPLTD(GRCIDE)) CALL GRBPIC
          NBUF = 0
          CALL GREXEC(GRGTYP,23,RBUF,NBUF,TEXT,LEN(TEXT))
      END IF
      END
