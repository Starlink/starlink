C*GRCLOS -- close graphics device
C+
      SUBROUTINE GRCLOS
C
C GRPCKG: Close the open plot on the current device. Any pending output
C is sent to the device, the device is released for other users or the
C disk file is closed, and no further plotting is allowed on the device
C without a new call to GROPEN.
C
C Arguments: none.
C--
C  1-Jun-1984 - [TJP].
C 17-Jul-1984 - ignore call if plot is not open [TJP].
C  1-Oct-1984 - reset color to default (1) and position text cursor
C               at bottom of VT screen [TJP].
C 19-Oct-1984 - add VV device [TJP].
C 22-Dec-1984 - use GRBUFL and GRIOTA parameters [TJP].
C  5-Aug-1986 - add GREXEC support [AFT].
C 21-Feb-1987 - modify END_PICTURE sequence [AFT].
C 11-Jun-1987 - remove built-ins [TJP].
C 31-Aug-1987 - do not eject blank page [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      REAL    RBUF(6)
      INTEGER NBUF,LCHR
      CHARACTER CHR
C
C Check a plot is open.
C
      IF (GRCIDE.LT.1) RETURN
C
C Reset color to default (1). This is useful
C for VT240 terminals, which use the color tables for text.
C
      CALL GRSCI(1)
C
C Flush buffer.
C
      CALL GRTERM
C
C End picture.
C
      CALL GREPIC
C
C This plot identifier is no longer in use.
C Set state to "workstation closed".
C
      GRSTAT(GRCIDE) = 0
      GRCIDE = 0
C
C Close workstation.
C
      CALL GREXEC(GRGTYP,10,RBUF,NBUF,CHR,LCHR)
C
      END
