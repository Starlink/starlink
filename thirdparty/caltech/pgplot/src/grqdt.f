
C*GRQDT -- inquire current device and type
C+
      SUBROUTINE GRQDT (DEVICE)
C
C GRPCKG: obtain the name and type of the current graphics device.
C
C Argument:
C
C DEVICE (output, character): receives the device name and type of the
C       currently active device in the form 'device/type'; this is a
C       valid string for input to GROPEN.
C--
C  1-Feb-1983
C 19-Feb-1988 - add quotes if necessary.
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      CHARACTER*(*) DEVICE
      CHARACTER*14 TYPE
      LOGICAL   JUNK
      INTEGER   L
C
      IF (GRCIDE.LT.1) THEN
          CALL GRWARN('GRQDT - no graphics device is active.')
          DEVICE = '/NULL'
      ELSE
          CALL GRQTYP(TYPE,JUNK)
          L = GRFNLN(GRCIDE)
          IF (L.LE.0) THEN
              DEVICE = '/'//TYPE
          ELSE IF (INDEX(GRFILE(GRCIDE)(1:L), '/').EQ.0) THEN
              DEVICE = GRFILE(GRCIDE)(1:L)//'/'//TYPE
          ELSE
              DEVICE = '"'//GRFILE(GRCIDE)(1:L)//'"/'//TYPE
          END IF
      END IF
      END
