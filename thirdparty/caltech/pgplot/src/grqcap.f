C*GRQCAP -- inquire device capabilities
C+
      SUBROUTINE GRQCAP (STRING)
      CHARACTER*(*) STRING
C
C GRPCKG: obtain the "device capabilities" string from the device
C driver for the current device.
C
C Arguments:
C
C STRING (output, CHARACTER*(*)): receives the device capabilities
C       string.
C--
C 26-Nov-92: new routine [TJP].
C  1-Sep-94: get from common instead of driver [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
C
      IF (GRCIDE.LT.1) THEN
          CALL GRWARN('GRQCAP - no graphics device is active.')
          STRING = 'NNNNNNNNNN'
      ELSE
          STRING = GRGCAP(GRCIDE)
      END IF
C
      END
