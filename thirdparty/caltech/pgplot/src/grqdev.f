
C*GRQDEV -- inquire current device
C+
      SUBROUTINE GRQDEV (DEVICE, L)
      CHARACTER*(*) DEVICE
      INTEGER L
C
C Obtain the name of the current graphics device or file.
C
C Argument:
C  DEVICE (output): receives the device name of the
C       currently active device.
C  L (output): number of characters in DEVICE, excluding trailing
C       blanks.
C--
C 19-Feb-1988
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
C
      IF (GRCIDE.LT.1) THEN
          DEVICE = '?'
          L = 1
      ELSE
          DEVICE = GRFILE(GRCIDE)
          L = GRFNLN(GRCIDE)
          IF (L.GT.LEN(DEVICE)) L = LEN(DEVICE)
      END IF
      END
