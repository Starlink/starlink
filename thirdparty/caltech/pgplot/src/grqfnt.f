C*GRQFNT -- inquire current font
C+
      SUBROUTINE GRQFNT (IF)
C
C GRPCKG: obtain the font number of the current graphics device.
C
C Argument:
C
C IF (integer, output): receives the current font number (1-3).
C--
C (19-Mar-1983)
C 15-Dec-1988 - change name [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      INTEGER  IF
C
      IF (GRCIDE.LT.1) THEN
          CALL GRWARN('GRQFNT - no graphics device is active.')
          IF = 1
      ELSE
          IF = GRCFNT(GRCIDE)
      END IF
      END
