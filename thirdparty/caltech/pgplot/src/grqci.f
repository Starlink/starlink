C*GRQCI -- inquire current color index
C+
      SUBROUTINE GRQCI (C)
C
C GRPCKG: obtain the color index of the current graphics device.
C
C Argument:
C
C C (integer, output): receives the current color index (0-255).
C--
C (1-Feb-1983)
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      INTEGER  C
C
      IF (GRCIDE.LT.1) THEN
          CALL GRWARN('GRQCI - no graphics device is active.')
          C = 1
      ELSE
          C = GRCCOL(GRCIDE)
      END IF
      END
