
C*GRQLS -- inquire current line-style
C+
      SUBROUTINE GRQLS (ISTYLE)
      INTEGER  ISTYLE
C
C GRPCKG: obtain the line-style of the current graphics device.
C
C Argument:
C  ISTYLE (output): receives the current line-style code.
C--
C (1-Feb-1983)
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
C
      IF (GRCIDE.LT.1) THEN
          CALL GRWARN('GRQLS - no graphics device is active.')
          ISTYLE = 1
      ELSE
          ISTYLE = GRSTYL(GRCIDE)
      END IF
      END
