C*GRQLW -- inquire current line width
C+
      SUBROUTINE GRQLW (IWIDTH)
      INTEGER  IWIDTH
C
C GRPCKG: obtain the line-width of the current graphics device.
C
C Argument:
C  IWIDTH (output): receives the current line-width.
C--
C (1-Feb-1983)
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
C
      IF (GRCIDE.LT.1) THEN
          CALL GRWARN('GRQLW - no graphics device is active.')
          IWIDTH = 1
      ELSE
          IWIDTH = ABS(GRWIDT(GRCIDE))
      END IF
      END
