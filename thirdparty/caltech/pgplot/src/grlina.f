C*GRLINA -- draw a line (absolute, world coordinates)
C+
      SUBROUTINE GRLINA (X,Y)
C
C GRPCKG: draw line from current position to a specified position.
C
C Arguments:
C
C X, Y (real, input): world coordinates of the end-point of the line.
C--
C (1-Feb-1983)
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      REAL     X,Y
C
      IF (GRCIDE.GE.1) THEN
C         WRITE (*,'(A,2F10.5)') 'GRLINA', X, Y
          CALL GRLIN0( X * GRXSCL(GRCIDE) + GRXORG(GRCIDE),
     1                 Y * GRYSCL(GRCIDE) + GRYORG(GRCIDE) )
      END IF
      END
