C*GRMOVA -- move pen (absolute, world coordinates)
C+
      SUBROUTINE GRMOVA (X,Y)
C
C GRPCKG: move the pen to a specified location.
C
C Arguments:
C
C X, Y (real, input): world coordinates of the new pen position.
C--
C (1-Feb-1983)
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      REAL     X,Y
C
      IF (GRCIDE.GE.1) THEN
C         WRITE (*,'(A,2F10.5)') 'GRMOVA', X, Y
          GRXPRE(GRCIDE) = X * GRXSCL(GRCIDE) + GRXORG(GRCIDE)
          GRYPRE(GRCIDE) = Y * GRYSCL(GRCIDE) + GRYORG(GRCIDE)
      END IF
      END
