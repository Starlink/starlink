
C*GRTXY0 -- convert world coordinates to device coordinates
C+
      SUBROUTINE GRTXY0 (ABSXY,X,Y,XT,YT)
C
C GRPCKG (internal routine): Convert scaled position to absolute
C position.
C
C Arguments:
C
C ABSXY (input, logical): if FALSE, convert world coordinates to
C       absolute device coordinates; if TRUE, return the input
C       coordinates unchanged.
C X, Y (input, real): input coordinates (absolute or world, depending
C       on setting of ABSXY).
C XT, YT (output, real): output absolute device coordinates.
C--
C (1-Feb-1983)
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      LOGICAL  ABSXY
      REAL     X, Y, XT, YT
C
      IF (ABSXY) THEN
          XT = X
          YT = Y
      ELSE
          XT = X * GRXSCL(GRCIDE) + GRXORG(GRCIDE)
          YT = Y * GRYSCL(GRCIDE) + GRYORG(GRCIDE)
      END IF
C
      END
