C*GRDOT0 -- draw a dot
C+
      SUBROUTINE GRDOT0 (X,Y)
C
C GRPCKG (internal routine): Draw a single dot (pixel) at a specified
C location.
C
C Arguments:
C
C X, Y (real, input): absolute device coordinates of the dot (these
C       are rounded to the nearest integer by GRDOT0).
C--
C (1-Jun-1984)
C 22-Oct-1984 - rewrite [TJP].
C 29-Jan-1985 - add HP2648 device [KS/TJP].
C  5-Aug-1986 - add GREXEC support [AFT].
C 21-Feb-1987 - If needed, calls begin picture [AFT].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      INTEGER  NBUF, LCHR
      REAL     X, Y, RBUF(6)
      CHARACTER CHR
C
C (X,Y) is the new current position.
C
      GRXPRE(GRCIDE) = X
      GRYPRE(GRCIDE) = Y
C
C Check window.
C
      IF (X .LT. GRXMIN(GRCIDE)) RETURN
      IF (X .GT. GRXMAX(GRCIDE)) RETURN
      IF (Y .LT. GRYMIN(GRCIDE)) RETURN
      IF (Y .GT. GRYMAX(GRCIDE)) RETURN
C
C Begin picture if necessary.
C
      IF (.NOT.GRPLTD(GRCIDE)) CALL GRBPIC
C
C If a "thick pen" is to be simulated, use the line-drawing routines
C instead.
C
      IF (GRWIDT(GRCIDE).GT.1) THEN
          CALL GRLIN3(X,Y,X,Y)
      ELSE
          RBUF(1)=X
          RBUF(2)=Y
          NBUF=2
          CALL GREXEC(GRGTYP,13,RBUF,NBUF,CHR,LCHR)
      END IF
      END
