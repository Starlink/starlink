C*GRLIN0 -- draw a line
C+
      SUBROUTINE GRLIN0 (XP,YP)
C
C GRPCKG (internal routine): draw a line from the current position to a
C specified position, which becomes the new current position. This
C routine takes care of clipping at the viewport boundary, dashed and
C thick lines.
C
C Arguments:
C
C XP, YP (input, real): absolute device coordinates of the end-point of
C       the line.
C--
C 13-Jul-1984
C  7-May-1985 - add MIN/MAX kluge to prevent integer overflow [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      LOGICAL  VIS
      REAL     XP,YP, X0,Y0, X1,Y1
C
C End-points of line are (X0,Y0), (X1,Y1).
C
      X0 = GRXPRE(GRCIDE)
      Y0 = GRYPRE(GRCIDE)
      X1 = MIN(2E9,MAX(-2E9,XP))
      Y1 = MIN(2E9,MAX(-2E9,YP))
      GRXPRE(GRCIDE) = X1
      GRYPRE(GRCIDE) = Y1
C
C Change the end-points of the line (X0,Y0) - (X1,Y1)
C to clip the line at the window boundary.
C
      CALL GRCLPL(X0,Y0,X1,Y1,VIS)
      IF (.NOT.VIS) RETURN
C
C Draw the line in the appropriate style.
C
      IF (GRDASH(GRCIDE)) THEN
C         ! dashed line
         CALL GRLIN1(X0,Y0,X1,Y1,.FALSE.)
      ELSE IF (GRWIDT(GRCIDE).GT.1) THEN
C         ! heavy line
         CALL GRLIN3(X0,Y0,X1,Y1)
      ELSE
C         ! full line
         CALL GRLIN2(X0,Y0,X1,Y1)
      END IF
      END
