C*GRDOT1 -- draw dots
C+
      SUBROUTINE GRDOT1(POINTS, X, Y)
      INTEGER POINTS
      REAL X(POINTS), Y(POINTS)
C
C GRPCKG (internal routine): Draw a set of dots.
C
C Arguments:
C
C POINTS (input, integer): the number of coordinate pairs.
C X, Y (input, real arrays, dimensioned POINTS or greater): the
C       X and Y world coordinates of the points.
C--
C 14-Mar-1997 - new routine to optimize drawing many dots [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      INTEGER  I, NBUF, LCHR
      REAL     RBUF(2), XP, YP
      CHARACTER CHR
      EQUIVALENCE (XP, RBUF(1)), (YP, RBUF(2))
C
C Begin picture if necessary.
C
      IF (.NOT.GRPLTD(GRCIDE)) CALL GRBPIC
C
C Loop for points: driver support.
C
      IF (GRWIDT(GRCIDE).LE.1) THEN
         NBUF = 2
         LCHR = 0
         DO 10 I=1,POINTS
C        -- Convert to device coordinates
            XP = X(I)*GRXSCL(GRCIDE) + GRXORG(GRCIDE)
            YP = Y(I)*GRYSCL(GRCIDE) + GRYORG(GRCIDE)
C           -- Clip against viewport
            IF (XP .GE. GRXMIN(GRCIDE) .AND.
     :          XP .LE. GRXMAX(GRCIDE) .AND.
     :          YP .GE. GRYMIN(GRCIDE) .AND.
     :          YP .LE. GRYMAX(GRCIDE)) THEN
               CALL GREXEC(GRGTYP,13,RBUF,NBUF,CHR,LCHR)
            END IF
 10      CONTINUE
C
C Thick line emulation required.
C
      ELSE
         DO 20 I=1,POINTS
C        -- Convert to device coordinates
            XP = X(I)*GRXSCL(GRCIDE) + GRXORG(GRCIDE)
            YP = Y(I)*GRYSCL(GRCIDE) + GRYORG(GRCIDE)
C           -- Clip against viewport
            IF (XP .GE. GRXMIN(GRCIDE) .AND.
     :          XP .LE. GRXMAX(GRCIDE) .AND.
     :          YP .GE. GRYMIN(GRCIDE) .AND.
     :          YP .LE. GRYMAX(GRCIDE)) THEN
               CALL GRLIN3(XP, YP, XP, YP)
            END IF
 20      CONTINUE
      END IF
C
C New pen position.
C
      GRXPRE(GRCIDE) = XP
      GRYPRE(GRCIDE) = YP
C
      END
