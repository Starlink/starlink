
C*GRGTC0 -- obtain character digitization
C+
      SUBROUTINE GRGTC0 (CHAR,CENTER,POINTS,X,Y,MORE)
C
C GRPCKG (internal routine): obtain character digitization.
C
C (10-Feb-1983)
C-----------------------------------------------------------------------
      EXTERNAL GRDAT2
      LOGICAL CENTER
      INTEGER POINTS, CHAR
      REAL X(1)
      REAL Y(1)
      LOGICAL MORE
C
      INTEGER CINDX1, CINDX2
      INTEGER CTD1, CTD2
      PARAMETER (CTD1 = 30, CTD2 = 128)
      INTEGER CHTBL(CTD1, CTD2)
      COMMON /GRCS02/ CINDX1, CINDX2, CHTBL
C
      INTEGER I
      INTEGER COORDS
      LOGICAL TAILED
C-----------------------------------------------------------------------
      IF (CINDX2.LE.0) CINDX2 = CHAR + 1
C
C Get the next segment of the character.
C
      POINTS = CHTBL(CINDX1, CINDX2)
      IF(POINTS .EQ. 0) GO TO 240
      DO 220 I = 1, POINTS
          CINDX1 = CINDX1 + 1
          COORDS = CHTBL(CINDX1, CINDX2)
          TAILED = COORDS .LT. 0
          IF(TAILED) COORDS = IABS(COORDS)
          X(I) = FLOAT(COORDS / 10)
          Y(I) = FLOAT(MOD(COORDS, 10))
          IF(TAILED) Y(I) = - Y(I)
          IF(.NOT. CENTER) GO TO 220
          X(I) = X(I) - 3.0
          Y(I) = Y(I) - 4.0
  220     CONTINUE
  240 CONTINUE
C
C Set status and return.
C
      IF(CINDX1 .EQ. CTD1) GO TO 320
      CINDX1 = CINDX1 + 1
      IF(CHTBL(CINDX1, CINDX2) .EQ. 0) GO TO 320
      MORE = .TRUE.
      RETURN
  320 MORE = .FALSE.
      CINDX1 = 1
      CINDX2 = 0
      RETURN
      END
