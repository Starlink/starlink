      SUBROUTINE PGCN01(Z, MX, MY, IA, IB, JA, JB, Z0, PLOT,
     1                  FLAGS, IS, JS, SDIR)
C
C Support routine for PGCNSC. This routine draws a continuous contour,
C starting at the specified point, until it either crosses the edge of
C the array or closes on itself.
C-----------------------------------------------------------------------
      INTEGER UP, DOWN, LEFT, RIGHT
      PARAMETER (UP=1, DOWN=2, LEFT=3, RIGHT=4)
      INTEGER  MAXEMX, MAXEMY
      PARAMETER (MAXEMX=100, MAXEMY=100)
      LOGICAL FLAGS(MAXEMX,MAXEMY,2)
      INTEGER MX, MY, IA, IB, JA, JB, IS, JS, I, J, II, JJ, DIR, SDIR
      REAL Z(MX,*)
      REAL Z0, X, Y, STARTX, STARTY
      EXTERNAL PLOT
C
      I = IS
      J = JS
      DIR = SDIR
      II = 1+I-IA
      JJ = 1+J-JA
      IF (DIR.EQ.UP .OR. DIR.EQ.DOWN) THEN
          X = REAL(I) + (Z0-Z(I,J))/(Z(I+1,J)-Z(I,J))
          Y = REAL(J)
      ELSE
          X = REAL(I)
          Y = REAL(J) + (Z0-Z(I,J))/(Z(I,J+1)-Z(I,J))
      END IF
CD    WRITE (*,*) 'SEGMENT'
C
C Move to start of contour and record starting point.
C
      CALL PLOT(0, X, Y, Z0)
      STARTX = X
      STARTY = Y
C
C We have reached grid-point (I,J) going in direction DIR (UP, DOWN,
C LEFT, or RIGHT). Look at the other three sides of the cell we are
C entering to decide where to go next. It is important to look to the
C two sides before looking straight ahead, in order to avoid self-
C intersecting contours. If all 3 sides have unused crossing-points,
C the cell is "degenerate" and we have to decide which of two possible 
C pairs of contour segments to draw; at present we make an arbitrary 
C choice. If we have reached the edge of the array, we have
C finished drawing an unclosed contour. If none of the other three
C sides of the cell have an unused crossing-point, we must have
C completed a closed contour, which requires a final segment back to
C the starting point.
C
  100 CONTINUE
CD    WRITE (*,*) I,J,DIR
      II = 1 + I - IA
      JJ = 1 + J - JA
      GOTO (110, 120, 130, 140), DIR
C
C DIR = UP
C
  110 CONTINUE
      FLAGS(II,JJ,1) = .FALSE.
      IF (J.EQ.JB) THEN
          RETURN
      ELSE IF (FLAGS(II,JJ,2)) THEN
          DIR = LEFT
          GOTO 200
      ELSE IF (FLAGS(II+1,JJ,2)) THEN
          DIR = RIGHT
          I = I+1
          GOTO 200
      ELSE IF (FLAGS(II,JJ+1,1)) THEN
C!        DIR = UP
          J = J+1
          GOTO 250
      ELSE
          GOTO 300
      END IF
C
C DIR = DOWN
C
  120 CONTINUE
      FLAGS(II,JJ,1) = .FALSE.
      IF (J.EQ.JA) THEN
          RETURN
      ELSE IF (FLAGS(II+1,JJ-1,2)) THEN
          DIR = RIGHT
          I = I+1
          J = J-1
          GOTO 200
      ELSE IF (FLAGS(II,JJ-1,2)) THEN
          DIR = LEFT
          J = J-1
          GOTO 200
      ELSE IF (FLAGS(II,JJ-1,1)) THEN
C!        DIR = DOWN
          J = J-1
          GOTO 250
      ELSE
          GOTO 300
      END IF
C
C DIR = LEFT
C
  130 CONTINUE
      FLAGS(II,JJ,2) = .FALSE.
      IF (I.EQ.IA) THEN
          RETURN
      ELSE IF (FLAGS(II-1,JJ,1)) THEN
          DIR = DOWN
          I = I-1
          GOTO 250
      ELSE IF (FLAGS(II-1,JJ+1,1)) THEN
          DIR = UP
          I = I-1
          J = J+1
          GOTO 250
      ELSE IF (FLAGS(II-1,JJ,2)) THEN
C!        DIR = LEFT
          I = I-1
          GOTO 200
      ELSE
          GOTO 300
      END IF
C
C DIR = RIGHT
C
  140 CONTINUE
      FLAGS(II,JJ,2) = .FALSE.
      IF (I.EQ.IB) THEN
          RETURN
      ELSE IF (FLAGS(II,JJ+1,1)) THEN
          DIR = UP
          J = J+1
          GOTO 250
      ELSE IF (FLAGS(II,JJ,1)) THEN
          DIR = DOWN
          GOTO 250
      ELSE IF (FLAGS(II+1,JJ,2)) THEN
C!        DIR = RIGHT
          I = I+1
          GOTO 200
      ELSE
          GOTO 300
      END IF
C
C Draw a segment of the contour.
C
  200 X = REAL(I)
      Y = REAL(J) + (Z0-Z(I,J))/(Z(I,J+1)-Z(I,J))
      CALL PLOT(1,X,Y,Z0)
      GOTO 100
  250 X = REAL(I) + (Z0-Z(I,J))/(Z(I+1,J)-Z(I,J))
      Y = REAL(J)
      CALL PLOT(1,X,Y,Z0)
      GOTO 100
C
C Close the contour and go look for another one.
C
  300 CALL PLOT(1,STARTX,STARTY,Z0)
      RETURN
C
      END
