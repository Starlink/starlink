      PROGRAM PGDEM7
C
C Demonstration program for 3D surface plotting routine FREDDY.
C
      INTEGER PGBEG
      REAL A(51,51), R, SIZE
      INTEGER I, J
C
      IF (PGBEG(0, '?', 1, 1) .NE. 1) THEN
         STOP
      END IF
C
C Calculate a sample data array.
C
      DO 20 I=1,51
         DO 10 J=1,51
            R = (I-26)**2 + (J-26)**2
            R = 0.5*SQRT(R)
            IF (R.GT.0.0) THEN
               A(I,J) = SIN(R)/R
            ELSE
               A(I,J) = 1.0
            END IF
 10      CONTINUE
 20   CONTINUE
C
C FREDDY assumes the window is square of size SIZE.
C
      SIZE = 1.0
      CALL PGENV(0., SIZE, 0., SIZE, 1, -2)
      CALL FREDDY(A,51,51, SIZE, 25.0)
      CALL PGEND
      END
C-----------------------------------------------------------------------
      SUBROUTINE FREDDY(ARRAY,KX,NY,SIZE,ANGLE)
      INTEGER KX, NY
      REAL ARRAY(KX,NY), SIZE, ANGLE
C
C Draws isometric plot of array
C
      REAL FMAX,FMIN,DELTAX,DELTAY,DELTAV,SINE,PEAK,X,DX,HEIGHT
      INTEGER I,J,KI,KJ,NX,MX,MY,STEP,LEFT,RIGHT,IT,MN,INCX
      LOGICAL VISBLE
      COMMON /FREDCM/ DELTAX,X,STEP,LEFT,RIGHT,IT,NX,VISBLE
C
      MN = KX*NY
      NX = KX
C     Check array size:
      IF(NX.LT.2 .OR. NY.LT.2) RETURN
      FMAX = ARRAY(1,1)
      FMIN = FMAX
      DO 20 J=1,NY
          DO 10 I=1,NX
              FMIN = AMIN1(ARRAY(I,J),FMIN)
              FMAX = AMAX1(ARRAY(I,J),FMAX)
   10     CONTINUE
   20 CONTINUE
      DELTAX = SIZE/(NX+NY)
      SINE = SIN(ANGLE/58.)
      DELTAY = DELTAX*SINE
      HEIGHT = SIZE*(1.-ABS(SINE))
      DELTAV = HEIGHT
      FMAX = FMAX-FMIN
      IF(FMAX.LT.0.0001) FMAX = DELTAV
      DELTAV = DELTAV/FMAX
      MX = NX+1
      MY = NY+1
      STEP = MX
C
C Start PGPLOT buffering.
C
      CALL PGBBUF
C
C Work our way down the Y axis, then up the X axis,
C calculating the Y plotter coordinates for each
C column of the plot, doing the hidden-line suppression
C at the same time.
C
      DO 50 J=1,NY
          KJ = MY-J
          KI = 1
C               ( KI,KJ are coordinates of bottom of column)
          ARRAY(KI,KJ) = DELTAY*(KI+KJ) + DELTAV*(ARRAY(KI,KJ)-FMIN)
   30     PEAK = ARRAY(KI,KJ)
   40     KI = KI+1
          KJ = KJ+1
          IF(KI.GT.NX .OR. KJ.GT.NY) GOTO 50
          ARRAY(KI,KJ) = DELTAY*(KI+KJ) + DELTAV*(ARRAY(KI,KJ)-FMIN)
          IF(ARRAY(KI,KJ).GT.PEAK) GOTO 30
          IF(ARRAY(KI,KJ).LE.PEAK) ARRAY(KI,KJ) = -ABS(ARRAY(KI,KJ))
          GOTO 40
   50 CONTINUE
C
C Now to work our way up the X axis
C
      DO 80 I=2,NX
          KI = I
          KJ = 1
          ARRAY(KI,KJ) = DELTAY*(KI+KJ)+DELTAV*(ARRAY(KI,KJ)-FMIN)
   60     PEAK = ARRAY(KI,KJ)
   70     KI = KI+1
          KJ = KJ+1
          IF(KI.GT.NX .OR. KJ.GT.NY) GOTO 80
          ARRAY(KI,KJ) = DELTAY*(KI+KJ)+DELTAV*(ARRAY(KI,KJ)-FMIN)
          IF(ARRAY(KI,KJ).GT.PEAK) GOTO 60
          IF(ARRAY(KI,KJ).LE.PEAK) ARRAY(KI,KJ) = -ABS(ARRAY(KI,KJ))
          GOTO 70
   80 CONTINUE
C
C Draw a line along the bottom of the vertical faces
C
      CALL PGMOVE(DELTAX*(NX+NY-2), DELTAY*(MX))
      CALL PGDRAW(DELTAX*(NY-1),    DELTAY*2)
      CALL PGDRAW(0.0,              DELTAY*MY)
C
C Array is now ready for plotting.  If a point is
C positive, then it is to be plotted at that Y
C coordinate; if it is negative, then it is
C invisible, but at minus that Y coordinate (the point
C where the line heading towards it disappears has to
C be determined by finding the intersection of it and
C the cresting line).
C
C Plot rows:
C
      DO 110 J=1,NY,2
          KJ = MY-J
          DX = DELTAX*(J-2)
          X = DX+DELTAX
          CALL PGMOVE(X,DELTAY*(KJ+1))
          CALL PGDRAW(X,ARRAY(1,KJ))
          VISBLE = .TRUE.
          DO 90 I=2,NX
              RIGHT = I+NX*(KJ-1)
              LEFT = RIGHT-1
              IT = RIGHT
              X = DX+DELTAX*I
              CALL FREDGO(ARRAY,MN)
   90     CONTINUE
C
C Now at far end of row so come back
C
          KJ = KJ-1
          IF(KJ.LE.0) GOTO 170
          VISBLE = ARRAY(NX,KJ).GE.0.0
          DX = DELTAX*(NX+J)
          IF(VISBLE) CALL PGMOVE(DX-DELTAX,ARRAY(NX,KJ))
          DELTAX = -DELTAX
          DO 100 I=2,NX
              KI = MX-I
              LEFT = KI+NX*(KJ-1)
              RIGHT = LEFT+1
              IT = LEFT
              X = DX+DELTAX*I
              CALL FREDGO(ARRAY,MN)
  100     CONTINUE
C
          X = DX+DELTAX*NX
          IF(.NOT.VISBLE) CALL PGMOVE(X,ARRAY(1,KJ))
          CALL PGDRAW(X,DELTAY*(KJ+1))
C               (set DELTAX positive for return trip)
          DELTAX = -DELTAX
  110 CONTINUE
C
C Now do the columns:
C as we fell out of the last DO-loop we do the
C columns in ascending-X order
C
      INCX = 1
      KI = 1
C               (set DELTAX -ve since scanning R to L)
  120 DX = DELTAX*(KI+NY-1)
      DELTAX = -DELTAX
      X = DX+DELTAX
      CALL PGMOVE(X,ARRAY(1,1))
  130 VISBLE = .TRUE.
      DO 140 J=2,NY
          LEFT = KI+NX*(J-1)
          RIGHT = LEFT-NX
          IT = LEFT
          X = DX+DELTAX*J
          CALL FREDGO(ARRAY,MN)
  140 CONTINUE
C
C At far end, increment X and check still inside array
C
      KI = KI+INCX
      IF(KI.LE.0 .OR. KI.GT.NX) GOTO 180
      VISBLE = ARRAY(KI,NY).GE.0.0
      DELTAX = -DELTAX
      DX = DELTAX*(KI-2)
      X = DX+DELTAX
      IF(VISBLE) CALL PGMOVE(X,ARRAY(KI,NY))
      DO 150 J=2,NY
          KJ = MY-J
          RIGHT = KI+NX*(KJ-1)
          LEFT = RIGHT+NX
          IT = RIGHT
          X = DX+DELTAX*J
          CALL FREDGO(ARRAY,MN)
  150 CONTINUE
C
      X = DX+DELTAX*NY
      IF(.NOT.VISBLE) CALL PGMOVE(X,ARRAY(KI,1))
      IF(KI.EQ.1) GOTO 180
      CALL PGDRAW(X,DELTAY*(KI+1))
      KI = KI+INCX
      IF(KI.GT.NX) GOTO 180
      IF(KI.EQ.1) GOTO 120
  160 DELTAX = -DELTAX
      DX = DELTAX*(1-KI-NY)
      X = DX+DELTAX
      CALL PGMOVE(X,DELTAY*(KI+1))
      CALL PGDRAW(X,ARRAY(KI,1))
      GOTO 130
C
C Do columns backwards because ended rows at far end of X
C
  170 KI = NX
      INCX = -1
      DX = DELTAX*(KI+NY)
      GOTO 160
C
C
  180 CALL PGEBUF
      END
C-----------------------------------------------------------------------
      SUBROUTINE FREDGO(ARRAY,MN)
      INTEGER MN
      REAL ARRAY(MN)
C
      INTEGER STEP,LEFT,RIGHT,IT,NX
      LOGICAL VISBLE
      REAL AL,AR,BL,EM,XX,X,Y,DELTAX
      COMMON /FREDCM/ DELTAX,X,STEP,LEFT,RIGHT,IT,NX,VISBLE
C
C Test visibility
C
      IF(ARRAY(IT).LT.0.0) GOTO 80
C
C This point is visible - was last?
C
      IF(VISBLE) GOTO 50
C
C No: calculate point where this line vanishes
C
   10 IF(LEFT.LE.NX .OR. MOD(LEFT-1,NX).EQ.0 .OR.
     1     RIGHT.LE.NX .OR. MOD(RIGHT-1,NX).EQ.0) GOTO 100
      AL = ABS(ARRAY(LEFT))
      AR = ABS(ARRAY(RIGHT))
      IF(ARRAY(LEFT).LT.0.0) GOTO 70
C               Right-hand point is crested
   20 RIGHT = RIGHT-STEP
      IF(ARRAY(RIGHT).LT.0.0) GOTO 20
C               Left-hand end of cresting line is either
C               RIGHT+NX or RIGHT-1
      LEFT = RIGHT+NX
      IF(ARRAY(LEFT).LT.0.0) LEFT = RIGHT-1
C
C               RIGHT and LEFT index into the endpoints of the
C               cresting line
   30 BL = ABS(ARRAY(LEFT))
      EM = ABS(ARRAY(RIGHT))-BL
      XX = EM-AR+AL
      IF(ABS(XX).LT.0.0001) GOTO 60
      XX = (AL-BL)/XX
   40 Y = EM*XX+BL
      IF(DELTAX.GT.0.0) XX = 1.0-XX
      XX = X-XX*DELTAX
      IF(VISBLE) GOTO 90
C               Drawing a line from an invisible point
C               to a visible one
      CALL PGMOVE(XX,Y)
      VISBLE = .TRUE.
   50 CALL PGDRAW(X,ARRAY(IT))
      RETURN
C
   60 XX = 0.5
      GOTO 40
C
C Left-hand point crested
C
   70 LEFT = LEFT-STEP
      IF(ARRAY(LEFT).LT.0.0) GOTO 70
C
C Right-hand end of cresting line is either LEFT+1 or LEFT-NX
C
      RIGHT = LEFT+1
      IF(ARRAY(RIGHT).LT.0.0) RIGHT = LEFT-NX
      GOTO 30
C
C This point is invisible; if last one was too, then forget it;
C else draw a line towards it
C
   80 IF(.NOT.VISBLE) RETURN
      GOTO 10
C
   90 CALL PGDRAW(XX,Y)
  100 VISBLE = .FALSE.
      RETURN
      END
