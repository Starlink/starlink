C*PGFUNY -- function defined by X = F(Y)
C+
      SUBROUTINE PGFUNY (FX, N, YMIN, YMAX, PGFLAG)
      REAL    FX
      EXTERNAL FX
      INTEGER N
      REAL    YMIN, YMAX
      INTEGER PGFLAG
C
C Draw a curve defined by the equation X = FX(Y), where FY is a
C user-supplied subroutine.
C
C Arguments:
C  FX     (external real function): supplied by the user, evaluates
C                    X value at a given Y-coordinate.
C  N      (input)  : the number of points required to define the
C                    curve. The function FX will be called N+1 times.
C                    If PGFLAG=0 and N is greater than 1000, 1000
C                    will be used instead.  If N is less than 1,
C                    nothing will be drawn.
C  YMIN   (input)  : the minimum value of Y.
C  YMAX   (input)  : the maximum value of Y.
C  PGFLAG (input)  : if PGFLAG = 1, the curve is plotted in the
C                    current window and viewport; if PGFLAG = 0,
C                    PGENV is called automatically by PGFUNY to
C                    start a new plot with Y limits (YMIN, YMAX)
C                    and automatic scaling in X.
C
C Note: The function FX must be declared EXTERNAL in the Fortran
C program unit that calls PGFUNY.  It has one argument, the
C y-coordinate at which the x value is required, e.g.
C   REAL FUNCTION FX(Y)
C   REAL Y
C   FX = .....
C   END
C--
C  5-Oct-1983
C 11-May-1990 - remove unnecessary include [TJP].
C 13-DEc-1990 - make errors non-fatal [TJP].
C-----------------------------------------------------------------------
      INTEGER MAXP
      PARAMETER (MAXP=1000)
      INTEGER  I
      REAL     X(0:MAXP), Y(0:MAXP), DT
      REAL     XMIN, XMAX
C
      IF (N.LT.1 .OR. N.GT.MAXP) THEN
          CALL GRWARN('PGFUNY: invalid arguments')
          RETURN
      END IF
      CALL PGBBUF
C
C Evaluate function.
C
      DT = (YMAX-YMIN)/N
      X(0) = FX(YMIN)
      Y(0) = YMIN
      XMIN = X(0)
      XMAX = X(0)
      DO 10 I=1,N
          X(I) = FX(YMIN+DT*I)
          Y(I) = YMIN + DT*I
          XMIN = MIN(XMIN,X(I))
          XMAX = MAX(XMAX,X(I))
   10 CONTINUE
      DT = 0.05*(XMAX-XMIN)
      IF (DT.EQ.0.0) THEN
          XMIN = XMIN - 1.0
          XMAX = XMAX + 1.0
      ELSE
          XMIN = XMIN - DT
          XMAX = XMAX + DT
      END IF
C
C Define environment if necessary.
C
      IF (PGFLAG.EQ.0) CALL PGENV(XMIN,XMAX,YMIN,YMAX,0,0)
C
C Draw curve.
C
      CALL PGMOVE(X(0),Y(0))
      DO 20 I=1,N
          CALL PGDRAW(X(I),Y(I))
   20 CONTINUE
C
      CALL PGEBUF
      END
