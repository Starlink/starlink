C*PGFUNX -- function defined by Y = F(X)
C+
      SUBROUTINE PGFUNX (FY, N, XMIN, XMAX, PGFLAG)
      REAL FY
      EXTERNAL FY
      INTEGER N
      REAL XMIN, XMAX
      INTEGER PGFLAG
C
C Draw a curve defined by the equation Y = FY(X), where FY is a
C user-supplied subroutine.
C
C Arguments:
C  FY     (external real function): supplied by the user, evaluates
C                    Y value at a given X-coordinate.
C  N      (input)  : the number of points required to define the
C                    curve. The function FY will be called N+1 times.
C                    If PGFLAG=0 and N is greater than 1000, 1000
C                    will be used instead.  If N is less than 1,
C                    nothing will be drawn.
C  XMIN   (input)  : the minimum value of X.
C  XMAX   (input)  : the maximum value of X.
C  PGFLAG (input)  : if PGFLAG = 1, the curve is plotted in the
C                    current window and viewport; if PGFLAG = 0,
C                    PGENV is called automatically by PGFUNX to
C                    start a new plot with X limits (XMIN, XMAX)
C                    and automatic scaling in Y.
C
C Note: The function FY must be declared EXTERNAL in the Fortran
C program unit that calls PGFUNX.  It has one argument, the
C x-coordinate at which the y value is required, e.g.
C   REAL FUNCTION FY(X)
C   REAL X
C   FY = .....
C   END
C--
C  6-Oct-1983 - TJP.
C  6-May-1985 - fix Y(0) bug - TJP.
C 11-May-1990 - remove unnecessary include - TJP.
C-----------------------------------------------------------------------
      INTEGER MAXP
      PARAMETER (MAXP=1000)
      INTEGER  I, NN
      REAL     Y(0:MAXP), DT, DY
      REAL     YMIN, YMAX
C
C Check N > 1, and find parameter increment.
C
      IF (N.LT.1) RETURN
      DT = (XMAX-XMIN)/N
      CALL PGBBUF
C
C Case 1: we do not have to find limits.
C
      IF (PGFLAG.NE.0) THEN
          CALL PGMOVE(XMIN,FY(XMIN))
          DO 10 I=1,N
              CALL PGDRAW(XMIN+I*DT,FY(XMIN+I*DT))
   10     CONTINUE
C
C Case 2: find limits and scale plot; function values must be stored
C in an array.
C
      ELSE
          NN = MIN(N,MAXP)
          Y(0) = FY(XMIN)
          YMIN = Y(0)
          YMAX = Y(0)
          DO 20 I=1,NN
              Y(I) = FY(XMIN+DT*I)
              YMIN = MIN(YMIN,Y(I))
              YMAX = MAX(YMAX,Y(I))
   20     CONTINUE
          DY = 0.05*(YMAX-YMIN)
          IF (DY.EQ.0.0) THEN
              YMIN = YMIN - 1.0
              YMAX = YMAX + 1.0
          ELSE
              YMIN = YMIN - DY
              YMAX = YMAX + DY
          END IF
          CALL PGENV(XMIN,XMAX,YMIN,YMAX,0,0)
          CALL PGMOVE(XMIN,Y(0))
          DO 30 I=1,NN
              CALL PGDRAW(XMIN+DT*I,Y(I))
   30     CONTINUE
      END IF
C
      CALL PGEBUF
      END
