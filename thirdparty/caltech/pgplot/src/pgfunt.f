C*PGFUNT -- function defined by X = F(T), Y = G(T)
C+
      SUBROUTINE PGFUNT (FX, FY, N, TMIN, TMAX, PGFLAG)
      REAL FX, FY
      EXTERNAL FX, FY
      INTEGER N
      REAL TMIN, TMAX
      INTEGER PGFLAG
C
C Draw a curve defined by parametric equations X = FX(T), Y = FY(T).
C
C Arguments:
C  FX     (external real function): supplied by the user, evaluates
C                    X-coordinate.
C  FY     (external real function): supplied by the user, evaluates
C                    Y-coordinate.
C  N      (input)  : the number of points required to define the
C                    curve. The functions FX and FY will each be
C                    called N+1 times.
C  TMIN   (input)  : the minimum value for the parameter T.
C  TMAX   (input)  : the maximum value for the parameter T.
C  PGFLAG (input)  : if PGFLAG = 1, the curve is plotted in the
C                    current window and viewport; if PGFLAG = 0,
C                    PGENV is called automatically by PGFUNT to
C                    start a new plot with automatic scaling.
C
C Note: The functions FX and FY must be declared EXTERNAL in the
C Fortran program unit that calls PGFUNT.
C--
C  5-Oct-1983
C 11-May-1990 - remove unnecessary include [TJP].
C 13-Dec-1990 - make errors non-fatal [TJP].
C-----------------------------------------------------------------------
      INTEGER MAXP
      PARAMETER (MAXP=1000)
      INTEGER  I
      REAL     X(0:MAXP), Y(0:MAXP), DT
      REAL     XMIN, XMAX, YMIN, YMAX
C
      IF (N.LT.1 .OR. N.GT.MAXP) THEN
          CALL GRWARN('PGFUNT: invalid arguments')
          RETURN
      END IF
      CALL PGBBUF
C
C Evaluate function.
C
      DT = (TMAX-TMIN)/N
      X(0) = FX(TMIN)
      Y(0) = FY(TMIN)
      XMIN = X(0)
      XMAX = X(0)
      YMIN = Y(0)
      YMAX = Y(0)
      DO 10 I=1,N
          X(I) = FX(TMIN+DT*I)
          Y(I) = FY(TMIN+DT*I)
          XMIN = MIN(XMIN,X(I))
          XMAX = MAX(XMAX,X(I))
          YMIN = MIN(YMIN,Y(I))
          YMAX = MAX(YMAX,Y(I))
   10 CONTINUE
      DT = 0.05*(XMAX-XMIN)
      IF (DT.EQ.0.0) THEN
          XMIN = XMIN - 1.0
          XMAX = XMAX + 1.0
      ELSE
          XMIN = XMIN - DT
          XMAX = XMAX + DT
      END IF
      DT = 0.05*(YMAX-YMIN)
      IF (DT.EQ.0.0) THEN
          YMIN = YMIN - 1.0
          YMAX = YMAX + 1.0
      ELSE
          YMIN = YMIN - DT
          YMAX = YMAX + DT
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
