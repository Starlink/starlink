C*PGERRX -- horizontal error bar
C%void cpgerrx(int n, const float *x1, const float *x2, \
C% const float *y, float t);
C+
      SUBROUTINE PGERRX (N, X1, X2, Y, T)
      INTEGER N
      REAL X1(*), X2(*), Y(*)
      REAL T
C
C Plot horizontal error bars.
C This routine draws an error bar only; to mark the data point in
C the middle of the error bar, an additional call to PGPT or
C PGERRY is required.
C
C Arguments:
C  N      (input)  : number of error bars to plot.
C  X1     (input)  : world x-coordinates of lower end of the
C                    error bars.
C  X2     (input)  : world x-coordinates of upper end of the
C                    error bars.
C  Y      (input)  : world y-coordinates of the data.
C  T      (input)  : length of terminals to be drawn at the ends
C                    of the error bar, as a multiple of the default
C                    length; if T = 0.0, no terminals will be drawn.
C
C Note: the dimension of arrays X1, X2, and Y must be greater
C than or equal to N. If N is 1, X1, X2, and Y may be scalar
C variables, or expressions, eg:
C       CALL PGERRX(1,X-SIGMA,X+SIGMA,Y)
C--
C (6-Oct-1983)
C 31-Mar-1997 - use pgtikl [TJP[.
C-----------------------------------------------------------------------
      INTEGER  I
      LOGICAL  PGNOTO
      REAL     XTIK, YTIK
C
      IF (PGNOTO('PGERRX')) RETURN
      IF (N.LT.1) RETURN
      CALL PGBBUF
C
      CALL PGTIKL(T, XTIK, YTIK)
      DO 10 I=1,N
          IF (T.NE.0.0) THEN
              CALL GRMOVA(X1(I),Y(I)-YTIK)
              CALL GRLINA(X1(I),Y(I)+YTIK)
          END IF
          CALL GRMOVA(X1(I),Y(I))
          CALL GRLINA(X2(I),Y(I))
          IF (T.NE.0.0) THEN
              CALL GRMOVA(X2(I),Y(I)-YTIK)
              CALL GRLINA(X2(I),Y(I)+YTIK)
          END IF
   10 CONTINUE
      CALL PGEBUF
      END
