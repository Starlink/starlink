C*PGERRY -- vertical error bar
C%void cpgerry(int n, const float *x, const float *y1, \
C% const float *y2, float t);
C+
      SUBROUTINE PGERRY (N, X, Y1, Y2, T)
      INTEGER N
      REAL X(*), Y1(*), Y2(*)
      REAL T
C
C Plot vertical error bars.
C This routine draws an error bar only; to mark the data point in
C the middle of the error bar, an additional call to PGPT or
C PGERRX is required.
C
C Arguments:
C  N      (input)  : number of error bars to plot.
C  X      (input)  : world x-coordinates of the data.
C  Y1     (input)  : world y-coordinates of top end of the
C                    error bars.
C  Y2     (input)  : world y-coordinates of bottom end of the
C                    error bars.
C  T      (input)  : length of terminals to be drawn at the ends
C                    of the error bar, as a multiple of the default
C                    length; if T = 0.0, no terminals will be drawn.
C
C Note: the dimension of arrays X, Y1, and Y2 must be greater
C than or equal to N. If N is 1, X, Y1, and Y2 may be scalar
C variables or expressions, eg:
C       CALL PGERRY(1,X,Y+SIGMA,Y-SIGMA)
C--
C (6-Oct-1983)
C 31-Mar-1997 - use pgtikl [TJP].
C-----------------------------------------------------------------------
      INTEGER  I
      LOGICAL  PGNOTO
      REAL     XTIK, YTIK
C
      IF (PGNOTO('PGERRY')) RETURN
      IF (N.LT.1) RETURN
      CALL PGBBUF
C
      CALL PGTIKL(T, XTIK, YTIK)
      DO 10 I=1,N
          IF (T.NE.0.0) THEN
              CALL GRMOVA(X(I)-XTIK,Y1(I))
              CALL GRLINA(X(I)+XTIK,Y1(I))
          END IF
          CALL GRMOVA(X(I),Y1(I))
          CALL GRLINA(X(I),Y2(I))
          IF (T.NE.0.0) THEN
              CALL GRMOVA(X(I)-XTIK,Y2(I))
              CALL GRLINA(X(I)+XTIK,Y2(I))
          END IF
   10 CONTINUE
      CALL PGEBUF
      END
