C*PGERRB -- horizontal or vertical error bar
C%void cpgerrb(int dir, int n, const float *x, const float *y, \
C% const float *e, float t);
C+
      SUBROUTINE PGERRB (DIR, N, X, Y, E, T)
      INTEGER DIR, N
      REAL X(*), Y(*), E(*)
      REAL T
C
C Plot error bars in the direction specified by DIR.
C This routine draws an error bar only; to mark the data point at
C the start of the error bar, an additional call to PGPT is required.
C
C Arguments:
C  DIR    (input)  : direction to plot the error bar relative to
C                    the data point. 
C                    One-sided error bar:
C                      DIR is 1 for +X (X to X+E);
C                             2 for +Y (Y to Y+E);
C                             3 for -X (X to X-E);
C                             4 for -Y (Y to Y-E).
C                    Two-sided error bar:
C                      DIR is 5 for +/-X (X-E to X+E); 
C                             6 for +/-Y (Y-E to Y+E).
C  N      (input)  : number of error bars to plot.
C  X      (input)  : world x-coordinates of the data.
C  Y      (input)  : world y-coordinates of the data.
C  E      (input)  : value of error bar distance to be added to the
C                    data position in world coordinates.
C  T      (input)  : length of terminals to be drawn at the ends
C                    of the error bar, as a multiple of the default
C                    length; if T = 0.0, no terminals will be drawn.
C
C Note: the dimension of arrays X, Y, and E must be greater
C than or equal to N. If N is 1, X, Y, and E may be scalar
C variables, or expressions.
C--
C  1-Mar-1991 - new routine [JM].
C 20-Apr-1992 - correct bug [ALF, TJP].
C 28-Mar-1995 - add options DIR = 5 or 6 [TJP].
C 31-Mar-1997 - use pgtikl [TJP].
C-----------------------------------------------------------------------
      INTEGER  I
      LOGICAL  PGNOTO
      REAL     XTIK, YTIK, XX, YY
C
      IF (PGNOTO('PGERRB')) RETURN
      IF (N.LT.1) RETURN
      IF (DIR.LT.1 .OR. DIR.GT.6) RETURN
      CALL PGBBUF
C
C Determine terminal length.
C
      CALL PGTIKL(T, XTIK, YTIK)
C
C Loop through points.
C
      DO 10 I=1,N
C
C Draw terminal at starting point if required.
C
         IF (DIR.EQ.5) THEN
            XX = X(I)-E(I)
            YY = Y(I)
         ELSE IF (DIR.EQ.6) THEN
            XX = X(I)
            YY = Y(I)-E(I)
         ELSE
            XX = X(I)
            YY = Y(I)
         END IF
         IF (T.NE.0.0) THEN
            IF (DIR.EQ.5) THEN
               CALL GRMOVA(XX,YY-YTIK)
               CALL GRLINA(XX,YY+YTIK)
            ELSE IF (DIR.EQ.6) THEN
               CALL GRMOVA(XX-XTIK,YY)
               CALL GRLINA(XX+XTIK,YY)
            END IF
         END IF
C
C Draw the error bar itself.
C
         CALL GRMOVA(XX,YY)
         IF (DIR.EQ.1 .OR. DIR.EQ.5) THEN
            XX = X(I)+E(I)
            YY = Y(I)
         ELSE IF (DIR.EQ.2 .OR. DIR.EQ.6) THEN
            XX = X(I)
            YY = Y(I)+E(I)
         ELSE IF (DIR.EQ.3) THEN
            XX = X(I)-E(I)
            YY = Y(I)
         ELSE IF (DIR.EQ.4) THEN
            XX = X(I)
            YY = Y(I)-E(I)
         END IF
         CALL GRLINA(XX,YY)
C
C Draw terminal at end point.
C
         IF (T.NE.0.0) THEN
            IF (MOD(DIR,2).EQ.1) THEN
               CALL GRMOVA(XX,YY-YTIK)
               CALL GRLINA(XX,YY+YTIK)
            ELSE
               CALL GRMOVA(XX-XTIK,YY)
               CALL GRLINA(XX+XTIK,YY)
            END IF
         END IF
C
 10   CONTINUE
      CALL PGEBUF
      END
