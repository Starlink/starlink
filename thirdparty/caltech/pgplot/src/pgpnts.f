C*PGPNTS -- draw several graph markers, not all the same
C%void cpgpnts(int n, const float *x, const float *y, \
C% const int *symbol, int ns);
C+
      SUBROUTINE PGPNTS (N, X, Y, SYMBOL, NS)
      INTEGER N, NS
      REAL X(*), Y(*)
      INTEGER SYMBOL(*)
C
C Draw Graph Markers. Unlike PGPT, this routine can draw a different
C symbol at each point. The markers are drawn using the current values
C of attributes color-index, line-width, and character-height
C (character-font applies if the symbol number is >31).  If the point
C to be marked lies outside the window, no marker is drawn.  The "pen 
C position" is changed to (XPTS(N),YPTS(N)) in world coordinates
C (if N > 0).
C
C Arguments:
C  N      (input)  : number of points to mark.
C  X      (input)  : world x-coordinate of the points.
C  Y      (input)  : world y-coordinate of the points.
C  SYMBOL (input)  : code number of the symbol to be plotted at each
C                    point (see PGPT).
C  NS     (input)  : number of values in the SYMBOL array.  If NS <= N,
C                    then the first NS points are drawn using the value
C                    of SYMBOL(I) at (X(I), Y(I)) and SYMBOL(1) for all
C                    the values of (X(I), Y(I)) where I > NS.
C
C Note: the dimension of arrays X and Y must be greater than or equal
C to N and the dimension of the array SYMBOL must be greater than or
C equal to NS.  If N is 1, X and Y may be scalars (constants or
C variables).  If NS is 1, then SYMBOL may be a scalar.  If N is
C less than 1, nothing is drawn.
C--
C 11-Mar-1991 - new routine [JM].
C 26-Feb-1997 - revised to use PGPT1 [TJP].
C-----------------------------------------------------------------------
      INTEGER I, SYMB
C
      IF (N.LT.1) RETURN
      CALL PGBBUF
      DO 10 I=1,N
          IF (I .LE. NS) THEN
              SYMB = SYMBOL(I)
          ELSE
              SYMB = SYMBOL(1)
          END IF
          CALL PGPT1(X(I), Y(I), SYMB)
   10 CONTINUE
      CALL PGEBUF
      END
