      SUBROUTINE PGCNSC (Z, MX, MY, IA, IB, JA, JB, Z0, PLOT)
      INTEGER MX, MY, IA, IB, JA, JB
      REAL Z(MX,*)
      REAL Z0
      EXTERNAL PLOT
C
C PGPLOT (internal routine): Draw a single contour.  This routine is
C called by PGCONT, but may be called directly by the user.
C
C Arguments:
C
C Z (real array dimension MX,MY, input): the array of function values.
C MX,MY (integer, input): actual declared dimension of Z(*,*).
C IA,IB (integer, input): inclusive range of the first index of Z to be
C       contoured.
C JA,JB (integer, input): inclusive range of the second index of Z to
C       be contoured.
C Z0 (real, input): the contour level sought.
C PLOT (the name of a subroutine declared EXTERNAL in the calling
C       routine): this routine is called by PGCNSC to do all graphical
C       output. The calling sequence is CALL PLOT(K,X,Y,Z) where Z is
C       the contour level, (X,Y) are the coordinates of a point (in the
C       inclusive range I1<X<I2, J1<Y<J2, and if K is 0, the routine is
C       to move then pen to (X,Y); if K is 1, it is to draw a line from
C       the current position to (X,Y).
C
C NOTE:  the intervals (IA,IB) and (JA,JB) must not exceed the
C dimensions of an internal array. These are currently set at 100.
C--
C 17-Sep-1989 - Completely rewritten [TJP]. The algorithm is my own,
C               but it is probably not original. It could probably be
C               coded more briefly, if not as clearly.
C  1-May-1994 - Modified to draw contours anticlockwise about maxima,
C               to prevent contours at different levels from
C               crossing in degenerate cells [TJP].
C-----------------------------------------------------------------------
      INTEGER UP, DOWN, LEFT, RIGHT
      PARAMETER (UP=1, DOWN=2, LEFT=3, RIGHT=4)
      INTEGER  MAXEMX, MAXEMY
      PARAMETER (MAXEMX=100, MAXEMY=100)
C
      LOGICAL FLAGS(MAXEMX,MAXEMY,2), RANGE
      INTEGER I, J, II, JJ, DIR
      REAL Z1, Z2, Z3, P, P1, P2
C
C The statement function RANGE decides whether a contour at level P
C crosses the line between two gridpoints with values P1 and P2. It is
C important that a contour cannot cross a line with equal endpoints.
C
      RANGE (P,P1,P2) = (P.GT.MIN(P1,P2)) .AND. (P.LE.MAX(P1,P2))
     1                  .AND. (P1.NE.P2)
C
C Check for errors.
C
      IF ( (IB-IA+1) .GT. MAXEMX .OR.  (JB-JA+1) .GT. MAXEMY ) THEN
          CALL GRWARN('PGCNSC - array index range exceeds'//
     1                ' built-in limit of 100')
          RETURN
      END IF
C
C Initialize the flags. The first flag for a gridpoint is set if
C the contour crosses the line segment to the right of the gridpoint
C (joining [I,J] to [I+1,J]); the second flag is set if if it crosses
C the line segment above the gridpoint (joining [I,J] to [I,J+1]).
C The top and right edges require special treatment. (For purposes
C of description only, we assume I increases horizontally to the right
C and J increases vertically upwards.)
C
      DO 20 I=IA,IB
          II = I-IA+1
          DO 10 J=JA,JB
              JJ = J-JA+1
              Z1 = Z(I,J)
              FLAGS(II,JJ,1) = .FALSE.
              FLAGS(II,JJ,2) = .FALSE.
              IF (I.LT.IB) THEN
                Z2 = Z(I+1,J)
                IF (RANGE(Z0,Z1,Z2)) FLAGS(II,JJ,1) = .TRUE.
              END IF
              IF (J.LT.JB) THEN
                Z3 = Z(I,J+1)
                IF (RANGE(Z0,Z1,Z3)) FLAGS(II,JJ,2) = .TRUE.
              END IF
   10     CONTINUE
   20 CONTINUE
C
C Search the edges of the array for the start of an unclosed contour.
C Note that (if the algorithm is implemented correctly) all unclosed
C contours must begin and end at the edge of the array. When one is
C found, call PGCN01 to draw the contour, telling it the correct
C starting direction so that it follows the contour into the array
C instead of out of it. A contour is only started if the higher
C ground lies to the left: this is to enforce the direction convention
C that contours are drawn anticlockwise around maxima. If the high
C ground lies to the right, we will find the other end of the contour
C and start there.
C
C Bottom edge.
C
      J = JA
      JJ = J-JA+1
      DO 26 I=IA,IB-1
          II = I-IA+1
          IF (FLAGS(II,JJ,1) .AND. (Z(I,J).GT.Z(I+1,J)))
     1          CALL PGCN01(Z, MX, MY, IA, IB, JA, JB,
     2                      Z0, PLOT, FLAGS, I, J, UP)
   26 CONTINUE
C
C Right edge.
C
      I = IB
      II = I-IA+1
      DO 27 J=JA,JB-1
          JJ = J-JA+1
          IF (FLAGS(II,JJ,2) .AND. (Z(I,J).GT.Z(I,J+1)))
     1          CALL PGCN01(Z, MX, MY, IA, IB, JA, JB,
     2                      Z0, PLOT, FLAGS, I, J, LEFT)
   27 CONTINUE
C
C Top edge.
C
      J = JB
      JJ = J-JA+1
      DO 28 I=IB-1,IA,-1
          II = I-IA+1
          IF (FLAGS(II,JJ,1) .AND. (Z(I+1,J).GT.Z(I,J)))
     1          CALL PGCN01(Z, MX, MY, IA, IB, JA, JB,
     2                      Z0, PLOT, FLAGS, I, J, DOWN)
   28 CONTINUE
C
C Left edge.
C
      I = IA
      II = I-IA+1
      DO 29 J=JB-1,JA,-1
          JJ = J-JA+1
          IF (FLAGS(II,JJ,2)  .AND. (Z(I,J+1).GT.Z(I,J)))
     1          CALL PGCN01(Z, MX, MY, IA, IB, JA, JB,
     2                      Z0, PLOT, FLAGS, I, J, RIGHT)
   29 CONTINUE
C
C Now search the interior of the array for a crossing point, which will
C lie on a closed contour (because all unclosed contours have been
C eliminated). It is sufficient to search just the horizontal crossings
C (or the vertical ones); any closed contour must cross a horizontal
C and a vertical gridline. PGCN01 assumes that when it cannot proceed
C any further, it has reached the end of a closed contour. Thus all
C unclosed contours must be eliminated first.
C
      DO 40 I=IA+1,IB-1
          II = I-IA+1
          DO 30 J=JA+1,JB-1
              JJ = J-JA+1
              IF (FLAGS(II,JJ,1)) THEN
                  DIR = UP
                  IF (Z(I+1,J).GT. Z(I,J)) DIR = DOWN
                  CALL PGCN01(Z, MX, MY, IA, IB, JA, JB,
     1                        Z0, PLOT, FLAGS, I, J, DIR)

              END IF
   30     CONTINUE
   40 CONTINUE
C
C We didn't find any more crossing points: we're finished.
C
      RETURN
      END
