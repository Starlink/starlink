C*GRFA -- fill area (polygon)
C+
      SUBROUTINE GRFA (N,PX,PY)
      INTEGER N
      REAL PX(*), PY(*)
C
C GRPCKG: FILL AREA: fill a polygon with solid color.  The polygon
C is defined by the (x,y) world coordinates of its N vertices.  If
C this is not a function supported by the device, shading is
C accomplished by drawing horizontal lines spaced by 1 pixel.  By
C selecting color index 0, the interior of the polygon can be erased
C on devices which permit it.  The polygon need not be convex, but if
C it is re-entrant (i.e., edges intersect other than at the vertices),
C it may not be obvious which regions are "inside" the polygon.  The
C following rule is applied: for a given point, create a straight line
C starting at the point and going to infinity. If the number of
C intersections between the straight line and the polygon is odd, the
C point is within the polygon; otherwise it is outside. If the
C straight line passes a polygon vertex tangentially, the
C intersection  count is not affected. The only attribute which applies
C to FILL AREA is color index: line-width and line-style are ignored.
C There is a limitation on the complexity of the polygon: GFA will
C fail if any horizontal line intersects more than 32 edges of the
C polygon.
C
C Arguments:
C
C N (input, integer): the number of vertices of the polygon (at least
C       3).
C PX, PY (input, real arrays, dimension at least N): world coordinates
C       of the N vertices of the polygon.
C--
C 16-Jul-1984 - [TJP].
C  5-Aug-1986 - add GREXEC support [AFT].
C 21-Feb-1987 - If needed, calls begin picture [AFT].
C  7-Sep-1994 - avoid driver call for capabilities [TJP].
C  1-May-1995 - fixed bug for re-entrant polygons, and optimized code
C               [A.F.Carman].
C 18-Oct-1995 - fixed bug: emulated fill failed for reversed y-axis
C               [S.C.Allendorf/TJP].
C  4-Dec-1995 - remove use of real variable as do-loop variable [TJP].
C 20-Mar-1996 - use another do loop 40 to avoid gaps between adjacent
C               polygons [RS]
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      INTEGER MAXSEC
      PARAMETER (MAXSEC=32)
      INTEGER I, J, NSECT, LW, LS, NBUF, LCHR, LINE
      REAL    RBUF(6)
      CHARACTER*32 CHR
      REAL    X(MAXSEC), Y, YMIN, YMAX, DY, YD, TEMP, S1, S2, T1, T2
      LOGICAL FORWD
C
      IF (GRCIDE.LT.1) RETURN
      IF (N.LT.3) THEN
          CALL GRWARN('GRFA - polygon has < 3 vertices.')
          RETURN
      END IF
C
C Devices with polygon fill capability.
C
      IF(GRGCAP(GRCIDE)(4:4).EQ.'A') THEN
         IF (.NOT.GRPLTD(GRCIDE)) CALL GRBPIC
         RBUF(1) = N
         CALL GREXEC(GRGTYP,20,RBUF,NBUF,CHR,LCHR)
         DO 10 I=1,N
            RBUF(1) = PX(I)*GRXSCL(GRCIDE) + GRXORG(GRCIDE)
            RBUF(2) = PY(I)*GRYSCL(GRCIDE) + GRYORG(GRCIDE)
            CALL GREXEC(GRGTYP,20,RBUF,NBUF,CHR,LCHR)
 10      CONTINUE
         RETURN
      END IF
C
C For other devices fill area is simulated.
C
C Save attributes.
C
      CALL GRQLS(LS)
      CALL GRQLW(LW)
      CALL GRSLS(1)
      CALL GRSLW(1)
C
C Find range of raster-lines to be shaded.
C
      YMIN = PY(1)*GRYSCL(GRCIDE) + GRYORG(GRCIDE)
      YMAX = YMIN
      DO 20 I=2,N
         YD = PY(I)*GRYSCL(GRCIDE) + GRYORG(GRCIDE)
         YMIN = MIN(YMIN,YD)
         YMAX = MAX(YMAX,YD)
 20   CONTINUE
      CALL GREXEC(GRGTYP, 3,RBUF,NBUF,CHR,LCHR)
      DY = ABS(RBUF(3))
C
C Find intersections of edges with current raster line.
C
      FORWD = .TRUE.
      S1 = PX(N)*GRXSCL(GRCIDE) + GRXORG(GRCIDE)
      T1 = PY(N)*GRYSCL(GRCIDE) + GRYORG(GRCIDE)
C
      DO 40 LINE = NINT(YMIN/DY),NINT(YMAX/DY)
         Y = LINE * DY
         NSECT = 0
         DO 30 I=1,N
            S2 = PX(I)*GRXSCL(GRCIDE) + GRXORG(GRCIDE)
            T2 = PY(I)*GRYSCL(GRCIDE) + GRYORG(GRCIDE)
            IF ((T1.LT.Y .AND. Y.LE.T2).OR.
     :          (T1.GE.Y .AND. Y.GT.T2)) THEN
               NSECT = NSECT+1
               IF (NSECT.GT.MAXSEC) THEN
                  CALL GRWARN('GRFA - polygon is too complex.')
                  RETURN
               END IF
               X(NSECT)=(S1+(S2-S1)*((Y-T1)/(T2-T1)))
            END IF
            S1 = S2
            T1 = T2
 30      CONTINUE
C
C Sort the intersections into increasing x order.
C
         DO 34 I=2,NSECT
            DO 32 J=1,I
               IF (X(J).GT.X(I)) THEN
                  TEMP = X(J)
                  X(J) = X(I)
                  X(I) = TEMP
               END IF
 32         CONTINUE
 34      CONTINUE
C
C Draw the horizontal line-segments.
C
         GRYPRE(GRCIDE) = Y
         IF (FORWD) THEN
            DO 36 I=1,NSECT-1,2
               GRXPRE(GRCIDE) = X(I)
               CALL GRLIN0(X(I+1),Y)
 36         CONTINUE
            FORWD = .FALSE.
         ELSE
            DO 38 I=NSECT,2,-2
               GRXPRE(GRCIDE) = X(I)
               CALL GRLIN0(X(I-1),Y)
 38         CONTINUE
            FORWD = .TRUE.
         END IF
 40   CONTINUE
C
C Restore attributes.
C
      CALL GRSLS(LS)
      CALL GRSLW(LW)
      END
