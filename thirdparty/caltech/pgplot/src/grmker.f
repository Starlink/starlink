C*GRMKER -- draw graph markers
C+
      SUBROUTINE GRMKER (SYMBOL,ABSXY,N,X,Y)
C
C GRPCKG: Draw a graph marker at a set of points in the current
C window. Line attributes (color, intensity, and  thickness)
C apply to markers, but line-style is ignored. After the call to
C GRMKER, the current pen position will be the center of the last
C marker plotted.
C
C Arguments:
C
C SYMBOL (input, integer): the marker number to be drawn. Numbers
C       0-31 are special marker symbols; numbers 32-127 are the
C       corresponding ASCII characters (in the current font). If the
C       number is >127, it is taken to be a Hershey symbol number.
C       If -ve, a regular polygon is drawn.
C ABSXY (input, logical): if .TRUE., the input corrdinates (X,Y) are
C       taken to be absolute device coordinates; if .FALSE., they are
C       taken to be world coordinates.
C N (input, integer): the number of points to be plotted.
C X, Y (input, real arrays, dimensioned at least N): the (X,Y)
C       coordinates of the points to be plotted.
C--
C (19-Mar-1983)
C 20-Jun-1985 - revise to window markers whole [TJP].
C  5-Aug-1986 - add GREXEC support [AFT].
C  1-Aug-1988 - add direct use of Hershey number [TJP].
C 15-Dec-1988 - standardize [TJP].
C 17-Dec-1990 - add polygons [PAH/TJP].
C 12-Jun-1992 - [TJP]
C 22-Sep-1992 - add support for hardware markers [TJP].
C  1-Sep-1994 - suppress driver call [TJP].
C 15-Feb-1994 - fix bug (expanding viewport!) [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      INTEGER  SYMBOL
      INTEGER  C 
      LOGICAL  ABSXY, UNUSED, VISBLE
      INTEGER  I, K, LSTYLE, LX, LY, LXLAST, LYLAST, N, SYMNUM, NV
      INTEGER  XYGRID(300)
      REAL     ANGLE, COSA, SINA, FACTOR, RATIO, X(*), Y(*)
      REAL     XCUR, YCUR, XORG, YORG
      REAL     THETA, XOFF(40), YOFF(40), XP(40), YP(40)
      REAL     XMIN, XMAX, YMIN, YMAX
      REAL     XMINX, XMAXX, YMINX, YMAXX
      REAL     RBUF(4)
      INTEGER  NBUF,LCHR
      CHARACTER*32 CHR
C
C Check that there is something to be plotted.
C
      IF (N.LE.0) RETURN
C
C Check that a device is selected.
C
      IF (GRCIDE.LT.1) THEN
          CALL GRWARN('GRMKER - no graphics device is active.')
          RETURN
      END IF
C
      XMIN = GRXMIN(GRCIDE)
      XMAX = GRXMAX(GRCIDE)
      YMIN = GRYMIN(GRCIDE)
      YMAX = GRYMAX(GRCIDE)
      XMINX = XMIN-0.01
      XMAXX = XMAX+0.01
      YMINX = YMIN-0.01
      YMAXX = YMAX+0.01
C
C Does the device driver do markers (only markers 0-31 at present)?
C
      IF (GRGCAP(GRCIDE)(10:10).EQ.'M' .AND.
     :     SYMBOL.GE.0 .AND. SYMBOL.LE.31) THEN
          IF (.NOT.GRPLTD(GRCIDE)) CALL GRBPIC
C         -- symbol number
          RBUF(1) = SYMBOL
C          -- scale factor
          RBUF(4) = GRCFAC(GRCIDE)/2.5
          NBUF = 4
          LCHR = 0
          DO 10 K=1,N
C             -- convert to device coordinates
              CALL GRTXY0(ABSXY, X(K), Y(K), XORG, YORG)
C             -- is the marker visible?
              CALL GRCLIP(XORG, YORG, XMINX, XMAXX, YMINX, YMAXX, C)
              IF (C.EQ.0) THEN
                  RBUF(2) = XORG
                  RBUF(3) = YORG
                  CALL GREXEC(GRGTYP,28,RBUF,NBUF,CHR,LCHR)
              END IF
   10     CONTINUE
          RETURN
      END IF
C
C Otherwise, draw the markers here.
C
C Save current line-style, and set style "normal".
C
      CALL GRQLS(LSTYLE)
      CALL GRSLS(1)
C
C Save current viewport, and open the viewport to include the full
C view surface.
C
      CALL GRAREA(GRCIDE, 0.0, 0.0, 0.0, 0.0)
C
C Compute scaling and orientation.
C
      ANGLE = 0.0
      FACTOR = GRCFAC(GRCIDE)/2.5
      RATIO = GRPXPI(GRCIDE)/GRPYPI(GRCIDE)
      COSA = FACTOR * COS(ANGLE)
      SINA = FACTOR * SIN(ANGLE)
C
C Convert the supplied marker number SYMBOL to a symbol number and
C obtain the digitization.
C
      IF (SYMBOL.GE.0) THEN
          IF (SYMBOL.GT.127) THEN
              SYMNUM = SYMBOL
          ELSE
              CALL GRSYMK(SYMBOL,GRCFNT(GRCIDE),SYMNUM)
          END IF
          CALL GRSYXD(SYMNUM, XYGRID, UNUSED)
C
C Positive symbols.
C
      DO 380 I=1,N
          CALL GRTXY0(ABSXY, X(I), Y(I), XORG, YORG)
          CALL GRCLIP(XORG, YORG, XMINX, XMAXX, YMINX, YMAXX, C)
          IF (C.NE.0) GOTO 380
          VISBLE = .FALSE.
          K = 4
          LXLAST = -64
          LYLAST = -64
  320       K = K+2
            LX = XYGRID(K)
            LY = XYGRID(K+1)
            IF (LY.EQ.-64) GOTO 380
            IF (LX.EQ.-64) THEN
                VISBLE = .FALSE.
            ELSE
                IF ((LX.NE.LXLAST) .OR. (LY.NE.LYLAST)) THEN
                    XCUR = XORG + (COSA*LX - SINA*LY)*RATIO
                    YCUR = YORG + (SINA*LX + COSA*LY)
                    IF (VISBLE) THEN
                        CALL GRLIN0(XCUR,YCUR)
                    ELSE
                        GRXPRE(GRCIDE) = XCUR
                        GRYPRE(GRCIDE) = YCUR
                    END IF
                END IF
                VISBLE = .TRUE.
                LXLAST = LX
                LYLAST = LY
            END IF
            GOTO 320
  380 CONTINUE
C
C Negative symbols.
C
      ELSE
C         ! negative symbol: filled polygon of radius 8
          NV = MIN(31,MAX(3,ABS(SYMBOL)))
          DO 400 I=1,NV
              THETA = 3.14159265359*(REAL(2*(I-1))/REAL(NV)+0.5) - ANGLE
              XOFF(I) = COS(THETA)*FACTOR*RATIO/GRXSCL(GRCIDE)*8.0
              YOFF(I) = SIN(THETA)*FACTOR/GRYSCL(GRCIDE)*8.0
  400     CONTINUE
          DO 420 K=1,N
              CALL GRTXY0(ABSXY, X(K), Y(K), XORG, YORG)
              CALL GRCLIP(XORG, YORG, XMINX, XMAXX, YMINX, YMAXX, C)
              IF (C.EQ.0) THEN
                  DO 410 I=1,NV
                      XP(I) = X(K)+XOFF(I)
                      YP(I) = Y(K)+YOFF(I)
  410             CONTINUE
                  CALL GRFA(NV, XP, YP)
              END IF
  420     CONTINUE
      END IF
C
C Set current pen position.
C
      GRXPRE(GRCIDE) = XORG
      GRYPRE(GRCIDE) = YORG
C
C Restore the viewport and line-style, and return.
C
      GRXMIN(GRCIDE) = XMIN
      GRXMAX(GRCIDE) = XMAX
      GRYMIN(GRCIDE) = YMIN
      GRYMAX(GRCIDE) = YMAX
      CALL GRSLS(LSTYLE)
C
      END
