C*GRREC0 -- fill a rectangle (device coordinates)
C+
      SUBROUTINE GRREC0 (X0,Y0,X1,Y1)
      REAL X0, Y0, X1, Y1
C
C GRPCKG: Fill a rectangle with solid color.  The rectangle
C is defined by the (x,y) device coordinates of its lower left and
C upper right corners; the edges are parallel to the coordinate axes.
C X0 is guaranteed to be <= X1 and Y0 <= Y1. The rectangle possible
C extends beyond the clipping boundaries
C
C Arguments:
C
C X0, Y0 (input, real): device coordinates of one corner of the 
C       rectangle.
C X1, Y1 (input, real): device coordinates of the opposite corner of 
C       the rectangle.
C--
C 23-Mar-1988 - [TJP].
C 18-Jan-1991 - Code moved from GRRECT to GRREC0 so that it can also be
C               used by GRPXRE.
C  1-Sep-1994 - suppress driver call [TJP].
C  4-Dec-1995 - avoid use of real variable as do-loop index [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      REAL    RBUF(6)
      INTEGER NBUF,LCHR
      CHARACTER*32 CHR
      REAL    XMIN, YMIN, XMAX, YMAX, Y, DY
      INTEGER LS, LW, I, NLINES
C
C Clip
C
      XMIN = X0
      XMAX = X1
      YMIN = Y0
      YMAX = Y1
      IF (XMIN .LT. GRXMIN(GRCIDE)) XMIN = GRXMIN(GRCIDE)
      IF (XMAX .GT. GRXMAX(GRCIDE)) XMAX = GRXMAX(GRCIDE)
      IF (YMIN .LT. GRYMIN(GRCIDE)) YMIN = GRYMIN(GRCIDE)
      IF (YMAX .GT. GRYMAX(GRCIDE)) YMAX = GRYMAX(GRCIDE)
      IF (XMIN .GT. XMAX) RETURN
      IF (YMIN .GT. YMAX) RETURN
C
C Use hardware rectangle fill if available.
C
      IF (GRGCAP(GRCIDE)(6:6).EQ.'R') THEN
          IF (.NOT.GRPLTD(GRCIDE)) CALL GRBPIC
          RBUF(1) = XMIN
          RBUF(2) = YMIN
          RBUF(3) = XMAX
          RBUF(4) = YMAX
          CALL GREXEC(GRGTYP,24,RBUF,NBUF,CHR,LCHR)
          RETURN
C
C Else use hardware polygon fill if available.
C
      ELSE IF (GRGCAP(GRCIDE)(4:4).EQ.'A') THEN
          IF (.NOT.GRPLTD(GRCIDE)) CALL GRBPIC
          RBUF(1) = 4
          CALL GREXEC(GRGTYP,20,RBUF,NBUF,CHR,LCHR)
          RBUF(1) = XMIN
          RBUF(2) = YMIN
          CALL GREXEC(GRGTYP,20,RBUF,NBUF,CHR,LCHR)
          RBUF(1) = XMAX
          RBUF(2) = YMIN
          CALL GREXEC(GRGTYP,20,RBUF,NBUF,CHR,LCHR)
          RBUF(1) = XMAX
          RBUF(2) = YMAX
          CALL GREXEC(GRGTYP,20,RBUF,NBUF,CHR,LCHR)
          RBUF(1) = XMIN
          RBUF(2) = YMAX
          CALL GREXEC(GRGTYP,20,RBUF,NBUF,CHR,LCHR)
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
      CALL GREXEC(GRGTYP, 3,RBUF,NBUF,CHR,LCHR)
      DY = RBUF(3)
C
C Draw horizontal raster lines.
C
      NLINES = ABS((YMAX-YMIN)/DY)
      Y = YMIN - DY/2.0
      DO 40 I=1,NLINES
         Y = Y + DY
         GRXPRE(GRCIDE) = XMIN
         GRYPRE(GRCIDE) = Y
         CALL GRLIN0(XMAX,Y)
   40 CONTINUE
C
C Restore attributes.
C
      CALL GRSLS(LS)
      CALL GRSLW(LW)
      END
