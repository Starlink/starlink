C*GRQTXT -- get text bounding box
C+
      SUBROUTINE GRQTXT (ORIENT,X0,Y0,STRING, XBOX, YBOX)
C
C GRPCKG: get the bounding box of a string drawn by GRTEXT.
C--
C 12-Sep-1993 - [TJP].
C  8-Nov-1994 - return something even if string is blank [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      LOGICAL UNUSED, VISBLE, PLOT
      INTEGER XYGRID(300)
      INTEGER LIST(256)
      CHARACTER*(*) STRING
      REAL XBOX(4), YBOX(4)
      REAL ANGLE, FACTOR, FNTBAS, FNTFAC, COSA, SINA, DX, DY, XORG, YORG
      REAL ORIENT, RATIO, X0, Y0, RLX, RLY
      REAL XG, YG, XGMIN, XGMAX, YGMIN, YGMAX
      INTEGER I, IFNTLV,NLIST,LX,LY, K, LXLAST,LYLAST
      INTRINSIC ABS, COS, LEN, MAX, MIN, SIN
C
C Default return values.
C
      DO 10 I=1,4
         XBOX(I) = X0
         YBOX(I) = Y0
 10   CONTINUE
C
C Check that there is something to be plotted.
C
      IF (LEN(STRING).LE.0) RETURN
C
C Check that a device is selected.
C
      IF (GRCIDE.LT.1) THEN
          CALL GRWARN('GRQTXT - no graphics device is active.')
          RETURN
      END IF
C
      XORG = GRXPRE(GRCIDE)
      YORG = GRYPRE(GRCIDE)
C
C Compute scaling and orientation.
C
      ANGLE = ORIENT*(3.14159265359/180.)
      FACTOR = GRCFAC(GRCIDE)/2.5
      RATIO = GRPXPI(GRCIDE)/GRPYPI(GRCIDE)
      COSA = FACTOR * COS(ANGLE)
      SINA = FACTOR * SIN(ANGLE)
      XORG = X0
      YORG = Y0
C
C Convert the string to a list of symbol numbers; to prevent overflow
C of array LIST, the length of STRING is limited to 256 characters.
C
      CALL GRSYDS(LIST,NLIST,STRING(1:MIN(256,LEN(STRING))),
     1             GRCFNT(GRCIDE))
C
C Run through the string of characters, getting bounding box
C in character coordinates. (XG, YG) is the starting point
C of the current character. The x/y limits of the bbox are
C XGMIN...XGMAX, YGMIN...YGMAX.
C
      FNTBAS = 0.0
      FNTFAC = 1.0
      IFNTLV = 0
      DX = 0.0
      DY = 0.0
      XG = 0.0
      YG = 0.0
      XGMIN = 1E30
      XGMAX = -1E30
      YGMIN = 1E30
      YGMAX = -1E30
      PLOT  = .FALSE.
      DO 380 I=1,NLIST
          IF (LIST(I).LT.0) THEN
              IF (LIST(I).EQ.-1) THEN
C                 ! up
                  IFNTLV = IFNTLV+1
                  FNTBAS = FNTBAS + 16.0*FNTFAC
                  FNTFAC = 0.75**ABS(IFNTLV)
              ELSE IF (LIST(I).EQ.-2) THEN
C                 ! down
                  IFNTLV = IFNTLV-1
                  FNTFAC = 0.75**ABS(IFNTLV)
                  FNTBAS = FNTBAS - 16.0*FNTFAC
              ELSE IF (LIST(I).EQ.-3) THEN
C                 ! backspace
                  XG = XG - DX*FNTFAC
              END IF
              GOTO 380
          END IF
          CALL GRSYXD(LIST(I),XYGRID,UNUSED)
          VISBLE = .FALSE.
          DX = XYGRID(5)-XYGRID(4)
          K = 4
          LXLAST = -64
          LYLAST = -64
  320     K = K+2
          LX = XYGRID(K)
          LY = XYGRID(K+1)
          IF (LY.EQ.-64) GOTO 330
          IF (LX.EQ.-64) THEN
              VISBLE = .FALSE.
          ELSE
              RLX = (LX - XYGRID(4))*FNTFAC
              RLY = (LY - XYGRID(2))*FNTFAC + FNTBAS
              IF ((LX.NE.LXLAST) .OR. (LY.NE.LYLAST)) THEN
                  XGMIN = MIN(XGMIN,XG+RLX)
                  XGMAX = MAX(XGMAX,XG+RLX)
                  YGMIN = MIN(YGMIN,RLY)
                  YGMAX = MAX(YGMAX,RLY)
                  PLOT = .TRUE.
              END IF
              VISBLE = .TRUE.
              LXLAST = LX
              LYLAST = LY
          END IF
          GOTO 320
  330     XG = XG + DX*FNTFAC
  380 CONTINUE
C
C Check whether anything was plotted.
C
      IF (.NOT.PLOT) RETURN
C
C Expand the box a bit to allow for line-width.
C
      XGMIN = XGMIN - 5.0
      XGMAX = XGMAX + 5.0
      YGMIN = YGMIN - 4.0
      YGMAX = YGMAX + 4.0
C
C Convert bounding box to device coordinates.
C
C     WRITE (*,*) XGMIN, XGMAX, YGMIN, YGMAX
      XBOX(1) = XORG + (COSA*XGMIN - SINA*YGMIN)*RATIO
      YBOX(1) = YORG + (SINA*XGMIN + COSA*YGMIN)
      XBOX(2) = XORG + (COSA*XGMIN - SINA*YGMAX)*RATIO
      YBOX(2) = YORG + (SINA*XGMIN + COSA*YGMAX)
      XBOX(3) = XORG + (COSA*XGMAX - SINA*YGMAX)*RATIO
      YBOX(3) = YORG + (SINA*XGMAX + COSA*YGMAX)
      XBOX(4) = XORG + (COSA*XGMAX - SINA*YGMIN)*RATIO
      YBOX(4) = YORG + (SINA*XGMAX + COSA*YGMIN)
C
      END
