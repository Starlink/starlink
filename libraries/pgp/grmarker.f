      SUBROUTINE GRMKER (SYMBOL,ABSXY,N,X,Y)
*+
*   - - - - - - - -
*     G R M K E R     (GKS emulation of GRPCKG)
*   - - - - - - - -
*
*   Draw a graph marker at a set of points in the current
*   window. Line attributes (color, intensity, and  thickness)
*   apply to markers, but line-style is ignored. After the call to
*   GRMKER, the current pen position will be the center of the last
*   marker plotted.
*
*   Given
*      SYMBOL    i    The marker number to be drawn. Numbers 0-31 are special
*                     marker symbols; numbers 32-127 are the corresponding
*                     ASCII characters (in the current font). Other values
*                     (e.g., negative) produce a single dot.
*      ABSXY     l    If .TRUE., the input corrdinates (X,Y) are taken to be
*                     absolute device coordinates; if .FALSE., they are
*                     taken to be world coordinates.
*      N         i    The number of points to be plotted.
*      X         r()  The x coordinates of the points to be plotted.
*      Y         r()  The y coordinates of the points to be plotted.
*
*   Read from COMMON
*      GRCIDE    i    Current device id
*      GRSTYL    i()  Line style
*      GRXPIN    r()  Workstation resolution in x
*      GRYPIN    r()  Workstation resolution in y
*
*   Written to COMMON
*      GRXPRE    r()  Current x point
*      GRYPRE    r()  Current y point
*
*   D.L.Terrett  Starlink  Aug 1987  (After T.J.Pearson)
*+
      IMPLICIT NONE
      INCLUDE 'grecom.inc'

      INCLUDE 'PGP_ERR'


      INTEGER SYMBOL, N
      LOGICAL ABSXY
      REAL X(N), Y(N)

      LOGICAL  UNUSED, VISBLE
      INTEGER  I, K, LSTYLE, LX, LY, LXLAST, LYLAST, SYMNUM, NV
      INTEGER XYGRID(300)
      REAL ANGLE, COSA, SINA, FACTOR, RATIO, THETA
      REAL XCUR, YCUR, XORG, YORG, XOFF(40), YOFF(40), XP(40), YP(40)
*   Check that there is something to be plotted.
      IF (N.LE.0) GOTO 9999

*   Check that a device is selected.
      IF (GRCIDE.LE.0) THEN
         CALL ERR_REP('GRNODO', 'GRMKER - No PGPLOT device open',
     :   GRNODO)
         GOTO 9999
      END IF

*   Clear line buffer
      CALL GRFLU0

*   Set linestyle solid
      LSTYLE = GRSTYL(GRCIDE)
      IF (LSTYLE.NE.1) CALL GRSLS(1)

*  Compute scaling and orientation.
      ANGLE = 0.0
      FACTOR = GRCFAC(GRCIDE)/2.5
      RATIO = GRXPIN(GRCIDE)/GRYPIN(GRCIDE)
      COSA = FACTOR * COS(ANGLE)
      SINA = FACTOR * SIN(ANGLE)

*   Convert the supplied marker number SYMBOL to a symbol number and
*   obtain the digitization.
      IF (SYMBOL.GE.0) THEN
         IF (SYMBOL.GT.127) THEN
            SYMNUM = SYMBOL
         ELSE
            CALL GRSYMK(SYMBOL,GRCFNT(GRCIDE),SYMNUM)
         ENDIF
         CALL GRSYXD(SYMNUM, XYGRID, UNUSED)

*   Plot the symbols.
         DO 380 I=1,N
            CALL GRTXY0(ABSXY,X(I),Y(I),XORG,YORG)

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
  380    CONTINUE

      ELSE
*         ! negative symbol: filled polygon of radius 8
          NV = MIN(31,MAX(3,ABS(SYMBOL)))
          DO 400 I=1,NV
              THETA = 3.141592653*(REAL(2*(I-1))/REAL(NV) + 0.5) - ANGLE
              XOFF(I) = COS(THETA)*FACTOR*RATIO/GRXSCL(GRCIDE)*8.0
              YOFF(I) = SIN(THETA)*FACTOR/GRYSCL(GRCIDE)*8.0
  400     CONTINUE
          DO 420 K=1,N
              DO 410 I=1,NV
                  XP(I) = X(K)+XOFF(I)
                  YP(I) = Y(K)+YOFF(I)
  410         CONTINUE
              CALL GRFA(NV, XP, YP)
  420     CONTINUE
      END IF

*  Set current pen position.
      GRXPRE(GRCIDE) = XORG
      GRYPRE(GRCIDE) = YORG

*   Clear line buffer
      CALL GRFLU0

*  Restore linestyle
      IF (LSTYLE.NE.1) CALL GRSLS(LSTYLE)

 9999 CONTINUE
      END
