      SUBROUTINE GRTEXT (CENTER,ORIENT,ABSXY,X0,Y0,STRING)
*+
*
*     - - - - - - - -
*       G R T E X T      (GKS emulation of GRPCKG)
*     - - - - - - - -
*
*   Write a text string using the high-quality character set.
*   The text is NOT windowed in the current viewport, but may extend over
*   the whole view surface.  Line attributes (colour, intensity thickness)
*   apply to text, but line-style is ignored.  The current pen position
*   after a call to GRTEXT is undefined.
*
*   Given
*      CENTER     l     Justification (ignored)
*      ORIENT     r     Orientation
*      ABSXY      l     Absolute or scaled coordinates
*      X0         r     String position X
*      Y0         r     String position Y
*      STRING     c     The character string to be plotted. This
*                       may include standard escape-sequences to represent
*                       non-ASCII characters and special commands. The
*                       number of characters in STRING (i.e., LEN(STRING))
*                       should not exceed 256.
*
*   Read from COMMON
*       GRCIDE    i     Current device
*       GRCFAC    r()   Character scale factor
*       GRCFNT    i()   Font
*       GRSTYL    i()   Linestyle
*       GRXPIN    r()   Resolution (x)
*       GRYPIN    r()   Resolution (y)
*
*   Written to COMMON
*       GRXPRE    r()   Current point (x)
*       GRYPRE    r()   Current point (y)
*
*   Constants from GRECOM.INC
*       TRN       i     Transformation number for viewport
*       TRN2      i     Transformation number for full workstation
*
*   D.L.Terrett (after T.J.Pearson) Jul 1987
*+
      IMPLICIT NONE
      INCLUDE 'grecom.inc'

      INCLUDE 'PGP_ERR'


      LOGICAL ABSXY, UNUSED, VISBLE, CENTER
      INTEGER XYGRID(300)
      INTEGER   LIST(256)
      CHARACTER*(*) STRING
      REAL ANGLE, FACTOR, FNTBAS, FNTFAC
      REAL COSA, SINA, DX, DY, XORG, YORG
      INTEGER LSTYLE
      INTEGER I, IFNTLV, NLIST, LX, LY, K, LXLAST, LYLAST
      REAL XCUR, YCUR, ORIENT, RATIO, X0, Y0, RLX, RLY

*   Check that a device is selected.
      IF (GRCIDE.LT.1) THEN
         CALL ERR_REP('GRNODO', 'GRTEXT - No PGPLOT device open',
     :   GRNODO)
         RETURN
      END IF

*  Save current line-style, and set style "normal".
      CALL GRQLS(LSTYLE)
      IF (LSTYLE.NE.1) CALL GRSLS(1)

*  Turn off clipping
      CALL GRTERM
      CALL GSELNT(TRN2)

*  Compute scaling and orientation.
      ANGLE = ORIENT*(3.14159265/180.)
      FACTOR = GRCFAC(GRCIDE)/2.5
      RATIO = GRXPIN(GRCIDE)/GRYPIN(GRCIDE)
      COSA = FACTOR * COS(ANGLE)
      SINA = FACTOR * SIN(ANGLE)
      CALL GRTXY0(ABSXY,X0,Y0,XORG,YORG)
      FNTBAS = 0.0
      FNTFAC = 1.0
      IFNTLV = 0
      DX = 0.0
      DY = 0.0

*   Convert the string to a list of symbol numbers; to prevent overflow of
*   array LIST, the length of STRING is limited to 256 ASCII characters.
      CALL GRSYDS(LIST,NLIST,STRING(1:MIN(256,LEN(STRING))),
     :                                              GRCFNT(GRCIDE))

*   Plot the string of characters
      DO 380 I = 1,NLIST
          IF (LIST(I).LT.0) THEN
            IF (LIST(I).EQ.-1) THEN            ! up
                IFNTLV = IFNTLV+1
                FNTBAS = FNTBAS + 16.0*FNTFAC
                FNTFAC = 0.75**IABS(IFNTLV)
            ELSE IF (LIST(I).EQ.-2) THEN      ! down
                IFNTLV = IFNTLV-1
                FNTFAC = 0.75**IABS(IFNTLV)
                FNTBAS = FNTBAS - 16.0*FNTFAC
            ELSE IF (LIST(I).EQ.-3) THEN      ! backspace
                XORG = XORG - DX*FNTFAC
                YORG = YORG - DY*FNTFAC
            END IF
              GOTO 380
          END IF
          CALL GRSYXD(LIST(I),XYGRID,UNUSED)
          VISBLE = .FALSE.
          LX = XYGRID(5)-XYGRID(4)
          DX = COSA*LX*RATIO
          DY = SINA*LX
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
                XCUR = XORG + (COSA*RLX - SINA*RLY)*RATIO
                YCUR = YORG + (SINA*RLX + COSA*RLY)
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
  330     XORG = XORG + DX*FNTFAC
          YORG = YORG + DY*FNTFAC
  380   CONTINUE

*   Restore linestyle and clipping
      CALL GRTERM
      CALL GSELNT(TRN)
      IF (LSTYLE.NE.1) CALL GRSLS(LSTYLE)

      END
