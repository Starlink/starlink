C*GRTEXT -- draw text
C+
      SUBROUTINE GRTEXT (CENTER,ORIENT,ABSXY,X0,Y0,STRING)
C
C GRPCKG: Write a text string using the high-quality character set.
C The text is NOT windowed in the current viewport, but may extend over
C the whole view surface.  Line attributes (color, intensity thickness)
C apply to text, but line-style is ignored.  The current pen position
C after a call to GRTEXT is undefined.
C
C Arguments:
C
C STRING (input, character): the character string to be plotted. This
C       may include standard escape-sequences to represent non-ASCII
C       characters and special commands. The number of characters in
C       STRING (i.e., LEN(STRING)) should not exceed 256.
C--
C (3-May-1983)
C  5-Aug-1986 - add GREXEC support [AFT].
C  6-Sep-1989 - standardize [TJP].
C 20-Apr-1995 - Verbose PS file support.  If PGPLOT_PS_VERBOSE_TEXT is
C               defined, text strings in PS files are preceded by a 
C               comment with the text of the string plotted as vectors
C               [TJP after D.S.Briggs].
C  4-Feb-1997 - grexec requires an RBUF array, not a scalar [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      LOGICAL ABSXY,UNUSED,VISBLE,CENTER
      INTEGER XYGRID(300)
      INTEGER LIST(256)
      CHARACTER*(*) STRING
      REAL ANGLE, FACTOR, FNTBAS, FNTFAC, COSA, SINA, DX, DY, XORG, YORG
      REAL XCUR, YCUR, ORIENT, RATIO, X0, Y0, RLX, RLY
      REAL XMIN, XMAX, YMIN, YMAX
      REAL RBUF(6)
      INTEGER I, IFNTLV,NLIST,LX,LY, K, LXLAST,LYLAST, LSTYLE
      INTEGER SLEN, GRTRIM
      INTRINSIC ABS, COS, LEN, MIN, SIN
      CHARACTER DEVTYP*14, STEMP*258
      LOGICAL DEVINT, VTEXT
C
C Check that there is something to be plotted.
C
      IF (LEN(STRING).LE.0) RETURN
C
C Check that a device is selected.
C
      IF (GRCIDE.LT.1) THEN
          CALL GRWARN('GRTEXT - no graphics device is active.')
          RETURN
      END IF
C
C Save current line-style, and set style "normal".
C
      CALL GRQLS(LSTYLE)
      CALL GRSLS(1)
C
C Put device dependent code here or at end
C
      VTEXT = .FALSE.
      CALL GRQTYP (DEVTYP, DEVINT)
      IF ((DEVTYP.EQ.'PS').OR.(DEVTYP.EQ.'VPS').OR.
     1    (DEVTYP.EQ.'CPS').OR.(DEVTYP.EQ.'VCPS')) THEN
         CALL GRGENV ('PS_VERBOSE_TEXT', STEMP, I)
         VTEXT = (I.GT.0)
         IF (VTEXT) THEN
            SLEN = GRTRIM(STRING)
            STEMP = '% Start "' // STRING(1:SLEN) // '"'
            CALL GREXEC (GRGTYP, 23, RBUF, 0, STEMP, SLEN+10)
         END IF
      END IF
C
C Save current viewport, and open the viewport to include the full
C view surface.
C
      XORG = GRXPRE(GRCIDE)
      YORG = GRYPRE(GRCIDE)
      XMIN = GRXMIN(GRCIDE)
      XMAX = GRXMAX(GRCIDE)
      YMIN = GRYMIN(GRCIDE)
      YMAX = GRYMAX(GRCIDE)
      CALL GRAREA(GRCIDE, 0.0, 0.0, 0.0, 0.0)
C
C Compute scaling and orientation.
C
      ANGLE = ORIENT*(3.14159265359/180.)
      FACTOR = GRCFAC(GRCIDE)/2.5
      RATIO = GRPXPI(GRCIDE)/GRPYPI(GRCIDE)
      COSA = FACTOR * COS(ANGLE)
      SINA = FACTOR * SIN(ANGLE)
      CALL GRTXY0(ABSXY, X0, Y0, XORG, YORG)
      FNTBAS = 0.0
      FNTFAC = 1.0
      IFNTLV = 0
      DX = 0.0
      DY = 0.0
C
C Convert the string to a list of symbol numbers; to prevent overflow
C of array LIST, the length of STRING is limited to 256 characters.
C
      CALL GRSYDS(LIST,NLIST,STRING(1:MIN(256,LEN(STRING))),
     1             GRCFNT(GRCIDE))
C
C Plot the string of characters
C
      DO 380 I = 1,NLIST
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
  380 CONTINUE
C
C Set pen position ready for next character.
C
      GRXPRE(GRCIDE) = XORG
      GRYPRE(GRCIDE) = YORG
C
C Another possible device dependent section
C
      IF (VTEXT) THEN
         STEMP = '% End "' // STRING(1:SLEN) // '"'
         CALL GREXEC(GRGTYP, 23, RBUF, 0, STEMP, SLEN+8)
      END IF
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
