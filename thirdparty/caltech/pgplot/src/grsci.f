C*GRSCI -- set color index
C+
      SUBROUTINE GRSCI (IC)
C
C GRPCKG: Set the color index for subsequent plotting. Calls to GRSCI
C are ignored for monochrome devices. The default color index is 1,
C usually white on a black background for video displays or black on a
C white background for printer plots. The color index is an integer in
C the range 0 to a device-dependent maximum. Color index 0 corresponds
C to the background color; lines may be "erased" by overwriting them
C with color index 0.
C
C Color indices 0-7 are predefined as follows: 0 = black (background
C color), 1 = white (default), 2 = red, 3 = green, 4 = blue, 5 = cyan
C (blue + green), 6 = magenta (red + blue), 7 = yellow (red + green).
C The assignment of colors to color indices can be changed with
C subroutine GRSCR (set color representation).
C
C Argument:
C
C IC (integer, input): the color index to be used for subsequent
C       plotting on the current device (in range 0-255). If the
C       index exceeds the device-dependent maximum, the result is
C       device-dependent.
C--
C 11-Apr-1983 - [TJP].
C  3-Jun-1984 - add GMFILE device [TJP].
C 13-Jun-1984 - add code for TK4100 devices [TJP].
C  2-Jul-1984 - add code for RETRO and VT125 (REGIS) devices [TJP].
C  2-Oct-1984 - change REGIS to improve VT240 behavior [TJP].
C 22-Dec-1984 - add PRTX, TRILOG, VERS and VV devices [TJP].
C 29-Jan-1985 - add HP2648 device [KS/TJP].
C  5-Aug-1986 - add GREXEC support [AFT].
C 21-Feb-1987 - delays setting color if picture not open [AFT].
C 11-Jun-1987 - remove built-in devices [TJP].
C 31-May-1989 - add check for valid color index [TJP].
C  1-Sep-1994 - use common data [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      INTEGER  IC, COLOR, IC1, IC2, NBUF,LCHR
      REAL     RBUF(6)
      CHARACTER*1 CHR
C
C Error if no workstation is open.
C
      IF (GRCIDE.LT.1) THEN
          CALL GRWARN('GRSCI - no graphics device is active.')
          RETURN
      END IF
C
C Use color index 1 if out of range.
C
      IC1 = GRMNCI(GRCIDE)
      IC2 = GRMXCI(GRCIDE)
      COLOR = IC
      IF (COLOR.LT.IC1 .OR. COLOR.GT.IC2) COLOR = 1
C
C If no change to color index is requested, take no action.
C
      IF (COLOR.EQ.GRCCOL(GRCIDE)) RETURN
C
C If the workstation is in "picture open" state, send command to
C driver.
C
      IF (GRPLTD(GRCIDE)) THEN
          RBUF(1) = COLOR
          CALL GREXEC(GRGTYP,15,RBUF,NBUF,CHR,LCHR)
      END IF
C
C Set the current color index.
C
      GRCCOL(GRCIDE)=COLOR
C
      END
