C*VTDRIV -- PGPLOT Regis (VT125) driver
C+
      SUBROUTINE VTDRIV (IFUNC, RBUF, NBUF, CHR, LCHR)
      INTEGER IFUNC, NBUF, LCHR
      REAL    RBUF(*)
      CHARACTER*(*) CHR
C
C PGPLOT driver for Regis devices.
C
C Version 1.1 - 1987 Aug 17 - add cursor (TJP).
C Version 1.3 - 1988 Mar 23 - add rectangle fill.
C Version 1.4 - 1991 Nov  6 - standardization (TJP).
C Version 1.5 - 1993 May 26 - more standardization (TJP).
C Version 1.6 - 1993 Jun  4 - add SAVE statements, use GRxTER routines (AFT)
C
C Supported devices: Digital Equipment Corporation VT125, VT240, or
C VT241 terminal; other REGIS devices may also work.
C
C Device type code: /VT125.
C
C Default file name: TT:PGPLOT.VTPLOT. This usually means the
C terminal you are logged in to (logical name TT), but the plot can be
C sent to another terminal by giving the device name, eg, TTC0:/VT, or
C it can be saved in a file by specifying a file name, eg,
C CITSCR:[TJP]XPLOT/VT (in this case a disk name must be included as
C part of the file name).
C
C Default view surface dimensions: Depends on monitor.
C
C Resolution: The default view surface is 768 (horizontal) x
C 460 (vertical) pixels.  On most Regis devices, the resolution is
C degraded in the vertical direction giving only 230 distinguishable
C raster lines. (There are actually 240 raster lines, but 10 are reserved
C for a line of text.)
C
C Color capability:  Color indices 0--3 are supported. By default,
C color index 0 is black (the background color). Color indices 1--3
C are white, red, and green on color monitors, or white, dark grey, and
C light grey on monochrome monitors.  The color representation of all
C the color indices can be changed, although only a finite number of
C different colors can be obtained (see the manual for the terminal).
C
C Input capability: The graphics cursor is a blinking
C diamond-crosshair. The user positions the cursor using the arrow keys
C and PF1--PF4 keys on his keyboard  [Note: NOT the keyboard of
C the terminal on which he is plotting, if that is different.]
C The arrow keys move the cursor in the appropriate direction; the size
C of the step for each keystroke is controlled by the PF1--PF4 keys: PF1
C -> 1 pixel, PF2 -> 4 pixels, PF3 -> 16 pixels, PF4 -> 64 pixels. [The
C VT240 terminal has a built-in capability to position the cursor, but
C PGPLOT does not use this as it is not available on the VT125.] The
C user indicates that the cursor has been positioned by typing any
C character other than an arrow or PF1-PF4 key [control characters, eg,
C control-C, and other special characters should be avoided, as they
C may be intercepted by the operating system].
C
C File format: A REGIS plot file is formatted in records of 80
C characters or less, and has no carriage-control attributes. The
C records are grouped into ``buffers,'' each of which begins with
C <esc>Pp to put the terminal into graphics mode and ends with <esc>\
C to put it back into text mode.  The terminal is in graphics mode only
C while a buffer is being transmitted, so a user's program can write to
C the terminal at any time (in text mode) without worrying if it might
C be in graphics mode. Everything between the escape sequences is
C REGIS: see the VT125 or VT240 manual for an explanation.  PGPLOT
C attempts to minimize the number of characters in the REGIS commands,
C but REGIS is not a very efficient format. It does have the great
C advantage, though, that it can easily be examined with an editor.
C The file may also contain characters outside the <esc>Pp ... <esc>\
C delimiters, eg, escape sequences to erase the text screen and home
C the cursor.
C
C The following escape sequences are used:
C
C [2J Erase entire screen (text)
C [H  Move cursor to home position
C Pp  Enter REGIS graphics mode
C \   Leave REGIS graphics mode
C
C PGPLOT uses a very limited subset of the REGIS commands supported
C by the VT125 and VT240. The following list summarizes the REGIS
C commands presently used.
C
C Initialization: the following standard commands are used to initialize
C the device every time a new frame is started; most of these restore a
C VT125 or VT240 to its default state, but the screen addressing mode is
C nonstandard.
C
C ;                         resynchronize
C W(R)                      replace mode writing
C W(I3)                     color index 1
C W(F3)                     both bit planes
C W(M1)                     unit multiplier
C W(N0)                     negative off
C W(P1)                     pattern 1
C W(P(M2))                  pattern multiplier 2
C W(S0)                     shading off
C S(E)                      erase screen
C S(G1)                     select graphics plane [Rainbow REGIS]
C S(A[0,479][767,0])        screen addressing, origin at bottom left
C S(I0)                     background dark
C S(S1)                     scale 1
C S(M0(L0)(AL0))            output map section 0 (black)
C S(M1(L30)(AH120L50S100))  output map section 1 (red/dim grey)
C S(M2(L59)(AH240L50S100))  output map section 2 (green/light grey)
C S(M3(L100)(AL100))        output map section 3 (white)
C
C Drawing lines: the P and V commands are used with absolute
C coordinates, relative coordinates, and pixel vectors. The (B)
C S), (E), and (W) modifiers are not used. Coordinates
C which do not change are omitted.
C
C P[x,y]                    move to position, eg P[499,0]
C V[x,y]                    draw vector to position, eg
C                           V[][767][,479][0][,0]
C
C Line attributes: the line style and line color attributes are
C specified with W commands, eg
C
C W(P2)                     line style 2
C W(I2)                     intensity (color index) 2
C
C and S commands are used to change the output map.  The PGPLOT color
C indices 0, 1, 2, 3 correspond to output map sections 0, 3, 1, 2.
C
C Obtaining hardcopy: A hardcopy of the plot can be obtained
C using a printer attached to the VT125/VT240 terminal (see the
C instruction manual for the terminal). A plot stored in disk file
C can be displayed by issuing a TYPE command (eg, TYPE PGPLOT.VTPLOT)
C on a VT125 or VT240.
C-----------------------------------------------------------------------
      CHARACTER*(*) TYPE, DEFNAM
      PARAMETER (TYPE='VT125 (DEC VT125 and other REGIS terminals)')
      PARAMETER (DEFNAM='PGPLOT.VTPLOT')
C
      CHARACTER*(*) VTINIT
      PARAMETER (VTINIT=';W(RI3F3M1N0P1P(M2)S0)S(E)'//
     1                  'S(G1A[0,479][767,0]I0S1)'//
     2                  'S(M0(L0)(AL0))'//
     3                  'S(M3(L100)(AL100))'//
     4                  'S(M1(L30)(AH120L50S100))'//
     5                  'S(M2(L59)(AH240L50S100))')
      CHARACTER*(*) CURSOR, VTERAS, VTHOME
      PARAMETER (CURSOR='[24;1f')
      PARAMETER (VTERAS='[2J')
      PARAMETER (VTHOME='[H')
      INTEGER BUFSIZ
      PARAMETER (BUFSIZ=500)
C
      INTEGER  IER, I0, J0, I1, J1, L, LASTI, LASTJ, UNIT
      SAVE LASTI, LASTJ, UNIT
      INTEGER  CI, NPTS, L1, L2, BUFLEV
      SAVE NPTS, BUFLEV
      INTEGER  MONO, IR, IG, IB, ICH, ICX, ICY, LTMP
      INTEGER  VTCODE(0:3)
      SAVE     VTCODE
      INTEGER  GROTER
      LOGICAL  APPEND
      SAVE     APPEND
      REAL     CH, CL, CS
      CHARACTER*(BUFSIZ) BUFFER
      SAVE BUFFER
      CHARACTER*80  CTEMP
      CHARACTER*64  INSTR
      CHARACTER*20  INSTR1,INSTR2
      CHARACTER*2   PIX(0:22)
      SAVE PIX
      DATA PIX    /'V5','V4','V3',7*'  ','V6',' ','V2',7*' ','V7',
     1             'V0','V1'/
      DATA VTCODE / 0, 3, 1, 2 /
C-----------------------------------------------------------------------
C
      GOTO( 10, 20, 30, 40, 50, 60, 70, 80, 90,100,110,120,
     :     130,140,150,160,170,180,190,200,210,220,230,240), IFUNC
  900 WRITE (CTEMP,901) IFUNC
  901 FORMAT('VTDRIV:  Unimplemented function:',I10)
      CALL GRWARN(CTEMP)
      NBUF = -1
      RETURN
C
C--- IFUNC = 1, Return device name.-------------------------------------
C
   10 CHR = TYPE
      LCHR = LEN(TYPE)
      RETURN
C
C--- IFUNC = 2, Return physical min and max for plot device, and range
C               of color indices.---------------------------------------
C
   20 RBUF(1) = 0
      RBUF(2) = 767
      RBUF(3) = 0
      RBUF(4) = 479
      RBUF(5) = 0
      RBUF(6) = 3
      NBUF = 6
      RETURN
C
C--- IFUNC = 3, Return device resolution. ------------------------------
C
   30 RBUF(1) = 100.0
      RBUF(2) = 100.0
      RBUF(3) = 1
      NBUF = 3
      RETURN
C
C--- IFUNC = 4, Return misc device info. -------------------------------
C    (This device is Interactive, Cursor, No dashed lines, No area fill,
C    No thick lines, Rectangle fill)
C
   40 CHR = 'ICNNNRNNNN'
      LCHR = 10
      RETURN
C
C--- IFUNC = 5, Return default file name. ------------------------------
C
   50 CALL GRTRML(CHR, LCHR)
      RETURN
C
C--- IFUNC = 6, Return default physical size of plot. ------------------
C
   60 RBUF(1) = 0
      RBUF(2) = 767
      RBUF(3) = 0
      RBUF(4) = 459
      NBUF = 4
      RETURN
C
C--- IFUNC = 7, Return misc defaults. ----------------------------------
C
   70 RBUF(1) = 1
      NBUF = 1
      RETURN
C
C--- IFUNC = 8, Select plot. -------------------------------------------
C
   80 CONTINUE
      RETURN
C
C--- IFUNC = 9, Open workstation. --------------------------------------
C
   90 CONTINUE
      APPEND = RBUF(3).NE.0.0
      RBUF(1) = UNIT
      IER = GROTER(CHR, LCHR)
      IF (IER.LT.0) THEN
          LTMP = MIN(LEN(CTEMP), 34+LCHR)
          CTEMP = 'Unable to access graphics device: '//CHR(:LCHR)
          CALL GRWARN(CTEMP(1:LTMP))
          RBUF(2) = 0
      ELSE
          UNIT = IER
          RBUF(1) = IER
          RBUF(2) = 1
          NBUF = 2
      END IF
      BUFLEV = 0
      LASTI = -1
      LASTJ = -1
      NPTS = 0
      RETURN
C
C--- IFUNC=10, Close workstation. --------------------------------------
C
  100 CONTINUE
C     -- reposition cursor
      LTMP = 1 + LEN(CURSOR)
      CALL GRWTER(UNIT, CHAR(27)//CURSOR, LTMP)
      CALL GRCTER(UNIT)
      RETURN
C
C--- IFUNC=11, Begin picture. ------------------------------------------
C
  110 CONTINUE
C     -- erase alpha screen and home cursor
      LTMP = 2 + LEN(VTERAS) + LEN(VTHOME)
      CALL GRWTER(UNIT, CHAR(27)//VTERAS//CHAR(27)//VTHOME, LTMP)
C     -- erase and initialize graphics screen
      IF (.NOT.APPEND) CALL GRVT02(VTINIT, BUFFER, BUFLEV, UNIT)
      RETURN
C
C--- IFUNC=12, Draw line. ----------------------------------------------
C
  120 CONTINUE
      I0 = NINT(RBUF(1))
      J0 = NINT(RBUF(2))
      I1 = NINT(RBUF(3))
      J1 = NINT(RBUF(4))
      IF (I0.NE.LASTI .OR. J0.NE.LASTJ) THEN
          CALL GRFAO('P[#,#]',L,INSTR,I0,J0,0,0)
          CALL GRVT02(INSTR(1:L), BUFFER, BUFLEV, UNIT)
          CALL GRVT02('V[]', BUFFER, BUFLEV, UNIT)
      END IF
      IF (I1.EQ.I0 .AND. J1.EQ.J0) THEN
          CONTINUE
      ELSE IF (ABS(I1-I0).LE.1 .AND. ABS(J1-J0).LE.1) THEN
          L = 10*(I1-I0+1) + (J1-J0+1)
          CALL GRVT02(PIX(L), BUFFER, BUFLEV, UNIT)
      ELSE
          IF (I1.EQ.I0) THEN
              INSTR1 = 'V['
              L1 = 2
          ELSE IF (ABS(I1-I0).GE.100) THEN
              CALL GRFAO('V[#',L1,INSTR1,I1,0,0,0)
          ELSE IF (I1.GT.I0) THEN
              CALL GRFAO('V[+#',L1,INSTR1,I1-I0,0,0,0)
          ELSE
              CALL GRFAO('V[#',L1,INSTR1,I1-I0,0,0,0)
          END IF
          IF (J1.EQ.J0) THEN
              INSTR2 = ']'
              L2 = 1
          ELSE IF (ABS(J1-J0).GE.100) THEN
              CALL GRFAO(',#]',L2,INSTR2,J1,0,0,0)
          ELSE IF (J1.GT.J0) THEN
              CALL GRFAO(',+#]',L2,INSTR2,J1-J0,0,0,0)
          ELSE
              CALL GRFAO(',#]',L2,INSTR2,J1-J0,0,0,0)
          END IF
          CALL GRVT02(INSTR1(1:L1)//INSTR2(1:L2),
     1                BUFFER, BUFLEV, UNIT)
      END IF
      LASTI = I1
      LASTJ = J1
      RETURN
C
C--- IFUNC=13, Draw dot. -----------------------------------------------
C
  130 CONTINUE
      I1 = NINT(RBUF(1))
      J1 = NINT(RBUF(2))
      IF (I1.NE.LASTI .OR. J1.NE.LASTJ) THEN
          CALL GRFAO('P[#,#]V[]',L,INSTR,I1,J1,0,0)
          CALL GRVT02(INSTR(1:L), BUFFER, BUFLEV, UNIT)
      END IF
      LASTI = I1
      LASTJ = J1
      RETURN
C
C--- IFUNC=14, End picture. --------------------------------------------
C
  140 CONTINUE
C     -- flush
      CALL GRVT03(BUFFER, UNIT, BUFLEV)
C     -- home cursor
      LTMP = 1 + LEN(VTHOME)
      CALL GRWTER(UNIT, CHAR(27)//VTHOME, LTMP)
      RETURN
C
C--- IFUNC=15, Select color index. -------------------------------------
C
  150 CONTINUE
      CI = NINT(RBUF(1))
      IF (CI.GT.3 .OR. CI.LT.0) CI = 1
      CALL GRFAO('W(I#)',L,INSTR,VTCODE(CI),0,0,0)
      CALL GRVT02(INSTR(1:L), BUFFER, BUFLEV, UNIT)
      LASTI = -1
      RETURN
C
C--- IFUNC=16, Flush buffer. -------------------------------------------
C
  160 CONTINUE
C     -- flush buffer
      CALL GRVT03(BUFFER, UNIT, BUFLEV)
      RETURN
C
C--- IFUNC=17, Read cursor. --------------------------------------------
C           RBUF(1)   in/out : cursor x coordinate.
C           RBUF(2)   in/out : cursor y coordinate.
C           CHR(1:1)  output : keystroke.
C
  170 CONTINUE
C     -- flush buffer
      CALL GRVT03(BUFFER, UNIT, BUFLEV)
      ICX = NINT(RBUF(1))
      ICY = NINT(RBUF(2))
  171 ICX = MAX(0,MIN(767,ICX))
      ICY = MAX(0,MIN(459,ICY))
C     -- position graphics cursor
      WRITE (INSTR,111) CHAR(27),ICX,ICY
  111 FORMAT(A,'PpP[', I4 ,',', I4 ,']')
      LTMP = 15
      CALL GRWTER(UNIT, INSTR, LTMP)
      CALL GRGETC(ICH)
C
      IF (ICH.LT.0) THEN
          CALL GRMCUR(ICH, ICX, ICY)
          GOTO 171
      END IF
C     -- back to text mode
      CALL GRWTER(UNIT,CHAR(27)//CHAR(92),2)
      RBUF(1) = ICX
      RBUF(2) = ICY
      CHR = CHAR(ICH)
      LASTI = -1
      NBUF = 2
      LCHR = 1
      RETURN
C
C--- IFUNC=18, Erase alpha screen. -------------------------------------
C
  180 CONTINUE
C     -- flush
      CALL GRVT03(BUFFER, UNIT, BUFLEV)
C     -- erase alpha screen and home cursor
      LTMP = 2 + LEN(VTERAS) + LEN(VTHOME)
      CALL GRWTER(UNIT, CHAR(27)//VTERAS//CHAR(27)//VTHOME, LTMP)
      RETURN
C
C--- IFUNC=19, Set line style. -----------------------------------------
C    (Not implemented: should not be called.)
C
  190 GOTO 900
C
C--- IFUNC=20, Polygon fill. -------------------------------------------
C    (Not implemented: should not be called.)
C
  200 GOTO 900
C
C--- IFUNC=21, Set color representation. -------------------------------
C
  210 CONTINUE
      CI = RBUF(1)
      MONO = NINT(30.*RBUF(2) + 59.*RBUF(3) + 11.*RBUF(4))
C     -- convertRGB to hue, lightness, saturation
      CALL GRXHLS(RBUF(2),RBUF(3),RBUF(4),CH,CL,CS)
      IR = NINT(CH)
      IG = NINT(100.*CL)
      IB = NINT(100.*CS)
      CALL GRFAO('S(M#(L#)',L,INSTR, VTCODE(CI), MONO, 0, 0)
      CALL GRVT02(INSTR(1:L), BUFFER, BUFLEV, UNIT)
      CALL GRFAO('(AH#L#S#))',L,INSTR, IR, IG, IB, 0)
      CALL GRVT02(INSTR(1:L), BUFFER, BUFLEV, UNIT)
      RETURN
C
C--- IFUNC=22, Set line width. -----------------------------------------
C    (Not implemented: should not be called.)
C
  220 GOTO 900
C
C--- IFUNC=23, Escape. -------------------------------------------------
C
  230 CONTINUE
C     -- flush
      CALL GRVT03(BUFFER, UNIT, BUFLEV)
C     -- write string
      CALL GRWTER(UNIT, CHR, LCHR)
      LASTI = -1
      RETURN
C
C--- IFUNC=24, Rectangle fill. -----------------------------------------
C
  240 CONTINUE
      I0 = NINT(RBUF(1))
      J0 = NINT(RBUF(2))
      I1 = NINT(RBUF(3))
      J1 = NINT(RBUF(4))
C     -- move to top left and turn shading on
      CALL GRFAO('W(S1[,#])P[#,#]V[]', L, INSTR, J0, I0, J1, 0)
      CALL GRVT02(INSTR(1:L), BUFFER, BUFLEV, UNIT)
C     -- draw to top right and turn shading off
      CALL GRFAO('V[#,#]W(S0)', L, INSTR, I1, J1, 0, 0)
      CALL GRVT02(INSTR(1:L), BUFFER, BUFLEV, UNIT)
      LASTI = -1
      RETURN
C-----------------------------------------------------------------------
      END
C*GRVT02 -- PGPLOT Regis (VT125) driver, transfer data to buffer
C+
      SUBROUTINE GRVT02 (INSTR, BUFFER, HWM, UNIT)
      INTEGER   HWM, UNIT
      CHARACTER*(*) INSTR, BUFFER
C
C Arguments:
C  INSTR  (input)  : text of instruction (bytes).
C  BUFFER (in/out) : output buffer.
C  HWM    (in/out) : number of bytes used in BUFFER.
C  UNIT   (input)  : channel number for output (when buffer is full).
C
C Subroutines called:
C   GRVT03
C-----------------------------------------------------------------------
      INTEGER BUFSIZ, N
C-----------------------------------------------------------------------
      BUFSIZ = LEN(BUFFER)
      N = LEN(INSTR)
      IF (HWM+N.GE.BUFSIZ) CALL GRVT03(BUFFER, UNIT, HWM)
      BUFFER(HWM+1:HWM+N) = INSTR(1:N)
      HWM = HWM+N
C-----------------------------------------------------------------------
      END
C*GRVT03 -- PGPLOT Regis (VT125) driver, copy buffer to device
C+
      SUBROUTINE GRVT03 (BUFFER, UNIT, N)
      CHARACTER*(*) BUFFER
      INTEGER UNIT, N
C
C Arguments:
C   BUFFER (input) address of buffer to be output
C   UNIT   (input) channel number for output
C   N      (input) number of bytes to transfer
C          (output) set to zero
C-----------------------------------------------------------------------
C Note: CHAR(27) = escape, CHAR(92) = backslash.
C-----------------------------------------------------------------------
      INTEGER   LTMP
C---
      IF (N.GE.1) THEN
         LTMP = 3
         CALL GRWTER(UNIT, CHAR(27)//'Pp', LTMP)
         CALL GRWTER(UNIT, BUFFER, N)
         LTMP = 2
         CALL GRWTER(UNIT, CHAR(27)//CHAR(92), LTMP)
      END IF
      N = 0
C-----------------------------------------------------------------------
      END
