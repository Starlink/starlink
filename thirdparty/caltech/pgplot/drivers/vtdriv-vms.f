* Date: Tue, 27 Aug 1996 13:28:34 -0400
* From: Jerry Johnson 123 ROB/MRL/PSU <JOHNSON@vax1.mrl.psu.edu>

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
C Version 1.2 - 1988 Jan 11 - use SMG$ routines for cursor control.
C Version 1.3 - 1988 Mar 23 - add rectangle fill.
C Version 1.4 - 1988 Oct 27 - correct cursor bug.
C Version 1.5 - 1992 Sep 04 - whq - add support for 16 color DEC VT340
C
C Supported devices: Digital Equipment Corporation VT125, VT240, or
C VT241 terminal; other REGIS devices may also work. (VT330, VT340 - whq)
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
c	whq - Color indices 0--15 are supported if the device is a VT340.
C
C Input capability: The graphics cursor is a blinking
C diamond-crosshair. The user positions the cursor using the arrow keys
C and PF1--PF4 keys on his keyboard (SYS$COMMAND) [Note: NOT the key-
C board of the terminal on which he is plotting, if that is different.]
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
C W(F15)                    all bit planes for VT340 (whq)
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
c
c		whq - output map sections for VT340 - colors per PGPLOT manual
c S(M0(L0)(AL0))            output map section 0 (black)	! whq
c S(M3(L100)(AL100))        output map section 3 (white)	! whq
c S(M1(L27)(AH120L46S72))   output map section 1 (red)		! whq
c S(M2(L52)(AH240L50S60))   output map section 2 (green)	! whq
c S(M4(L6)(AH0L50S60))      output map section 4 (blue)		! whq
c S(M5(L86)(AH300L50S60))   output map section 5 (cyan)		! whq
c S(M6(L19)(AH60L50S60))    output map section 6 (magenta)	! whq
c S(M7(L59)(AH180L50S60))   output map section 7 (yellow)	! whq
c S(M8(L46)(AH150L50S60))   output map section 8 (red+yellow)	! whq
c S(M9(L72)(AH210L50S60))   output map section 9 (green+yellow)	! whq
c S(M10(L79)(AH270L50S60))  output map section 10 (green+cyan)	! whq
c S(M11(L92)(AH330L50S60))  output map section 11 (blue+cyan)	! whq
c S(M12(L12)(AH30L50S60))   output map section 12 (blue+magenta) ! whq
c S(M13(L39)(AH90L50S60))   output map section 13 (red+magenta)	! whq
c S(M14(L33)(AL33))         output map section 14 (dark gray)	! whq
c S(M15(L66)(AL66))         output map section 15 (light gray)	! whq
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
      PARAMETER (TYPE='VT125')
      PARAMETER (DEFNAM='PGPLOT.VTPLOT')
C
      CHARACTER*(*) VTINIT
      PARAMETER (VTINIT=';W(RI3F3M1N0P1P(M2)S0)S(E)'//
     1                  'S(G1A[0,479][767,0]I0S1)'//
     2                  'S(M0(L0)(AL0))'//
     3                  'S(M3(L100)(AL100))'//
     4                  'S(M1(L30)(AH120L50S100))'//
     5                  'S(M2(L59)(AH240L50S100))')
      character*(*) vt340init					! whq
      parameter (vt340init=';W(RI3F15M1N0P1P(M2)S0)S(E)'//	! whq
     +                  'S(G1A[0,479][767,0]I0S1)'//		! whq
     +                  'S(M0(L0)(AL0))'//			! whq
     +                  'S(M3(L100)(AL100))'//			! whq
     +                  'S(M1(L27)(AH120L46S72))'//		! whq
     +                  'S(M2(L52)(AH240L50S60))'//		! whq
     +                  'S(M4(L6)(AH0L50S60))'//		! whq
     +                  'S(M5(L86)(AH300L50S60))'//		! whq
     +                  'S(M6(L19)(AH60L50S60))'//		! whq
     +                  'S(M7(L59)(AH180L50S60))'//		! whq
     +                  'S(M8(L46)(AH150L50S60))'//		! whq
     +                  'S(M9(L72)(AH210L50S60))'//		! whq
     +                  'S(M10(L79)(AH270L50S60))'//		! whq
     +                  'S(M11(L92)(AH330L50S60))'//		! whq
     +                  'S(M12(L12)(AH30L50S60))'//		! whq
     +                  'S(M13(L39)(AH90L50S60))'//		! whq
     +                  'S(M14(L33)(AL33))'//			! whq
     +                  'S(M15(L66)(AL66))')			! whq
      CHARACTER*(*) CURSOR, VTERAS
      PARAMETER (CURSOR=CHAR(27)//'[24;1f')
      PARAMETER (VTERAS=CHAR(27)//'[2J'//CHAR(27)//'[H')
      CHARACTER*10 MSG
      INTEGER  IER, I0, J0, I1, J1, L, LASTI, LASTJ, UNIT
      INTEGER  CI, LW, NPTS, L1, L2, BUFFER, BUFSIZ, BUFLEV
      INTEGER  MONO, IR, IG, IB, IX, IY, ICH
      REAl     CH, CL, CS
      PARAMETER (BUFSIZ=1024)
      INTEGER  LIB$GET_VM, LIB$FREE_VM
      INTEGER  SMG$CREATE_VIRTUAL_KEYBOARD
      INTEGER  SMG$DELETE_VIRTUAL_KEYBOARD
      INTEGER  SMG$SET_KEYPAD_MODE
      INTEGER  KBID
      LOGICAL  START, APPEND
      CHARACTER*64  INSTR
      CHARACTER*20  INSTR1,INSTR2
      CHARACTER*2   PIX(0:22)
      DATA PIX    /'V5','V4','V3',7*'  ','V6',' ','V2',7*' ','V7',
     1             'V0','V1'/
c      INTEGER VTCODE(0:3)		! whq - old vtcode array
c      DATA VTCODE/ 0, 3, 1, 2 /	! whq - maintain 1st 4 in new array

      integer vtcode(0:15)		! whq - new vtcode array
      data vtcode /0,3,1,2,4,5,6,7,8,9,10,11,12,13,14,15/	! whq

      logical lvt340 /.false./		! whq - true if vt340 terminal
      logical grvt340			! whq - function for vt340 testing
C-----------------------------------------------------------------------
C
      GOTO( 10, 20, 30, 40, 50, 60, 70, 80, 90,100,
     &     110,120,130,140,150,160,170,180,190,200,
     &     210,220,230,240), IFUNC
  900 WRITE (MSG,'(I10)') IFUNC
      CALL GRWARN('Unimplemented function in VT device driver: '//MSG)
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
      if (lvt340) RBUF(6) = 15		! whq - max index for VT340
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
   50 CHR = 'TT:PGPLOT.VTPLOT'
      LCHR = 16
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
C     -- allocate buffer
      IER = LIB$GET_VM(BUFSIZ, BUFFER)
      IF (IER.NE.1) THEN
          CALL GRGMSG(IER)
          CALL GRWARN('Failed to allocate plot buffer.')
          RBUF(2) = IER
          RETURN
      END IF
      CALL GRGLUN(UNIT)
      RBUF(1) = UNIT
      OPEN (UNIT=UNIT, FILE=CHR(:LCHR), CARRIAGECONTROL='NONE',
     1      DEFAULTFILE=DEFNAM, DISPOSE='keep', STATUS='NEW',
     2      FORM='FORMATTED', RECORDTYPE='VARIABLE', IOSTAT=IER)
      IF (IER.NE.0) THEN
          CALL GRWARN('Cannot open output file for '//TYPE//' plot: '//
     1                CHR(:LCHR))
          RBUF(2) = 0
          CALL GRFLUN(UNIT)
          CALL LIB$FREE_VM(BUFSIZ, BUFFER)
	  return			! whq - return here if can't open
      endif				! whq - then endif
c      ELSE				! whq - so don't need else ... endif
          INQUIRE (UNIT=UNIT, NAME=CHR)
          LCHR = LEN(CHR)
   91     IF (CHR(LCHR:LCHR).EQ.' ') THEN
              LCHR = LCHR-1
              GOTO 91
          END IF
          RBUF(2) = 1
c      END IF				! whq

      lvt340 = grvt340(chr, lchr)	! whq - flag if device is a vt340

      BUFLEV = 0
      LASTI = -1
      LASTJ = - 1
      NPTS = 0
C     -- create a virtual keyboard for cursor control
      IER = SMG$CREATE_VIRTUAL_KEYBOARD(KBID, 'SYS$COMMAND')
      IER = SMG$SET_KEYPAD_MODE(KBID, 1)
      RETURN
C
C--- IFUNC=10, Close workstation. --------------------------------------
C
  100 CONTINUE
C     -- reposition cursor
      WRITE (UNIT, '(A)') CURSOR
      CLOSE (UNIT, DISPOSE='KEEP')
      CALL GRFLUN(UNIT)
      IER = LIB$FREE_VM(BUFSIZ, BUFFER)
      IF (IER.NE.1) THEN
          CALL GRWARN('Error deallocating plot buffer.')
          CALL GRGMSG(IER)
      END IF
C     -- delete virtual keyboard
      IER = SMG$DELETE_VIRTUAL_KEYBOARD(KBID)
      RETURN
C
C--- IFUNC=11, Begin picture. ------------------------------------------
C
  110 CONTINUE
C     -- erase alpha screen and home cursor
      WRITE (UNIT, '(A)') VTERAS
C     -- erase and initialize graphics screen
      if (lvt340) then							! whq
       if (.not.append) call grvt02(vt340init,%val(buffer),buflev,unit) ! whq
      else								! whq
       IF (.NOT.APPEND) CALL GRVT02(VTINIT, %val(BUFFER), BUFLEV, UNIT)
      endif								! whq
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
          CALL SYS$FAO('P[!SL,!SL]',L,INSTR,%VAL(I0),%VAL(J0))
          CALL GRVT02(INSTR(1:L), %val(BUFFER), BUFLEV, UNIT)
          CALL GRVT02('V[]', %val(BUFFER), BUFLEV, UNIT)
      END IF
      IF (I1.EQ.I0 .AND. J1.EQ.J0) THEN
          CONTINUE
      ELSE IF (ABS(I1-I0).LE.1 .AND. ABS(J1-J0).LE.1) THEN
          L = 10*(I1-I0+1) + (J1-J0+1)
          CALL GRVT02(PIX(L), %val(BUFFER), BUFLEV, UNIT)
      ELSE
          IF (I1.EQ.I0) THEN
              INSTR1 = 'V['
              L1 = 2
          ELSE IF (ABS(I1-I0).GE.100) THEN
              CALL SYS$FAO('V[!SL',L1,INSTR1,%VAL(I1))
          ELSE IF (I1.GT.I0) THEN
              CALL SYS$FAO('V[+!SL',L1,INSTR1,%VAL(I1-I0))
          ELSE
              CALL SYS$FAO('V[!SL',L1,INSTR1,%VAL(I1-I0))
          END IF
          IF (J1.EQ.J0) THEN
              INSTR2 = ']'
              L2 = 1
          ELSE IF (ABS(J1-J0).GE.100) THEN
              CALL SYS$FAO(',!SL]',L2,INSTR2,%VAL(J1))
          ELSE IF (J1.GT.J0) THEN
              CALL SYS$FAO(',+!SL]',L2,INSTR2,%VAL(J1-J0))
          ELSE
              CALL SYS$FAO(',!SL]',L2,INSTR2,%VAL(J1-J0))
          END IF
          CALL GRVT02(INSTR1(1:L1)//INSTR2(1:L2), 
     1        %val(BUFFER), BUFLEV, UNIT)
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
          CALL SYS$FAO('P[!SL,!SL]V[]',L,INSTR,%VAL(I1),%VAL(J1))
          CALL GRVT02(INSTR(1:L), %val(BUFFER), BUFLEV, UNIT)
      END IF
      LASTI = I1
      LASTJ = J1
      RETURN
C
C--- IFUNC=14, End picture. --------------------------------------------
C
  140 CONTINUE
C     -- flush
      CALL GRVT03(%val(BUFFER), UNIT, BUFLEV)
      RETURN
C
C--- IFUNC=15, Select color index. -------------------------------------
C
  150 CONTINUE
      CI = NINT(RBUF(1))
      if (lvt340) then				! whq
        if (ci.gt.15 .or. ci.lt.0) ci = 1	! whq
      else					! whq
        IF (CI.GT.3 .OR. CI.LT.0) CI = 1
      endif					! whq
      CALL SYS$FAO('W(I!SL)',L,INSTR,%VAL(VTCODE(CI)))
      CALL GRVT02(INSTR(1:L), %val(BUFFER), BUFLEV, UNIT)
      LASTI = -1
      RETURN
C
C--- IFUNC=16, Flush buffer. -------------------------------------------
C
  160 CONTINUE
C     -- flush
      CALL GRVT03(%val(BUFFER), UNIT, BUFLEV)
      RETURN
C
C--- IFUNC=17, Read cursor. --------------------------------------------
C           RBUF(1)   in/out : cursor x coordinate.
C           RBUF(2)   in/out : cursor y coordinate.
C           CHR(1:1)  output : keystroke.
C
  170 CONTINUE
C     -- flush buffer
      CALL GRVT03(%val(BUFFER), UNIT, BUFLEV)
C     -- 
      IX = NINT(RBUF(1))
      IY = NINT(RBUF(2))
      CALL GRVT04(IX, IY, ICH, IER, UNIT, KBID)
      IF (IER.EQ.1) THEN
          RBUF(1) = IX
          RBUF(2) = IY
          CHR = CHAR(ICH)
      ELSE
          CHR = CHAR(0)
      END IF
      NBUF = 2
      LCHR = 1
      LASTI = -1
      RETURN
C
C--- IFUNC=18, Erase alpha screen. -------------------------------------
C
  180 CONTINUE
C     -- flush
      CALL GRVT03(%val(BUFFER), UNIT, BUFLEV)
C     -- erase alpha screen and home cursor
      WRITE (UNIT, '(A)') VTERAS
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
      CALL GRXHLS(RBUF(2),RBUF(3),RBUF(4),CH,CL,CS)
      IR = NINT(CH)	! hue
      IG = NINT(100.*CL)	! lightness
      IB = NINT(100.*CS)	! saturation
      CALL SYS$FAO('S(M!SL(L!SL)(AH!SLL!SLS!SL))',L,INSTR,
     1	      %VAL(VTCODE(CI)),%VAL(MONO),%VAL(IR),%VAL(IG),%VAL(IB))
      CALL GRVT02(INSTR(1:L), %val(BUFFER), BUFLEV, UNIT)
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
      CALL GRVT03(%val(BUFFER), UNIT, BUFLEV)
C     -- write string
      WRITE (UNIT, '(A)') CHR(:LCHR)
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
      CALL SYS$FAO('W(S1[,!SL])P[!SL,!SL]V[]',L,INSTR,%VAL(J0),
     1             %VAL(I0),%VAL(J1))
      CALL GRVT02(INSTR(1:L), %val(BUFFER), BUFLEV, UNIT)
C     -- draw to top right and turn shading off
      CALL SYS$FAO('V[!SL,!SL]W(S0)',L,INSTR,
     1             %VAL(I1),%VAL(J1))
      CALL GRVT02(INSTR(1:L), %val(BUFFER), BUFLEV, UNIT)
      LASTI = -1
      RETURN
C-----------------------------------------------------------------------
      END

C*GRVT02 -- PGPLOT Regis (VT125) driver, transfer data to buffer
C+
      SUBROUTINE GRVT02 (INSTR, BUFFER, HWM, UNIT)
      INTEGER   HWM, UNIT
      CHARACTER*(*) INSTR
      BYTE      BUFFER(*)
C
C Arguments:
C  INSTR  (input)  : text of instruction (bytes).
C  BUFFER (input)  : output buffer.
C  HWM    (in/out) : number of bytes used in BUFFER.
C  UNIT   (input)  : channel number for output (when buffer is full).
C
C Subroutines called:
C   GRVT03
C-----------------------------------------------------------------------
      INTEGER BUFSIZ
      PARAMETER (BUFSIZ=1024)
      INTEGER  I, N
C-----------------------------------------------------------------------
      N = LEN(INSTR)
      IF (HWM+N.GE.BUFSIZ) CALL GRVT03(BUFFER, UNIT, HWM)
      DO 10 I=1,N
          HWM = HWM + 1
          BUFFER(HWM) = ICHAR(INSTR(I:I))
   10 CONTINUE
C-----------------------------------------------------------------------
      END

C*GRVT03 -- PGPLOT Regis (VT125) driver, copy buffer to device
C+
      SUBROUTINE GRVT03 (BUFFER, UNIT, N)
      BYTE BUFFER(*)
      INTEGER UNIT, N
C
C Arguments:
C   BUFFER (input) address of buffer to be output
C   UNIT   (input) channel number for output
C   N      (input) number of bytes to transfer
C          (output) set to zero
C-----------------------------------------------------------------------
      INTEGER I
      BYTE PREFIX(3), SUFFIX(2)
      DATA PREFIX / 27, 'P', 'p' /
      DATA SUFFIX /27, '\' /
C-----------------------------------------------------------------------
      IF (N.GE.1) 
     1    WRITE (UNIT, '(130A1)') PREFIX, (BUFFER(I),I=1,N), SUFFIX
      N = 0
C-----------------------------------------------------------------------
      END

C*GRVT04 -- PGPLOT Regis (VT125) driver, cursor routine
C+
      SUBROUTINE GRVT04 (IX, IY, IC, IER, UNIT, KBID)
      INTEGER IX, IY, IC, IER, UNIT, KBID
C
C Arguments:
C   IX, IY (in/out) : initial/final coordinates of cursor (device 
C                     coordinates).
C   IC     (output) : character code.
C   IER    (output) : error status (1 => OK).
C   UNIT   (input)  : channel for output to device.
C   KBID   (input)  : SMG keyboard identifier for control.
C
C The cursor is moved by using the arrow keys on the
C terminal; the cursor "speed" (step size) is controlled by the
C PF1 (smallest step) to PF4 (largest step) keys. The numeric keys
C on the keypad can be used in place of the arrow keys, with the
C addition of diagonal motion:
C        ^
C      7 8 9
C    < 4   6 >
C      1 2 3
C        v
C
C The user indicates that the cursor has been positioned by
C typing any character on his keyboard (SYS$COMMAND), with the
C following exceptions: control characters (^C, ^O, ^Q, ^R, ^S,
C ^T, ^U, ^X, ^Y, DEL) are intercepted by the operating system
C and cannot be used; NUL, ESC (^[) and escape sequences (e.g., 
C arrow keys) are ignored by GRCURS.
C-----------------------------------------------------------------------
      INTEGER        IXG,IYG
      INTEGER        SMG$READ_KEYSTROKE
      INTEGER        STEP
      DATA           STEP/4/      ! initial step size
C-----------------------------------------------------------------------
   10 IXG = IX
      IYG = IY
C     -- position graphics cursor
      WRITE (UNIT,111) CHAR(27),IX,IY
  111 FORMAT(A,'PpP[', I4 ,',', I4 ,']')
      IER = SMG$READ_KEYSTROKE(KBID, IC)
      IF (IER.NE.1) RETURN
      IF (IC.EQ.274 .OR. IC.EQ.268) THEN
C         key UP or KP8
          IY = MIN(479,IY+STEP)
      ELSE IF (IC.EQ.275 .OR. IC.EQ.262) THEN
C         key DOWN or KP2
          IY = MAX(0,IY-STEP)
      ELSE IF (IC.EQ.276 .OR. IC.EQ.264) THEN
C         key LEFT or KP4
          IX = MAX(0,IX-STEP)
      ELSE IF (IC.EQ.277 .OR. IC.EQ.266) THEN
C         key RIGHT or KP6
          IX = MIN(767,IX+STEP)
      ELSE IF (IC.EQ.267) THEN
C         key KP7
          IX = MAX(0,IX-STEP)
          IY = MIN(479,IY+STEP)
      ELSE IF (IC.EQ.269) THEN
C         key KP9
          IX = MIN(767,IX+STEP)
          IY = MIN(479,IY+STEP)
      ELSE IF (IC.EQ.263) THEN
C         key KP3
          IX = MIN(767,IX+STEP)
          IY = MAX(0,IY-STEP)
      ELSE IF (IC.EQ.261) THEN
C         key KP1
          IX = MAX(0,IX-STEP)
          IY = MAX(0,IY-STEP)
      ELSE IF (IC.EQ.256) THEN
C         key PF1
          STEP = 1
      ELSE IF (IC.EQ.257) THEN
C         key PF2
          STEP = 4
      ELSE IF (IC.EQ.258) THEN
C         key PF3
          STEP = 16
      ELSE IF (IC.EQ.259) THEN
C         key PF4
          STEP = 64
      END IF
C     -- back to text mode
      WRITE (UNIT,112) CHAR(27)
  112 FORMAT(A,'\')
      IF (IC.LE.0 .OR. IC.GT.255) GOTO 10
C-----------------------------------------------------------------------
      END

C-----------------------------------------------------------------------
C*GRVT340 -- PGPLOT Regis (VT125) driver, check for DEC VT340 routine
C+
	logical function grvt340(chr, lchr)
	character*(*) chr
	integer lchr
C
C If device is a terminal, sends primary and secondary da (device attributes)
C to determine if it is a DEC VT340.  As implemented, the terminal must be
C set for 7 bit controls in order to be recognized as a VT340.
C
C Arguments:
C chr	 : (input) the name of the device (character)
C lchr	 : (input) the length of the device name (integer)
C
C Returns:
C  GRVT340 (logical) .TRUE. if device is a DEC VT340 terminal, .FALSE. if not.
C
C--
C  4-Sep-1992 [whq]
C-----------------------------------------------------------------------
	logical grchkt				! uses GRVMS grchkt
	logical testing, intime			! parse and timeout flags
	character*(*) primaryda, secondaryda	! da request strings
	character*(*) vt3xx, vt340		! da response strings
	parameter (primaryda=char(27)//'[c')	! primary da request
	parameter (secondaryda=char(27)//'[>c')	! secondary da request
	parameter (vt3xx=char(27)//'[?63')	! 5 bytes of vt3xx response
	parameter (vt340=char(27)//'[>19')	! 5 bytes of vt340 response
	integer i, ic				! counter and response char
	character image*40			! request buffer
	integer imi(1)				! request buffer 
	equivalence (image, imi)		! pass buffer as integer array

	grvt340 = .false.			! assume not vt340 initially
	if (grchkt(chr(1:lchr))) then		! if device is a terminal
c	  write(*, '(a,/)')' device is a terminal'	! debug
	  call grvtttopen			! open it for i/o

	  image = primaryda			! make primary da request
	  call grvtttout(imi, 3)
	  
	  testing = .true.			! test for vt3xx response
	  i = 1
	  image = vt3xx
	  do while (testing)
	    call grvtttin(ic, intime)			! get response
	    if (intime .and. (char(ic) .eq. image(i:i))) then
	      if (i .eq. 5) then			! got vt3xx response
	        do while (intime.and.(ic.ne.ichar('c')))
	          call grvtttin(ic, intime)		! eat until get 'c'
	        enddo
c	        write(*, '(a,/)')' got vt3xx response'		! debug

	        image = secondaryda		! make secondary da request
	        call grvtttout(imi, 4)
	        i = 1
	        image = vt340			! test for vt340 response
	        do while (testing)
	          call grvtttin(ic, intime)		! get response
	          if (intime .and. (char(ic) .eq. image(i:i))) then
	            if (i .eq. 5) then			! got vt340 response
	              do while (intime.and.(ic.ne.ichar('c')))
	                call grvtttin(ic, intime)	! eat until get 'c'
	              enddo
c	              write(*, '(a,/)')' got vt340 response'	! debug

	              grvt340 = .true.			! flag success!
	              testing = .false.

	            else
	              i = i + 1
	            endif
	          else 
	            if (i .ge. 3) then		! got a DEC escape response
	              do while (intime.and.(ic.ne.ichar('c')))
	                call grvtttin(ic,intime)	! eat until get 'c'
	              enddo
	            endif
	            testing = .false.
	          endif
	        enddo

	      else
	        i = i + 1			! matching OK, test next
	      endif
	    else 
	      if (i .ge. 3) then		! got a DEC escape response
	        do while (intime .and. (ic .ne. ichar('c')))
	          call grvtttin(ic,intime)	! eat until get 'c'
	        enddo
	      else if (intime) then		! got some kind of response
	        i = 1				! eat chars until time out or
	        do while (intime.and.(i.le.40))	! have eaten arbitrary 40
	          call grvtttin(ic,intime)
	          i = i + 1
	        enddo
	      endif
	      testing = .false.
	    endif
	  enddo

	  call grvtttclose 			! close the terminal

	endif

	return
	end

C-----------------------------------------------------------------------
C*GRVTTTOPEN -- PGPLOT Regis (VT125) driver, TT i/o routine for GRVT340
C+
	subroutine grvtttopen
C
C open TT for i/o
C
C--
C  4-Sep-1992 [whq]
C-----------------------------------------------------------------------
	include '($iodef)'
	include '($syssrvnam)'
	include '($ssdef)'

	integer ttchan, istat		! terminal channel and error status
	integer idata			! character data
	integer terminators(2) /0,0/	! i/o terminators - none
	integer*2 status(4)		! i/o status

	integer ians, irequest, inum
	logical lttintime

	istat = sys$assign('TT',ttchan,,)
	call sys$setef(%val(1))
	return

C-----------------------------------------------------------------------
C*GRVTTTIN -- PGPLOT Regis (VT125) driver, TT i/o routine for GRVT340
C+
	entry grvtttin(ians, lttintime)
C
C get character from TT - uses 8 s time out
C
C Arguments:
C  ians		: receives character
C  lttintime	: receives .true. unless time out occurs, then .false.
C--
C  4-Sep-1992 [whq]
C-----------------------------------------------------------------------
	lttintime = .true.

	istat = sys$qiow(,%val(ttchan),
     +          %val(IO$_TTYREADALL.or.IO$M_NOECHO.or.IO$M_TIMED),
     +          status,,,idata,%val(1),%val(8),terminators,,)

	if ((status(1) .eq. SS$_TIMEOUT) .or. (.not. istat))
     +    lttintime = .false.

	ians = idata
	return

C-----------------------------------------------------------------------
C*GRVTTTOUT -- PGPLOT Regis (VT125) driver, TT i/o routine for GRVT340
C+
	entry grvtttout(irequest, inum)
C
C send the character buffer irequest, of length inum, to TT
C
C Arguments:
C  irequest	: (input) the character buffer (integer array)
C  inum		: (input) number of characters in the buffer
C--
C  4-Sep-1992 [whq]
C-----------------------------------------------------------------------
	call sys$waitfr(%val(1))
	call sys$qio(%val(1),%val(ttchan),
     +               %val(IO$_WRITEVBLK.or.IO$M_NOFORMAT),
     +               ,,,irequest,%val(inum),,%val(0),,)
	call sys$waitfr(%val(1))
	return

C-----------------------------------------------------------------------
C*GRVTTTCLOSE -- PGPLOT Regis (VT125) driver, TT i/o routine for GRVT340
C+
	entry grvtttclose
C
C close the TT channel
C
C--
C  4-Sep-1992 [whq]
C-----------------------------------------------------------------------
	istat = sys$dassgn(ttchan)
	return

	end
