C*TTDRIV -- PGPLOT Tektronix terminal drivers
C+
      SUBROUTINE TTDRIV (IFUNC, RBUF, NBUF, CHR, LCHR, MODE)
      INTEGER IFUNC, NBUF, LCHR, MODE
      REAL    RBUF(*)
      CHARACTER*(*) CHR
C
C PGPLOT driver for Tektronix terminals and emulators.
C
C 1993 Jan 18 - T. J. Pearson.
C 1993 Jun 24 - L. Staveley-Smith, minor alteration of 
C               flush-buffer for better /tek
C               compatibility. Also added MODE 7 for
C               Visual 603's and MODE 8 for IBM-PCs 
C               running as remote terminals using
C               Kermit version 3 (for DOS).
C 1994 Dec 19 - TJP: better XTERM support.
C 1994 Dec 29 - TJP: and Tek4100 support (MODE 9).
C 1996 Apr 18 - TJP: prevent concurrent access.
C 1998 Mar 09 - M. Zolliker: new MODE 10 for VersaTerm-PRO for Macintosh
C
C Supported device:
C 1. Tektronix 4006/4010 storage-tube terminal; can be used with
C   emulators, but the options below take advantage of features not
C   present in the basic Tektronix terminal.
C 2. GraphOn Corporation 200-series terminals. These emulate a
C   Tektronix-4010 with enhancements (selective erase, rectangle fill,
C   switch between Tek and VT100 modes).
C 3. Digital Engineering, Inc., Retrographics modified VT100
C   terminal (VT640). 
C 4. IRAF GTERM Tektronix terminal emulator, with color extensions.
C 5. Xterm window on an X-window server. Emulates a Tektronix-4014,
C   with extensions (switch between Tek and VT100 windows).
C 6. ZSTEM 240 and ZSTEM 4014 terminal emulators for the IBM PC and
C   clones. ZSTEM supports Tektronix 4014 emulation and the 4105 color
C   escape sequences. ZSTEM can be obtained from: KEA Systems Ltd.,
C   2150 West Broadway, Suite 412, Vancouver, British Columbia, Canada,
C   V6K 4L9.
C 7.Visual-603 and 630 terminals. These are VT100/220 compatible
C   terminals with Tektronix 4010/4014 emulation (Visual Technology
C   Incorporated, 1703 Middlesex Street, Lowell, Mass 01851). The 
C   Visual 630 has the capability of displaying dual text and graphics.
C   This feature is not used in this driver. Graphics mode is entered 
C   automatically when the graph is drawn but only exited when PGPAGE 
C   or PGEND is called. Therefore, for multiple plots interspersed
C   with text I/O, use PGPAGE at the end of each plot. This will prompt
C   for a carriage return before switching. If this is not done,
C   intervening text will appear on the graphics screen. Graphics mode
C   can be entered and exited from the setup menu, or by SHIFT-PF1.
C   Graphics extensions include rectangle fill, selective erase and 
C   switch between Tek and VT100 modes.
C 8.IBM PC's and compatibles running MS-Kermit 3 as a terminal emulator.
C   The video board is assumed to have sufficient memory to retain the 
C   graphics image in memory when switched to text. This will be true 
C   for VGA and EGA, but some early PCs might not be able to do this. 
C   If Kermit is using full VGA resolution (ie SET TERMINAL GRAPHICS 
C   VGA), there is not usually enough memory to store the full 480 
C   vertical lines, so the bottom few lines may disappear. Tektronix 
C   enhancements include selective erase, colours, rectangle fill, and
C   switching between text and graphics mode. The cursor may be
C   operated with the mouse. Tested with Kermit version 3.1.
C 9.Tektronix 4100 series color terminals (and emulators)
C 10.Versaterm-PRO for Macintosh (Tek 4105 emulation).
C
C Device type codes:
C 1.  /TEK4010 Tektronix-4010 terminal
C 2.  /GF      GraphOn terminal
C 3.  /RETRO   Retrographics VT640 terminal
C 4.  /GTERM   GTERM terminal emulator
C 5.  /XTERM   XTERM terminal emulator
C 6.  /ZSTEM   ZSTEM terminal emulator
C 7.  /V603    Visual V603 terminal
C 8.  /KRM3    Kermit 3 on IBM-PC
C 9.  /TK4100  Tektronix 4100 series terminals
C 10. /VMAC    VersaTerm-PRO for Macintosh
C
C Default device name: the logged-in terminal
C   /dev/tty (UNIX)
C   TT: (VMS)
C
C Default view surface dimensions:
C   Depends on monitor; nominally 8in (horizontal) by 6in (vertical).
C
C Resolution:
C   A standard Tektronix terminal displays a screen of 1024 pixels
C   (horizontal) by 780 pixels (vertical), with a nominal resolution
C   of 130 pixels per inch. The actual resolution may be less.
C
C Color capability:
C   /TEK4010, /XTERM: none; only color index 1 is available; selective
C       erase is not possible. Requests to draw in color index 0 are
C       ignored.
C   /GF, /RETRO, /V603: color indices 0 (erase, black) and 1 (bright:
C       usually white, green or amber) are supported. It is not 
C       possible to change color representation.
C   /GTERM:  color indices 0 to 15 are available and default to the
C       standard PGPLOT colors. The color representation can be changed.
C   /ZSTEM: color indices 0 to 7 are available and default to the
C       indicated in the ZSTEM setup menu (which default to the standard
C       PGPLOT colors).  The color representation cannot be changed.
C   /KRM3: color indices 0 to 7 are the standard PGPLOT colors. Indices
C       8 to 14 are also available, but are BRIGHT versions of 1 to 7,
C       and thus non-standard. Color representation can't be changed.
C   /TK4100: color indices 0-15.
C   /VMAC: color indices 0 to 15 are available and default to the
C       standard PGPLOT colors. The color representation can be changed.
C       Caution: this does not work reliably, owing to bugs (?) in
C       Versaterm.
C
C Input capability:
C   Depending on the emulation, the graphics cursor may be a pointer,
C   a small cross, or a crosshair across the entire screen. The user
C   positions the cursor using thumbwheels, mouse, trackball, or the
C   arrow keys on the keyboard. The user indicates that the cursor has
C   been positioned by typing any printable ASCII character on the 
C   keyboard. Most control characters (eg, ^C) are intercepted by the
C   operating system and cannot be used. 
C
C File format: 
C   Binary byte stream. Under Unix, the output may be directed to
C   a file; under VMS, this is not possible: the output device must
C   be a terminal.
C
C Obtaining hardcopy:
C
C Environment variables:
C   None.
C--
C Implementation Notes:
C
C Standard Tektronix codes:
C   graph mode: [GS]=char(29)
C   alpha mode: [US]=char(31)
C The emulators provide various extensions to basic Tektronix
C operation, using the following codes:
C    [SOH]=char(1),  [STX]=char(2),  [ETX]=char(3),
C    [DLE]=char(16), [CAN]=char(24), [ESC]=char(27)
C
C Enter Tektronix mode (from VT100 mode):
C   graphon: automatic on receipt of [GS]
C   gterm:   [GS]
C   tek:     not available
C   retro:   automatic on receipt of [GS]
C   xterm:   [ESC][?38h
C   zstem:   [ESC][?38h
C   v603:    [GS]
C   krm3:    [ESC][?38h
C Return to VT100 mode (from Tektronix mode):
C   graphon: [CAN]
C   gterm:   [CAN]
C   tek:     not available
C   xterm:   [ESC][ETX]
C   zstem:   [CAN]
C   v603:    [CAN][ESC][?38l
C   krm3:    [ESC][?38l
C Rectangle fill:
C   graphon: draw the diagonal in special rectangle mode, 
C     entered with [ESC][STX], exit with [ESC][ETX]
C   v603: bottom corner and rectangle width
C   krm3: bottom corner and rectangle width
C   vmac: use panel boundary commands [ESC]LP and [ESC]LE
C Color index zero (erase):
C   graphon select erase:        [ESC][DLE]
C   graphon unselect erase:      [ESC][SOH]
C   retro,v603 select erase:     [ESC]/1d
C   retro,v603 unselect erase:   [ESC]/0d
C   krm3, select erase:          [ESC][0;30m
C   krm3, unselect erase:        [ESC][0;37m
C-----------------------------------------------------------------------
      INTEGER NDEVS
      PARAMETER (NDEVS=10)
      INTEGER CAN, ESC, GS, US
      PARAMETER (CAN=24, ESC=27, GS=29, US=31)
C
      CHARACTER*48 DEVICE(NDEVS)
      SAVE         DEVICE
      CHARACTER*80 TEXT
      CHARACTER*32 CTMP, CADD, CSCR(4)*4
      CHARACTER*500 CBUF
      SAVE          CBUF
      INTEGER I, J, INTEN, I0, J0, I1, J1, LADD, LTMP, ICH, IER
      INTEGER XSIZE(NDEVS), YSIZE(NDEVS), MAXCI(NDEVS), I4014(NDEVS)
      SAVE    XSIZE,        YSIZE,        MAXCI,        I4014
      INTEGER ICHAN, LASTI, LASTJ, NPAGE, ICI, LBUF, STATE
      SAVE    ICHAN, LASTI, LASTJ, NPAGE, ICI, LBUF, STATE
      INTEGER GROTER
      INTEGER IBUF(4), ITOT
      LOGICAL APPEND
      SAVE    APPEND
      REAL    XRESLN(NDEVS), YRESLN(NDEVS)
      SAVE    XRESLN,        YRESLN
      REAL    HUE,SAT,LIG
      LOGICAL SEFCOL
      SAVE    SEFCOL
C
      INTEGER IRGB(3,0:15), TKRGB(3,0:15)
C
      DATA DEVICE(1) /'TEK4010 (Tektronix 4010 terminal)'/
      DATA DEVICE(2) /'GF    (GraphOn Tek terminal emulator)'/
      DATA DEVICE(3) /'RETRO (Retrographics VT640 Tek emulator)'/
      DATA DEVICE(4) /'GTERM (Color gterm terminal emulator)'/
      DATA DEVICE(5) /'XTERM (XTERM Tek terminal emulator)'/
      DATA DEVICE(6) /'ZSTEM (ZSTEM Tek terminal emulator)'/
      DATA DEVICE(7) /'V603  (Visual 603 terminal)'/
      DATA DEVICE(8) /'KRM3  (Kermit 3 IBM-PC terminal emulator)'/
      DATA DEVICE(9) /'TK4100 (Tektronix 4100 terminals)'/
      DATA DEVICE(10) /'VMAC  (VersaTerm-PRO for Mac, Tek 4105)'/
C                 TEK  GF   RET  GTER XTER ZSTE V603 KRM3 TK41 VMAC
      DATA XSIZE /1023,1023,1023,1023,1023,1023,1023,1023,1023,1023/
      DATA YSIZE / 779, 779, 779, 779, 779, 779, 779, 779, 779, 779/
      DATA MAXCI /   1,   1,   1,  15,   1,   7,   1,  14,  15,  15/
      DATA XRESLN/130.,128.,128.,130.,128.,130.,115.,110.,100.,128./
      DATA YRESLN/130.,130.,130.,130.,130.,130.,115.,110.,100.,128./
      DATA I4014/    0,   0,   0,   0,   1,   1,   0,   0,   1,   1/
      DATA IRGB /  0,  0,  0, 255,255,255, 255,  0,  0,   0,255,  0,
     1             0,  0,255,   0,255,255, 255,  0,255, 255,255,  0,
     2           255,128,  0, 128,255,  0,   0,255,128,   0,128,255,
     3           128,  0,255, 255,  0,128, 085,085,085, 170,170,170/
      DATA TKRGB/  0,  0,  0, 100,100,100, 100,  0,  0,   0,100,  0,
     1             0,  0,100,   0,100,100, 100,  0,100, 100,100,  0,
     2           100, 50,  0,  50,100,  0,   0,100, 50,   0, 50,100,
     3            50,  0,100, 100,  0, 50,  33, 33, 33,  67, 67, 67/
      DATA STATE/0/
C-----------------------------------------------------------------------
C
      IF (MODE.LT.1 .OR. MODE.GT.NDEVS) CALL GRWARN('Error in GRTT00')
      GOTO( 10, 20, 30, 40, 50, 60, 70, 80, 90,100,
     1     110,120,130,140,150,160,170,180,900,900,
     2     210,900,900,240), IFUNC
C     -- Ignore unimplemented function
  900 RETURN
C
C--- IFUNC = 1, Return device name.-------------------------------------
C
   10 CONTINUE
      CHR  = DEVICE(MODE)
      LCHR = LEN(DEVICE(MODE))
      RETURN
C
C--- IFUNC = 2, Return physical min and max for plot device, and range
C               of color indices.---------------------------------------
C
   20 CONTINUE
      RBUF(1) = 0
      RBUF(2) = XSIZE(MODE)
      RBUF(3) = 0
      RBUF(4) = YSIZE(MODE)
      RBUF(5) = 0
      RBUF(6) = MAXCI(MODE)
      NBUF = 6
      RETURN
C
C--- IFUNC = 3, Return device resolution. ------------------------------
C
   30 CONTINUE
      RBUF(1) = XRESLN(MODE)
      RBUF(2) = YRESLN(MODE)
      RBUF(3) = 1
      NBUF = 3
      RETURN
C
C--- IFUNC = 4, Return misc device info. -------------------------------
C    (This device is Interactive, Cursor, No dashed lines, No areafill, 
C    No thick lines, No markers; some varieties have rectangle fill)
C
   40 CONTINUE
      CHR = 'ICNNNNNNNN'
      IF (MODE.EQ.2 .OR. MODE.EQ.7 .OR. MODE.EQ.8) CHR(6:6) = 'R'
      IF (MODE.EQ.10) THEN
C       -- VMAC: rect. fill and wait before closing graph window
        CHR(6:6) = 'R'
        CHR(8:8) = 'V'
      ENDIF
      LCHR = 10
      RETURN
C
C--- IFUNC = 5, Return default file name. ------------------------------
C
   50 CONTINUE
      CALL GRTRML(CHR,LCHR)
      RETURN
C
C--- IFUNC = 6, Return default physical size of plot. ------------------
C
   60 CONTINUE
      RBUF(1) = 0
      RBUF(2) = XSIZE(MODE)
      RBUF(3) = 0
      RBUF(4) = YSIZE(MODE)
      NBUF = 4
      RETURN
C
C--- IFUNC = 7, Return misc defaults. ----------------------------------
C
   70 CONTINUE
      RBUF(1) = 2.0
      NBUF = 1
      RETURN
C
C--- IFUNC = 8, Select plot. -------------------------------------------
C
   80 CONTINUE
C     -- do nothing
      RETURN
C
C--- IFUNC = 9, Open workstation. --------------------------------------
C
   90 CONTINUE
C     -- check for concurrent access
      IF (STATE.EQ.1) THEN
         CALL GRWARN('a PGPLOT Tektronix device is already open')
         RBUF(1) = 0
         RBUF(2) = 0
         RETURN
      END IF
      APPEND = RBUF(3) .NE. 0.0
      RBUF(1) = 0.0
      NBUF = 2
      ICHAN = GROTER(CHR, LCHR)
      IF (ICHAN .LT. 0) THEN
          TEXT = 'Cannot open output device for plot type '//
     :           DEVICE(MODE)
          CALL GRWARN(TEXT)
          RBUF(2) = 0.0
          RETURN
      ELSE
          STATE = 1
          RBUF(2) = 1.0
      END IF
      LASTI = -1
      LASTJ = -1
      ICI = 1
      NPAGE = 0
      LBUF = 0
      IF (.NOT.APPEND) THEN
         IF ( MODE.EQ.4 ) THEN
C           -- load gterm default color table.
            DO 91 I=0,15
               CTMP(1:6) = CHAR(GS)//CHAR(ESC)//'TG14'
               LTMP = 6
               CALL GRTT05(I,         CADD, LADD)
               CTMP(LTMP+1:LTMP+LADD) = CADD(:LADD)
               LTMP = LTMP + LADD
C              -- red
               CALL GRTT05(IRGB(1,I), CADD, LADD)
               CTMP(LTMP+1:LTMP+LADD) = CADD(:LADD)
               LTMP = LTMP + LADD
C              -- green
               CALL GRTT05(IRGB(2,I), CADD, LADD)
               CTMP(LTMP+1:LTMP+LADD) = CADD(:LADD)
               LTMP = LTMP + LADD
C              -- blue
               CALL GRTT05(IRGB(3,I), CADD, LADD)
               CTMP(LTMP+1:LTMP+LADD) = CADD(:LADD)
               LTMP = LTMP + LADD
               CTMP(LTMP+1:LTMP+1) = CHAR(US)
               LTMP = LTMP + 1
               CALL GRTT02(ICHAN, MODE, CTMP, LTMP, CBUF, LBUF)
 91         CONTINUE
            CALL GRTT02(ICHAN, MODE, CHAR(CAN), 1, CBUF, LBUF)
         ELSE IF (MODE.EQ.10) THEN
C           -- VMAC: put into Tek 4105 mode
            CTMP(1:5)=CHAR(ESC)//'%!1'//CHAR(GS)
            CALL GRTT02(ICHAN, MODE, CTMP, 5, CBUF, LBUF)
            SEFCOL = .TRUE.
C           -- set default color representation
            DO 92,I=0,15
              CALL GRXHLS(IRGB(1,I)/255.,IRGB(2,I)/255.,IRGB(3,I)/255.
     :              ,HUE,LIG,SAT)
              CALL GRTT06(I, NINT(HUE), NINT(LIG*100), NINT(SAT*100)
     :              , CTMP, LTMP)
              CALL GRTT02(ICHAN, MODE, CTMP, LTMP, CBUF, LBUF)
92          CONTINUE
         END IF
      END IF
      RETURN
C
C--- IFUNC=10, Close workstation. --------------------------------------
C
  100 CONTINUE
      IF ( MODE.EQ.6 ) THEN
C         -- For zstem switch back to alpha mode at the last possible
C            moment.
          LTMP = 1
          CALL GRWTER(ICHAN, CHAR(CAN), LTMP)
      ELSE IF ( MODE.EQ.7 ) THEN
C         -- For v603 switch back to alpha mode at the last possible
C            moment.
          CTMP(1:7) = CHAR(CAN)//CHAR(ESC)//CHAR(91)//CHAR(63)//
     :                CHAR(51)//CHAR(56)//CHAR(108)
          LTMP=7
          CALL GRWTER(ICHAN, CTMP, LTMP)
      ELSE IF (MODE.EQ.10) THEN
C         -- VMAC: put into VT100 Mode without window resize
          CTMP(1:5)=CHAR(GS)//CHAR(ESC)//'%!7'
          LTMP=5
          CALL GRWTER(ICHAN, CTMP, LTMP)
      END IF
      CALL GRCTER(ICHAN)
      STATE = 0
      RETURN
C
C--- IFUNC=11, Begin picture. ------------------------------------------
C
  110 CONTINUE
      NPAGE = NPAGE+1
      LASTI = -1
      IF (.NOT.APPEND) THEN
         IF (MODE.EQ.5 .OR. MODE.EQ.6 .OR. MODE. EQ.8) THEN
C           -- xterm, zstem, krm3: select Tek mode, erase screen
            CTMP(1:1) = CHAR(ESC)
            CTMP(2:2) = CHAR(12)
            CALL GRTT02(ICHAN, MODE, CTMP, 2, CBUF, LBUF)
         ELSE IF (MODE.EQ.7) THEN
C           -- V603: select Tek mode
            CTMP(1:1) = CHAR(GS)
            CTMP(2:2) = CHAR(ESC)
            CTMP(3:3) = CHAR(12)
            CALL GRTT02(ICHAN, MODE, CTMP, 3, CBUF, LBUF)
         ELSE
C           -- erase graphics screen
            CTMP(1:1) = CHAR(GS)
            CTMP(2:2) = CHAR(ESC)
            CTMP(3:3) = CHAR(12)
            CTMP(4:4) = CHAR(CAN)
            CALL GRTT02(ICHAN, MODE, CTMP, 4, CBUF, LBUF)
         END IF
      ELSE IF (MODE.EQ.8) THEN
C        -- krm3: enter graph mode without deleting screen
         CTMP(1:1) = CHAR(ESC)
         CTMP(2:2) = CHAR(91)
         CTMP(3:3) = CHAR(63)
         CTMP(4:4) = CHAR(51)
         CTMP(5:5) = CHAR(56)
         CTMP(6:6) = CHAR(104)
         CALL GRTT02(ICHAN, MODE, CTMP, 6, CBUF, LBUF)
      ELSE IF (MODE.EQ.9) THEN
C        -- TK4100: put device in graphics mode, erase screen
         CTMP(1:1) = CHAR(ESC)
         CTMP(2:4) = '%!0'
         CTMP(5:5) = CHAR(ESC)
         CTMP(6:6) = CHAR(12)
         CALL GRTT02(ICHAN, MODE, CTMP, 6, CBUF, LBUF)
         CTMP(1:1) = CHAR(ESC)
         CTMP(2:6) = 'RU1;4'
         CALL GRTT02(ICHAN, MODE, CTMP, 6, CBUF, LBUF)
C        -- set default color representation
         DO 111 I=0,15
            CTMP(1:5) = CHAR(ESC)//'TG14'
            LTMP = 5
            CALL GRTT05(I,         CADD, LADD)
            CTMP(LTMP+1:LTMP+LADD) = CADD(:LADD)
            LTMP = LTMP + LADD
C           -- red
            CALL GRTT05(TKRGB(1,I), CADD, LADD)
            CTMP(LTMP+1:LTMP+LADD) = CADD(:LADD)
            LTMP = LTMP + LADD
C           -- green
            CALL GRTT05(TKRGB(2,I), CADD, LADD)
            CTMP(LTMP+1:LTMP+LADD) = CADD(:LADD)
            LTMP = LTMP + LADD
C           -- blue
            CALL GRTT05(TKRGB(3,I), CADD, LADD)
            CTMP(LTMP+1:LTMP+LADD) = CADD(:LADD)
            LTMP = LTMP + LADD
            CALL GRTT02(ICHAN, MODE, CTMP, LTMP, CBUF, LBUF)
 111     CONTINUE
C         -- set color index 1
         CTMP(1:1) = CHAR(ESC)
         CTMP(2:4) = 'ML1'
         CALL GRTT02(ICHAN, MODE, CTMP, 4, CBUF, LBUF)
      END IF
      RETURN
C
C--- IFUNC=12, Draw line. ----------------------------------------------
C     (omitted for color 0 on devices without selective erase)
C
  120 CONTINUE
      IF (ICI.EQ.0 .AND. (MODE.EQ.1 .OR. MODE.EQ.5)) RETURN
      IF ( I4014(MODE).EQ.0 ) THEN
         I0 = NINT(RBUF(1))
         J0 = NINT(RBUF(2))
         I1 = NINT(RBUF(3))
         J1 = NINT(RBUF(4))
      ELSE
         I0 = NINT(4.*RBUF(1))
         J0 = NINT(4.*RBUF(2))
         I1 = NINT(4.*RBUF(3))
         J1 = NINT(4.*RBUF(4))
      END IF
      CALL GRTT01(ICHAN, MODE, I4014(MODE), LASTI, LASTJ,
     :      I0, J0, I1, J1, CBUF, LBUF)
      RETURN
C
C--- IFUNC=13, Draw dot. -----------------------------------------------
C     (omitted for color 0 on devices without selective erase)
C
  130 CONTINUE
      IF (ICI.EQ.0 .AND. (MODE.EQ.1 .OR. MODE.EQ.5)) RETURN
      IF ( I4014(MODE).EQ.0 ) THEN
         I0 = NINT(RBUF(1))
         J0 = NINT(RBUF(2))
      ELSE
         I0 = NINT(4.*RBUF(1))
         J0 = NINT(4.*RBUF(2))
      END IF
      CALL GRTT01(ICHAN, MODE, I4014(MODE), LASTI, LASTJ,
     :      I0, J0, I0, J0, CBUF, LBUF)
      RETURN
C
C--- IFUNC=14, End picture. --------------------------------------------
C
  140 CONTINUE
      IF (MODE.EQ.7) THEN
C        -- V603: enter alphanumerics and unset graphics
         CTMP(1:7) = CHAR(CAN)//CHAR(ESC)//CHAR(91)//CHAR(63)//
     :               CHAR(51)//CHAR(56)//CHAR(108)
         LTMP=7
         CALL GRWTER(ICHAN, CTMP, LTMP)
      ELSE IF (MODE.EQ.8) THEN
C         -- krm3: enter alphanumerics and unset graphics
         CTMP(1:6) = CHAR(ESC)//CHAR(91)//CHAR(63)//
     :               CHAR(51)//CHAR(56)//CHAR(108)
         LTMP=6
         CALL GRWTER(ICHAN, CTMP, LTMP)
      ELSE IF (MODE.EQ.9 .OR. MODE.EQ.10) THEN
C        -- TK4100, VMAC: return to text mode
         CTMP(1:1) = CHAR(ESC)
         CTMP(2:4) = '%!1'
         LTMP=4
         CALL GRWTER(ICHAN, CTMP, LTMP)
      END IF
      RETURN
C
C--- IFUNC=15, Select color index. -------------------------------------
C
  150 CONTINUE
      ICI = RBUF(1)
      IF (ICI.LT.0 .OR. ICI.GT.MAXCI(MODE)) THEN
          ICI = 1
          RBUF(1) = ICI
      END IF
      LASTI = -1
      IF (MODE.EQ.2) THEN
C         -- GraphOn
          CTMP(1:1) = CHAR(GS)
          CTMP(2:2) = CHAR(ESC)
          CTMP(3:3) = CHAR(1)
          IF (ICI.EQ.0) CTMP(3:3) = CHAR(16)
          CALL GRTT02(ICHAN, MODE, CTMP, 3, CBUF, LBUF)
      ELSE IF (MODE.EQ.3 .OR. MODE.EQ.7) THEN
C         -- Retrographics, V603
          CTMP(1:1) = CHAR(GS)
          CTMP(2:2) = CHAR(ESC)
          CTMP(3:3) = CHAR(47)
          CTMP(4:4) = CHAR(49-ICI)
          CTMP(5:5) = CHAR(100)
          CALL GRTT02(ICHAN, MODE, CTMP, 5, CBUF, LBUF)
      ELSE IF ( MODE.EQ.4 .OR. MODE.EQ.6 .OR. MODE.EQ.10) THEN
C         -- gterm and zstem, VMAC
          CTMP(1:4) = CHAR(GS)//CHAR(ESC)//'ML'
          CALL GRTT02(ICHAN, MODE, CTMP, 4, CBUF, LBUF)
          CALL GRTT05(ICI, CTMP, LTMP)
          CALL GRTT02(ICHAN, MODE, CTMP, LTMP, CBUF, LBUF)
          SEFCOL=.TRUE.
      ELSE IF (MODE.EQ.9) THEN
C         -- TK4100
          CTMP(1:3) = CHAR(ESC)//'ML'
          CALL GRTT02(ICHAN, MODE, CTMP, 3, CBUF, LBUF)
          CALL GRTT05(ICI, CTMP, LTMP)
          CALL GRTT02(ICHAN, MODE, CTMP, LTMP, CBUF, LBUF)
      ELSE IF( MODE.EQ.8) THEN
C         -- krm3: all attributes off
          CTMP(1:1) = CHAR(27)
          CTMP(2:2) = CHAR(91)
          CTMP(3:3) = CHAR(48)
          CTMP(4:4) = CHAR(59)
C
C  Load color definitions (8-14 are bold versions of 1-7, so are not the
C  standard PGPLOT ones)
C
          IF ( ICI.EQ.0 ) I=0
          IF ( ICI.EQ.1 .OR. ICI.EQ.8)  I=7
          IF ( ICI.EQ.2 .OR. ICI.EQ.9)  I=1
          IF ( ICI.EQ.3 .OR. ICI.EQ.10) I=2
          IF ( ICI.EQ.4 .OR. ICI.EQ.11) I=4
          IF ( ICI.EQ.5 .OR. ICI.EQ.12) I=6
          IF ( ICI.EQ.6 .OR. ICI.EQ.13) I=5
          IF ( ICI.EQ.7 .OR. ICI.EQ.14) I=3
          CTMP(5:5) = CHAR(51)
          CTMP(6:6) = CHAR(48+I)
          IF (ICI.GT.7) THEN
              CTMP(7:7) = CHAR(59)
              CTMP(8:8) = CHAR(49)
              CTMP(9:9) = CHAR(109)
              CALL GRTT02(ICHAN, MODE, CTMP, 9, CBUF, LBUF)
          ELSE
              CTMP(7:7) = CHAR(109)
              CALL GRTT02(ICHAN, MODE, CTMP, 7, CBUF, LBUF)
          END IF
      END IF
      RETURN
C
C--- IFUNC=16, Flush buffer. -------------------------------------------
C
  160 CONTINUE
      IF (MODE.EQ.1 .OR. MODE.GT.5) THEN
C         -- tek4010, zstem, v603, krm3, or tk4100
          CTMP(1:6) = CHAR(GS)//CHAR(55)//CHAR(127)//CHAR(32)//
     :                CHAR(64)//CHAR(US)
          CALL GRTT02(ICHAN, MODE, CTMP, 6, CBUF, LBUF)
      ELSE IF (MODE.EQ.5) THEN
C         -- xterm
          CTMP(1:3) = CHAR(US)//CHAR(ESC)//CHAR(3)
          CALL GRTT02(ICHAN, MODE, CTMP, 3, CBUF, LBUF)
      ELSE
          CTMP(1:8) = CHAR(GS)//CHAR(55)//CHAR(127)//CHAR(32)//
     :                CHAR(64)//CHAR(3)//CHAR(CAN)//CHAR(US)
          CALL GRTT02(ICHAN, MODE, CTMP, 8, CBUF, LBUF)
      END IF
      CALL GRWTER(ICHAN, CBUF, LBUF)
      LASTI = -1
      RETURN
C
C--- IFUNC=17, Read cursor. --------------------------------------------
C
  170 CONTINUE
C     -- flush buffer
      CALL GRWTER(ICHAN, CBUF, LBUF)
      LASTI = -1
      IF ( MODE.EQ.5 .OR. MODE.EQ.6 ) THEN
C         -- xterm and zstem make sure terminal is in Tektronix mode.
          LTMP = 6
          CALL GRWTER(ICHAN, CHAR(ESC)//'[?38h', LTMP)
      END IF
C     -- initial cursor position
      I0 = NINT(RBUF(1))
      J0 = NINT(RBUF(2))
C     -- read cursor
      CALL GRTT03(ICHAN, I0, J0, ICH, IER)
C     -- on XTERM, map mouse button clicks onto A, D, X.
      IF (MODE.EQ.5) THEN
         IF (ICH.EQ.236) THEN
            ICH = ICHAR('a')
         ELSE IF (ICH.EQ.237) THEN
            ICH = ICHAR('d')
         ELSE IF (ICH.EQ.242) THEN
            ICH = ICHAR('x')
         ELSE IF (ICH.EQ.204) THEN
            ICH = ICHAR('A')
         ELSE IF (ICH.EQ.205) THEN
            ICH = ICHAR('D')
         ELSE IF (ICH.EQ.210) THEN
            ICH = ICHAR('X')
         END IF
      END IF
C     -- return result
      IF (IER.EQ.0) THEN
          RBUF(1) = I0
          RBUF(2) = J0
          CHR(1:1) = CHAR(ICH)
      ELSE
          CHR(1:1) = CHAR(0)
      END IF
      NBUF = 2
      LCHR = 1
      RETURN
C
C--- IFUNC=18, Erase alpha screen. -------------------------------------
C
  180 CONTINUE
      IF (MODE.EQ.2 .OR. MODE.EQ.3) THEN
C         -- GraphOn, Retrographics: return to VT100 mode and
C            issue VT100 erase-screen command
          CTMP(1:8) = CHAR(GS)//CHAR(55)//CHAR(127)//CHAR(32)//
     :                CHAR(64)//CHAR(3)//CHAR(CAN)//CHAR(US)
          CALL GRTT02(ICHAN, MODE, CTMP, 8, CBUF, LBUF)
          CTMP(1:7) = CHAR(ESC)//'[2J'//CHAR(ESC)//'[H'
          CALL GRTT02(ICHAN, MODE, CTMP, 7, CBUF, LBUF)
          LASTI = -1
      END IF
      RETURN
C
C--- IFUNC=21, Set color representation. -------------------------------
C
  210 CONTINUE
      IF (MODE.EQ.4) THEN
C        -- gterm
         I = RBUF(1)
         CTMP(1:6) = CHAR(GS)//CHAR(ESC)//'TG14'
         LTMP = 6
         CALL GRTT05(I,     CADD, LADD)
         CTMP(LTMP+1:LTMP+LADD) = CADD(:LADD)
         LTMP = LTMP + LADD
C        -- red
         INTEN = RBUF(2)*255.0
         CALL GRTT05(INTEN, CADD, LADD)
         CTMP(LTMP+1:LTMP+LADD) = CADD(:LADD)
         LTMP = LTMP + LADD
C        -- green
         INTEN = RBUF(3)*255.0
         CALL GRTT05(INTEN, CADD, LADD)
         CTMP(LTMP+1:LTMP+LADD) = CADD(:LADD)
         LTMP = LTMP + LADD
C        -- blue
         INTEN = RBUF(4)*255.0
         CALL GRTT05(INTEN, CADD, LADD)
         CTMP(LTMP+1:LTMP+LADD) = CADD(:LADD)
         LTMP = LTMP + LADD
C
         CTMP(LTMP+1:LTMP+2) = CHAR(US)//CHAR(CAN)
         LTMP = LTMP + 2
         CALL GRTT02(ICHAN, MODE, CTMP, LTMP, CBUF, LBUF)
         CALL GRWTER(ICHAN, CBUF, LBUF)
         LASTI = -1
      ELSE IF (MODE.EQ.9) THEN
C        -- TK4100
         I = RBUF(1)
         CTMP(1:5) = CHAR(ESC)//'TG14'
         LTMP = 5
         CALL GRTT05(I,     CADD, LADD)
         CTMP(LTMP+1:LTMP+LADD) = CADD(:LADD)
         LTMP = LTMP + LADD
C        -- red
         INTEN = RBUF(2)*100.0
         CALL GRTT05(INTEN, CADD, LADD)
         CTMP(LTMP+1:LTMP+LADD) = CADD(:LADD)
         LTMP = LTMP + LADD
C        -- green
         INTEN = RBUF(3)*100.0
         CALL GRTT05(INTEN, CADD, LADD)
         CTMP(LTMP+1:LTMP+LADD) = CADD(:LADD)
         LTMP = LTMP + LADD
C        -- blue
         INTEN = RBUF(4)*100.0
         CALL GRTT05(INTEN, CADD, LADD)
         CTMP(LTMP+1:LTMP+LADD) = CADD(:LADD)
         LTMP = LTMP + LADD
         CALL GRTT02(ICHAN, MODE, CTMP, LTMP, CBUF, LBUF)
         CALL GRWTER(ICHAN, CBUF, LBUF)
         LASTI = -1
      ELSE IF (MODE.EQ.10) THEN
C        -- VersaTerm
         CTMP(1:1)=CHAR(GS)
         CALL GRTT02(ICHAN, MODE, CTMP, 1, CBUF, LBUF)
         CALL GRXHLS(RBUF(2), RBUF(3), RBUF(4), HUE, LIG, SAT)
         CALL GRTT06(NINT(RBUF(1))
     :     , NINT(HUE), NINT(LIG*100), NINT(SAT*100), CTMP, LTMP)
         CALL GRTT02(ICHAN, MODE, CTMP, LTMP, CBUF, LBUF)
         CALL GRWTER(ICHAN, CBUF, LBUF)
         LASTI = -1
      END IF
      RETURN
C
C--- IFUNC=24, Rectangle fill. -----------------------------------------
C
  240 CONTINUE
      IF ( I4014(MODE).EQ.0 ) THEN
         I0 = NINT(RBUF(1))
         J0 = NINT(RBUF(2))
         I1 = NINT(RBUF(3))
         J1 = NINT(RBUF(4))
      ELSE
         I0 = NINT(4.*RBUF(1))
         J0 = NINT(4.*RBUF(2))
         I1 = NINT(4.*RBUF(3))
         J1 = NINT(4.*RBUF(4))
      END IF
      IF (MODE.EQ.2) THEN
C         -- GraphOn
C         -- enter rectangle mode
          CALL GRTT02(ICHAN, MODE, CHAR(GS)//CHAR(ESC)//CHAR(2), 3,
     :         CBUF, LBUF)
C         -- draw rectangle
          CALL GRTT01(ICHAN, MODE, I4014(MODE), LASTI, LASTJ,
     :      I0, J0, I1, J1, CBUF, LBUF)
C         -- exit rectangle mode
          CALL GRTT02(ICHAN, MODE, CHAR(ESC)//CHAR(3), 2, CBUF, LBUF)
      ELSE IF (MODE.EQ.7 .OR. MODE.EQ.8) THEN
C         -- v603, krm3: needs bottom left corner and rectangle
C            dimensions
          IBUF(1)=I0+1
          IBUF(2)=J0+1
          IBUF(3)=I1+1
          IBUF(4)=J1+1
          DO 241 I=1,4
             IF (IBUF(I) .LT. 1)    IBUF(I)=1
             IF (IBUF(I) .GT. 1056) IBUF(I)=1056
  241     CONTINUE
          IBUF(3)=IBUF(3)-IBUF(1)
          IBUF(4)=IBUF(4)-IBUF(2)
          ITOT=0
          DO 244 I=1,4
             WRITE (CSCR(I)(1:4), '(I4)') IBUF(I)
             IBUF(I)=1
             DO 242 J=1,4
                IF (CSCR(I)(J:J) .NE. ' ') THEN
                   GOTO 243
                END IF
                IBUF(I)=IBUF(I)+1
  242        CONTINUE
  243        CONTINUE
             ITOT=ITOT+5-IBUF(I)
  244     CONTINUE
          CTMP(1:8+ITOT)=
     :         CHAR(ESC)//CHAR(47)//CSCR(1)(IBUF(1):4)//CHAR(59)//
     :         CSCR(2)(IBUF(2):4)//CHAR(59)//CSCR(3)(IBUF(3):4)//
     :         CHAR(59)//CSCR(4)(IBUF(4):4)//CHAR(59)//CHAR(49)//
     :         CHAR(121)
          CALL GRTT02(ICHAN, MODE, CTMP, 8+ITOT, CBUF, LBUF)
      ELSE IF (MODE.EQ.10) THEN
C         -- VMAC: use polygon fill commands
          IF (SEFCOL) THEN
C set fill color
            SEFCOL=.FALSE.
            CTMP(1:3) = CHAR(ESC)//'MP'
            CALL GRTT02(ICHAN, MODE, CTMP, 3, CBUF, LBUF)
            CALL GRTT05(-ICI, CTMP, LTMP)
            CALL GRTT02(ICHAN, MODE, CTMP, LTMP, CBUF, LBUF)
          ENDIF
C send "start polygon fill"
          CTMP(1:3) = CHAR(ESC)//'LP'
          LTMP=3
C make lasti,lastj different from i0,j0 in each bit
          LASTI=4095-I0
          LASTJ=4095-J0
C send first coordinate
          CALL GRTT04(I4014(MODE), LASTI, LASTJ, I0, J0, CTMP, LTMP)
          LASTI=I0
          LASTJ=J0
C '0' means: boundary has the same the color as fill area
          CTMP(LTMP+1:LTMP+1)='0'
          LTMP=LTMP+1
          CALL GRTT02(ICHAN, MODE, CTMP, LTMP, CBUF, LBUF)
C further edges:
          CALL GRTT01(ICHAN, MODE, I4014(MODE), LASTI, LASTJ,
     :      I0, J1, I1, J1, CBUF, LBUF)
          CALL GRTT01(ICHAN, MODE, I4014(MODE), LASTI, LASTJ,
     :      I1, J1, I1, J0, CBUF, LBUF)
C send "end polygon fill"
          CTMP(1:3)=CHAR(ESC)//'LE'
          CALL GRTT02(ICHAN, MODE, CTMP, 3, CBUF, LBUF)
      END IF
      RETURN
C-----------------------------------------------------------------------
      END
C*GRTT01 -- PGPLOT Tektronix driver, draw line segment
C+
      SUBROUTINE GRTT01(ICHAN, MODE, I4014, LASTI, LASTJ, I0, J0,
     :         I1, J1, CBUF, LBUF)
      INTEGER   ICHAN, MODE, I4014, LASTI, LASTJ, I0, J0, I1, J1, LBUF
      CHARACTER CBUF*(*)
C
C This routine draws a line from (I0, J0) to (I1, J1).  If LASTI>=0
C assume that the cursor is at the position is at (LASTI, LASTJ).
C For this case, a minimum length move is done from (LASTI, LASTJ) to
C the nearer point.  Of course, if (LASTI, LASTJ) and the nearer point
C are the same, then no bytes of positioning data are generated and
C sent to the terminal.  If LASTI<0 then a move is done with the
C coordinate fully specified.  In both cases the line end point
C is specified using the fewest number of bytes allowed by the protocol.
C Upon return, LASTI,LASTJ will contain the current cursor position. 
C If I4014=0 then 10 bit (4010) coordinates are generated, for I4014=1,
C full 12 bit Tektronix (4014 and higher) coordinates are generated.
C Note:  The 'delete' character (127) can occur in LOY or EXTRA byte;
C it can be replaced by escape-? if desired.
C
C Arguments:
C   ICHAN       (in)     : passed to GRTT02 if called.
C   MODE        (in)     : passed to GRTT02 if called.
C   I4014       (in)     : =0 generate 4010 coords, =1 generate 4014.
C   LASTI,LASTJ (in/out) : current position
C   I0, J0      (in/out) : device coordinates of the starting point.
C   I1, J1      (in/out) : device coordinates of the end point.
C   CBUF        (in/out) : buffer for instruction.
C   LBUF        (in/out) : Number of valid characters in CBUF.
C
C 1993-Feb-02 - Created from GRZS01 - [AFT]
C-----------------------------------------------------------------------
      INTEGER    GS
      PARAMETER (GS = 29)
      INTEGER    MASKLX, MASKHX
      PARAMETER (MASKLX = 64, MASKHX = 32)
      INTEGER   MASKLY, MASKHY
      PARAMETER (MASKLY = 96, MASKHY = 32)
      INTEGER   MASKEX
      PARAMETER (MASKEX = 96)
C
      CHARACTER CTMP*12
      INTEGER ID0, ID1, IFLUSH, ITMP
      INTEGER IEX, ILOX, IHIX, ILOY, IHIY, LTMP
C
C If it is possible for this routine to generate enough data to fill
C the buffer, and thus cause it to be flushed to the terminal, then we
C force the write to take place now.  This will ensure that terminal
C is in the correct state for the following commands.
      IF ( LBUF+11.GE.LEN(CBUF) ) THEN
          CALL GRWTER(ICHAN, CBUF, LBUF)
          IFLUSH = 1
      ELSE
          IFLUSH = 0
      END IF
C
      LTMP = 0
      IF(LASTI.LT.0) THEN
C Last position is invalid, therefore do a dark vector move with all
C coordinates specified.
          LTMP=LTMP+1
          CTMP(LTMP:LTMP)=CHAR(GS)
          IF ( I4014.EQ.0 ) THEN
              IHIY = J0/32
              ILOY = MOD(J0, 32)
              IHIX = I0/32
              ILOX = MOD(I0, 32)
              CTMP(LTMP+1:LTMP+4) =
     :            CHAR( MASKHY + IHIY )//
     :            CHAR( MASKLY + ILOY )//
     :            CHAR( MASKHX + IHIX )//
     :            CHAR( MASKLX + ILOX )
              LTMP = LTMP + 4
          ELSE
              IHIY = J0/128
              ILOY = MOD(J0/4, 32)
              IHIX = I0/128
              ILOX = MOD(I0/4, 32)
              IEX  = 4*MOD(J0, 4) + MOD(I0, 4)
              CTMP(LTMP+1:LTMP+5) =
     :            CHAR( MASKHY + IHIY )//
     :            CHAR( MASKEX + IEX )//
     :            CHAR( MASKLY + ILOY )//
     :            CHAR( MASKHX + IHIX )//
     :            CHAR( MASKLX + ILOX )
              LTMP = LTMP + 5
          END IF
      ELSE
C Last position is valid, move pen to nearest end point of line.
          ID0=ABS(LASTI-I0)+ABS(LASTJ-J0)
          ID1=ABS(LASTI-I1)+ABS(LASTJ-J1)
          IF(ID1.LT.ID0) THEN
C Swap coordinates to minimize 'pen motion'.  For optimized coordinates
C this can reduce the amount of I/O to the the terminal.
              ITMP=I0
              I0=I1
              I1=ITMP
              ITMP=J0
              J0=J1
              J1=ITMP
              ITMP=ID0
              ID0=ID1
              ID1=ITMP
          END IF
          IF(ID0.NE.0 .OR. ID1.NE.0) THEN
C Position has changed, so do a move operation.
              LTMP=LTMP+1
              CTMP(LTMP:LTMP)=CHAR(GS)
              CALL GRTT04(I4014,LASTI,LASTJ,I0,J0,CTMP,LTMP)
          ELSE
              IF(IFLUSH.NE.0) THEN
C The position is valid, but the buffer was flushed, so terminal may
C no longer be in graph mode.  Therefore, send GS and followed by a
C zero length dark move (i.e., just resend LOX coordinate).
                  IF ( I4014.EQ.0 ) THEN
                      ILOX = MOD(I0, 32)
                  ELSE
                      ILOX = MOD(I0/4, 32)
                  END IF
                  CTMP(LTMP+1:LTMP+2)=CHAR(GS)//CHAR(MASKLX+ILOX)
                  LTMP=LTMP+2
              END IF
          END IF
      END IF
C
C Terminal is now in graph mode, and the `pen' has been positioned.
C Do an optimized draw.
      CALL GRTT04(I4014,I0,J0,I1,J1,CTMP,LTMP)
      CALL GRTT02(ICHAN, MODE, CTMP, LTMP, CBUF, LBUF)
C
C Remember current position.
      LASTI=I1
      LASTJ=J1
      RETURN
C
      END
C*GRTT02 -- PGPLOT Tektronix driver, transfer data to buffer
C+
      SUBROUTINE GRTT02 (ICHAN, MODE, CADD, LADD, CBUF, LBUF)
      INTEGER   ICHAN, MODE, LADD, LBUF
      CHARACTER CADD*(*), CBUF*(*)
C
C Arguments:
C   ICHAN  (input)  : channel number for output (when buffer is full).
C   MODE   (input)  : emulation type.
C   CADD   (input)  : text to add to buffer.
C   LADD   (input)  : number of characters to transfer.
C   CBUF   (input)  : output buffer.
C   LBUF   (in/out) : number of valid characters in CBUF.
C
C Subroutines called:
C   GRWTER
C-----------------------------------------------------------------------
      IF (LBUF+LADD.GE.LEN(CBUF) ) THEN
          CALL GRWTER(ICHAN, CBUF, LBUF)
      END IF
C
      IF ( LADD.GT.0 ) THEN
          IF ( LBUF.EQ.0 ) THEN
              IF ( MODE.EQ.5 .OR. MODE.EQ.6 ) THEN
                  CBUF(1:6) = CHAR(27)//'[?38h'
                  LBUF = 6
              END IF
          END IF
          CBUF(LBUF+1:LBUF+LADD) = CADD(1:LADD)
          LBUF = LBUF + LADD
      END IF
C-----------------------------------------------------------------------
      END
C*GRTT03 -- PGPLOT Tektronix driver, cursor routine
C+
      SUBROUTINE GRTT03 (ICHAN, IX, IY, IC, IER)
      INTEGER   ICHAN, IX, IY, IC, IER
C
C Arguments:
C   ICHAN  (input)  : channel for output to device.
C   IX, IY (in/out) : initial/final coordinates of cursor (device 
C                     coordinates).
C   IC     (output) : character code.
C   IER    (output) : error status (0 is OK).
C
C-----------------------------------------------------------------------
      CHARACTER CBUF*8, CPROM*10
      INTEGER   I1, I2, LBUF
C
C Position cursor (by drawing a dark vector).
C
      CPROM(1:1) = CHAR(29)
      CPROM(2:2) = CHAR(32+(IY/32))
      CPROM(3:3) = CHAR(96+MOD(IY,32))
      CPROM(4:4) = CHAR(32+(IX/32))
      CPROM(5:5) = CHAR(64+MOD(IX,32))
      CPROM(6:6) = CHAR(27)
      CPROM(7:7) = CHAR(47)
      CPROM(8:8) = CHAR(102)
      CPROM(9:9) = CHAR(27)
      CPROM(10:10) = CHAR(26)
C
C Do a read with prompt.
C
      LBUF = 5
      CALL GRPTER(ICHAN, CPROM, 10, CBUF, LBUF)
C
C Must read at least 5 characters.
C
      IF( LBUF.LT.5) THEN
         IER = 1
      ELSE
C
C Decode coordinates.
C
         IC = ICHAR( CBUF(1:1) )
         I1 = MOD( ICHAR(CBUF(2:2)), 32 )
         I2 = MOD( ICHAR(CBUF(3:3)), 32 )
         IX = I1*32 + I2
         I1 = MOD( ICHAR(CBUF(4:4)), 32 )
         I2 = MOD( ICHAR(CBUF(5:5)), 32 )
         IY = I1*32 + I2
         IER = 0
      END IF
      RETURN
C-----------------------------------------------------------------------
      END
C*GRTT04 -- PGPLOT Tektronix driver, encode coordinate pair, optimize
C+
      SUBROUTINE GRTT04(I4014, LASTI, LASTJ, I0, J0, CTMP, LTMP)
      INTEGER   I4014, LASTI, LASTJ, I0, J0, LTMP
      CHARACTER CTMP*(*)
C
C Assume cursor is at position LASTI, LASTJ and that the light or
C dark vector condition has been correctly set.  Add up to 5 characters
C to CTMP to draw a vector to I0, J0.  The minimum number of characters
C are encoded to obtain the motion.
C-----------------------------------------------------------------------
      INTEGER    MASKLX, MASKHX
      PARAMETER (MASKLX = 64, MASKHX = 32)
      INTEGER   MASKLY, MASKHY
      PARAMETER (MASKLY = 96, MASKHY = 32)
      INTEGER   MASKEX
      PARAMETER (MASKEX=96)
C
      INTEGER IEX, ILOX, IHIX, ILOY, IHIY
      INTEGER LEX, LLOX, LHIX, LLOY, LHIY
C
      IF ( I4014.EQ.0 ) THEN
          LHIY = LASTJ/32
          LLOY = MOD(LASTJ, 32)
          LHIX = LASTI/32
          LLOX = MOD(LASTI, 32)
          LEX = 0
          IHIY = J0/32
          ILOY = MOD(J0, 32)
          IHIX = I0/32
          ILOX = MOD(I0, 32)
          IEX = 0
      ELSE
          LHIY = LASTJ/128
          LLOY = MOD(LASTJ/4, 32)
          LHIX = LASTI/128
          LLOX = MOD(LASTI/4, 32)
          LEX = 4*MOD(LASTJ, 4) + MOD(LASTI, 4)
          IHIY = J0/128
          ILOY = MOD(J0/4, 32)
          IHIX = I0/128
          ILOX = MOD(I0/4, 32)
          IEX = 4*MOD(J0, 4) + MOD(I0, 4)
      END IF
C
      IF(IHIY.NE.LHIY) THEN
          LTMP=LTMP+1
          CTMP(LTMP:LTMP) = CHAR(32+IHIY)
      END IF
C Note, for 4010 mode, IEX=LEX (by definition)
      IF(IEX.NE.LEX) THEN
          LTMP=LTMP+1
          CTMP(LTMP:LTMP) = CHAR(96+IEX)
      END IF
      IF(IEX.NE.LEX .OR. ILOY.NE.LLOY .OR. IHIX.NE.LHIX) THEN
          LTMP=LTMP+1
          CTMP(LTMP:LTMP) = CHAR(96+ILOY)
      END IF
      IF(IHIX.NE.LHIX) THEN
          LTMP=LTMP+1
          CTMP(LTMP:LTMP) = CHAR(32+IHIX)
      END IF
      LTMP=LTMP+1
      CTMP(LTMP:LTMP) = CHAR(64+ILOX)
      RETURN
      END
C*GRTT05 -- PGPLOT Tektronix 4100 driver, encode integer
C+
      SUBROUTINE GRTT05(I, C, NC)
      INTEGER I
      CHARACTER*(*) C
      INTEGER NC
C
C Encode integer in host syntax. Input integer I; output encoded string
C C, containing NC characters (1, 2, or 3).  This version encodes
C integers up to 1023, which fit in two characters.
C-----------------------------------------------------------------------
      INTEGER J
C
      J = IABS(I)
      IF (J.LT.16) THEN
          IF (I.LT.0) THEN
              C(1:1) = CHAR(J+32)
          ELSE
              C(1:1) = CHAR(J+48)
          END IF
          NC = 1
      ELSE
          C(1:1) = CHAR(J/16+64)
          IF (I.LT.0) THEN
              C(2:2) = CHAR(MOD(J,16)+32)
          ELSE
              C(2:2) = CHAR(MOD(J,16)+48)
          END IF
          NC = 2
      END IF
C
      END
C*GRTT06 -- PGPLOT Tektronix 4100 driver, encode color definition
C+
      SUBROUTINE GRTT06(IDX, I1, I2, I3, C, NC)
      INTEGER ESC, GS, US
      PARAMETER (ESC=27, GS=29, US=31)
      INTEGER IDX, I1, I2, I3
      CHARACTER*(*) C
      INTEGER NC
C
C Encode color definition, Color index IDX, I1,I2,I3 are the 3 integer
C color components (definiton is device-dependent).
C output encoded string containing NC characters (max 20).
C-----------------------------------------------------------------------
      INTEGER L
C
      C(1:5) = CHAR(ESC)//'TG14'
      NC=5
      CALL GRTT05(IDX, C(NC+1:NC+3), L)
      NC=NC+L
      CALL GRTT05(I1, C(NC+1:NC+3), L)
      NC=NC+L
      CALL GRTT05(I2, C(NC+1:NC+3), L)
      NC=NC+L
      CALL GRTT05(I3, C(NC+1:NC+3), L)
      NC=NC+L
      END
