C*NEDRIV -- PGPLOT NeXTstep driver
C+
      SUBROUTINE NEDRIV(IFUNC, RBUF, NBUF, CHR, LCHR)
      INTEGER IFUNC, NBUF, LCHR
      REAL    RBUF(*)
      CHARACTER*(*) CHR
C
C PGPLOT driver for NeXT computers.
C
C 1992-Feb-16 - Copied from psdriv.f - [AFT]
C
C Supported device:
C   Any computer running NeXTstep.
C
C Device type code:
C   /NEXT
C
C Default file name:
C   none
C
C Default view surface dimensions:
C   The default screen size is 720 by 535 pixels (about 8 by 6 inches
C   on a 19 inch monitor).  The aspect ratio was selected to match
C   the /PS device.  The window can be resized larger or smaller.
C
C Resolution:
C   The screen resolution is 92 dpi.  The driver generates PostScript
C   commands with a resolution 10 times greater than the screen resolution.
C   This allows the window to be resized and/or a hardcopy to be made with
C   no loss of resolution.
C
C Color capability:
C   On all devices, color indices 0-15 are supported.  The default colors
C   are 0 is white, 1 is black, 14 is light gray, 15 dark gray.  On
C   monochrome devices, color indices 2-13 default to black.  If the
C   driver detects a color server, then color indices 0-255 are allowed
C   and color indices 2-13 default to the standard PGPLOT colors.
C
C Input capability:
C   The PGPLOT cursor is supported.  When a cursor read is requested the
C   the viewer becomes the active application and the active plot window
C   becomes the key window.  This allows the user to terminate the cursor
C   read by either a mouse click (which generates an 'A' character) or
C   by pressing a key on the keyboard.
C
C File format:
C   By using the print command, in the main menu, you can send the
C   contents of the current window to a file.  This file can then be
C   printed on any PostScript printer.
C
C Obtaining hardcopy:
C   If you click on the print item in the main menu, then the standard
C   NeXT print panel comes up.  This allows the contents of the current
C   window to be sent to a printer, disk file or previewer.
C
C References:
C
C (1) Adobe Systems, Inc.: PostScript Language Reference Manual.
C Addison-Wesley, Reading, Massachusetts, 1985.
C (2) Adobe Systems, Inc.: PostScript Language Tutorial and Cookbook.
C Addison-Wesley, Reading, Massachusetts, 1985.
C (3) Adobe Systems, Inc.: PostScript Language Reference Manual, Second
C Edition. Addison-Wesley, Reading, Massachusetts, 1990.
C (4) Adobe Systems, Inc.: Programming the Display PostScript System
C with NeXTstep. Addison-Wesley, Reading, Massachusetts, 1992.
C-----------------------------------------------------------------------
      CHARACTER*(*) DTYPE
      PARAMETER (DTYPE= 'NEXT  (Display on NeXT console)')
      CHARACTER*120 INSTR, MSG
      CHARACTER CBUF*4
      REAL      RTMP(4)
      INTEGER   HEIGHT, RESOL, WIDTH
      SAVE      HEIGHT, RESOL, WIDTH
      INTEGER   LASTI, LASTJ, NPTS
      SAVE      LASTI, LASTJ, NPTS
      INTEGER   CI, LW
      INTEGER   I0, J0, I1, J1, L
      INTEGER   NXP, NYP, XORG, YORG, XLEN, YLEN, N, RGB(3)
      INTEGER   HIGH, LOW, I, K, KMAX
      LOGICAL   START, COLOR
      SAVE      START, COLOR
      REAL RVALUE(0:255), GVALUE(0:255), BVALUE(0:255)
      SAVE RVALUE,        GVALUE,        BVALUE
C
      REAL          SHADE(0:15), RINIT(0:15), GINIT(0:15), BINIT(0:15)
      SAVE          SHADE,       RINIT,       GINIT,       BINIT
      CHARACTER*1   HEXDIG(0:15)
      DATA HEXDIG/'0','1','2','3','4','5','6','7',
     1            '8','9','A','B','C','D','E','F'/
      DATA SHADE /1.00, 13*0.00, 0.33, 0.67/
      DATA RINIT
     1     / 1.00, 0.00, 1.00, 0.00, 0.00, 0.00, 1.00, 1.00,
     2       1.00, 0.50, 0.00, 0.00, 0.50, 1.00, 0.33, 0.67/
      DATA GINIT
     1     / 1.00, 0.00, 0.00, 1.00, 0.00, 1.00, 0.00, 1.00,
     2       0.50, 1.00, 1.00, 0.50, 0.00, 0.00, 0.33, 0.67/
      DATA BINIT
     1     / 1.00, 0.00, 0.00, 0.00, 1.00, 1.00, 1.00, 0.00,
     2       0.00, 0.00, 0.50, 1.00, 1.00, 0.50, 0.33, 0.67/
      DATA WIDTH,HEIGHT/0.,0./
C-----------------------------------------------------------------------
C
      GOTO( 10, 20, 30, 40, 50, 60, 70, 80, 90,100,
     1     110,120,130,140,150,160,170,180,190,200,
     2     210,220,230,900,900,260,900,900,290), IFUNC
      GOTO 900
C
C--- IFUNC = 1, Return device name.-------------------------------------
C
   10 CONTINUE
      CHR = DTYPE
      LCHR = LEN(DTYPE)
      RETURN
C
C--- IFUNC = 2, Return physical min and max for plot device, and range
C               of color indices.---------------------------------------
C
   20 CONTINUE
      IF(HEIGHT.EQ.0.) THEN
         CALL NEXSUP(1,CBUF,RTMP)
         HEIGHT = RTMP(1)
         WIDTH  = RTMP(2)
         RESOL  = RTMP(3)
      END IF
      RBUF(1) = 0
      RBUF(3) = 0
      RBUF(5) = 0
      RBUF(2) = HEIGHT-1
      RBUF(4) = WIDTH-1
      RBUF(6) = 255
      NBUF = 6
      RETURN
C
C--- IFUNC = 3, Return device resolution. ------------------------------
C
   30 CONTINUE
      IF(HEIGHT.EQ.0.) THEN
         CALL NEXSUP(1,CBUF,RTMP)
         HEIGHT = RTMP(1)
         WIDTH  = RTMP(2)
         RESOL  = RTMP(3)
      END IF
      RBUF(1) = RESOL*92.0
      RBUF(2) = RESOL*92.0
      RBUF(3) = 5
      NBUF = 3
      RETURN
C
C--- IFUNC = 4, Return misc device info. -------------------------------
C    (This device is Interactive, Cursor, No dashed lines, Area fill,
C    Thick lines, No Rectangle fill, Array images.)
C
   40 CHR = 'ICNATNQNNN'
      LCHR = 10
      RETURN
C
C--- IFUNC = 5, Return default file name. ------------------------------
C
   50 CONTINUE
      CHR  = ' '
      LCHR = 0
      RETURN
C
C--- IFUNC = 6, Return default physical size of plot. ------------------
C
   60 CONTINUE
      IF(HEIGHT.EQ.0.) THEN
         CALL NEXSUP(1,CBUF,RTMP)
         HEIGHT = RTMP(1)
         WIDTH  = RTMP(2)
         RESOL  = RTMP(3)
      END IF
      RBUF(1) = 0
      RBUF(3) = 0
      RBUF(2) = HEIGHT-1
      RBUF(4) = WIDTH-1
      NBUF = 4
      RETURN
C
C--- IFUNC = 7, Return misc defaults. ----------------------------------
C
   70 RBUF(1) = 8
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
      CALL NEXSUP(1,CBUF,RTMP)
      COLOR =  RTMP(4).NE.0.0
      IF (COLOR) THEN
         DO 91 CI=0,15
            RVALUE(CI) = RINIT(CI)
            GVALUE(CI) = GINIT(CI)
            BVALUE(CI) = BINIT(CI)
 91      CONTINUE
      ELSE
         DO 92 CI=0,15
            RVALUE(CI) = SHADE(CI)
            GVALUE(CI) = SHADE(CI)
            BVALUE(CI) = SHADE(CI)
 92      CONTINUE
      END IF
      DO 93 CI=16,255
         RVALUE(CI) = 0.0
         GVALUE(CI) = 0.0
         BVALUE(CI) = 0.0
 93   CONTINUE
C
      LASTI = -1
      LASTJ = -1
      NPTS = 0
C
      RBUF(1) = 1
      RBUF(2) = 1
      NBUF = 2
      RETURN
C
C--- IFUNC=10, Close workstation. --------------------------------------
C
  100 CONTINUE
      CALL NEXSUP(7,CBUF,RTMP)
      RETURN
C
C--- IFUNC=11, Begin picture. ------------------------------------------
C
  110 CONTINUE
C If user has deleted window, create a new one now.
      CALL NEXSUP(1,CBUF,RTMP)
      HEIGHT = RTMP(1)
      WIDTH  = RTMP(2)
      RESOL  = RTMP(3)
      COLOR =  RTMP(4).NE.0.0
C Send begin picture message.
      CALL NEXSUP(2,CBUF,RTMP)
C The following line clears the screen.
      CALL GRFAO('1.0 setgray 0 0 # # rectfill', L, INSTR,
     :         HEIGHT, WIDTH, 0, 0)
      CALL GRNE02(0, INSTR(:L), 0)
      CALL GRNE02(0, '1 setlinejoin 1 setlinecap 1 SLW', 0)
      RETURN
C
C--- IFUNC=12, Draw line. ----------------------------------------------
C
  120 CONTINUE
      I0 = NINT(RBUF(1))
      J0 = NINT(RBUF(2))
      I1 = NINT(RBUF(3))
      J1 = NINT(RBUF(4))
      IF (I0.EQ.LASTI .AND. J0.EQ.LASTJ) THEN
          CALL GRFAO('# # c', L, INSTR, (I1-I0), (J1-J0), 0, 0)
      ELSE
          CALL GRFAO('# # # # l', L, INSTR, (I1-I0), (J1-J0), I0, J0)
      END IF
      LASTI = I1
      LASTJ = J1
      CALL GRNE02(0, INSTR(:L), 0)
      RETURN
C
C--- IFUNC=13, Draw dot. -----------------------------------------------
C
  130 CONTINUE
      I1 = NINT(RBUF(1))
      J1 = NINT(RBUF(2))
      CALL GRFAO('# # d', L, INSTR, I1, J1, 0, 0)
      LASTI = I1
      LASTJ = J1
      CALL GRNE02(0, INSTR(:L), 0)
      RETURN
C
C--- IFUNC=14, End picture. --------------------------------------------
C
  140 CONTINUE
      CALL GRNE02(0, ' ', 1)
      CALL NEXSUP(6, CBUF, RTMP)
      RETURN
C
C--- IFUNC=15, Select color index. -------------------------------------
C
  150 CONTINUE
      CI = NINT(RBUF(1))
      IF (COLOR) THEN
          WRITE(INSTR,151) RVALUE(CI), GVALUE(CI), BVALUE(CI)
  151     FORMAT(3(F5.3,1X),'setrgbcolor')
          L = 29
      ELSE
          WRITE(INSTR,'(F5.3,1X,''setgray'')') RVALUE(CI)
          L = 13
      END IF
      LASTI = -1
      CALL GRNE02(0, INSTR(:L), 0)
      RETURN
C
C--- IFUNC=16, Flush buffer. -------------------------------------------
C
  160 CONTINUE
      CALL GRNE02(0, ' ', 1)
      CALL NEXSUP(5, CBUF, RTMP)
      LASTI=-1
      RETURN
C
C--- IFUNC=17, Read cursor. --------------------------------------------
C
  170 CONTINUE
      CALL NEXSUP(4, CBUF, RTMP)
      RBUF(1)=RTMP(1)
      RBUF(2)=RTMP(2)
      NBUF=2
      CHR(1:1)=CHAR(NINT(RTMP(3)))
      LCHR=1
      RETURN
C
C--- IFUNC=18, Erase alpha screen. -------------------------------------
C    (Null operation: there is no alpha screen.)
C
  180 CONTINUE
      RETURN
C
C--- IFUNC=19, Set line style. -----------------------------------------
C    (Not implemented: should not be called.)
C
  190 GOTO 900
C
C--- IFUNC=20, Polygon fill. -------------------------------------------
C
  200 CONTINUE
      IF (NPTS.EQ.0) THEN
          NPTS = RBUF(1)
          START = .TRUE.
          RETURN
      ELSE
          NPTS = NPTS-1
          I0 = NINT(RBUF(1))
          J0 = NINT(RBUF(2))
          IF (START) THEN
              CALL GRFAO('# # BP', L, INSTR, I0, J0, 0, 0)
              START = .FALSE.
              LASTI = I0
              LASTJ = J0
          ELSE IF (NPTS.EQ.0) THEN
              CALL GRFAO('# # EP', L, INSTR, (I0-LASTI),
     1                     (J0-LASTJ), 0, 0)
              LASTI = -1
              LASTJ = -1
          ELSE
              CALL GRFAO('# # LP', L, INSTR, (I0-LASTI),
     1                     (J0-LASTJ), 0, 0)
              LASTI = I0
              LASTJ = J0
          END IF
          CALL GRNE02(0, INSTR(:L), 0)
          RETURN
      END IF
C
C--- IFUNC=21, Set color representation. -------------------------------
C
  210 CONTINUE
      IF (COLOR) THEN
          CI = RBUF(1)
          RVALUE(CI) = RBUF(2)
          GVALUE(CI) = RBUF(3)
          BVALUE(CI) = RBUF(4)
      ELSE
          CI = RBUF(1)
          RVALUE(CI) = 0.30*RBUF(2) + 0.59*RBUF(3) + 0.11*RBUF(4)
          GVALUE(CI) = RVALUE(CI)
          BVALUE(CI) = RVALUE(CI)
      END IF
      RETURN
C
C--- IFUNC=22, Set line width. -----------------------------------------
C
  220 CONTINUE
      LW = NINT(RBUF(1))
      CALL GRFAO('# SLW', L, INSTR, LW, 0, 0, 0)
      LASTI = -1
      CALL GRNE02(0, INSTR(:L), 0)
      RETURN
C
C--- IFUNC=23, Escape. -------------------------------------------------
C
  230 CONTINUE
      CALL GRNE02(0, CHR(:LCHR), 1)
      LASTI = -1
      RETURN
C
C--- IFUNC=26, Image.---------------------------------------------------
C
  260 CONTINUE
      N = RBUF(1)
      IF (N.EQ.0) THEN
C         -- First: setup for image
C         -- Set clipping region (RBUF(2...5))
          NXP = RBUF(2)
          NYP = RBUF(3)
          XORG = RBUF(4)
          XLEN = RBUF(5) - RBUF(4)
          YLEN = RBUF(7) - RBUF(6)
          YORG = RBUF(6)
          CALL GRNE02(0, 'gsave newpath', 0)
          CALL GRFAO('# # moveto # 0 rlineto 0 # rlineto', L, INSTR,
     :               XORG, YORG, XLEN, YLEN)
          CALL GRNE02(0, INSTR(:L), 0)
          CALL GRFAO('# 0 rlineto closepath clip', L, INSTR, -XLEN,
     :                0, 0, 0)
          CALL GRNE02(0, INSTR(:L), 0)
C         --
          CALL GRFAO('/picstr # string def', L, INSTR, NXP, 0, 0, 0)
          CALL GRNE02(0, INSTR(:L), 0)
          CALL GRFAO('# # 8 [', L, INSTR, NXP, NYP, 0, 0)
          CALL GRNE02(0, INSTR(:L), 0)
          WRITE (INSTR, '(6(1PE10.3, 1X), '']'')') (RBUF(I),I=8,13)
          CALL GRNE02(0, INSTR(:67), 0)
          IF (COLOR) THEN
              CALL GRNE02(0,
     :   '{currentfile picstr readhexstring pop} false 3 colorimage',0)
          ELSE
              CALL GRNE02(0,
     :      '{currentfile picstr readhexstring pop} image',0)
          END IF
      ELSE IF (N.EQ.-1) THEN
C         -- Last: terminate image
          CALL GRNE02(0, 'grestore', 0)
      ELSE
C         -- Middle: write N image pixels; each pixel uses 6 chars
C            in INSTR, so N must be <= 20.
          L = 0
          KMAX = 1
          IF (COLOR) KMAX = 3
          DO 262 I=1,N
              CI = RBUF(I+1)
              RGB(1) = NINT(255.0*RVALUE(CI))
              RGB(2) = NINT(255.0*GVALUE(CI))
              RGB(3) = NINT(255.0*BVALUE(CI))
              DO 261 K=1,KMAX
                  HIGH = RGB(K)/16
                  LOW  = RGB(K)-16*HIGH
                  L = L+1
                  INSTR(L:L) = HEXDIG(HIGH)
                  L = L+1
                  INSTR(L:L) = HEXDIG(LOW)
 261          CONTINUE
 262      CONTINUE
          CALL GRNE02(0, INSTR(1:L), 1)
      END IF
      RETURN
C
C--- IFUNC=29, Query color representation.------------------------------
C
 290  CONTINUE
      CI = NINT(RBUF(1))
      NBUF = 4
      RBUF(2) = RVALUE(CI)
      RBUF(3) = GVALUE(CI)
      RBUF(4) = BVALUE(CI)
      RETURN
C
C-----------------------------------------------------------------------
C Error: unimplemented function.
C
  900 WRITE (MSG, 901) IFUNC
  901 FORMAT('Unimplemented function in PS device driver: ',I10)
      CALL GRWARN(MSG)
      NBUF = -1
      RETURN
C-----------------------------------------------------------------------
      END

C*GRNE02 -- PGPLOT NeXT driver, buffer routine.
C+
      SUBROUTINE GRNE02(UNIT, CIN, IFLUSH)
      CHARACTER CIN*(*)
      INTEGER   UNIT, IFLUSH
C
C Support routine for NEdriver: Add string to buffer,
C flushing if needed.  If IFLUSH>0 flush buffer.
C-----------------------------------------------------------------------
      CHARACTER CBUF*132
      INTEGER   LBUF, LIN
      SAVE      CBUF, LBUF
      REAL      RTMP
      DATA      LBUF/0/
C
      LIN = LEN(CIN)
      IF(LIN.GT.0) THEN
         IF(LBUF+LIN+1.GE.LEN(CBUF)) THEN
            CBUF(LBUF+1:LBUF+1)=CHAR(0)
            CALL NEXSUP(3, CBUF, RTMP)
            LBUF=0
         END IF
         IF(LBUF.GT.0) THEN
            CBUF(LBUF+1:LBUF+1)=' '
            LBUF=LBUF+1
         END IF
         CBUF(LBUF+1:LBUF+LIN)=CIN
         LBUF=LBUF+LIN
      END IF
      IF(IFLUSH.GT.0) THEN
         IF(LBUF.GT.0) THEN
            CBUF(LBUF+1:LBUF+1)=CHAR(0)
            CALL NEXSUP(3, CBUF, RTMP)
            LBUF=0
         END IF
      END IF
C-----------------------------------------------------------------------
      END
