      SUBROUTINE GLDRIV (IFUNC, RBUF, NBUF, CHR, LCHR, MODE)
      INTEGER IFUNC, NBUF, LCHR, MODE
      REAL    RBUF(*)
      CHARACTER*(*) CHR
C-----------------------------------------------------------------------
C PGPLOT driver for Hewlett Packard HPGL plotter(s).
C-----------------------------------------------------------------------
C Version 1.0  - 1988 Mar 14 - B. H. Toby
C                9/88 bull: added page eject to IFUNC 14
C Version 2.0  - 1994 Mar 16 - T. J. Pearson (dual mode, standard F77)
C Version 2.1  - 1994 Nov  6 - TJP: use PGPLOT_GL_TERMINAL.
C Version 3.0  - 1997 Jun 11 - TJP: add SC to specify page size.
C Version 3.1  - 1998 Oct 23 - TJP: correct failure to close file.
C Version 3.2  - 1998 Nov 9  - TJP: allow up to 8 pens.
C-----------------------------------------------------------------------
C     This routine has been written specifically for the HP7475A
C     Plotter, but should support most HPGL devices, perhaps with
C     minor modifications.
C
C     Color:
C       Color Index 1-8 are mapped to requests for pens 1-8, so
C        the actual color depends on what pen is installed. 
C       Color representation requests (PGSCR) are ignored.
C
C     If environment variable PGPLOT_GL_TERMINAL has value YES (or any
C     string beginning with Y or y), it is assumed that the
C     output device is a plotter connected BEFORE the terminal using
C     the Y-cable (HP part #17455A), in which case the plotter is
C     ``turned on'' using a "<ESC>.(" and xon/xoff handshaking is
C     enabled using "<ESC>.I81;;17:" and "<ESC>.N;19:". 
C     Otherwise, it is the user's responsibility to
C     add control codes, if needed.
C
C     If there is more than one plot and the plot is on a terminal,
C     a prompt will be generated, allowing the page to be advanced.
C
C     ref. HP 7475A Interfacing and Programming Manual P/N 7475-90001
C-----------------------------------------------------------------------
      CHARACTER*(*) LTYPE, PTYPE, DEFNAM
      PARAMETER (LTYPE=
     :  'HPGL  (Hewlett Packard HPGL plotter, landscape orientation)')
      PARAMETER (PTYPE=
     :  'VHPGL (Hewlett Packard HPGL plotter, portrait orientation)')
      PARAMETER (DEFNAM='pgplot.hpgl')
      CHARACTER*80 MSG
      CHARACTER*80 INSTR
      INTEGER LASTI, LASTJ, UNIT, IC, IER
      INTEGER I0, J0, I1, J1, L
      INTEGER I, PLOTNO
      LOGICAL ITERM
      INTEGER GRGCOM, GROPTX
      SAVE
C-----------------------------------------------------------------------
C
      GOTO( 10, 20, 30, 40, 50, 60, 70, 80, 90,100,
     1     110,120,130,140,150,160,170,180,190,200,
     2     210,220,230), IFUNC
      GOTO 900
C
C--- IFUNC = 1, Return device name.-------------------------------------
C
10    IF (MODE.EQ.1) THEN
C         -- landscape (mode=1)
          CHR = LTYPE
          LCHR = LEN(LTYPE)
      ELSE
C         -- portrait (mode=2)
          CHR = PTYPE
          LCHR = LEN(PTYPE)
      END IF
      RETURN
C
C--- IFUNC = 2, Return physical min and max for plot device, and range
C               of color indices.---------------------------------------
C
20    RBUF(1) = 0
      RBUF(3) = 0
      RBUF(5) = 1
      RBUF(6) = 8
      NBUF = 6
      IF (MODE.EQ.1) THEN
C        -- landscape (mode=1)
          RBUF(2) = 16640
          RBUF(4) = 11040
      ELSE
C         -- portrait (mode=2)
          RBUF(2) = 11040
          RBUF(4) = 16640
      END IF
      RETURN
C
C--- IFUNC = 3, Return device resolution. ------------------------------
C
30    RBUF(1) = 1016.0
      RBUF(2) = 1016.0
      RBUF(3) = 20
      NBUF = 3
      RETURN
C
C--- IFUNC = 4, Return misc device info. -------------------------------
C    (This device is Hardcopy, No cursor, No dashed lines, No area fill,
C    No thick lines)
C
40    CHR = 'HNNNNNNNNN'
      LCHR = 10
      RETURN
C
C--- IFUNC = 5, Return default file name. ------------------------------
C
50    CHR = DEFNAM
      LCHR = LEN(DEFNAM)
      RETURN
C
C--- IFUNC = 6, Return default physical size of plot. ------------------
C
60    RBUF(1) = 0
      RBUF(3) = 0
      NBUF = 4
      IF (MODE.EQ.1) THEN
C         -- landscape (mode=1)
          RBUF(2) = 10365
          RBUF(4) =  7962
      ELSE
C         -- portrait (mode=2)
          RBUF(2) =  7962
          RBUF(4) = 10365
      END IF
      RETURN
C
C--- IFUNC = 7, Return misc defaults. ----------------------------------
C
70    RBUF(1) = 10
      NBUF = 1
      RETURN
C
C--- IFUNC = 8, Select plot. -------------------------------------------
C
80    CONTINUE
      RETURN
C
C--- IFUNC = 9, Open workstation. --------------------------------------
C
90    CONTINUE
C Try to open the graphics device
      CALL GRGLUN(UNIT)
      IER = GROPTX(UNIT, CHR(1:LCHR), DEFNAM, 1)
      IF (IER.NE.0) THEN
        MSG='Cannot open graphics device '//CHR(1:LCHR)
        CALL GRWARN(MSG)
        RBUF(2) = 0
        RETURN
      END IF
C is the output device a terminal?
      CALL GRGENV('GL_TERMINAL', INSTR, L)
      ITERM = (INSTR(1:1).EQ.'Y' .OR. INSTR(1:1).EQ.'y')
      RBUF(1) = UNIT
      RBUF(2) = 1
      NBUF = 2
      LASTI = -1
      LASTJ = -1
      IF (ITERM) THEN
C this turns on the plotter
        WRITE (UNIT, '(A)') CHAR(27)//'.('
C this sets up Xon/Xoff protocol
        WRITE (UNIT, '(A)') CHAR(27)//'.I81;;17:'
        WRITE (UNIT, '(A)') CHAR(27)//'.N;19:'
      ENDIF
      IF (MODE.EQ.1) THEN
C         -- landscape (mode=1)
          WRITE (UNIT, '(A)') 'IN;'
      ELSE
C         -- portrait (mode=2)
          WRITE (UNIT, '(A)') 'IN;RO90;IP;IW;'
      END IF
      PLOTNO = 1
      RETURN
C
C--- IFUNC=10, Close workstation. --------------------------------------
C
  100 CONTINUE
      WRITE (UNIT, '(A)') 'SP;'
      IF (ITERM) THEN
C this turns off the plotter
         WRITE (UNIT, '(A)') CHAR(27)//'.)'
      ENDIF
      CLOSE (UNIT)
      CALL GRFLUN(UNIT)
      RETURN
C
C--- IFUNC=11, Begin picture. ------------------------------------------
C
  110 CONTINUE
C if the plot is interactive, and we are starting a second or third (...)
C picture, allow a chance to change the paper.
      IF (ITERM .AND. PLOTNO .GT. 1) THEN
C turn off the plotter
        WRITE (UNIT, '(A)') CHAR(27)//'.)'
C send a prompt
        IER = GRGCOM(MSG, CHAR(7)//
     1            'Reload paper, then press <RETURN>: ', I)
C turn on the plotter
        WRITE (UNIT, '(A)') CHAR(27)//'.('
      ENDIF
      CALL GRFAO('SC0,#,0,#;SP1;', L, INSTR,
     :   NINT(RBUF(1)), NINT(RBUF(2)), 0, 0)
      WRITE (UNIT, '(A)') INSTR(:L)
      PLOTNO = PLOTNO + 1
      WRITE (UNIT, '(A)') 'PA;'
      RETURN
C
C--- IFUNC=12, Draw line. ----------------------------------------------
C
  120 CONTINUE
      I0 = NINT(RBUF(1))
      J0 = NINT(RBUF(2))
      I1 = NINT(RBUF(3))
      J1 = NINT(RBUF(4))
      IF ( (I0.NE.LASTI) .OR. (J0.NE.LASTJ) ) THEN
C -- move with pen up
C     -- Encode the coordinates into the command string
      CALL GRFAO('PU#,#;', L, INSTR, I0, J0, 0, 0)
C     -- Write the command string to the plot file
      WRITE (UNIT, '(A)') INSTR(:L)
      ENDIF
C -- move with pen down
C     -- Encode the coordinates into the command string
      CALL GRFAO('PD#,#;', L, INSTR, I1, J1, 0, 0)
C     -- Write the command string to the plot file
      WRITE (UNIT, '(A)') INSTR(:L)
      LASTI = I1
      LASTJ = J1
      RETURN
C
C--- IFUNC=13, Draw dot. -----------------------------------------------
C
  130 CONTINUE
      I0 = NINT(RBUF(1))
      J0 = NINT(RBUF(2))
      IF ((I0.NE.LASTI) .OR. (J0.NE.LASTJ)) THEN
C -- move with pen up
C     -- Encode the coordinates into the command string
        CALL GRFAO('PU#,#;PD;', L, INSTR, I0, J0, 0, 0)
C     -- Write the command string to the plot file
        WRITE (UNIT, '(A)') INSTR(:L)
      ELSE
C -- no need to move, just lower the pen
        WRITE (UNIT, '(A)') 'PD;'
      ENDIF
      LASTI = I0
      LASTJ = J0
      RETURN
C
C--- IFUNC=14, End picture. --------------------------------------------
C
  140 CONTINUE
C  move the pen off the page
      WRITE (UNIT, '(A)') 'PU32000,32000;PG;'
      RETURN
C
C--- IFUNC=15, Select color index. -------------------------------------
C
  150 CONTINUE
      IC = NINT(RBUF(1))
      IF (IC.LT.1) IC = 1
      WRITE (UNIT,'(A,I2,A)') 'SP',IC,';'
      RETURN
C
C--- IFUNC=16, Flush buffer. -------------------------------------------
C    (Null operation: buffering is not implemented.)
C
160   CONTINUE
      RETURN
C
C--- IFUNC=17, Read cursor. --------------------------------------------
C    (Not implemented: should not be called.)
C
170   GOTO 900
C
C--- IFUNC=18, Erase alpha screen. -------------------------------------
C    (Null operation: there is no alpha screen.)
C
180   CONTINUE
      RETURN
C
C--- IFUNC=19, Set line style. -----------------------------------------
C    (Not implemented: should not be called.)
C
190   GOTO 900
C
C--- IFUNC=20, Polygon fill. -------------------------------------------
C    (Not implemented: should not be called.)
C
200   GOTO 900
C
C--- IFUNC=21, Set color representation. -------------------------------
C    IGnored.
C
210   CONTINUE
      RETURN
C
C--- IFUNC=22, Set line width. -----------------------------------------
C    (Not implemented: should not be called.)
C
220   GOTO 900
C
C--- IFUNC=23, Escape. -------------------------------------------------
C
230   CONTINUE
      WRITE (UNIT, '(A)') CHR(:LCHR)
      LASTI = -1
      RETURN
C-----------------------------------------------------------------------
C Error: unimplemented function.
C
  900 WRITE (MSG,'(I10)') IFUNC
      CALL GRWARN('Unimplemented function in HPGL device driver: '//MSG)
      NBUF = -1
      RETURN
C-----------------------------------------------------------------------
      END




