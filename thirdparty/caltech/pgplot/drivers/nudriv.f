C*NUDRIV -- PGPLOT Null device driver
C+
      SUBROUTINE NUDRIV (IFUNC, RBUF, NBUF, CHR, LCHR)
      INTEGER IFUNC, NBUF, LCHR
      REAL    RBUF(*)
      CHARACTER*(*) CHR
C
C PGPLOT driver for Null device (no graphical output)
C
C Version 1.0  - 1987 May 26 - T. J. Pearson.
C Version 1.1  - 1988 Mar 23 - add rectangle fill.
C Version 1.2  - 1992 Sep  3 - add line-of-pixels.
C Version 1.3  - 1992 Sep 21 - add markers.
C Version 1.4  - 1993 Apr 22 - add optional debugging.
C Version 1.5  - 1994 Aug 31 - use image primitives.
C Version 2.0  - 1996 Jan 22 - allow multiple active devices;
C                              add QCR primitive.
C Version 2.1  - 1997 Jun 13 - correctly initialize STATE.
C
C Supported device: The ``null'' device can be used to suppress
C all graphic output from a program.  If environment variable
C PGPLOT_DEBUG is defined, some debugging information is
C reported on standard output.
C
C Device type code: /NULL.
C
C Default device name: None (the device name, if specified, is 
C ignored).
C
C Default view surface dimensions: Undefined (The device pretends to
C be a hardcopy device with 1000 pixels/inch and a view surface 8in 
C high by 10.5in wide.)
C
C Resolution: Undefined.
C
C Color capability: Color indices 0--255 are accepted.
C
C Input capability: None.
C
C File format: None.
C
C Obtaining hardcopy: Not possible.
C-----------------------------------------------------------------------
C Notes:
C  Up to MAXDEV "devices" may be open at once. ACTIVE is the number
C  of the currently selected device, or 0 if no devices are open.
C  STATE(i) is 0 if device i is not open, 1 if it is open but with
C  no current picture, or 2 if it is open with a current picture.
C
C  When debugging is enabled, open/close device and begin/end picture
C  calls are reported on stdout, and a cumulative count of all
C  driver calls is kept.
C-----------------------------------------------------------------------
      CHARACTER*(*) DEVICE
      PARAMETER (DEVICE='NULL  (Null device, no output)')
      INTEGER MAXDEV, MAXD1
      PARAMETER (MAXDEV=8)
      PARAMETER (MAXD1=MAXDEV+1)
      INTEGER NOPCOD
      PARAMETER (NOPCOD=29)
      CHARACTER*10 MSG
      CHARACTER*32 TEXT
      CHARACTER*8  LAB(NOPCOD)
      INTEGER COUNT(NOPCOD), I, STATE(0:MAXDEV), L, NPIC(MAXDEV)
      INTEGER ACTIVE
      LOGICAL DEBUG
      INTEGER CTABLE(3,0:255), CDEFLT(3,0:15)
      SAVE COUNT, STATE, NPIC, DEBUG, CTABLE, CDEFLT, ACTIVE
C
      DATA ACTIVE/-1/
      DATA STATE/MAXD1*0/
      DATA COUNT/NOPCOD*0/
      DATA DEBUG/.FALSE./
      DATA LAB  /'qdev    ', 'qmaxsize', 'qscale  ', 'qcapab  ',
     1           'qdefnam ', 'qdefsize', 'qmisc   ', 'select  ',
     2           'open    ', 'close   ', 'beginpic', 'line    ',
     3           'dot     ', 'endpic  ', 'set CI  ', 'flush   ',
     4           'cursor  ', 'eralpha ', 'set LS  ', 'polygon ',
     5           'set CR  ', 'set LW  ', 'escape  ', 'rectangl',
     6           'set patt', 'pix/imag', 'scaling ', 'marker  ',
     7           'query CR'/
      DATA CDEFLT /000,000,000, 255,255,255, 255,000,000, 000,255,000,
     1             000,000,255, 000,255,255, 255,000,255, 255,255,000,
     2             255,128,000, 128,255,000, 000,255,128, 000,128,255,
     3             128,000,255, 255,000,128, 085,085,085, 170,170,170/
C-----------------------------------------------------------------------
C
      IF (ACTIVE.EQ.-1) THEN
           CALL GRGENV('DEBUG', TEXT, L)
           DEBUG = L.GT.0
           ACTIVE = 0
      END IF
C
      IF (IFUNC.LT.1 .OR. IFUNC.GT.NOPCOD) GOTO 900
      COUNT(IFUNC) = COUNT(IFUNC) + 1
      GOTO( 10, 20, 30, 40, 50, 60, 70, 80, 90,100,
     1     110,120,130,140,150,160,170,180,190,200,
     2     210,220,230,240,250,260,270,280,290), IFUNC
  900 WRITE (MSG, '(I10)') IFUNC
      CALL GRWARN('Unimplemented function in NULL device driver: '//MSG)
      NBUF = -1
      RETURN
C
C--- IFUNC = 1, Return device name.-------------------------------------
C
   10 CHR = DEVICE
      LCHR = LEN(DEVICE)
      RETURN
C
C--- IFUNC = 2, Return physical min and max for plot device, and range
C               of color indices.---------------------------------------
C
   20 RBUF(1) = 0
      RBUF(2) = 65535
      RBUF(3) = 0
      RBUF(4) = 65535
      RBUF(5) = 0
      RBUF(6) = 255
      NBUF = 6
      RETURN
C
C--- IFUNC = 3, Return device resolution. ------------------------------
C
   30 RBUF(1) = 1000.0
      RBUF(2) = 1000.0
      RBUF(3) = 1
      NBUF = 3
      RETURN
C
C--- IFUNC = 4, Return misc device info. -------------------------------
C    (This device is Hardcopy, No cursor, Dashed lines, Area fill, Thick
C    lines, Rectangle fill, Images, , , Markers, query color rep)
C
   40 CHR = 'HNDATRQNYM'
      LCHR = 10
      RETURN
C
C--- IFUNC = 5, Return default file name. ------------------------------
C
   50 CHR = 'NL:'
      LCHR = 3
      RETURN
C
C--- IFUNC = 6, Return default physical size of plot. ------------------
C
   60 RBUF(1) = 0
      RBUF(2) = 10499
      RBUF(3) = 0
      RBUF(4) = 7999
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
      I = RBUF(2) - 67890
      IF (I.LT.1 .OR. I.GT.MAXDEV) THEN
         CALL GRWARN('internal error: NULL opcode 8')
      ELSE IF (STATE(I).GT.0) THEN
         ACTIVE = I
      ELSE
         CALL GRNU00(IFUNC,0)
      END IF
      RETURN
C
C--- IFUNC = 9, Open workstation. --------------------------------------
C
   90 CONTINUE
C     -- Find an inactive device, and select it
      DO 91 I=1,MAXDEV
         IF (STATE(I).EQ.0) THEN
            ACTIVE = I
            STATE(ACTIVE) = 1
            GOTO 92
         END IF
 91   CONTINUE
      IF (DEBUG) CALL GRWARN ('09 Open workstation')
      CALL GRWARN('maximum number of devices of type NULL exceeded')
      RBUF(1) = 0
      RBUF(2) = 0 
      NBUF = 2
      RETURN
C     -- Initialize the new device
 92   CONTINUE
      RBUF(1) = ACTIVE + 67890
      RBUF(2) = 1
      NBUF = 2
      NPIC(ACTIVE) = 0
C     -- Initialize color table
      DO 95 I=0,15
         CTABLE(1,I) = CDEFLT(1,I)
         CTABLE(2,I) = CDEFLT(2,I)
         CTABLE(3,I) = CDEFLT(3,I)
 95   CONTINUE
      DO 96 I=16,255
         CTABLE(1,I) = 128
         CTABLE(2,I) = 128
         CTABLE(3,I) = 128
 96   CONTINUE
      IF (DEBUG) THEN
         CALL GRFAO('09 Open workstation: device #',
     :        L, TEXT, ACTIVE, 0, 0, 0)
         CALL GRWARN(TEXT(1:L))
      END IF
      RETURN
C
C--- IFUNC=10, Close workstation. --------------------------------------
C
  100 CONTINUE
      IF (STATE(ACTIVE).NE.1) CALL GRNU00(IFUNC,STATE(ACTIVE))
      STATE(ACTIVE) = 0
      IF (DEBUG) THEN
         CALL GRFAO('10 Close workstation: device #',
     :        L, TEXT, ACTIVE, 0, 0, 0)
         CALL GRWARN(TEXT(1:L))
         CALL GRWARN('Device driver calls:')
         DO 101 I=1,NOPCOD
            IF (COUNT(I).GT.0) THEN
               WRITE (TEXT,'(3X,I2,1X,A8,I10)') I, LAB(I), COUNT(I)
               CALL GRWARN(TEXT)
            END IF
 101     CONTINUE
      END IF
      RETURN
C
C--- IFUNC=11, Begin picture. ------------------------------------------
C
  110 CONTINUE
      IF (STATE(ACTIVE).NE.1) CALL GRNU00(IFUNC,STATE(ACTIVE))
      STATE(ACTIVE) = 2
      NPIC(ACTIVE) = NPIC(ACTIVE)+1
      IF (DEBUG) THEN
         CALL GRFAO('11   Begin picture # on device #',
     :        L, TEXT, NPIC(ACTIVE), ACTIVE, 0,0)
         CALL GRWARN(TEXT(:L))
      END IF
      RETURN
C
C--- IFUNC=12, Draw line. ----------------------------------------------
C
  120 CONTINUE
      IF (STATE(ACTIVE).NE.2) CALL GRNU00(IFUNC,STATE(ACTIVE))
      RETURN
C
C--- IFUNC=13, Draw dot. -----------------------------------------------
C
  130 CONTINUE
      IF (STATE(ACTIVE).NE.2) CALL GRNU00(IFUNC,STATE(ACTIVE))
      RETURN
C
C--- IFUNC=14, End picture. --------------------------------------------
C
  140 CONTINUE
      IF (STATE(ACTIVE).NE.2) CALL GRNU00(IFUNC,STATE(ACTIVE))
      STATE(ACTIVE) = 1
      IF (DEBUG) THEN
         CALL GRFAO('14   End picture   # on device #',
     :        L, TEXT, NPIC(ACTIVE), ACTIVE, 0,0)
         CALL GRWARN(TEXT(:L))
      END IF
      RETURN
C
C--- IFUNC=15, Select color index. -------------------------------------
C
  150 CONTINUE
      IF (STATE(ACTIVE).LT.1) CALL GRNU00(IFUNC,STATE(ACTIVE))
      RETURN
C
C--- IFUNC=16, Flush buffer. -------------------------------------------
C
  160 CONTINUE
      IF (STATE(ACTIVE).LT.1) CALL GRNU00(IFUNC,STATE(ACTIVE))
      RETURN
C
C--- IFUNC=17, Read cursor. --------------------------------------------
C    (Not implemented: should not be called.)
C
  170 GOTO 900
C
C--- IFUNC=18, Erase alpha screen. -------------------------------------
C
  180 CONTINUE
      IF (STATE(ACTIVE).LT.1) CALL GRNU00(IFUNC,STATE(ACTIVE))
      RETURN
C
C--- IFUNC=19, Set line style. -----------------------------------------
C
  190 CONTINUE
      IF (STATE(ACTIVE).NE.2) CALL GRNU00(IFUNC,STATE(ACTIVE))
      RETURN
C
C--- IFUNC=20, Polygon fill. -------------------------------------------
C
  200 CONTINUE
      IF (STATE(ACTIVE).NE.2) CALL GRNU00(IFUNC,STATE(ACTIVE))
      RETURN
C
C--- IFUNC=21, Set color representation. -------------------------------
C
  210 CONTINUE
      IF (STATE(ACTIVE).LT.1) CALL GRNU00(IFUNC,STATE(ACTIVE))
      I = RBUF(1)
      CTABLE(1, I) = NINT(RBUF(2)*255)
      CTABLE(2, I) = NINT(RBUF(3)*255)
      CTABLE(3, I) = NINT(RBUF(4)*255)
      RETURN
C
C--- IFUNC=22, Set line width. -----------------------------------------
C
  220 CONTINUE
      IF (STATE(ACTIVE).NE.2) CALL GRNU00(IFUNC,STATE(ACTIVE))
      RETURN
C
C--- IFUNC=23, Escape. -------------------------------------------------
C
  230 CONTINUE
      RETURN
C
C--- IFUNC=24, Rectangle fill. -----------------------------------------
C
  240 CONTINUE
      IF (DEBUG.AND.STATE(ACTIVE).NE.2) CALL GRNU00(IFUNC,STATE(ACTIVE))
      RETURN
C
C--- IFUNC=25, Not implemented -----------------------------------------
C
  250 CONTINUE
      RETURN
C
C--- IFUNC=26, Line of pixels ------------------------------------------
C
  260 CONTINUE
      IF (STATE(ACTIVE).NE.2) CALL GRNU00(IFUNC,STATE(ACTIVE))
      RETURN
C
C--- IFUNC=27, Scaling info -- -----------------------------------------
C
  270 CONTINUE
      IF (STATE(ACTIVE).NE.2) CALL GRNU00(IFUNC,STATE(ACTIVE))
      RETURN
C
C--- IFUNC=28, Draw marker ---------------------------------------------
C
  280 CONTINUE
      IF (STATE(ACTIVE).NE.2) CALL GRNU00(IFUNC,STATE(ACTIVE))
C     WRITE (*,'(1X,A,I4,1X,3F10.1)') 'MARKER', NINT(RBUF(1)), RBUF(2),
C    1      RBUF(3), RBUF(4)
      RETURN
C
C--- IFUNC=29, Query color representation. -----------------------------
C
  290 CONTINUE
      IF (STATE(ACTIVE).LT.1) CALL GRNU00(IFUNC,STATE(ACTIVE))
      I = RBUF(1)
      RBUF(2) = CTABLE(1,I)/255.0
      RBUF(3) = CTABLE(2,I)/255.0
      RBUF(4) = CTABLE(3,I)/255.0
      NBUF = 4
      RETURN
C-----------------------------------------------------------------------
      END

      SUBROUTINE GRNU00(IFUNC, STATE)
      INTEGER IFUNC, STATE
C
C PGPLOT NULL device driver: report error
C-----------------------------------------------------------------------
      INTEGER L
      CHARACTER*80 MSG
C
      CALL GRFAO('++ internal error: driver in state # for opcode #',
     :           L, MSG, STATE, IFUNC, 0, 0)
      CALL GRWARN(MSG(1:L))
      RETURN
      END
