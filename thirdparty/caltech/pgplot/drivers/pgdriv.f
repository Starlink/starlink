C*PGDRIV -- PGPLOT metafile driver
C+
      SUBROUTINE PGDRIV (IFUNC, RBUF, NBUF, CHR, LCHR)
      INTEGER IFUNC, NBUF, LCHR
      REAL    RBUF(*)
      CHARACTER*(*) CHR
C
C PGPLOT driver for PGPLOT metafile (private format).
C
C-----------------------------------------------------------------------
      INTEGER DWD, DHT, DRES
      CHARACTER*(*) TYPE, DEFNAM
      PARAMETER (TYPE= 'PGMF   (PGPLOT metafile)')
      PARAMETER (DEFNAM='pgplot.pgmf')
      PARAMETER (DWD=6400, DHT=4800, DRES=1000)
C
      INTEGER WIDTH, HEIGHT
      SAVE    WIDTH, HEIGHT
      INTEGER NSYM, MFAC
      INTEGER  IER, I0, J0, I1, J1, L, LL, LASTI, LASTJ, UNIT, I
      SAVE                                 LASTI, LASTJ, UNIT
      INTEGER  CI, LW, LS, NPTS, NPAGE, IOERR, LFNAME
      SAVE         LW, LS, NPTS, NPAGE, IOERR, LFNAME
      INTEGER  STATE, INPIC
      SAVE     STATE, INPIC
      INTEGER  GROPTX, GRCTOI
      LOGICAL  STDOUT
      SAVE     STDOUT
      INTEGER  RVALUE(0:255), GVALUE(0:255), BVALUE(0:255)
      SAVE     RVALUE,        GVALUE,        BVALUE
      INTEGER  OP(0:255)
      SAVE     OP
      CHARACTER*120 INSTR, MSG
      CHARACTER*255 FNAME
      SAVE          FNAME
      REAL          RINIT(0:15), GINIT(0:15), BINIT(0:15)
      SAVE          RINIT,       GINIT,       BINIT
      DATA RINIT 
     1     / 1.00, 0.00, 1.00, 0.00, 0.00, 0.00, 1.00, 1.00,
     2       1.00, 0.50, 0.00, 0.00, 0.50, 1.00, 0.33, 0.67/
      DATA GINIT
     1     / 1.00, 0.00, 0.00, 1.00, 0.00, 1.00, 0.00, 1.00,
     2       0.50, 1.00, 1.00, 0.50, 0.00, 0.00, 0.33, 0.67/
      DATA BINIT
     1     / 1.00, 0.00, 0.00, 0.00, 1.00, 1.00, 1.00, 0.00,
     2       0.00, 0.00, 0.50, 1.00, 1.00, 0.50, 0.33, 0.67/
      DATA STATE/0/
C-----------------------------------------------------------------------
C
      GOTO( 10, 20, 30, 40, 50, 60, 70, 80, 90,100,
     1     110,120,130,140,150,160,900,180,900,200,
     2     210,220,230,240,900,260,900,280,290), IFUNC
      GOTO 900
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
      RBUF(2) = -1
      RBUF(3) = 0
      RBUF(4) = -1
      RBUF(5) = 0
      RBUF(6) = 255
      NBUF = 6
      RETURN
C
C--- IFUNC = 3, Return device resolution. ------------------------------
C
   30 RBUF(1) = REAL(DRES)
      RBUF(2) = REAL(DRES)
      RBUF(3) = 5
      NBUF = 3
      RETURN
C
C--- IFUNC = 4, Return misc device info. -------------------------------
C
   40 CONTINUE
      CHR = 'HNNATRQNYM'
      LCHR = 10
      RETURN
C
C--- IFUNC = 5, Return default file name. ------------------------------
C
   50 CHR = DEFNAM
      LCHR = LEN(DEFNAM)
      RETURN
C
C--- IFUNC = 6, Return default physical size of plot. ------------------
C
   60 RBUF(1) = 0
      RBUF(3) = 0
      RBUF(2) = WIDTH
      RBUF(4) = HEIGHT
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
C     -- check for concurrent access
      IF (STATE.EQ.1) THEN
         CALL GRWARN('a PGPLOT metafile is already open')
         RBUF(1) = 0
         RBUF(2) = 0
         RETURN
      END IF
      DO 91 CI=0,15
         RVALUE(CI) = NINT(255*RINIT(CI))
         GVALUE(CI) = NINT(255*GINIT(CI))
         BVALUE(CI) = NINT(255*BINIT(CI))
 91   CONTINUE
      DO 93 CI=16,255
         RVALUE(CI) = 0
         GVALUE(CI) = 0
         BVALUE(CI) = 0
 93   CONTINUE
C     -- Device dimensions
      WIDTH = DWD
      HEIGHT = DHT
      CALL GRGENV('PGMF_WIDTH', INSTR, L)
      LL = 1
      IF (L.GT.0) WIDTH = GRCTOI(INSTR(:L),LL)
      CALL GRGENV('PGMF_HEIGHT', INSTR, L)
      LL = 1
      IF (L.GT.0) HEIGHT = GRCTOI(INSTR(:L),LL)
      STDOUT =CHR(1:LCHR).EQ.'-'
      IF (STDOUT) THEN
         UNIT = 6
C        -- machine-dependent!
      ELSE
         CALL GRGLUN(UNIT)
      END IF
      NBUF = 2
      RBUF(1) = UNIT
      IF (.NOT.STDOUT) THEN
         IER = GROPTX(UNIT, CHR(1:LCHR), DEFNAM, 1)
         IF (IER.NE.0) THEN
            MSG = 'Cannot open output file for PGPLOT metafile: '//
     1           CHR(:LCHR)
            CALL GRWARN(MSG)
            RBUF(2) = 0
            CALL GRFLUN(UNIT)
            RETURN
         ELSE
            INQUIRE (UNIT=UNIT, NAME=CHR)
            LCHR = LEN(CHR)
 94         IF (CHR(LCHR:LCHR).EQ.' ') THEN
               LCHR = LCHR-1
               GOTO 94
            END IF
            RBUF(2) = 1
            FNAME = CHR(:LCHR)
            LFNAME = LCHR
         END IF
      ELSE
         RBUF(2) = 1
         FNAME = '-'
         LFNAME= 1
      END IF
      STATE = 1
      IOERR = 0
      LASTI = -1
      LASTJ = -1
      LW = 1
      LS = 1
      NPTS = 0
      INPIC = 0
      NPAGE = 0
      CALL GRPG02(IOERR, UNIT, '%PGMF (PGPLOT metafile)')
      CALL GRUSER(INSTR, L)
      IF (L.GT.0) CALL GRPG02(IOERR, UNIT, '% Creator: '//INSTR(1:L))
      CALL GRDATE(INSTR, L)
      IF (L.GT.0) CALL GRPG02(IOERR, UNIT, '% Date: '//INSTR(1:L))
      RETURN
C
C--- IFUNC=10, Close workstation. --------------------------------------
C
  100 CONTINUE
      IF (.NOT.STDOUT) THEN
         CLOSE (UNIT, IOSTAT=IOERR)
         IF (IOERR.NE.0) THEN
           CALL GRWARN('Error closing PGPLOT metafile '//FNAME(:LFNAME))
         END IF
         CALL GRFLUN(UNIT)
      END IF
      STATE = 0
      RETURN
C
C--- IFUNC=11, Begin picture. ------------------------------------------
C
 110  CONTINUE
      WIDTH = RBUF(1)
      HEIGHT = RBUF(2)
      NPAGE = NPAGE+1
      INPIC = 1
      CALL GRFAO('B# # # #', L, INSTR, NPAGE, WIDTH, HEIGHT, DRES)
      CALL GRPG02(IOERR, UNIT, INSTR(:L))
      DO 111 I=0,255
         OP(I) = 0
 111  CONTINUE
      CALL GRFAO('C# # # #', L, INSTR, 0, RVALUE(0), GVALUE(0),
     :        BVALUE(0))
      CALL GRPG02(IOERR, UNIT, INSTR(1:L))
      OP(0) = 1
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
C        -- suppress zero-length continuation segment
         IF (I0.EQ.I1 .AND. J0.EQ.J1) RETURN
         CALL GRFAO('L# #', L, INSTR, (I1-I0), (J1-J0), 0, 0)
         CALL GRPG02(IOERR, UNIT, INSTR(:L))
      ELSE
         CALL GRFAO('M# #', L, INSTR, I0, J0, 0, 0)
         CALL GRPG02(IOERR, UNIT, INSTR(:L))
         CALL GRFAO('L# #', L, INSTR, (I1-I0), (J1-J0), 0, 0)
         CALL GRPG02(IOERR, UNIT, INSTR(:L))
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
      CALL GRFAO('D# #', L, INSTR, I1, J1, 0, 0)
      CALL GRPG02(IOERR, UNIT, INSTR(:L))
      LASTI = I1
      LASTJ = J1
      RETURN
C
C--- IFUNC=14, End picture. --------------------------------------------
C
  140 CONTINUE
      CALL GRPG02(IOERR, UNIT, 'E')
      INPIC = 0
      RETURN
C
C--- IFUNC=15, Select color index. -------------------------------------
C
  150 CONTINUE
      CI = NINT(RBUF(1))
      IF (INPIC.EQ.1) THEN
         IF (OP(CI).EQ.0) THEN
            CALL GRFAO('C# # # #', L, INSTR, CI, RVALUE(CI),
     :           GVALUE(CI), BVALUE(CI))
            CALL GRPG02(IOERR, UNIT, INSTR(1:L))
            OP(CI) = 1
         END IF
         CALL GRFAO('I#', L, INSTR, CI, 0, 0, 0)
         CALL GRPG02(IOERR, UNIT, INSTR(:L))
         LASTI = -1
      END IF
      RETURN
C
C--- IFUNC=16, Flush buffer. -------------------------------------------
C
  160 CONTINUE
      RETURN
C
C--- IFUNC=18, Erase alpha screen. -------------------------------------
C    (Null operation: there is no alpha screen.)
C
 180  CONTINUE
      RETURN
C
C--- IFUNC=20, Polygon fill. -------------------------------------------
C
 200  CONTINUE
      IF (NPTS.EQ.0) THEN
          NPTS = RBUF(1)
          CALL GRFAO('Y#', L, INSTR, NPTS, 0, 0, 0)
      ELSE
          NPTS = NPTS-1
          I0 = NINT(RBUF(1))
          J0 = NINT(RBUF(2))
          CALL GRFAO('X# #', L, INSTR, I0, J0, 0, 0)
      END IF
      CALL GRPG02(IOERR, UNIT, INSTR(1:L))
      LASTI = -1
      RETURN
C
C--- IFUNC=21, Set color representation. -------------------------------
C
  210 CONTINUE
      CI = RBUF(1)
      RVALUE(CI) = NINT(255*RBUF(2))
      GVALUE(CI) = NINT(255*RBUF(3))
      BVALUE(CI) = NINT(255*RBUF(4))
      OP(CI) = 0
      RETURN
C
C--- IFUNC=22, Set line width. -----------------------------------------
C    (Convert requested line width, unit 1/200 inch, to device units)
C
  220 CONTINUE
      LW = NINT(DRES*RBUF(1)/200.0)
      CALL GRFAO('W#', L, INSTR, LW, 0, 0, 0)
      CALL GRPG02(IOERR, UNIT, INSTR(1:L))
      LASTI = -1
      RETURN
C
C--- IFUNC=23, Escape. -------------------------------------------------
C
  230 CONTINUE
      CALL GRPG02(IOERR, UNIT, CHR(:LCHR))
      LASTI = -1
      RETURN
C
C--- IFUNC=24, Rectangle fill. -----------------------------------------
C
  240 CONTINUE
      CALL GRFAO('R# # # #', L, INSTR, NINT(RBUF(1)), NINT(RBUF(2)),
     :           NINT(RBUF(3)), NINT(RBUF(4)))
      CALL GRPG02(IOERR, UNIT, INSTR(1:L))
      LASTI = -1
      RETURN
C
C--- IFUNC=26, Image.---------------------------------------------------
C
  260 CONTINUE
C     Not yet implemented
      RETURN
C
C--- IFUNC=28, Marker.--------------------------------------------------
C
  280 CONTINUE
      NSYM = NINT(RBUF(1))
      I1 = NINT(RBUF(2))
      J1 = NINT(RBUF(3))
      MFAC = NINT(1000.0*RBUF(4))
      CALL GRFAO('S# # # #', L, INSTR, NSYM, I1, J1, MFAC)
      CALL GRPG02(IOERR, UNIT, INSTR(1:L))
      LASTI = -1
      RETURN
C
C--- IFUNC=29, Query color representation.------------------------------
C
 290  CONTINUE
      CI = NINT(RBUF(1))
      NBUF = 4
      RBUF(2) = RVALUE(CI)/255.0
      RBUF(3) = GVALUE(CI)/255.0
      RBUF(4) = BVALUE(CI)/255.0
      RETURN
C
C-----------------------------------------------------------------------
C Error: unimplemented function.
C
  900 WRITE (MSG,
     1  '(''Unimplemented function in PG device driver: '',I10)') IFUNC
      CALL GRWARN(MSG)
      NBUF = -1
      RETURN
C-----------------------------------------------------------------------
      END

C*GRPG02 -- PGPLOT metafile driver, copy buffer to file
C+
      SUBROUTINE GRPG02 (IER, UNIT, S)
C
C Support routine for PGdriver: write character string S on
C specified Fortran unit.
C
C Error handling: if IER is not 0 on input, the routine returns
C immediately. Otherwise IER receives the I/O status from the Fortran
C write (0 => success).
C-----------------------------------------------------------------------
      INTEGER IER, UNIT
      CHARACTER*(*) S
C
      IF (IER.EQ.0) THEN
          WRITE (UNIT, '(A)', IOSTAT=IER) S
          IF (IER.NE.0) CALL 
     1        GRWARN('++WARNING++ Error writing PGPLOT metafile')
      END IF
C-----------------------------------------------------------------------
      END
