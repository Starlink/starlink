C*HIDRIV -- PGPLOT HIDMP plotter driver
C+
      SUBROUTINE HIDRIV (IFUNC, RBUF, NBUF, CHR, LCHR)
      INTEGER IFUNC, NBUF, LCHR
      REAL    RBUF(*)
      CHARACTER*(*) CHR
C
C PGPLOT driver for Houston Instruments HIDMP pen plotter.
C
C Version 1.0  - 1987 May 26 - T. J. Pearson.
C version 2.0  - 1992 Apr  7 - standard Fortran (TJP).
C-----------------------------------------------------------------------
      CHARACTER*(*) DEVICE
      CHARACTER*(*) DEFNAM
      PARAMETER (DEVICE='HIDMP (Houston Instruments pen plotter)')
      PARAMETER (DEFNAM='pgplot.hiplot')
      CHARACTER*80 MSG
      CHARACTER*64 INSTR
      INTEGER      IER, I0, J0, I1, J1, L, LASTI, LASTJ, UNIT
      INTEGER      GROPTX
C-----------------------------------------------------------------------
C
      GOTO( 10, 20, 30, 40, 50, 60, 70, 80, 90,100,
     1     110,120,130,140,150,160,170,180,190,200,
     2     210,220,230), IFUNC
      GOTO 900
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
      RBUF(2) = 4799
      RBUF(3) = 0
      RBUF(4) = 7199
      RBUF(5) = 1
      RBUF(6) = 1
      NBUF = 6
      RETURN
C
C--- IFUNC = 3, Return device resolution. ------------------------------
C
   30 RBUF(1) = 200.0
      RBUF(2) = 200.0
      RBUF(3) = 2
      NBUF = 3
      RETURN
C
C--- IFUNC = 4, Return misc device info. -------------------------------
C    (This device is Hardcopy, No cursor, No dashed lines, No area fill, 
C    No thick lines)
C
   40 CHR = 'HNNNNNNNNN'
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
      RBUF(2) = 2099
      RBUF(3) = 0
      RBUF(4) = 1599
      NBUF = 4
      RETURN
C
C--- IFUNC = 7, Return misc defaults. ----------------------------------
C
   70 RBUF(1) = 3
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
      CALL GRGLUN(UNIT)
      NBUF = 2
      RBUF(1) = UNIT
      IER = GROPTX(UNIT, CHR(1:LCHR), DEFNAM, 1)
      IF (IER.NE.0) THEN
          MSG = 'Cannot open plot file: '//CHR(:LCHR)
          CALL GRWARN(MSG)
          RBUF(2) = 0
          CALL GRFLUN(UNIT)
          RETURN
      ELSE
          INQUIRE (UNIT=UNIT, NAME=CHR)
          LCHR = LEN(CHR)
   91     IF (CHR(LCHR:LCHR).EQ.' ') THEN
              LCHR = LCHR-1
              GOTO 91
          END IF
          RBUF(2) = 1
      END IF
      LASTI = -1
      LASTJ = -1
      WRITE (UNIT, '(A)') ';:H EC5 A '
      RETURN
C
C--- IFUNC=10, Close workstation. --------------------------------------
C
  100 CONTINUE
      WRITE (UNIT, '(A)') 'H EL @ '
      CLOSE (UNIT)
      CALL GRFLUN(UNIT)
      RETURN
C
C--- IFUNC=11, Begin picture. ------------------------------------------
C
  110 CONTINUE
      WRITE (UNIT, '(A)') 'EL '
      WRITE (UNIT, '(A)') ';:H EC5 A '
      RETURN
C
C--- IFUNC=12, Draw line. ----------------------------------------------
C
  120 CONTINUE
      I0 = NINT(RBUF(1))
      J0 = NINT(RBUF(2))
      I1 = NINT(RBUF(3))
      J1 = NINT(RBUF(4))
  121 CONTINUE
      IF ( (I0.NE.LASTI) .OR. (J0.NE.LASTJ) ) THEN
          CALL GRFAO('U #,# D #,# ', L, INSTR, I0, J0, I1, J1)
      ELSE
          CALL GRFAO('#,# ', L, INSTR, I1, J1, 0, 0)
      END IF
      WRITE (UNIT,'(A)') INSTR(1:L)
      LASTI = I1
      LASTJ = J1
      RETURN
C
C--- IFUNC=13, Draw dot. -----------------------------------------------
C
  130 CONTINUE
      I1 = NINT(RBUF(1))
      J1 = NINT(RBUF(2))
      I0 = I1
      J0 = J1
      GOTO 121
C
C--- IFUNC=14, End picture. --------------------------------------------
C
  140 CONTINUE
      RETURN
C
C--- IFUNC=15, Select color index. -------------------------------------
C    (Not implemented.)
C
  150 CONTINUE
      RETURN
C
C--- IFUNC=16, Flush buffer. -------------------------------------------
C    (Null operation: buffering is not implemented.)
C
  160 CONTINUE
      RETURN
C
C--- IFUNC=17, Read cursor. --------------------------------------------
C    (Not implemented: should not be called.)
C
  170 GOTO 900
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
C    (Not implemented: should not be called.)
C
  200 GOTO 900
C
C--- IFUNC=21, Set color representation. -------------------------------
C    (Not implemented)
C
  210 CONTINUE
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
      WRITE (UNIT, '(A)') CHR(:LCHR)
      LASTI = -1
      RETURN
C-----------------------------------------------------------------------
C Error: unimplemented function.
C
  900 WRITE (MSG,'(I10)') IFUNC
      CALL GRWARN('Unimplemented function in HI device driver: '//MSG)
      NBUF = -1
      RETURN
C-----------------------------------------------------------------------
      END
