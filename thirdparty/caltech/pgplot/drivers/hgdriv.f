C*HGDRIV -- PGPLOT HPGL-2 driver
C+
      SUBROUTINE HGDRIV (IFUNC, RBUF, NBUF, CHR, LCHR)
      INTEGER IFUNC, NBUF, LCHR
      REAL    RBUF(*)
      CHARACTER*(*) CHR
C-----------------------------------------------------------------------
C PGPLOT driver for HPGL-2 device.
C-----------------------------------------------------------------------
C Version 0.0  - 1990 Oct 18 - C. J. Lonsdale.
C Version 1.0  - 1995 Jul 10 - renamed HGDRIV (TJP).
C Version 1.1  - 1997 Jun  2 - removed some non-standard Fortran (TJP).
C-----------------------------------------------------------------------
C
C This driver should work with any HP-GL/2 device, but has been tested
C only with a Laserjet III printer.  It takes advantage of the "PE" mode
C compressed data format.  In the Laserjet III used for testing, there
C were two problems with using the PE format, both of which required
C inelegant workarounds in the code.  First, the PE mode is designed
C to be used with relative addressing, but when PGPLOT tries to draw long
C dashed/dotted lines there seems to be a systematic rounding error
C internal to the printer which grows as n instead of sqrt(n), so that
C serious cumulative positional errors can result.  Attempts to work
C around this by periodically inserting an absolute positioning command
C revealed a second problem, namely that the "=" absolute positioning
C flag in PE mode does not work.  It was thus necessary to periodically
C exit from PE mode, do absolute positioning in normal mode, then return
C to PE mode.  The file size overhead of this workaround is miminal (maybe
C 5%).  The printer rounding errors were ignored in the polygon fill
C opcode, because in general the vectors will be of pseudo-random length
C and direction, so errors will grow only as sqrt(n).
C
C Supported device: Any HPGL-2 device (presently tested only on HP laserjet 3)
C
C Device type code: /HPGL2
C
C Default device name: PGPLOT.HPPLOT.
C
C Default view surface dimensions: 8.0in (horizontal) by 10.0in
C (vertical).
C
C Resolution: 1016 (x) x 1016 (y) pixels/inch.
C
C Color capability: Color indices 0 (erase, white) and 1 (black) are
C supported. 7 other shades of grey are available for lines and
C area fill patterns.
C It is not possible to change color representation.
C
C Input capability: None.
C
C File format: Ascii
C
C Obtaining hardcopy: Extremely system dependent
C-----------------------------------------------------------------------
      CHARACTER*(*) TYPE, DEFNAM
      PARAMETER (TYPE='HPGL2 (Hewlett-Packard graphics)')
      PARAMETER (DEFNAM='pgplot.hpplot')
      CHARACTER*1 FF, EC
C
      LOGICAL START, PEON
      INTEGER GROPTX
      INTEGER UNIT, IER, IC, NPTS, PCT, NREL
      INTEGER I0, J0, I1, J1, L, L1, L2, LASTI, LASTJ, LOBUF
      REAL LW
      CHARACTER*80 INSTR, MSG, DUMMY, DUMMY1, DUMMY2
      CHARACTER*132 OBUF
      SAVE UNIT, IC, LASTI, LASTJ, LOBUF, OBUF, PEON, NREL
      SAVE FF, EC
C-----------------------------------------------------------------------
C
      GOTO( 10, 20, 30, 40, 50, 60, 70, 80, 90,100,
     1     110,120,130,140,150,160,170,180,190,200,
     2     210,220,230), IFUNC
  900 WRITE (MSG,'(I10)') IFUNC
      CALL GRWARN('Unimplemented function in '//TYPE//' device driver:'
     1    //MSG)
      NBUF = -1
      RETURN
C
C--- IFUNC = 1, Return device name -------------------------------------
C
   10 CHR = TYPE
      LCHR = LEN(TYPE)
      FF=CHAR(12)
      EC=CHAR(27)
      RETURN
C
C--- IFUNC = 2, Return physical min and max for plot device, and range
C               of color indices ---------------------------------------
C
   20 RBUF(1) = 0
      RBUF(2) = 8128
      RBUF(3) = 0
      RBUF(4) = 10160
      RBUF(5) = 0
      RBUF(6) = 9
      NBUF = 6
      RETURN
C
C--- IFUNC = 3, Return device resolution -------------------------------
C
   30 RBUF(1) = 1016.0
      RBUF(2) = 1016.0
      RBUF(3) = 1
      NBUF = 3
      RETURN
C
C--- IFUNC = 4, Return misc device info --------------------------------
C    (This device is Hardcopy, No cursor, No dashed lines, Area fill,
C    Thick lines)
C
   40 CHR = 'HNNATNNNNN'
      LCHR = 10
      RETURN
C
C--- IFUNC = 5, Return default file name -------------------------------
C
   50 CHR = DEFNAM
      LCHR = LEN(DEFNAM)
      RETURN
C
C--- IFUNC = 6, Return default physical size of plot -------------------
C
   60 RBUF(1) = 0
      RBUF(2) = 8128
      RBUF(3) = 0
      RBUF(4) = 10160
      NBUF = 4
      RETURN
C
C--- IFUNC = 7, Return misc defaults -----------------------------------
C
   70 RBUF(1) = 1
      NBUF=1
      RETURN
C
C--- IFUNC = 8, Select plot --------------------------------------------
C
   80 CONTINUE
      RETURN
C
C--- IFUNC = 9, Open workstation ---------------------------------------
C
   90 CONTINUE
      CALL GRGLUN(UNIT)
      NBUF = 2
      RBUF(1) = UNIT
      IER = GROPTX(UNIT, CHR(1:LCHR), DEFNAM, 1)
      IF (IER.NE.0) THEN
          DUMMY = CHR(:LCHR)
          CALL GRWARN('Cannot open output file for '//TYPE//' plot: '//
     1                DUMMY(:LCHR))
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
      CALL GRHG02(UNIT,EC//'E')
      PEON = .FALSE.
      NREL = 0
      RETURN
C
C--- IFUNC=10, Close workstation ---------------------------------------
C
  100 CONTINUE
      CLOSE(UNIT)
      CALL GRFLUN(UNIT)
      RETURN
C
C--- IFUNC=11, Begin picture -------------------------------------------
C
C Enter HPGL, black pen, thick lines have rounded ends and joins
  110 CONTINUE
      CALL GRHG02(UNIT,EC//'%0BINSP1LA1,4,2,4PA1,1')
      LASTI = 1
      LASTJ = 1
      RETURN
C
C--- IFUNC=12, Draw line -----------------------------------------------
C
  120 CONTINUE
      I0 = NINT(RBUF(1))
      J0 = NINT(RBUF(2))
      I1 = NINT(RBUF(3))
      J1 = NINT(RBUF(4))
C simple pen-down move
      IF (I0.EQ.LASTI .AND. J0.EQ.LASTJ) THEN

          CALL GRHGEC((I1-I0), (J1-J0), INSTR, L)

C pen up to start of line, then pen down to end of line
      ELSE
          CALL GRHGEC((I0-LASTI), (J0-LASTJ), DUMMY1, L1)
          CALL GRHGEC((I1-I0), (J1-J0), DUMMY2, L2)
          INSTR = '<'//DUMMY1(1:L1)//DUMMY2(1:L2)
          L = 1 + L1 + L2

C Make sure we are doing this in PE mode
      END IF
      IF(.NOT. PEON) THEN
          INSTR = 'PE7'//INSTR
          L = L + 3
          PEON = .TRUE.
          NREL = 0
      ENDIF
      LASTI = I1
      LASTJ = J1
C Insert absolute position once in a while to keep LJIII in line
      NREL = NREL + 1
      IF(NREL .GE. 30) THEN
          CALL GRFAO(';PUPA#,#PE7',L1,DUMMY1,LASTI,LASTJ,0,0)
          INSTR = INSTR(1:L)//DUMMY1(1:L1)
          L = L + L1
          NREL = 0
      ENDIF
      GOTO 800
C
C--- IFUNC=13, Draw dot ------------------------------------------------
C
  130 CONTINUE
      I1 = NINT(RBUF(1))
      J1 = NINT(RBUF(2))
C pen up move to position, then pen down
      CALL GRHGEC((I1-LASTI), (J1-LASTJ), DUMMY1, L1) 
      CALL GRHGEC(0, 0, DUMMY2, L2)

      INSTR = '<'//DUMMY1(1:L1)//DUMMY2(1:L2)
      L = 1 + L1 + L2
      IF(.NOT. PEON) THEN
          INSTR = 'PE7'//INSTR
          L = L + 3
          PEON = .TRUE.
          NREL = 0
      ENDIF
      LASTI = I1
      LASTJ = J1
C LJIII obedience code as in line draw opcode
      NREL = NREL + 1
      IF(NREL .GE. 30) THEN
          CALL GRFAO(';PUPA#,#PE7',L1,DUMMY1,LASTI,LASTJ,0,0)
          INSTR = INSTR(1:L)//DUMMY1(1:L1)
          L = L + L1
          NREL = 0
      ENDIF
      GOTO 800
C
C--- IFUNC=14, End picture ---------------------------------------------
C
  140 CONTINUE
      IF (LOBUF.NE.0) THEN
          CALL GRHG02(UNIT, OBUF(1:LOBUF))
          LOBUF = 0
      END IF
      CALL GRHG02(UNIT, ';'//EC//'E')
      PEON = .FALSE.
      RETURN
C
C--- IFUNC=15, Select color index --------------------------------------
C
  150 CONTINUE
      IC = RBUF(1)
C white ... disable transparency mode.
      IF (IC.EQ.0) THEN
          INSTR = ';TR0SP0FT10,0SV0'
          L = 16
C Some shade of grey, enable transparency
C Had to disable greys ... bad effects on line drawing CJL 3/6/92
      ELSE
          PCT = 100
C         IF(IC.EQ.1) PCT = 100
C         IF(IC.EQ.2) PCT = 1
C         IF(IC.EQ.3) PCT = 5
C         IF(IC.EQ.4) PCT = 15
C         IF(IC.EQ.5) PCT = 25
C         IF(IC.EQ.6) PCT = 40
C         IF(IC.EQ.7) PCT = 70
C         IF(IC.EQ.8) PCT = 85
          CALL GRFAO(';TR1SP1FT10,#SV1,#',L,INSTR,PCT,PCT,0,0)
      END IF
      PEON = .FALSE.
      GOTO 800
C
C--- IFUNC=16, Flush buffer. -------------------------------------------
C
  160 CONTINUE
      IF (LOBUF.NE.0) THEN
          CALL GRHG02(UNIT, OBUF(1:LOBUF))
          LOBUF = 0
      END IF
      RETURN
C
C--- IFUNC=17, Read cursor. --------------------------------------------
C    (Not implemented: should not be called)
C
  170 CONTINUE
      GOTO 900
C
C--- IFUNC=18, Erase alpha screen. -------------------------------------
C    (Not implemented: no alpha screen)
C
  180 CONTINUE
      RETURN
C
C--- IFUNC=19, Set line style. -----------------------------------------
C    (Not implemented: should not be called)
C
  190 CONTINUE
      GOTO 900
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
C enter PE mode at first vertex
          IF (START) THEN
              CALL GRFAO(';PUPA#,#PMPE7', L, INSTR, I0, J0, 0, 0)
              START = .FALSE.
              LASTI = I0
              LASTJ = J0
C last point, exit PE mode, polygon mode, and issue polygon fill.  Then
C reset the position to be safe.
          ELSE IF (NPTS.EQ.0) THEN
              CALL GRHGEC((I0-LASTI),(J0-LASTJ),DUMMY1,L1)
              INSTR = DUMMY1(1:L1)//';PM2FPPUPA1,1'
              L = L1 + 13
              LASTI = 1
              LASTJ = 1
              PEON = .FALSE.
              NREL = 0
C Just another point
          ELSE
              CALL GRHGEC((I0-LASTI),(J0-LASTJ),INSTR,L)
              LASTI = I0
              LASTJ = J0
          END IF

          GOTO 800
      ENDIF
C
C--- IFUNC=21, Set color representation. -------------------------------
C    (Not implemented: ignored)
C
  210 CONTINUE
      RETURN
C
C--- IFUNC=22, Set line width. -----------------------------------------
C
  220 CONTINUE
C Fudged this ... lines looked too thick, maybe its the res. enhancement.
      LW = RBUF(1) * 0.127 - 0.05
      IF(LW .EQ. 0.0) LW = 0.025
      WRITE(DUMMY,'(F5.3)') LW
      INSTR = ';PW'//DUMMY
      L = 8
      PEON = .FALSE.
      GOTO 800
C
C--- IFUNC=23, Escape --------------------------------------------------
C    (Not implemented: ignored)
C
  230 CONTINUE
      RETURN
C-----------------------------------------------------------------------
C Buffer output if possible.  Same as PS driver.
C
  800 IF ( (LOBUF+L+1). GT. 132) THEN
          CALL GRHG02(UNIT, OBUF(1:LOBUF))
          OBUF(1:L) = INSTR(1:L)
          LOBUF = L
      ELSE
          OBUF(LOBUF+1:LOBUF+L) = INSTR(1:L)
          LOBUF = LOBUF+L
      END IF
      RETURN
C-----------------------------------------------------------------------
      END

C*GRHGEC -- PGPLOT HPGL2 driver, convert integers to coded ascii
C+
      SUBROUTINE GRHGEC(X,Y,OUTSTR,L)
C
C Support routine for HPGL2 driver:  converts integer coordinates
C X and Y to base 32 in the ascii format suitable for PE command.
C Output is a string, number of characters encoded returned in L.
C----------------------------------------------------------------
      INTEGER X, Y, L
      CHARACTER*(*) OUTSTR
C
      INTEGER XREM, YREM
C
C Set the sign bit
C
      X = X * 2
      IF(X .LT. 0) X = 1 - X
      Y = Y * 2
      IF(Y .LT. 0) Y = 1 - Y
C
C Loop through base-32 digits, treat last one with extra bit in 
C ascii character.  Goes from least to most significant.
C
      L = 0
      OUTSTR = ' '
   10 CONTINUE
          XREM = MOD(X,32)
          X = X / 32
          L = L + 1
          IF(X .EQ. 0) GO TO 20
          OUTSTR(L:L) = CHAR(63+XREM)
      GO TO 10
   20 OUTSTR(L:L) = CHAR(95+XREM)
C
C Do Y coordinate same way as X above
C
   30 CONTINUE
          YREM = MOD(Y,32)
          Y = Y / 32
          L = L + 1
          IF(Y .EQ. 0) GO TO 40
          OUTSTR(L:L) = CHAR(63+YREM)
      GO TO 30
   40 OUTSTR(L:L) = CHAR(95+YREM)
C
      RETURN
      END


C*GRHG02 -- PGPLOT HPGL driver, copy buffer to file
C+
      SUBROUTINE GRHG02 (UNIT, S)
C
C Support routine for PSdriver: write character string S on
C specified Fortran unit.
C-----------------------------------------------------------------------
      INTEGER UNIT
      CHARACTER*(*) S
C
      WRITE (UNIT, '(A)') S
C-----------------------------------------------------------------------
      END
