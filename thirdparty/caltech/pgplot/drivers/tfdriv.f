C*TFDRIV -- PGPLOT Tektronix file driver
C+
      SUBROUTINE TFDRIV (IFUNC, RBUF, NBUF, CHR, LCHR)
      INTEGER IFUNC, NBUF, LCHR
      REAL    RBUF(*)
      CHARACTER*(*) CHR
C
C PGPLOT driver for Tektronix disk file. This version uses 
C the extended (12-bit) addressing of the Tektronix-4014.
C
C Version 1.0 - 1987 Aug 18 - T. J. Pearson.
C Version 1.1 - 1995 Mar 20 - David R. Chang (chang@firm.drea.dnd.ca):
C                             Optimized coordinate output.
C                             Implemented escape function.
C
C
C Supported device:  disk file which may be printable on a
C Tektronix-compatible device.
C
C Device type code: /TFILE. 
C
C Default device name: PGPLOT.TFPLOT.
C
C Default view surface dimension: Depends on printer; nominally
C 400 pixels/inch giving 10.24 in (horizontal) x 7.80 in (vertical).
C
C Resolution: The coordinate system used for Tektronix emulation is
C 4096 x 3120 pixels.
C
C Color capability: Only color index 1 is supported. Primitives drawn 
C in "erase" mode (color index 0) are ignored (not erased). It is not 
C possible to change color representation. 
C
C Input capability: None.
C 
C File format: Binary, variable length records (maximum 1024 bytes);
C no carriage-control attribute.
C 
C Obtaining hardcopy: depends on the available printer. e.g., for an
C Imagen printer with IMPRINT software,
C    $ IMPRINT/STYLE=TEKTRONIX file.type
C
C Note: the file cannot easily be displayed on a Tektronix-compatible
C *terminal* because it contains control characters which will be
C interpreted by the operating system. The terminal must be set to
C "Passall" mode before the file can be displayed.
C-----------------------------------------------------------------------
      CHARACTER*(*) TYPE, DEFNAM
      PARAMETER (DEFNAM='PGPLOT.TFPLOT')
      PARAMETER (TYPE='TFILE (disk file in Tektronix format)')
      INTEGER BUFSIZ
      PARAMETER (BUFSIZ=1024)
      INTEGER BUFFER
      INTEGER BUFLEV
      INTEGER UNIT, IER, I0, I1, J0, J1
      INTEGER I, LASTI, LASTJ
      INTEGER GRGMEM, GRFMEM
      CHARACTER*10 MSG
      INTEGER IC
      BYTE    TKBUF(12)
      INTEGER NW
      BYTE    TKTERM(6)
      DATA    TKTERM/29, 55, 127, 32, 64, 31/
      SAVE BUFFER, BUFLEV, IC, LASTI, LASTJ, TKTERM, UNIT
C-----------------------------------------------------------------------
C
      GOTO( 10, 20, 30, 40, 50, 60, 70, 80, 90,100,
     1     110,120,130,140,150,160,170,180,190,200,
     2     210,220,230,240), IFUNC
  900 WRITE (MSG,'(I10)') IFUNC
      CALL GRWARN('Unimplemented function in TFILE device driver:'
     1    //MSG)
      NBUF = -1
      RETURN
C
C--- IFUNC = 1, Return device name -------------------------------------
C
   10 CHR = TYPE
      LCHR = LEN(TYPE)
      RETURN
C
C--- IFUNC = 2, Return physical min and max for plot device, and range
C               of color indices ---------------------------------------
C
   20 RBUF(1) = 0
      RBUF(2) = 4095
      RBUF(3) = 0
      RBUF(4) = 3119
      RBUF(5) = 0
      RBUF(6) = 1
      NBUF = 6
      RETURN
C
C--- IFUNC = 3, Return device resolution -------------------------------
C    (Nominal values)
C
   30 RBUF(1) = 400.0
      RBUF(2) = 400.0
C      (multiple strokes are spaced by 2 pixels, or 1/200 inch; ideally
C       the printer `pen' width should be 1/200 inch)
      RBUF(3) = 2
      NBUF = 3
      RETURN
C
C--- IFUNC = 4, Return misc device info --------------------------------
C    (Non-interactive, No cursor, No dashed lines, No area fill,
C    no thick lines)
C
   40 CHR = 'NNNNNNNNNN'
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
      RBUF(2) = 4095
      RBUF(3) = 0
      RBUF(4) = 3119
      NBUF = 4
      RETURN
C
C--- IFUNC = 7, Return misc defaults -----------------------------------
C
   70 RBUF(1) = 8.0
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
C     -- allocate buffer
      IER = GRGMEM(BUFSIZ, BUFFER)
      IF (IER.NE.1) THEN
          CALL GRGMSG(IER)
          CALL GRWARN('Failed to allocate plot buffer.')
          RBUF(2) = IER
          RETURN
      END IF
C     -- open device
      CALL GRGLUN(UNIT)
      NBUF = 2
      RBUF(1) = UNIT
      OPEN (UNIT=UNIT, FILE=CHR(:LCHR), CARRIAGECONTROL='NONE',
     1      DEFAULTFILE=DEFNAM, DISPOSE='DELETE', STATUS='NEW',
     2      FORM='UNFORMATTED', RECORDTYPE='VARIABLE', IOSTAT=IER,
     3      RECL=256)
      IF (IER.NE.0) THEN
          CALL GRWARN('Cannot open output file for '//TYPE//' plot: '//
     1                CHR(:LCHR))
          RBUF(2) = 0
          CALL GRFLUN(UNIT)
          IER = GRFMEM(BUFSIZ, BUFFER)
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
      IC = 1
      LASTI = -1
      LASTJ = -1
      BUFLEV=0
C     -- no device initialization required
      RETURN
C
C--- IFUNC=10, Close workstation ---------------------------------------
C
  100 CONTINUE
      CLOSE (UNIT, DISPOSE='KEEP')
      CALL GRFLUN(UNIT)
      IER = GRFMEM(BUFSIZ, BUFFER)
      IF (IER.NE.1) THEN
          CALL GRWARN('Error deallocating plot buffer.')
          CALL GRGMSG(IER)
      END IF
      RETURN
C
C--- IFUNC=11, Begin picture -------------------------------------------
C
  110 CONTINUE
C     -- erase screen; no wait required
      TKBUF(1) = 29
      TKBUF(2) = 27
      TKBUF(3) = 12
      TKBUF(4) = 24
      NW = 4
      LASTI = -1
      GOTO 1000
C
C--- IFUNC=12, Draw line -----------------------------------------------
C
  120 CONTINUE
      IF (IC.EQ.0) RETURN
      I0 = NINT(RBUF(1))
      J0 = NINT(RBUF(2))
      I1 = NINT(RBUF(3))
      J1 = NINT(RBUF(4))
      CALL GRTF01(LASTI, LASTJ, I0, J0, I1, J1, TKBUF, NW)
      GOTO 1000
C
C--- IFUNC=13, Draw dot ------------------------------------------------
C
  130 CONTINUE
      IF (IC.EQ.0) RETURN
      I0 = NINT(RBUF(1))
      J0 = NINT(RBUF(2))
      CALL GRTF01(LASTI, LASTJ, I0, J0, I0, J0, TKBUF, NW)
      GOTO 1000
C
C--- IFUNC=14, End picture ---------------------------------------------
C
  140 CONTINUE
      RETURN
C
C--- IFUNC=15, Select color index --------------------------------------
C
  150 CONTINUE
      IC = RBUF(1)
      IF (IC.LT.0 .OR. IC.GT.1) THEN
          IC = 1
          RBUF(1) = IC
      END IF
      RETURN
C
C--- IFUNC=16, Flush buffer. -------------------------------------------
C
  160 CONTINUE
      CALL GRTF02(TKTERM,6,%val(BUFFER),BUFLEV,UNIT)
      CALL GRTF03(%val(BUFFER), UNIT, BUFLEV)
      LASTI = -1
      RETURN
C
C--- IFUNC=17, Read cursor. --------------------------------------------
C           Not implemented.
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
C    (Not implemented: should not be called)
C
  200 CONTINUE
      GOTO 900
C
C--- IFUNC=21, Set color representation. -------------------------------
C    (Not implemented: ignored)
C
  210 CONTINUE
      RETURN
C
C--- IFUNC=22, Set line width. -----------------------------------------
C    (Not implemented: should not be called)
C
  220 CONTINUE
      GOTO 900
C
C--- IFUNC=23, Escape --------------------------------------------------
C
  230 CONTINUE
      IF (LCHR.LE.BUFSIZ) THEN
        LASTI = -1
        CALL GRTF02(%ref(CHR),LCHR,%val(BUFFER),BUFLEV,UNIT)
      ENDIF
      RETURN
C
C--- IFUNC=24, Rectangle fill. -----------------------------------------
C    (Not implemented: should not be called)
C
  240 CONTINUE
      GOTO 900
C
C--- Send the command. -------------------------------------------------
C
 1000 CALL GRTF02(TKBUF,NW,%val(BUFFER),BUFLEV,UNIT)
      RETURN
C-----------------------------------------------------------------------
      END

C*GRTF01 -- PGPLOT Tektronix file driver, draw line segment
C+
      SUBROUTINE GRTF01(LASTI, LASTJ, I0, J0, I1, J1, TKBUF, NW)
      INTEGER   LASTI, LASTJ, I0, J0, I1, J1, NW
      BYTE TKBUF(*)
C
C This routine draws a line from (I0, J0) to (I1, J1).  If LASTI>=0
C assume that the cursor is at the position is at (LASTI, LASTJ).
C For this case, a minimum length move is done from (LASTI, LASTJ) to
C the nearer point.  Of course, if (LASTI, LASTJ) and the nearer point
C are the same, then no bytes of positioning data are generated and
C sent to the file.  If LASTI<0 then a move is done with the
C coordinate fully specified.  In both cases the line end point
C is specified using the fewest number of bytes allowed by the protocol.
C Upon return, LASTI,LASTJ will contain the current cursor position. 
C Note:  The 'delete' character (127) can occur in LOY or EXTRA byte;
C it can be replaced by escape-? if desired.
C
C Arguments:
C   LASTI,LASTJ (in/out) : current position
C   I0, J0      (in/out) : device coordinates of the starting point.
C   I1, J1      (in/out) : device coordinates of the end point.
C   TKBUF       (out)    : buffer for instruction.
C   NW          (out)    : Number of valid characters in TKBUF.
C
C 1995-Mar-17 - Created from GRTT01 - [DRC]
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
      INTEGER ID0, ID1, ITMP
      INTEGER IEX, ILOX, IHIX, ILOY, IHIY
C
      NW = 0
      IF(LASTI.LT.0) THEN
C Last position is invalid, therefore do a dark vector move with all
C coordinates specified.
          NW=NW+1
          TKBUF(NW) = GS
          IHIY = J0/128
          ILOY = MOD(J0/4, 32)
          IHIX = I0/128
          ILOX = MOD(I0/4, 32)
          IEX  = 4*MOD(J0, 4) + MOD(I0, 4)
          TKBUF(NW+1) = MASKHY + IHIY
          TKBUF(NW+2) = MASKEX + IEX
          TKBUF(NW+3) = MASKLY + ILOY
          TKBUF(NW+4) = MASKHX + IHIX
          TKBUF(NW+5) = MASKLX + ILOX
          NW = NW + 5
      ELSE
C Last position is valid, move pen to nearest end point of line.
          ID0=ABS(LASTI-I0)+ABS(LASTJ-J0)
          ID1=ABS(LASTI-I1)+ABS(LASTJ-J1)
          IF(ID1.LT.ID0) THEN
C Swap coordinates to minimize 'pen motion'.  For optimized coordinates
C this can reduce the amount of I/O to the file.
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
              NW=NW+1
              TKBUF(NW)=GS
              CALL GRTF04(LASTI,LASTJ,I0,J0,TKBUF,NW)
          END IF
      END IF
C
C File is now in graph mode, and the `pen' has been positioned.
C Do an optimized draw.
      CALL GRTF04(I0,J0,I1,J1,TKBUF,NW)
C
C Remember current position.
      LASTI=I1
      LASTJ=J1
      RETURN
C-----------------------------------------------------------------------
      END

C*GRTF02 -- PGPLOT Tektronix file driver, transfer data to buffer
C+
      SUBROUTINE GRTF02 (INSTR, N, BUFFER, HWM, UNIT)
      INTEGER   N, HWM, UNIT
      BYTE      INSTR(*), BUFFER(*)
C
C Arguments:
C  INSTR  (input)  : text of instruction (bytes).
C  N      (input)  : number of bytes to transfer.
C  BUFFER (input)  : output buffer.
C  HWM    (in/out) : number of bytes used in BUFFER.
C  UNIT   (input)  : channel number for output (when buffer is full).
C
C Subroutines called:
C   GRTF03
C-----------------------------------------------------------------------
      INTEGER BUFSIZ
      PARAMETER (BUFSIZ=1024)
      INTEGER  I
C-----------------------------------------------------------------------
      IF (HWM+N.GE.BUFSIZ) CALL GRTF03(BUFFER, UNIT, HWM)
      DO 10 I=1,N
          HWM = HWM + 1
          BUFFER(HWM) = INSTR(I)
   10 CONTINUE
      RETURN
C-----------------------------------------------------------------------
      END

C*GRTF03 -- PGPLOT Tektronix file driver, copy buffer to file
C+
      SUBROUTINE GRTF03 (BUFFER, UNIT, N)
      BYTE BUFFER(*)
      INTEGER UNIT, N
C
C Arguments:
C   BUFFER (input) address of buffer to be output
C   UNIT   (input) unit number for output
C   N      (input) number of bytes to transfer
C          (output) set to zero
C-----------------------------------------------------------------------
      INTEGER J
C-----------------------------------------------------------------------
      IF (N.GT.0) WRITE (UNIT) (BUFFER(J),J=1,N)
      N = 0
      RETURN
C-----------------------------------------------------------------------
      END

C*GRTF04 -- PGPLOT Tektronix file driver, encode coordinate pair
C+
      SUBROUTINE GRTF04(LASTI, LASTJ, I0, J0, TKBUF, NW)
      INTEGER   LASTI, LASTJ, I0, J0, NW
      BYTE TKBUF(*)
C
C Assume cursor is at position LASTI, LASTJ and that the light or
C dark vector condition has been correctly set.  Add up to 5 characters
C to TKBUF to draw a vector to I0, J0.  The minimum number of characters
C are encoded to obtain the motion.
C
C 1995-Mar-17 - Created from GRTT04 - [DRC]
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
C
      IF(IHIY.NE.LHIY) THEN
          NW=NW+1
          TKBUF(NW) = 32+IHIY
      END IF
      IF(IEX.NE.LEX) THEN
          NW=NW+1
          TKBUF(NW) = 96+IEX
      END IF
      IF(IEX.NE.LEX .OR. ILOY.NE.LLOY .OR. IHIX.NE.LHIX) THEN
          NW=NW+1
          TKBUF(NW) = 96+ILOY
      END IF
      IF(IHIX.NE.LHIX) THEN
          NW=NW+1
          TKBUF(NW) = 32+IHIX
      END IF
      NW=NW+1
      TKBUF(NW) = 64+ILOX
      RETURN
      END
