C*PXDRIV -- PGPLOT Printronix driver
C+
      SUBROUTINE PXDRIV (IFUNC, RBUF, NBUF, CHR, LCHR)
      INTEGER IFUNC, NBUF, LCHR
      REAL    RBUF(*)
      CHARACTER*(*) CHR
C
C PGPLOT driver for Printronix device.
C
C Version 1.0  - 1987 Jun 8 - T. J. Pearson.
C Version 1.1  - 1987 Aug 31 - truncate output lines as necessary.
C=======================================================================
C
C Supported device: Printronix P300 or P600 dot-matrix printer.
C
C Device type code: /PRINTRONIX.
C
C Default device name: PGPLOT.PRPLOT.
C
C Default view surface dimensions: 13.2in (horizontal) by 10.25in
C (vertical).
C
C Resolution: 60 (x) x 72 (y) pixels/inch.
C
C Color capability: Color indices 0 (erase, white) and 1 (black) are
C supported. It is not possible to change color representation.
C
C Input capability: None.
C
C File format: Variable-length records, maximum 135 bytes, with
C embedded carriage-control characters. A full-page plot occupies
C 200 512-byte blocks.
C
C Obtaining hardcopy: Use the command PRINT/PASSALL.
C-----------------------------------------------------------------------
      CHARACTER*(*) TYPE, DEFNAM
      PARAMETER (TYPE='PRINTRONIX (Printronix P300/P600 printer)')
      PARAMETER (DEFNAM='PGPLOT.PRPLOT')
      BYTE FF
      PARAMETER (FF=12)
C
      INTEGER UNIT, IER, IC, BX, BY, NPICT
      INTEGER GRGMEM, GRFMEM
      CHARACTER*10 MSG
      INTEGER BITMAP
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
      RETURN
C
C--- IFUNC = 2, Return physical min and max for plot device, and range
C               of color indices ---------------------------------------
C
   20 RBUF(1) = 0
      RBUF(2) = 791
      RBUF(3) = 0
      RBUF(4) = -1
      RBUF(5) = 0
      RBUF(6) = 1
      NBUF = 6
      RETURN
C
C--- IFUNC = 3, Return device resolution -------------------------------
C
   30 RBUF(1) = 60.0
      RBUF(2) = 72.0
      RBUF(3) = 1
      NBUF = 3
      RETURN
C
C--- IFUNC = 4, Return misc device info --------------------------------
C    (This device is Hardcopy, No cursor, No dashed lines, No area fill,
C    no thick lines)
C
   40 CHR = 'HNNNNNNNNN'
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
      RBUF(2) = 791
      RBUF(3) = 0
      RBUF(4) = 737
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
C     -- dimensions of plot buffer
      BX = 132 ! 792/6
      BY = 738
      CALL GRGLUN(UNIT)
      RBUF(1) = UNIT
      NPICT = 0
      OPEN (UNIT=UNIT, FILE=CHR(:LCHR), CARRIAGECONTROL='NONE',
     1      DEFAULTFILE=DEFNAM, DISPOSE='DELETE', STATUS='NEW',
     2      RECL=128,
     3      FORM='UNFORMATTED', RECORDTYPE='VARIABLE', IOSTAT=IER)
      IF (IER.NE.0) THEN
          CALL GRWARN('Cannot open output file for '//TYPE//' plot: '//
     1                CHR(:LCHR))
          RBUF(2) = 0
          CALL GRFLUN(UNIT)
      ELSE
          INQUIRE (UNIT=UNIT, NAME=CHR)
          LCHR = LEN(CHR)
   91     IF (CHR(LCHR:LCHR).EQ.' ') THEN
              LCHR = LCHR-1
              GOTO 91
          END IF
          RBUF(2) = 1
      END IF
      IER = GRGMEM(BX*BY, BITMAP)
      IF (IER.NE.1) THEN
          CALL GRGMSG(IER)
          CALL GRWARN('Failed to allocate plot buffer.')
          RBUF(2) = IER
          CLOSE (UNIT=UNIT, DISPOSE='DELETE')
          CALL GRFLUN(UNIT)
      END IF
      RETURN
C
C--- IFUNC=10, Close workstation ---------------------------------------
C
  100 CONTINUE
      CLOSE (UNIT=UNIT, DISPOSE='KEEP')
      CALL GRFLUN(UNIT)
      IER = GRFMEM(BX*BY, BITMAP)
      IF (IER.NE.1) THEN
          CALL GRGMSG(IER)
          CALL GRWARN('Failed to deallocate plot buffer.')
      END IF
      RETURN
C
C--- IFUNC=11, Begin picture -------------------------------------------
C
  110 CONTINUE
      NPICT = NPICT+1
C%    type *,'Begin picture',NPICT
      IF (NPICT.GT.1) WRITE (UNIT=UNIT) FF
      CALL GRPX03(BX*BY, %val(BITMAP), 'C0'X)
      RETURN
C
C--- IFUNC=12, Draw line -----------------------------------------------
C
  120 CONTINUE
      CALL GRPX01(1, RBUF, IC, BX, BY, %val(BITMAP))
      RETURN
C
C--- IFUNC=13, Draw dot ------------------------------------------------
C
  130 CONTINUE
      CALL GRPX01(0, RBUF, IC, BX, BY, %val(BITMAP))
      RETURN
C
C--- IFUNC=14, End picture ---------------------------------------------
C
  140 CONTINUE
C%    type *,'End picture  ',NPICT
      CALL GRPX02(UNIT, BX, BY, %val(BITMAP))
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
C    (Not used.)
C
  160 CONTINUE
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
C    (Not implemented: ignored)
C
  230 CONTINUE
      RETURN
C-----------------------------------------------------------------------
      END

C*GRPX01 -- PGPLOT Printronix driver, draw line
C+
      SUBROUTINE GRPX01 (LINE,RBUF,ICOL, BX, BY, BITMAP)
      INTEGER LINE
      REAL RBUF(4)
      INTEGER ICOL, BX, BY
      BYTE BITMAP(BX,BY)
C
C Draw a straight-line segment from absolute pixel coordinates
C (RBUF(1),RBUF(2)) to (RBUF(3),RBUF(4)).  The line either overwrites
C (sets to black) or erases (sets to white) the previous contents
C of the bitmap, depending on the current color index. Setting bits
C is accomplished with a VMS BISB2 instruction, expressed in
C Fortran as .OR.; clearing bits is accomplished with a VMS BICB2
C instruction, expressed in Fortran as .AND..NOT.. The line is
C generated with a Simple Digital Differential Analyser (ref:
C Newman & Sproull). 
C
C Arguments:
C
C LINE            I I      =0 for dot, =1 for line.
C RBUF(1),RBUF(2) I R      Starting point of line.
C RBUF(3),RBUF(4) I R      End point of line.
C ICOL            I I      =0 for erase, =1 for write.
C BITMAP        I/O B      (address of) the frame buffer.
C
C-----------------------------------------------------------------------
      BYTE    QMASK(0:5)
      INTEGER LENGTH, KX, KY, K
      REAL    D, XINC, YINC, XP, YP
      DATA    QMASK /'01'x,'02'x,'04'x,'08'x,'10'x,'20'x/
C
      IF (LINE.GT.0) THEN
          D = MAX(ABS(RBUF(3)-RBUF(1)), ABS(RBUF(4)-RBUF(2)))
          LENGTH = D
          IF (LENGTH.EQ.0) THEN
              XINC = 0.
              YINC = 0.
          ELSE
              XINC = (RBUF(3)-RBUF(1))/D
              YINC = (RBUF(4)-RBUF(2))/D
          END IF
      ELSE
          LENGTH = 0
          XINC = 0.
          YINC = 0.
      END IF
      XP = RBUF(1)+0.5
      YP = RBUF(2)+0.5
      IF (ICOL.NE.0) THEN
          DO K=0,LENGTH
              KX = XP
              KY = (BY-1)-INT(YP)
              BITMAP(KX/6+1,KY+1) = BITMAP(KX/6+1,KY+1) .OR. 
     1                              QMASK(MOD(KX,6))
              XP = XP + XINC
              YP = YP + YINC
          END DO
      ELSE
          DO K=0,LENGTH
              KX = XP
              KY = (BY-1)-INT(YP)
              BITMAP(KX/6+1,KY+1) = BITMAP(KX/6+1,KY+1) .AND.
     1                              (.NOT.QMASK(MOD(KX,6)))
              XP = XP + XINC
              YP = YP + YINC
          END DO
      END IF
      END

C*GRPX02 -- PGPLOT Printronix driver, copy bitmap to output file
C+
      SUBROUTINE GRPX02 (UNIT, BX, BY, BITMAP)
      INTEGER UNIT, BX, BY
      BYTE BITMAP(BX,BY)
C
C Arguments:
C  UNIT   (input)  Fortran unit number for output
C  BX, BY (input)  dimensions of BITMAP
C  BITMAP (input)  the bitmap array
C-----------------------------------------------------------------------
      BYTE SUFFIX(3)
      DATA SUFFIX/ 5, 13, 10/
      INTEGER I, J, K
C
C Write bitmap.
C
      DO J=1,BY
          DO K=BX,2,-1
              IF (BITMAP(K,J).NE.'C0'X) GOTO 10
          END DO
   10     WRITE (UNIT=UNIT) (BITMAP(I,J),I=1,K),SUFFIX
      END DO
C
C Write blank plot lines to fill up page
C
      END

C*GRPX03 -- PGPLOT Printronix driver, fill buffer with a specified character
C+
      SUBROUTINE GRPX03 (BUFSIZ,BUFFER,FILL)
C
C GRPCKG (internal routine): fill a buffer with a given character.
C
C Arguments:
C
C BUFFER (byte array, input): (address of) the buffer.
C BUFSIZ (integer, input): number of bytes in BUFFER.
C FILL (integer, input): the fill character. BUFSIZ bytes starting at
C       address BUFFER are set to contents of FILL.
C--
C (1-Feb-1983)
C-----------------------------------------------------------------------
      INTEGER  BUFSIZ, I
      BYTE     FILL
      BYTE     BUFFER(BUFSIZ)
C
      DO 10 I=1,BUFSIZ
          BUFFER(I) = FILL
   10 CONTINUE
      END
