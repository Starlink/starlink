C*GCDRIV -- PGPLOT Genicom printer driver
C+
      SUBROUTINE GCDRIV (IFUNC, RBUF, NBUF, CHR, LCHR)
      INTEGER IFUNC, NBUF, LCHR
      REAL    RBUF(*)
      CHARACTER*(*) CHR
C
C PGPLOT driver for Genicom printer device.
C
C This driver is a copy of pxdriver.for with minor changes to put
C the genicom printer in the proper mode and scale correctly.
C Version 1.0  - 1990 Feb 12 - J. H. Trice.
C=======================================================================
C
C Supported device: Genicom 4410 dot-matrix printer.
C
C Device type code: /GENICOM.
C
C Default device name: PGPLOT.PRPLOT.
C
C Default view surface dimensions: 10.25in (horizontal) by 7.8in
C (vertical).
C
C Resolution: 144 (x) x 140 (y) pixels/inch.
C
C Color capability: Color indices 0 (erase, white) and 1 (black) are
C supported. It is not possible to change color representation.
C
C Input capability: None.
C
C File format: Variable-length records, maximum 197 bytes, with
C embedded carriage-control characters. A full-page plot occupies
C 600 512-byte blocks.
C
C Obtaining hardcopy: Use the command PRINT/PASSALL.
C-----------------------------------------------------------------------
      CHARACTER*(*) TYPE, DEFNAM
      PARAMETER (TYPE=
     :     'GENICOM (Genicom 4410 dot-matrix printer, landscape)')
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
      RBUF(2) = 1510
      RBUF(3) = 0
      RBUF(4) = 1154
      RBUF(5) = 0
      RBUF(6) = 1
      NBUF = 6
      RETURN
C
C--- IFUNC = 3, Return device resolution -------------------------------
C
   30 RBUF(1) = 144.0
      RBUF(2) = 140.0
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
      RBUF(2) = 1510
      RBUF(3) = 0
      RBUF(4) = 1154
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
      BY = 194 ! 1164/6
      BX = 1520
      CALL GRGLUN(UNIT)
      RBUF(1) = UNIT
      NPICT = 0
      OPEN (UNIT=UNIT, FILE=CHR(:LCHR), CARRIAGECONTROL='NONE',
     1      DEFAULTFILE=DEFNAM, DISPOSE='DELETE', STATUS='NEW',
     2      RECL=197,
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
      CALL GRGC03(BX*BY, %val(BITMAP), 'C0'X)
      RETURN
C
C--- IFUNC=12, Draw line -----------------------------------------------
C
  120 CONTINUE
      CALL GRGC01(1, RBUF, IC, BX, BY, %val(BITMAP))
      RETURN
C
C--- IFUNC=13, Draw dot ------------------------------------------------
C
  130 CONTINUE
      CALL GRGC01(0, RBUF, IC, BX, BY, %val(BITMAP))
      RETURN
C
C--- IFUNC=14, End picture ---------------------------------------------
C
  140 CONTINUE
C%    type *,'End picture  ',NPICT
      CALL GRGC02(UNIT, BX, BY, %val(BITMAP))
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

C*GRGC01 -- PGPLOT Genicom printer driver, draw line
C+
      SUBROUTINE GRGC01 (LINE,RBUF,ICOL, BX, BY, BITMAP)
      INTEGER LINE
      REAL RBUF(4)
      INTEGER ICOL, BX, BY
      BYTE BITMAP(BY,BX)
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
              KY = BX - XP -5
              KX = (BY*6-1)-INT(YP)
              BITMAP(KX/6+1,KY+1) = BITMAP(KX/6+1,KY+1) .OR. 
     1                              QMASK(MOD(KX,6))
              XP = XP + XINC
              YP = YP + YINC
          END DO
      ELSE
          DO K=0,LENGTH
              KY = BX - XP -5
              KX = (BY*6-1)-INT(YP)
              BITMAP(KX/6+1,KY+1) = BITMAP(KX/6+1,KY+1) .AND.
     1                              (.NOT.QMASK(MOD(KX,6)))
              XP = XP + XINC
              YP = YP + YINC
          END DO
      END IF
      END

C*GRGC02 -- PGPLOT Genicom driver, copy bitmap to output file
C+
      SUBROUTINE GRGC02 (UNIT, BX, BY, BITMAP)
      INTEGER UNIT, BX, BY
      BYTE BITMAP(BY,BX)
C
C Arguments:
C  UNIT   (input)  Fortran unit number for output
C  BX, BY (input)  dimensions of BITMAP
C  BITMAP (input)  the bitmap array
C-----------------------------------------------------------------------
      BYTE SUFFIX(3),PREGEN(10),POSTGEN(2)
      DATA SUFFIX/ 5, 13, 10/
      DATA PREGEN/27, 91,52,59,54,59,53,113,27,80/
      DATA POSTGEN/27, 92/
      INTEGER I, J, K
C
C   WRITE PREFIX TO PUT IN HIGH DENSITY GRAPHICS MODE
C
      WRITE(UNIT=UNIT) PREGEN
C
C Write bitmap.
C
      DO J=1,BX
          DO K=BY,2,-1
              IF (BITMAP(K,J).NE.'C0'X) GOTO 10
          END DO
   10     WRITE (UNIT=UNIT) (BITMAP(I,J),I=1,K),SUFFIX
      END DO
      WRITE(UNIT=UNIT) POSTGEN
C
C Write blank plot lines to fill up page
C
      END

C*GRGC03 -- fill buffer with a specified character
C+
      SUBROUTINE GRGC03 (BUFSIZ,BUFFER,FILL)
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
