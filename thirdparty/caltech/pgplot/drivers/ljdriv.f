C*LJDRIV -- PGPLOT Hewlett Packard LaserJet driver
C+
      SUBROUTINE LJDRIV (IFUNC, RBUF, NBUF, CHR, LCHR)
      INTEGER IFUNC, NBUF, LCHR
      REAL    RBUF(*)
      CHARACTER*(*) CHR
C
C PGPLOT driver for Hewlett packard Laserjet device.
C
C Version 1.0  - 1989 Apr 09 - S. C. Allendorf
C                              Combined all drivers into one driver that
C                              uses a logical name to choose the format.
C TJP 1997-Jul-24: replaced ENCODE with WRITE, but still VMS-specific.
C=======================================================================
C
C Supported device: Hewlett Packard LaserJet, LaserJet+, or LaserJet II.
C
C Device type code: /LJnn where nn is a number 1 - NDEV inclusive.
C
C Default device name: PGPLOT.LJPLT.
C
C Default view surface dimensions: Depends on which version of the driver
C is chosen via the logical name PGPLOT_LJ_MODE.
C
C       Driver  Equivalence         Size (H x V)
C       ------  -----------     ---------------------
C        LJ01       LHOR        10.50 by  8.00 inches
C        LJ02       PHOR         8.00 by 10.50 inches
C        LJ03       PHOT         8.00 by 10.50 inches
C        LJ04       LHBR         6.54 by  4.91 inches
C        LJ05       PHBS         5.65 by  5.65 inches
C        LJ06       LMBR        10.50 by  8.00 inches
C        LJ07       PMBR         8.00 by 10.50 inches
C        LJ08       PMBS         4.48 by  4.48 inches
C        LJ09       PLBS         6.00 by  6.00 inches
C
C Resolution: Depends on which version of the driver is chosen via the
C logical name PGPLOT_LJ_MODE.
C
C       Driver  Equivalence     Resolution
C       ------  -----------     ----------
C        LJ01       LHOR          300 DPI
C        LJ02       PHOR          300 DPI
C        LJ03       PHOT          300 DPI
C        LJ04       LHBR          300 DPI
C        LJ05       PHBS          300 DPI
C        LJ06       LMBR          150 DPI
C        LJ07       PMBR          150 DPI
C        LJ08       PMBS          150 DPI
C        LJ09       PLBS          100 DPI
C
C Color capability: Color indices 0 (erase, white) and 1 (black) are
C supported. It is not possible to change color representation.
C
C Input capability: None.
C
C File format: See the LaserJet Printer Technical Reference Manual for 
C details of the file format.
C
C Obtaining hardcopy: Use the command PRINT/PASSALL.
C-----------------------------------------------------------------------
C 
C To choose one of the specific LaserJet drivers, you must execute a DCL
C command of the following form before executing your program:
C
C $ DEFINE PGPLOT_LJ_MODE LJnn
C
C where nn is a number 1 - NDEV inclusive.  You may also use one of the
C equivalent names listed above.  These equivalent names are an attempt
C to make the driver names make sense.  They are decoded as follows:
C
C    1st character: P for protrait orientation or
C                   L for landscape orientation.
C    2nd character: H for high resolution (300 dpi) or
C                   M for medium resolution (150 dpi) or
C                   L for low resolution (100 dpi).
C    3rd character: B for a straight bitmap dump (subroutine GRLJ01) or
C                   O for an optimized bitmap dump (subroutine GRLJ02).
C    4th character: R for a rectangular view surface or
C                   S for a square view surface.
C
C A few notes are in order.  First, not all of the possible combinations
C above are supported (currently).  The driver that goes by the name of
C PHOT is a driver that puts out bitmaps suitable for inclusion in TeX
C output if you are using the Arbortext DVIHP program.  The only drivers
C that will work with unexpanded LaserJet are LJ08 and LJ09.  The other
C seven drivers require a LaserJet Plus or LaserJet II.  Finally, do NOT
C attempt to send grayscale plots to the drivers that use the optimized 
C bitmap dumps.  Terrible things will happen.
C
C If you add a driver to this file, please try to use the naming
C convention outlined above and send me a copy of the revisions.  I may
C be reached at sca@iowa.physics.uiowa.edu on the Internet or IOWA::SCA 
C on SPAN.
C-----------------------------------------------------------------------
C                                       This is the number of currently
C                                       installed devices.
      INTEGER*4  NDEV
      PARAMETER  (NDEV = 9)
C
      BYTE       ESC, FF
      LOGICAL    BITMAP(NDEV), INIT, PORTRAIT(NDEV), TEX
      INTEGER    BUFFER, BX, BY, DEVICE, HC(NDEV), I, IC, IER
      INTEGER    GRFMEM, GRGMEM, LUN, NPICT
      INTEGER    VC(NDEV)
      REAL       MAXX(NDEV), MAXY(NDEV), RESOL(NDEV), XBUF(4)
      REAL       XMAX, YMAX
      CHARACTER  ALTTYP(NDEV)*3, DEFNAM*12, MODE*20, MSG*10
      CHARACTER  TYPE(NDEV)*4
      PARAMETER  (ESC = 27)
      PARAMETER  (FF = 12)
      PARAMETER  (DEFNAM = 'pgplot.ljplt')
      SAVE
      DATA INIT  /.TRUE./
C                                       These are the NDEV sets of 
C                                       device characteristics.
      DATA BITMAP   /.FALSE., .FALSE., .FALSE.,  .TRUE.,  .TRUE.,
     1                .TRUE.,  .TRUE.,  .TRUE.,  .TRUE./
      DATA PORTRAIT /.FALSE.,  .TRUE.,  .TRUE., .FALSE.,  .TRUE.,
     1               .FALSE.,  .TRUE.,  .TRUE.,  .TRUE./
      DATA HC       /      0,       0,       0,    1139,     878,
     1                     0,       0,    1300,     754/
      DATA VC       /      0,       0,       0,    1411,    1743,
     1                     0,       0,    2156,    1605/
      DATA MAXX     / 3149.0,  2399.0,  2399.0,  1962.0,  1695.0,
     1                1574.0,  1199.0,   671.0,   599.0/
      DATA MAXY     / 2399.0,  3149.0,  3149.0,  1471.0,  1695.0,
     1                1199.0,  1574.0,   671.0,   599.0/
      DATA RESOL    /  300.0,   300.0,   300.0,   300.0,   300.0,
     1                 150.0,   150.0,   150.0,   100.0/
C                                       These are around only for
C                                       (pre)historical reasons.
      DATA ALTTYP   /  'HPN',   'HPV',   'TEX',   'HPR',   'HPE',
     1                 'HPF',   'HPT',   'HPH',   'HPM'/
      DATA TYPE     / 'LHOR',  'PHOR',  'PHOT',  'LHBR',  'PHBS',
     1                'LMBR',  'PMBR',  'PMBS',  'PLBS'/
C-----------------------------------------------------------------------
C                                       First time, translate logical
C                                       name PGPLOT_LJ_MODE and set 
C                                       device accordingly.
      IF (INIT) THEN
         CALL GRGENV ('LJ_MODE', MODE, I)
         DO 1 I = 1, NDEV
            WRITE (MSG, '(A2, I2.2)') 'LJ', I
            IF (MODE(1:4) .EQ. TYPE(I) .OR. 
     1          MODE(1:3) .EQ. ALTTYP(I) .OR.
     2          MODE(1:4) .EQ. MSG(1:4)) THEN
               DEVICE = I
               GOTO 2
            END IF
    1    CONTINUE
C                                       If no match, choose LHBR
         DEVICE = 4
    2    INIT = .FALSE.
C                                       See if user has chosen the
C                                       TeX plotfile format.
         TEX = .FALSE.
         IF (DEVICE .EQ. 3) TEX = .TRUE.
      END IF
C                                       Branch on opcode.
      GOTO ( 10,  20,  30,  40,  50,  60,  70,  80,  90, 100,
     1      110, 120, 130, 140, 150, 160, 170, 180, 190, 200,
     2      210, 220, 230, 240, 250, 260), IFUNC
C                                       Signal an error.
  900 WRITE (MSG, '(I10)') IFUNC
      CALL GRWARN ('Unimplemented function in LaserJet device driver:' 
     1             // MSG)
      NBUF = -1
      RETURN
C
C--- IFUNC = 1, Return device name -------------------------------------
C
   10 CONTINUE
      WRITE (MSG, '(I2.2)') DEVICE
      CHR = 'LJ' // MSG(1 : 2) // ' (' // TYPE(DEVICE) // ')'
      NBUF = 0
      LCHR = 11
      RETURN
C
C--- IFUNC = 2, Return physical min and max for plot device, and range
C               of color indices ---------------------------------------
C
   20 CONTINUE
      RBUF(1) = 0.0
      RBUF(2) = MAXX(DEVICE)
      RBUF(3) = 0.0
      RBUF(4) = MAXY(DEVICE)
      RBUF(5) = 0.0
      RBUF(6) = 1.0
      NBUF = 6
      LCHR = 0
      RETURN
C
C--- IFUNC = 3, Return device resolution -------------------------------
C
   30 CONTINUE
      RBUF(1) = RESOL(DEVICE)
      RBUF(2) = RESOL(DEVICE)
      RBUF(3) = 1.0
      NBUF = 3
      LCHR = 0
      RETURN
C
C--- IFUNC = 4, Return misc device info --------------------------------
C    (This device is Hardcopy, No cursor, No dashed lines, No area fill,
C    no thick lines)
C
   40 CONTINUE
      CHR = 'HNNNNNNNNN'
      NBUF = 0
      LCHR = 10
      RETURN
C
C--- IFUNC = 5, Return default file name -------------------------------
C
   50 CONTINUE
      CHR = DEFNAM
      NBUF = 0
      LCHR = LEN(DEFNAM)
      RETURN
C
C--- IFUNC = 6, Return default physical size of plot -------------------
C
   60 CONTINUE
      RBUF(1) = 0.0
      RBUF(2) = MAXX(DEVICE)
      RBUF(3) = 0.0
      RBUF(4) = MAXY(DEVICE)
      NBUF = 4
      LCHR = 0
      RETURN
C
C--- IFUNC = 7, Return misc defaults -----------------------------------
C
   70 CONTINUE
      IF (RESOL(DEVICE) .EQ. 300.0) THEN
         RBUF(1) = 3.0
      ELSE IF (RESOL(DEVICE) .EQ. 150.0) THEN
         RBUF(1) = 2.0
      ELSE 
         RBUF(1) = 1.0
      END IF
      NBUF = 1
      LCHR = 0
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
C                                       Assume success.
      RBUF(2) = 1.0
C                                       Obtain a logical unit number.
      CALL GRGLUN (LUN)
C                                       Check for an error.
      IF (LUN .EQ. -1) THEN
          CALL GRWARN ('Cannot allocate a logical unit.')
          RBUF(2) = 0
          RETURN
      ELSE
         RBUF(1) = LUN
      END IF
C                                       Open the output file.
      OPEN (UNIT = LUN, FILE = CHR(:LCHR), CARRIAGECONTROL = 'NONE',
     1      DEFAULTFILE = DEFNAM, STATUS = 'NEW',
     2      RECL = 128, FORM = 'UNFORMATTED', RECORDTYPE = 'VARIABLE', 
     3      IOSTAT = IER)
C                                       Check for an error and cleanup if
C                                       one occurred.
      IF (IER .NE. 0) THEN
          CALL GRWARN ('Cannot open output file for LaserJet plot: ' //
     1                 CHR(:LCHR))
          RBUF(2) = 0
          CALL GRFLUN (LUN)
          RETURN
      ELSE
C                                       Get the full file specification
C                                       and calculate the length of the 
C                                       string
          INQUIRE (UNIT = LUN, NAME = CHR)
          LCHR = LEN (CHR)
   91     IF (CHR (LCHR:LCHR) .EQ. ' ') THEN
              LCHR = LCHR - 1
              GOTO 91
          END IF
      END IF
C                                       Initialize the plot file.

      IF (.NOT. TEX) THEN
C                                       Choose portrait orientation
         WRITE (LUN) ESC, '&l0O'
C                                       Set horizontal and vertical 
C                                       spacing
         IF (BITMAP(DEVICE)) THEN
            WRITE (LUN) ESC, '&l6C'
            WRITE (LUN) ESC, '&k10H'
         ELSE
            WRITE (LUN) ESC, '&k.4H'
            WRITE (LUN) ESC, '&l.16C'
         END IF
         WRITE (LUN) ESC, '&l2E'
      END IF
C                                       Set the graphics resolution
      WRITE (MSG, '(I3)') INT (RESOL(DEVICE))
      WRITE (LUN) ESC, '*t', MSG(1:3), 'R'
C                                       Initialize the page counter.
      NPICT = 0
      RETURN
C
C--- IFUNC = 10, Close workstation -------------------------------------
C
  100 CONTINUE
      IF (BITMAP(DEVICE)) THEN
         WRITE (LUN) ESC, '&l8C'
      ELSE IF (.NOT. TEX) THEN
         WRITE (LUN) ESC, '&l6D'
         WRITE (LUN) ESC, '&k10H'
         WRITE (LUN) ESC, '&l2E'
      END IF
C                                       Close the file.
      CLOSE (LUN, STATUS = 'KEEP')
C                                       Deallocate the logical unit.
      CALL GRFLUN (LUN)
C
      RETURN
C
C--- IFUNC = 11, Begin picture -----------------------------------------
C
  110 CONTINUE
C                                       Set the bitmap size.
      XMAX = RBUF(1)
      YMAX = RBUF(2)
C                                       Calculate the dimensions of the
C                                       plot buffer.
      IF (PORTRAIT(DEVICE)) THEN
         BX = INT (XMAX) / 8 + 1
         BY = INT (YMAX) + 1
      ELSE
         BX = INT (YMAX) / 8 + 1
         BY = INT (XMAX) + 1
      END IF
C                                       Allocate a plot buffer.      
      IER = GRGMEM (BX * BY, BUFFER)
C                                       Check for error and clean up
C                                       if one was found.
      IF (IER .NE. 1) THEN
          CALL GRGMSG (IER)
          CALL GRQUIT ('Failed to allocate a plot buffer.')
      END IF
C                                       Increment the page number.
      NPICT = NPICT + 1
C                                       Eject the page from the printer.
      IF (NPICT .GT. 1) WRITE (LUN) FF
C                                       Set the cursor position and
C                                       start graphics mode.
      IF (BITMAP(DEVICE)) THEN
         WRITE (MSG(1:4), '(I4.4)') HC(DEVICE)
         WRITE (MSG(5:8), '(I4.4)') VC(DEVICE)
         WRITE (LUN) ESC, '&a', MSG(1:4), 'h', MSG(5:8), 'V'
      END IF
C                                       Zero out the plot buffer.
      CALL GRLJ04 (BX * BY, %VAL(BUFFER))
      RETURN
C
C--- IFUNC = 12, Draw line ---------------------------------------------
C
  120 CONTINUE
C                                       Apply any needed tranformation.
      IF (PORTRAIT(DEVICE)) THEN
         DO 125 I = 1, 4
            XBUF(I) = RBUF(I)
  125    CONTINUE
      ELSE      
         XBUF(1) = RBUF(2)
         XBUF(2) = XMAX - RBUF(1)
         XBUF(3) = RBUF(4)
         XBUF(4) = XMAX - RBUF(3)
      END IF
C                                       Draw the point into the bitmap.
      CALL GRLJ00 (1, XBUF, IC, BX, BY, %VAL (BUFFER))
      RETURN
C
C--- IFUNC = 13, Draw dot ----------------------------------------------
C
  130 CONTINUE
C                                       Apply any needed tranformation.
      IF (PORTRAIT(DEVICE)) THEN
         DO 135 I = 1, 2
            XBUF(I) = RBUF(I)
  135    CONTINUE
      ELSE
         XBUF(1) = RBUF(2)
         XBUF(2) = XMAX - RBUF(1)
      END IF
C                                       Draw the point into the bitmap.
      CALL GRLJ00 (0, XBUF, IC, BX, BY, %VAL(BUFFER))
      RETURN
C
C--- IFUNC = 14, End picture -------------------------------------------
C
  140 CONTINUE
C                                       Write out the bitmap.
      IF (BITMAP(DEVICE)) THEN
         CALL GRLJ01 (LUN, BX, BY, %VAL (BUFFER))
      ELSE
         CALL GRLJ02 (LUN, BX, BY, %VAL (BUFFER), TEX)
      END IF
C                                       Deallocate the plot buffer.
      IER = GRFMEM (BX * BY, BUFFER)
C                                       Check for an error.
      IF (IER .NE. 1) THEN
          CALL GRGMSG (IER)
          CALL GRWARN ('Failed to deallocate plot buffer.')
      END IF
      RETURN
C
C--- IFUNC = 15, Select color index ------------------------------------
C
  150 CONTINUE
C                                       Save the requested color index.
      IC = RBUF(1)
C                                       If out of range set to black.
      IF (IC .LT. 0 .OR. IC .GT. 1) THEN
          IC = 1
          RBUF(1) = IC
      END IF
      RETURN
C
C--- IFUNC = 16, Flush buffer. -----------------------------------------
C    (Not implemented: ignored.)
C
  160 CONTINUE
      RETURN
C
C--- IFUNC = 17, Read cursor. ------------------------------------------
C    (Not implemented: should not be called.)
C
  170 CONTINUE
      GOTO 900
C
C--- IFUNC = 18, Erase alpha screen. -----------------------------------
C    (Not implemented: ignored.)
C
  180 CONTINUE
      RETURN
C
C--- IFUNC = 19, Set line style. ---------------------------------------
C    (Not implemented: should not be called.)
C
  190 CONTINUE
      GOTO 900
C
C--- IFUNC = 20, Polygon fill. -----------------------------------------
C    (Not implemented: should not be called.)
C
  200 CONTINUE
      GOTO 900
C
C--- IFUNC = 21, Set color representation. -----------------------------
C    (Not implemented: ignored.)
C
  210 CONTINUE
      RETURN
C
C--- IFUNC = 22, Set line width. ---------------------------------------
C    (Not implemented: should not be called.)
C
  220 CONTINUE
      GOTO 900
C
C--- IFUNC = 23, Escape ------------------------------------------------
C    (Not implemented: ignored.)
C
  230 CONTINUE
      RETURN
C
C--- IFUNC = 24, Rectangle fill. ---------------------------------------
C    (Not implemented: should not be called.)
C
  240 CONTINUE
      GOTO 900
C
C--- IFUNC = 25, -------------------------------------------------------
C    (Not implemented: should not be called.)
C
  250 CONTINUE
      GOTO 900
C
C--- IFUNC = 26, Line of pixels. ---------------------------------------
C    (Not implemented: should not be called.)
C
  260 CONTINUE
      GOTO 900
C-----------------------------------------------------------------------
      END

C*GRLJ00 -- PGPLOT Hewlett Packard LaserJet driver, draw line
C+
      SUBROUTINE GRLJ00 (LINE, RBUF, ICOL, BX, BY, BITMAP)
      INTEGER    BX, BY, ICOL, LINE
      BYTE       BITMAP(BX, BY)
      REAL       RBUF(4)
C
C Draw a straight line segment from absolute pixel coordinates (RBUF(1),
C RBUF(2)) to (RBUF(3), RBUF(4)).  The line either overwrites (sets to
C black) or erases (sets to white) the previous contents of the bitmap,
C depending on the current color index. Setting bits is accomplished
C with a VMS BISB2 instruction, expressed in Fortran as .OR.; clearing
C bits is accomplished with a VMS BICB2 instruction, expressed in
C Fortran as .AND. .NOT.. The line is generated with a Simple Digital
C Differential Analyser (ref: Newman & Sproull). 
C
C Arguments:
C
C LINE            I I      =0 for dot, =1 for line.
C RBUF(1),RBUF(2) I R      Starting point of line.
C RBUF(3),RBUF(4) I R      Ending point of line.
C ICOL            I I      =0 for erase, =1 for write.
C BITMAP        I/O B      (address of) the frame buffer.
C
C-----------------------------------------------------------------------
      BYTE       QMASK(0 : 7)
      INTEGER    K, KX, KY, LENGTH
      REAL       D, XINC, XP, YINC, YP
      DATA       QMASK /'80'X, '40'X, '20'X, '10'X, 
     1                  '08'X, '04'X, '02'X, '01'X/
C-----------------------------------------------------------------------
      IF (LINE .GT. 0) THEN
         D = MAX (ABS (RBUF(3) - RBUF(1)), ABS (RBUF(4) - RBUF(2)))
         LENGTH = D
         IF (LENGTH .EQ. 0) THEN
            XINC = 0.0
            YINC = 0.0
         ELSE
            XINC = (RBUF(3) - RBUF(1)) / D
            YINC = (RBUF(4) - RBUF(2)) / D
         END IF
      ELSE
         LENGTH = 0
         XINC = 0.0
         YINC = 0.0
      END IF
      XP = RBUF(1) + 0.5
      YP = RBUF(2) + 0.5
      IF (ICOL .NE. 0) THEN
         DO K = 0, LENGTH
            KX = XP
            KY = (BY - 1) - INT (YP)
            BITMAP(KX / 8 + 1, KY + 1) = BITMAP(KX / 8 + 1, KY + 1) .OR.
     1                                         QMASK(MOD (KX, 8))
            XP = XP + XINC
            YP = YP + YINC
         END DO
      ELSE
         DO K = 0,LENGTH
            KX = XP
            KY = (BY - 1) - INT (YP)
            BITMAP(KX / 8 + 1, KY + 1) = BITMAP(KX / 8 + 1, KY + 1) 
     1                                  .AND. (.NOT. QMASK(MOD (KX, 8)))
            XP = XP + XINC
            YP = YP + YINC
         END DO
      END IF
C-----------------------------------------------------------------------
      RETURN
      END

C*GRLJ01 -- PGPLOT LaserJet driver, copy bitmap to output file
C+
      SUBROUTINE GRLJ01 (LUN, BX, BY, BITMAP)
      INTEGER  BX, BY, LUN
      BYTE     BITMAP(BX, BY)
C
C Arguments:
C
C  LUN    (input)  Fortran unit number for output
C  BX, BY (input)  dimensions of BITMAP
C  BITMAP (input)  the bitmap array
C-----------------------------------------------------------------------
      BYTE      ESC
      INTEGER   I, J, K
      CHARACTER KSTR*3
      PARAMETER (ESC = 27)
C-----------------------------------------------------------------------
C                                       Start graphics mode
      WRITE (LUN) ESC, '*r1A'
C                                       Loop through bitmap
      DO J = 1, BY
C                                       Search for last non-NUL
         DO K = BX, 2, -1
            IF (BITMAP(K, J) .NE. '00'X) GO TO 10
         END DO
C                                       Guarantee that we know what K
C                                       is after loop. 
C                                       (Remember FORTRAN IV!?)
         K = 1
C                                       Encode length of line
   10    WRITE (KSTR, '(I3.3)') K
C                                       Write out the raster line
         WRITE (LUN) ESC, '*b', KSTR, 'W', (BITMAP(I, J), I = 1, K)
      END DO
C                                       Turn off graphics mode.
      WRITE (LUN) ESC, '*rB'
C-----------------------------------------------------------------------
      RETURN
      END

C*GRLJ02 -- PGPLOT LaserJet+ driver, dump bitmap to device
C+
      SUBROUTINE GRLJ02 (LUN, BX, BY, BITMAP, TEX)
      LOGICAL TEX
      INTEGER LUN, BX, BY
      BYTE BITMAP(BX, BY)
C
C Output raster for this page.  This routine has been optimised to 
C minimize the memory usage in the LaserJet.  This sometimes leads to a
C larger file than if a straight bitmap approach had been used.
C
C NOTE:  This subroutine is a kludge to make a 512K LaserJet produce 
C        full page plots at 300dpi.  It will not always produce the plot
C        on one page.  If you overrun the memory restrictions, two pages
C        will be printed, each containing parts of the plot.  One must 
C        then resort to cut and paste techniques to restore the plot.  
C        Most simple line graphs do not come close to the memory limit, 
C        but sometimes a messy contour plot will.  DON'T EVEN THINK
C        ABOUT SENDING A GREYSCALE TO THIS SUBROUTINE!
C
C Arguments:
C
C LUN             I I      Logical unit number of output file
C BX, BY          I I      Dimensions of frame buffer
C BITMAP        I/O B      (address of) the frame buffer.
C
C Version 1.0  03-Sep-1986  S. C. Allendorf
C Version 2.0  08-Dec-1986  S. C. Allendorf  Use relative positioning
C Version 2.1  28-Dec-1986  S. C. Allendorf  Optimize positioning code
C Version 3.0  02-Jan-1987  S. C. Allendorf  Add code for rules
C VERSION 3.1  10-FEB-1988  S. C. Allendorf  Attempt to speed up code
C-----------------------------------------------------------------------
      BYTE       ESC, N0
      LOGICAL    NOBIT
      INTEGER    CNUM, CONUM, CURCOL, CURROW, FB(35), FB2(25), I, IPOS
      INTEGER    IYOFF, J, K, L, M, N, NB(35), NBNUM, NBTOT, NBNUM2
      INTEGER    NB2(25), RNUM, RONUM, GRLJ03
      CHARACTER  ALLONE*300, COL*5, NBYTE*4, NULLS*(10), ROW*5, X*300
      PARAMETER  (N0 = 0)
      PARAMETER  (ESC = 27)
C-----------------------------------------------------------------------
C                                       Define some useful constants
      IF (TEX) THEN
         IYOFF = 0
      ELSE
         IYOFF = 75
      END IF
      DO J = 1, 10
         NULLS(J:J) = CHAR (0)
      END DO
      DO J = 1, 300
         ALLONE(J:J) = CHAR (255)
      END DO
C                                       Initialize some variables
      CURCOL = 0
      CURROW = 0
C                                       Position the cursor
      IF (.NOT. TEX) THEN
         WRITE (LUN) ESC, '*p0y0X'
      END IF
C                                       Set up vertical rule height
      WRITE (LUN) ESC, '*c1B'
C                                       Write out each line on page
      DO K = 1, BY
C                                       Copy raster to buffer and find
C                                       the beginning and end of the 
C                                       bitmap line
         NOBIT = .TRUE.
         NBTOT = 0
         FB(1) = BX
         DO J = 1, BX
            X(J:J) = CHAR (BITMAP(J,K))
            IF (X(J:J) .NE. NULLS(1:1)) THEN
               NOBIT = .FALSE.
               NBTOT = J
               FB(1) = MIN (FB(1), J)
            END IF
         END DO
C                                       Break line into pieces
         IF (.NOT. NOBIT) THEN
            L = 1
            GO TO 20
   10       NB(L) = FB(L) + IPOS - 2
            L = L + 1
C                                       Search for first non-null
            DO J = NB(L-1) + 11, NBTOT
               IF (X(J:J) .NE. NULLS(1:1)) THEN
                  FB(L) = J
                  GO TO 20
               END IF
            END DO
C                                       Search for a string of nulls
   20       IPOS = INDEX (X(FB(L):NBTOT), NULLS)
            IF (IPOS .EQ. 0) THEN
               NB(L) = NBTOT
               GO TO 30
            ELSE
               GO TO 10
            END IF
C                                       Loop through each substring
   30       DO J = 1, L
C                                       Search for rules
               M = 1
               FB2(1) = FB(J)
               GO TO 50
   40          IF (IPOS .NE. 1) THEN
                  NB2(M) = 0
                  DO I = FB2(M), FB2(M) + IPOS - 2
                     IF (X(I:I) .NE. NULLS(1:1)) THEN
                        NB2(M) = MAX (FB2(M), I)
                     END IF
                  END DO
                  M = M + 1
                  FB2(M) = FB2(M-1) + IPOS - 1
                  IF (NB2(M-1) .EQ. 0) THEN
                     FB2(M-1) = FB2(M)
                     M = M - 1
                  END IF
               END IF
C                                       Search for first non-<XFF>
               DO N = FB2(M) + 25, NB(J)
                  IF (X(N:N) .NE. ALLONE(1:1)) THEN
                     NB2(M) = N - 1
                     M = M + 1
                     FB2(M) = N
                     GO TO 50
                  END IF
               END DO
               NB2(M) = NB(J)
               GO TO 60
C                                       Search for a string of <XFF>s
   50          IPOS = INDEX (X(FB2(M):NB(J)), ALLONE(1:25))
               IF (IPOS .EQ. 0) THEN
                  NB2(M) = NB(J)
                  GO TO 60
               ELSE
                  GO TO 40
               END IF
C                                       Print each of the substrings
   60          DO I = 1, M
C                                       Get the number of bytes
                  NBNUM = NB2(I) - FB2(I) + 1
C                 ENCODE (4, 1000, NBYTE) NBNUM
                  WRITE (NBYTE, 1000) NBNUM
                  NBNUM2 = GRLJ03 (NBNUM)
C                                       Calculate the row and column
                  RONUM = K + IYOFF
                  CONUM = (FB2(I) - 1) * 8
C                                       Determine the positioning
C                                       sequence and write it out
                  IF (RONUM .NE. CURROW .AND. CONUM .NE. CURCOL) THEN
                     RNUM = RONUM - CURROW
                     CNUM = CONUM - CURCOL
C                    ENCODE (5, 1010, ROW) RNUM
C                    ENCODE (5, 1010, COL) CNUM
                     WRITE (ROW, 1010) RNUM
                     WRITE (COL, 1010) CNUM
                     RNUM = GRLJ03 (ABS (RNUM)) + 1
                     CNUM = GRLJ03 (ABS (CNUM)) + 1
                     WRITE (LUN) ESC, '*p', ROW(6-RNUM:5), 'y', 
     +                                              COL(6-CNUM:5), 'X'
                  ELSE IF (RONUM .NE. CURROW) THEN
                     RNUM = RONUM - CURROW
C                    ENCODE (5, 1010, ROW) RNUM
                     WRITE (ROW, 1010) RNUM
                     RNUM = GRLJ03 (ABS (RNUM)) + 1
                     WRITE (LUN) ESC, '*p', ROW(6-RNUM:5), 'Y'
                  ELSE IF (CONUM .NE. CURCOL) THEN
                     CNUM = CONUM - CURCOL
C                    ENCODE (5, 1010, COL) CNUM
                     WRITE (COL, 1010) CNUM
                     CNUM = GRLJ03 (ABS (CNUM)) + 1
                     WRITE (LUN) ESC, '*p', COL(6-CNUM:5), 'X'
                  END IF
C                                       Check for all bits set in
C                                       substring
                  IF ((INDEX (X(FB2(I):NB2(I)), ALLONE(1:NBNUM)) .EQ. 1)
     +                 .AND. NBNUM .GE. 5) THEN
                     NBNUM = NBNUM * 8
C                    ENCODE (4, 1000, NBYTE) NBNUM
                     WRITE (NBYTE, 1000) NBNUM
                     NBNUM2 = GRLJ03 (NBNUM)
                     WRITE (LUN) ESC, '*c', NBYTE(5-NBNUM2:4), 'A'
                     WRITE (LUN) ESC, '*c0P'
                     CURROW = RONUM
                     CURCOL = CONUM
                  ELSE
C                                       Write out raster line
                     WRITE (LUN) ESC, '*r1A'
                     WRITE (LUN) ESC, '*b', NBYTE(5-NBNUM2:4), 'W', 
     +                                                X(FB2(I):NB2(I))
                     WRITE (LUN) ESC, '*rB'
                     CURROW = RONUM + 1
                     CURCOL = CONUM
                  END IF
               END DO
            END DO
         END IF
      END DO
C-----------------------------------------------------------------------
 1000 FORMAT (I4.4)
 1010 FORMAT (SP,I5)
      RETURN
      END

C*GRLJ03 -- PGPLOT LaserJet+ driver, calculate length of an integer
C+
      INTEGER FUNCTION GRLJ03 (I)
      INTEGER I
C
C This function calculates the number of digits in a supplied integer.
C
C Arguments:
C
C I               I I      Integer value of number
C GRLJ03          O I      Length of printed representation of I
C
C Version 1.0  10-Feb-1988  S. C. Allendorf
C-----------------------------------------------------------------------
      IF (I .GE. 10) THEN
         IF (I .GE. 100) THEN
            IF (I .GE. 1000) THEN
               GRLJ03 = 4
            ELSE
               GRLJ03 = 3
            END IF
         ELSE
            GRLJ03 = 2
         END IF
      ELSE
         GRLJ03 = 1
      END IF
C-----------------------------------------------------------------------
      RETURN
      END

C*GRLJ04 -- zero fill buffer
C+
      SUBROUTINE GRLJ04 (BUFSIZ,BUFFER)
C
C Arguments:
C
C BUFFER (byte array, input): (address of) the buffer.
C BUFSIZ (integer, input): number of bytes in BUFFER.
C-----------------------------------------------------------------------
      INTEGER  BUFSIZ, I
      BYTE     BUFFER(BUFSIZ), FILL
      DATA     FILL /0/
C
      DO 10 I=1,BUFSIZ
          BUFFER(I) = FILL
   10 CONTINUE
      END
