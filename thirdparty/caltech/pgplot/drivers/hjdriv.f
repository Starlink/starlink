*HJDRIV -- PGPLOT Hewlett Packard [Desk/Laser] Jet driver
C+
      SUBROUTINE HJDRIV (IFUNC, RBUF, NBUF, CHR, LCHR)
      INTEGER IFUNC, NBUF, LCHR
      REAL    RBUF(*)
      CHARACTER*(*) CHR
C
C PGPLOT driver for Hewlett Packard Desk/Laserjet device.
C
C Version 1.0  - 1989 Apr 09 - S. C. Allendorf
C                              Combined all drivers into one driver that
C                              uses a logical name to choose the format.
C Version 1.1  - 1989 Sept - B. H. Toby
C                              (1) adapt for PC version of PGPLOT
C                              (2) use alternate logical name definitions
C                              (3) support for DeskJet/ " Plus/ " 500
C                              (4) reduce page size to 10.25 to fix PGIDENT
C
C Version 1.2  - 1991 Aug - B. H. Toby
C                              Clean up and add code for GRIFB1 since the
C                              subroutine is not in GRPCKG as of PGPLOT V4.9d
C
C   IBM PC / HP DeskJet printer usage
C       Default file name is LPT1 (parallel port#1)
C       Setup the port using MODE LPTn:,,P         (parallel)
C                         or MODE COMn:96,N,8,1,P  (serial)
C   Use COMn/HJ or LPTn/HJ to send output directly to a device
C       or FILE.EXT/HJ or d:\path\file.ext/HJ to send the output
C      to a file
C   Files can be written to disk and then copied to the printer.
C     However, there is a problem in treating plot files, since they
C     may contain ^Z (end-of-file) and other control characters. Use
C              COPY file.ext /B   LPT1:
C     to print the file.
C   Note that logical name PGPLOT_xx under VMS corresponds to MS-DOS
C       environment variable PG_xxx
C Ported back to VAX/VMS, lines of code changed are indicated by a "C!" flag.
C=======================================================================
C
C Supported device: Hewlett Packard LaserJet, LaserJet+, or LaserJet II.
C                   DeskJet, DeskJet Plus, DeskJet 500
C
C Device type code: /HJ
C
C Default device name: PGPLOT.HJPLT.
C
C Default view surface dimensions: Depends on which driver settings are
C chosen, via logical names PGPLOT_HJ_MODE, PGPLOT_HJ_MAR, PGPLOT_HJ_SIZE
C and PGPLOT_HJ_PAGE.
C
C Resolution: Depends on which driver settings are chosen, via
C logical names PGPLOT_HJ_MODE or PGPLOT_HJ_RES.
C
C Color capability: Color indices 0 (erase, white) and 1 (black) are
C supported. It is not possible to change color representation.
C
C Input capability: None.
C
C File format: See the LaserJet & DeskJet Printer Technical Reference Manuals
C for details of the file format.
C
C Obtaining hardcopy: Use the command PRINT/PASSALL.
C
C Logical Name Usage:
C ------- ---- ------
C
C   PGPLOT_HJ_MODE: use $ DEFINE PGPLOT_HJ_MODE HJnn
C
C     where nn is a number 1 - NDEV inclusive.  You may also use one of the
C     equivalent names listed below.
C       Thus  $ DEFINE PGPLOT_HJ_MODE HJ01
C        and  $ DEFINE PGPLOT_HJ_MODE LHOR  are equivalent (etc.)
C     The equivalent names are an attempt to make the driver names make
C     sense.  They are decoded as follows:
C
C        1st character: P for protrait orientation or
C                       L for landscape orientation.
C        2nd character: H for high resolution (300 dpi) or
C                       M for medium resolution (150 dpi) or
C                       L for low resolution (100 dpi).
C        3rd character: B for a straight bitmap dump (subroutine GRHJ01) or
C                       O for an optimized bitmap dump (subroutine GRHJ02).
C        4th character: R for a rectangular view surface or
C                       S for a square view surface.
C
C     A few notes are in order.  First, not all of the possible combinations
C     above are supported (currently).  The driver that goes by the name of
C     PHOT is a driver that puts out bitmaps suitable for inclusion in TeX
C     output if you are using the Arbortext DVIHP program.  The only drivers
C     that will work with unexpanded LaserJet are HJ08 and HJ09.  The other
C     seven drivers require a LaserJet Plus or LaserJet II.  Finally, do NOT
C     attempt to send grayscale plots to the drivers that use the optimized
C     bitmap dumps.  Terrible things will happen.
C
C       Driver  Equiv         Size (H x V)          Resolution
C       ------  -----     ---------------------    ----------
C        HJ01   LHOR        10.25 by  8.00 inches   300 DPI
C        HJ02   PHOR         8.00 by 10.25 inches   300 DPI
C        HJ03   PHOT         8.00 by 10.25 inches   300 DPI
C        HJ04   LHBR         6.54 by  4.91 inches   300 DPI
C        HJ05   PHBS         5.65 by  5.65 inches   300 DPI
C        HJ06   LMBR        10.25 by  8.00 inches   150 DPI
C        HJ07   PMBR         8.00 by 10.25 inches   150 DPI
C        HJ08   PMBS         4.48 by  4.48 inches   150 DPI
C        HJ09   PLBS         6.00 by  6.00 inches   100 DPI
C
C  The following logical names will override the PGPLOT_HJ_MODE settings,
C  if used.
C
C   PGPLOT_HJ_RES: use $ DEFINE PGPLOT_HJ_RES x  where x is H, M, L or V
C          H or HIGH     for 300 bpi
C          M or MEDIUM   for 150 bpi
C          L or LOW      for 100 bpi
C          V or VERYLOW  for  75 bpi
C
C   PGPLOT_HJ_MAR: use $ DEFINE PGPLOT_HJ_MAR  "xx.xx,yy.yy"
C       where "xx.xx" and "yy.yy" are the vertical and horizontal
C       margin dimensions in inches. The number of characters, including
C       spaces preceeding and following the comma, should not exceed five.
C         $ DEFINE PGPLOT_HJ_MAR  "1.0,1.0" is valid
C         $ DEFINE PGPLOT_HJ_MAR  " 1.0 ,1.0" is valid
C         but $ DEFINE PGPLOT_HJ_MAR  " 1.00 ,1.0" is not valid
C
C   PGPLOT_HJ_SIZE: use $ DEFINE PGPLOT_HJ_SIZE  "xx.xx,yy.yy"
C       where "xx.xx" and "yy.yy" are the vertical and horizontal
C       plot dimensions in inches. The number of characters, including
C       spaces preceeding and following the comma, should not exceed five.
C         $ DEFINE PGPLOT_HJ_SIZE  "10.,8." is valid
C         $ DEFINE PGPLOT_HJ_SIZE  "10.0 , 8.0 " is valid
C         but $ DEFINE PGPLOT_HJ_SIZE  " 10.0 ,8.0" is not valid
C
C   PGPLOT_HJ_TEX: use $ DEFINE PGPLOT_HJ_TEX  T
C         if PGPLOT_HJ_TEX is defined with any value, TeX mode (see above)
C         will be used.
C
C   PGPLOT_HJ_NOFF: use $ DEFINE PGPLOT_HJ_NOFF  T
C         if PGPLOT_HJ_NOFF is defined with any value, the form feed
C         needed to eject the final page will be omitted. This is useful
C         for spooled printers -- it prevents wasted (blank) pages.
C
C   PGPLOT_HJ_PAGE: use $ DEFINE PGPLOT_HJ_PAGE x  where x is L or P
C     Use L (or LANDSCAPE) for Landscape mode
C     Use P (or PORTRAIT) for Portrait mode
C
C   PGPLOT_HJ_OPT: use $ DEFINE PGPLOT_HJ_OPT x  where x is O or C
C     Use O (or OPTIMIZE) so that bitmap will be "optimized"
C     Use C (or COMPRESS) so that bitmap will be "compressed"
C
C       "Optimized" mode minimizes the memory usage for the LaserJet devices.
C       This sometimes leads to a larger file than if optimization is not
C       used. Optimized mode may not be used with the DeskJet devices.
C
C       "Compressed" mode decreases the size of the bitmap file for later
C       model HP devices, particularly the DeskJet devices.
C
C-----------------------------------------------------------------------
C
C This driver was originally written by S. C. Allendorf and modified
C by B. H. Toby. Any bugs are likely due to my (BHT) kludges. Send
C improvements and fixes to this driver to sca@iowa.physics.uiowa.edu
C (Internet) or IOWA::SCA (SPAN) and to TOBY@PETVAX.LRSM.UPENN.EDU.
C
C-----------------------------------------------------------------------
C                                       This is the number of currently
C                                       installed device types.
      INTEGER*4  NDEV
      PARAMETER  (NDEV = 9)
C
      LOGICAL    INIT  /.TRUE./
      INTEGER*4  BX, BY, DEVICE, I, IC, IER
      INTEGER*4  LUN, NPICT
      REAL*4     XBUF(4)
      REAL*4     XMAX, YMAX
      CHARACTER  ALTTYP(NDEV)*3, MODE*30, MSG*10
      CHARACTER  TYPE(NDEV)*4
      INTEGER    GRTRIM
C! VAX/VMS
      INTEGER*4  GRFMEM, GRGMEM
      CHARACTER  DEFNAM*12
      PARAMETER  (DEFNAM = 'PGPLOT.HJPLT')
      BYTE       ESC, FF
      INTEGER*4  BUFFER
C! PC:
C!      CHARACTER  DEFNAM*4
C!      PARAMETER  (DEFNAM = 'LPT1')
C!      INTEGER*1  ESC, FF
C!      INTEGER*1  BUFFER[ALLOCATABLE, HUGE] (:,:)
C
      PARAMETER  (ESC = 27)
      PARAMETER  (FF = 12)
C actual settings
      LOGICAL    TEX,NOFF
      REAL*4     T1,T2
      REAL*4     dev_VC, dev_HC
      REAL*4     dev_resol,dev_maxX,dev_maxY
      LOGICAL    dev_bitmap_L, dev_port_L, dev_cmprs_L
      CHARACTER  dev_name*80
 
C                                       These are the NDEV sets of
C                                       device characteristics.
      LOGICAL    BITMAP(NDEV)
     1              /.FALSE., .FALSE., .FALSE.,  .TRUE.,  .TRUE.,
     2                .TRUE.,  .TRUE.,  .TRUE.,  .TRUE./
      LOGICAL    PORTRAIT(NDEV)
     1              /.FALSE.,  .TRUE.,  .TRUE., .FALSE.,  .TRUE.,
     2               .FALSE.,  .TRUE.,  .TRUE.,  .TRUE./
      REAL*4  HC(NDEV)
     1              /     0.,      0.,      0.,    1.58,    1.22,
     2                    0.,      0.,    1.80,    1.05/
      REAL*4  VC(NDEV)
     1              /     0.,      0.,      0.,    1.96,    2.42,
     2                    0.,      0.,    3.00,    2.23/
      REAL*4     XPAGMX(NDEV)
     1              /  10.25,    8.00,    8.00,    6.54,    5.65,
     2                 10.25,    8.00,    4.48,    6.00/
      REAL*4     YPAGMX(NDEV)
     1              /   8.00,   10.25,   10.25,    4.91,    5.65,
     2                  8.00,   10.25,    4.48,    6.00/
      INTEGER*2  RESOL(NDEV)
     1              /    300,     300,     300,     300,     300,
     2                   150,     150,     150,     100/
C Names for PGPLOT_HJ_MODE
      DATA TYPE     / 'LHOR',  'PHOR',  'PHOT',  'LHBR',  'PHBS',
     1                'LMBR',  'PMBR',  'PMBS',  'PLBS'/
C These names are around only for (pre)historical reasons.
      DATA ALTTYP   /  'HPN',   'HPV',   'TEX',   'HPR',   'HPE',
     1                 'HPF',   'HPT',   'HPH',   'HPM'/
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C                                       First time, translate logical
C                                       name PGPLOT_HJ_MODE and set
C                                       device accordingly.
      IF (INIT) THEN
         CALL GRGENV ('HJ_MODE', MODE, I)
         DO 1 I = 1, NDEV
            WRITE (MSG, '(A2, I2.2)') 'HJ', I
            IF (MODE(1:4) .EQ. TYPE(I) .OR.
     1          MODE(1:3) .EQ. ALTTYP(I) .OR.
     2          MODE(1:4) .EQ. MSG(1:4)) THEN
               DEVICE = I
               GOTO 2
            END IF
    1    CONTINUE
C                                       If no match, choose LMBR
         DEVICE = 6
    2    INIT = .FALSE.
C                                       See if user has chosen the
C                                       TeX plotfile format.
         TEX = .FALSE.
         IF (DEVICE .EQ. 3) TEX = .TRUE.
         dev_cmprs_L = .FALSE.
C-----------------------------------------------------------------------
C set actual device settings from table entries
C  dev_VC and dev_HC are margin settings in inches: for non-optimized bitmaps
         dev_VC = VC(DEVICE)
         dev_HC = HC(DEVICE)
C  dev_resol  is the resolution in dots per inch
         dev_resol = RESOL(DEVICE)
C  dev_maxX and dev_maxY are the X and Y plot limits in inches
         dev_maxX = Xpagmx(DEVICE)
         dev_maxY = Ypagmx(DEVICE)
C  if dev_bitmap_L is false then the file can be optimized
         dev_bitmap_L = BITMAP(DEVICE)
C  if dev_port_L is false then a landscape orientation is used
         dev_port_L = PORTRAIT(DEVICE)
C  if TEX is true then much of the device control code is omitted so that
C    the file can be included by the TeX post-processor
C-----------------------------------------------------------------------
C Override the device settings according to logical variables:
C   PGPLOT_HJ_RES  can be H or HIGH     for 300 bpi
C                         M or MEDIUM   for 150 bpi
C                         L or LOW      for 100 bpi
C                         V or VERYLOW  for  75 bpi
          CALL GRGENV ('HJ_RES', MODE, I)
          IF (mode(1:1) .eq. 'H')  then
            dev_resol = 300
          ELSEIF (mode(1:1) .eq. 'M')  then
            dev_resol = 150
          ELSEIF (mode(1:1) .eq. 'L')  then
            dev_resol = 100
          ELSEIF (mode(1:1) .eq. 'V')  then
            dev_resol =  75
C!          ELSE
C!   for PC, set resolution to 150 dpi or less unless it has been
C!   specifically set to 300
C!            dev_resol = min(150.,dev_resol)
          ENDIF
C   PGPLOT_HJ_MAR  contains the vertical and horizontal margins in inches
          CALL GRGENV ('HJ_MAR', MODE, I)
          IF (i .gt. 0 .and. mode(:I) .ne. ' ') THEN
            read(mode(:I),'(2f6.0)',err=34) t1,t2
            dev_VC = t1
            dev_HC = t2
          ENDIF
C   PGPLOT_HJ_SIZE if defined contains the X and Y page size in inches
34        CALL GRGENV ('HJ_SIZE', MODE, I)
          IF (i .gt. 0 .and. mode(:I) .ne. ' ') THEN
            read(mode(:I),'(2f6.0)',err=35) t1,t2
            dev_maxX = t1
            dev_maxY = t2
          ENDIF
C   PGPLOT_HJ_TEX  can have any value, if defined will set TeX mode
35        CALL GRGENV ('HJ_TEX', MODE, I)
          IF (i .gt. 0 .and. mode .ne. ' ') then
            TEX = .TRUE.
          ENDIF
C   PGPLOT_HJ_NOFF can have any value, if defined will skip the final
C   form feed -- this prevents wasted (blank) pages from spooled jobs
          NOFF = .false.
          CALL GRGENV ('HJ_NOFF', MODE, I)
          IF ((i .gt. 0 .and. mode .ne. ' ') .or. TEX) then
            NOFF = .true.
          ENDIF
C If PGPLOT_HJ_PAGE is set to L (or LANDSCAPE) for Landscape mode
C                   is set to P (or PORTRAIT) for Portrait mode
          CALL GRGENV ('HJ_PAGE', MODE, I)
          IF (mode(1:1) .eq. 'L' .or. mode(1:1) .eq. 'l')
     1        dev_port_L = .false.
          IF (mode(1:1) .eq. 'P' .or. mode(1:1) .eq. 'p')
     1        dev_port_L = .true.
C If PGPLOT_HJ_OPT  is set to O (or OPTIMIZE) the bitmap will be optimized
C                   is set to C (or COMPRESS) the bitmap will be compressed
          CALL GRGENV ('HJ_OPT', MODE, I)
          IF (mode(1:1) .eq. 'O' .or. mode(1:1) .eq. 'o')
     1                dev_bitmap_L = .FALSE.
          IF (mode(1:1) .eq. 'C' .or. mode(1:1) .eq. 'c')
     1                dev_cmprs_L = .TRUE.
C Define the device name to include the settings: name will be of form
C                /HJ -string
C    where the string will be "obrT  x.x  y.y" where
C  o   P for Portrait orientation, L for landscape, blank otherwise
C  b   O for optimized bitmaps, C for compressed bitmaps, B otherwise
C  r   is the resolution in dots per inch: 300 - H; 150 - M; 100 - L; 75 - V
C  T   for TeX mode, blank otherwise
C  x.x  is the size of the page in the x direction
C  y.y  is the size of the page in the y direction
          mode = 'L B'
          IF (dev_port_L) mode(1:1) = 'P'
          IF (.not. dev_bitmap_L) mode(2:2) = 'O'
          IF (dev_cmprs_L) mode(2:2) = 'C'
          IF (dev_resol .eq. 300) then
            mode(3:3) = 'H'
          ELSEIF (dev_resol .eq. 150) then
            mode(3:3) = 'M'
          ELSEIF (dev_resol .eq. 100) then
            mode(3:3) = 'L'
          ELSEIF (dev_resol .eq. 75) then
            mode(3:3) = 'V'
          ELSE
            mode(3:3) = '?'
          ENDIF
          IF (TEX) mode(4:4) = 'T'
          IF (dev_maxX .gt. 10) then
            WRITE (mode(5:),'(f3.0)') dev_maxX
          ELSE
            WRITE (mode(5:),'(f3.1)') dev_maxX
          ENDIF
          IF (dev_maxY .gt. 10) then
            WRITE (mode(9:),'(f3.0)') dev_maxY
          ELSE
            WRITE (mode(9:),'(f3.1)') dev_maxY
          ENDIF
         DEV_NAME = 'HJ (Hewlett-Packard Deskjet/Laserjet) ' // mode
        ENDIF
C-----------------------------------------------------------------------
C                                       Branch on opcode.
      GOTO ( 10,  20,  30,  40,  50,  60,  70,  80,  90, 100,
     1      110, 120, 130, 140, 150, 160, 170, 180, 190, 200,
     2      210, 220, 230, 240, 250, 260), IFUNC
C                                       Signal an error.
  900 WRITE (MSG, '(I10)') IFUNC
      CALL GRWARN ('Unimplemented function in HJ "Jet" device driver:'
     1             // MSG)
      NBUF = -1
      RETURN
C
C--- IFUNC = 1, Return device name -------------------------------------
C
   10 CONTINUE
      CHR = dev_name
      NBUF = 0
      LCHR = GRTRIM(dev_name)
      RETURN
C
C--- IFUNC = 2, Return physical min and max for plot device, and range
C               of color indices ---------------------------------------
C
   20 CONTINUE
      RBUF(1) = 0.0
C  convert dev_maxX and dev_maxY from inches to pixels
      RBUF(2) = dev_maxX * dev_resol - 1
      RBUF(3) = 0.0
      RBUF(4) = dev_maxY * dev_resol - 1
      RBUF(5) = 0.0
      RBUF(6) = 1.0
      NBUF = 6
      LCHR = 0
      RETURN
C
C--- IFUNC = 3, Return device resolution -------------------------------
C
   30 CONTINUE
      RBUF(1) = dev_resol
      RBUF(2) = dev_resol
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
C  convert dev_maxX and dev_maxY from inches to pixels
      RBUF(2) = dev_maxX * dev_resol - 1
      RBUF(3) = 0.0
      RBUF(4) = dev_maxY * dev_resol - 1
      NBUF = 4
      LCHR = 0
      RETURN
C
C--- IFUNC = 7, Return misc defaults -----------------------------------
C
   70 CONTINUE
      IF (dev_resol .EQ. 300.0) THEN
         RBUF(1) = 3.0
      ELSE IF (dev_resol .EQ. 150.0) THEN
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
C!      OPEN (UNIT = LUN, FILE = CHR(:LCHR), STATUS = 'UNKNOWN',
C!     2      FORM = 'BINARY',
C!     3      IOSTAT = IER)
C                                       Check for an error and cleanup if
C                                       one occurred.
      IF (IER .NE. 0) THEN
          CALL GRWARN ('Cannot open output file for HP "Jet: plot: ' //
     1                 CHR(:LCHR))
C!          CALL GRWARN ('Cannot open output file for HP "Jet" plot: ')
C!          CALL GRWARN (CHR(:LCHR))
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
         IF (dev_bitmap_L) THEN
            WRITE (LUN) ESC, '&l6C'
            WRITE (LUN) ESC, '&k10H'
         ELSE
            WRITE (LUN) ESC, '&k.4H'
            WRITE (LUN) ESC, '&l.16C'
         END IF
         WRITE (LUN) ESC, '&l2E'
      END IF
C                                       Set the graphics resolution
      WRITE (MSG, '(I3.3)') INT (dev_resol)
      WRITE (LUN) ESC, '*t', MSG(1:3), 'R'
C                                       Initialize the page counter.
      NPICT = 0
      RETURN
C
C--- IFUNC = 10, Close workstation -------------------------------------
C
  100 CONTINUE
      IF (dev_bitmap_L) THEN
         WRITE (LUN) ESC, '&l8C'
      ELSEIF (.NOT. TEX) THEN
         WRITE (LUN) ESC, '&l6D'
         WRITE (LUN) ESC, '&k10H'
         WRITE (LUN) ESC, '&l2E'
      END IF
C eject the page
      IF (.not. NOFF) WRITE (LUN) FF
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
      IF (dev_port_L) THEN
         BX = INT (XMAX) / 8 + 1
         BY = INT (YMAX) + 1
      ELSE
         BX = INT (YMAX) / 8 + 1
         BY = INT (XMAX) + 1
      END IF
C                                       Allocate a plot buffer.
C                                       Check for error and clean up
C                                       if one was found.
C! VAX
      IER = GRGMEM (BX * BY, BUFFER)
      IF (IER .NE. 1) THEN
          CALL GRGMSG (IER)
C! PC
C!      ALLOCATE (BUFFER(BX,BY), STAT = IER)
C!      IF (IER .NE. 0) THEN
          CALL GRQUIT ('Failed to allocate a plot buffer.')
      END IF
C                                       Increment the page number.
      NPICT = NPICT + 1
C                                       Eject the page from the printer.
      IF (NPICT .GT. 1) WRITE (LUN) FF
C                                       Set the cursor position and
C                                       start graphics mode.
      IF (dev_bitmap_L) THEN
         WRITE (MSG(1:4), '(I4.4)') nint(dev_HC*720.)
         WRITE (MSG(5:8), '(I4.4)') nint(dev_VC*720.)
         WRITE (LUN) ESC, '&a', MSG(1:4), 'h', MSG(5:8), 'V'
      END IF
C                                       Zero out the plot buffer.
      CALL GRHJ05 (BX * BY, %VAL(BUFFER))
      RETURN
C
C--- IFUNC = 12, Draw line ---------------------------------------------
C
  120 CONTINUE
C                                       Apply any needed tranformation.
      IF (dev_port_L) THEN
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
      CALL GRHJ00 (1, XBUF, IC, BX, BY, %VAL (BUFFER))
C!      CALL GRHJ00 (1, XBUF, IC, BX, BY, BUFFER)
      RETURN
C
C--- IFUNC = 13, Draw dot ----------------------------------------------
C
  130 CONTINUE
C                                       Apply any needed tranformation.
      IF (dev_port_L) THEN
         DO 135 I = 1, 2
            XBUF(I) = RBUF(I)
  135    CONTINUE
      ELSE
         XBUF(1) = RBUF(2)
         XBUF(2) = XMAX - RBUF(1)
      END IF
C                                       Draw the point into the bitmap.
      CALL GRHJ00 (0, XBUF, IC, BX, BY, %VAL(BUFFER))
C!      CALL GRHJ00 (0, XBUF, IC, BX, BY, BUFFER)
      RETURN
C
C--- IFUNC = 14, End picture -------------------------------------------
C
  140 CONTINUE
C                                       Write out the bitmap.
      IF (dev_bitmap_L .and. dev_cmprs_L) THEN
         CALL GRHJ04 (LUN, BX, BY, %VAL(BUFFER))
C!         CALL GRHJ04 (LUN, BX, BY, BUFFER)
      ELSEIF (dev_bitmap_L) THEN
         CALL GRHJ01 (LUN, BX, BY, %VAL (BUFFER))
C!         CALL GRHJ01 (LUN, BX, BY, BUFFER)
      ELSE
         CALL GRHJ02 (LUN, BX, BY, %VAL (BUFFER), TEX)
C!         CALL GRHJ02 (LUN, BX, BY, BUFFER, TEX)
      END IF
C                                       Deallocate the plot buffer.
C                                       Check for an error.
C! VAX
      IER = GRFMEM (BX * BY, BUFFER)
      IF (IER .NE. 1) THEN
          CALL GRGMSG (IER)
C! PC
C!      DEALLOCATE (BUFFER, STAT=IER)
C!      IF (IER .NE. 0) THEN
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
 
C*GRHJ00 -- PGPLOT Hewlett Packard LaserJet driver, draw line
C+
      SUBROUTINE GRHJ00 (LINE, RBUF, ICOL, BX, BY, BITMAP)
      INTEGER*4  BX, BY, ICOL, LINE
      BYTE       BITMAP(BX, BY)
C!      INTEGER*1    BITMAP(BX, BY)
      REAL*4     RBUF(4)
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
C!      INTEGER*1    QMASK(0 : 7)
      INTEGER*4  K, KX, KY, LENGTH
      REAL*4     D, XINC, XP, YINC, YP
      DATA       QMASK /'80'X, '40'X, '20'X, '10'X,
     1                  '08'X, '04'X, '02'X, '01'X/
C!      DATA       QMASK /16#80, 16#40, 16#20, 16#10,
C!     1                  16#08, 16#04, 16#02, 16#01/
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
 
C*GRHJ01 -- PGPLOT LaserJet driver, copy bitmap to output file
C+
      SUBROUTINE GRHJ01 (LUN, BX, BY, BITMAP)
      INTEGER  BX, BY, LUN
      BYTE     BITMAP(BX, BY)
C!      INTEGER*1  BITMAP(BX, BY)
C
C Arguments:
C
C  LUN    (input)  Fortran unit number for output
C  BX, BY (input)  dimensions of BITMAP
C  BITMAP (input)  the bitmap array
C-----------------------------------------------------------------------
      BYTE      ESC
C!      INTEGER*1   ESC
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
            IF (BITMAP(K, J) .NE. 0) GO TO 10
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
 
C*GRHJ02 -- PGPLOT LaserJet+ driver, dump bitmap to device
C+
      SUBROUTINE GRHJ02 (LUN, BX, BY, BITMAP, TEX)
      LOGICAL TEX
      INTEGER LUN, BX, BY
      BYTE BITMAP(BX, BY)
C!      INTEGER*1 BITMAP(BX, BY)
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
C!      INTEGER*1  ESC, N0
      LOGICAL    NOBIT
      INTEGER*4  CNUM, CONUM, CURCOL, CURROW, FB(35), FB2(25), I, IPOS
      INTEGER*4  IYOFF, J, K, L, M, N, NB(35), NBNUM, NBTOT, NBNUM2
      INTEGER*4  NB2(25), RNUM, RONUM, GRHJ03
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
                  WRITE (NBYTE, 1000) NBNUM
                  NBNUM2 = GRHJ03 (NBNUM)
C                                       Calculate the row and column
                  RONUM = K + IYOFF
                  CONUM = (FB2(I) - 1) * 8
C                                       Determine the positioning
C                                       sequence and write it out
                  IF (RONUM .NE. CURROW .AND. CONUM .NE. CURCOL) THEN
                     RNUM = RONUM - CURROW
                     CNUM = CONUM - CURCOL
                     WRITE (ROW, 1010) RNUM
                     WRITE (COL, 1010) CNUM
                     RNUM = GRHJ03 (ABS (RNUM)) + 1
                     CNUM = GRHJ03 (ABS (CNUM)) + 1
                     WRITE (LUN) ESC, '*p', ROW(6-RNUM:5), 'y',
     +                                              COL(6-CNUM:5), 'X'
                  ELSE IF (RONUM .NE. CURROW) THEN
                     RNUM = RONUM - CURROW
                     WRITE (ROW, 1010) RNUM
                     RNUM = GRHJ03 (ABS (RNUM)) + 1
                     WRITE (LUN) ESC, '*p', ROW(6-RNUM:5), 'Y'
                  ELSE IF (CONUM .NE. CURCOL) THEN
                     CNUM = CONUM - CURCOL
                     WRITE (COL, 1010) CNUM
                     CNUM = GRHJ03 (ABS (CNUM)) + 1
                     WRITE (LUN) ESC, '*p', COL(6-CNUM:5), 'X'
                  END IF
C                                       Check for all bits set in
C                                       substring
                  IF ((INDEX (X(FB2(I):NB2(I)), ALLONE(1:NBNUM)) .EQ. 1)
     +                 .AND. NBNUM .GE. 5) THEN
                     NBNUM = NBNUM * 8
                     WRITE (NBYTE, 1000) NBNUM
                     NBNUM2 = GRHJ03 (NBNUM)
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
 
C*GRHJ03 -- PGPLOT LaserJet+ driver, calculate length of an integer
C+
      INTEGER FUNCTION GRHJ03 (I)
      INTEGER I
C
C This function calculates the number of digits in a supplied integer.
C
C Arguments:
C
C I               I I      Integer value of number
C GRHJ03          O I      Length of printed representation of I
C
C Version 1.0  10-Feb-1988  S. C. Allendorf
C-----------------------------------------------------------------------
      IF (I .GE. 10) THEN
         IF (I .GE. 100) THEN
            IF (I .GE. 1000) THEN
               GRHJ03 = 4
            ELSE
               GRHJ03 = 3
            END IF
         ELSE
            GRHJ03 = 2
         END IF
      ELSE
         GRHJ03 = 1
      END IF
C-----------------------------------------------------------------------
      RETURN
      END
 
C*GRHJ04 -- PGPLOT LaserJet driver, copy bitmap to output file with
C                compression -- for DESKJET PLUS and possibly other printers
C+
      SUBROUTINE GRHJ04 (LUN, BX, BY, BITMAP)
      INTEGER  BX, BY, LUN
      BYTE     BITMAP(BX, BY)
C!      INTEGER*1  BITMAP(BX, BY)
C
C Arguments:
C
C  LUN    (input)  Fortran unit number for output
C  BX, BY (input)  dimensions of BITMAP
C  BITMAP (input)  the bitmap array
C-----------------------------------------------------------------------
      BYTE      ESC
C!      INTEGER*1   ESC
      INTEGER   K1, J, K, BXMAX,BXMIN
      CHARACTER KSTR*3
      PARAMETER (ESC = 27)
      CHARACTER*10 BUFF1
C!      integer*1 BUFF2(400)
      byte BUFF2(400)
        integer lbuf1,lbuf2,tbuf
        byte tbufb(2)
        equivalence (tbuf,tbufb)
C-----------------------------------------------------------------------
C                                       Start graphics mode
      WRITE (LUN) ESC, '*r1A'
C                                       Loop through bitmap
      DO J = 1, BY
C                                       Search for last non-NUL
         DO K = BX, 2, -1
            IF (BITMAP(K, J) .NE. 0) GO TO 10
         END DO
C                                       Guarantee that we know what K
C                                       is after loop.
C                                       (Remember FORTRAN IV!?)
         K = 1
10       BXMAX = K
         BXMIN = 1
         K = 1
         BUFF1(1:1) = CHAR(27)
         BUFF1(2:5) = '*b2m'
         lbuf1 = 5
C If there are less than 4 bytes don't bother with an offset
         IF (BXMAX .LE. 4) GOTO 25
C Count the number of Zero bits at beginning of line
         DO K = BXMIN,BXMAX-1
           IF (BITMAP(K, J) .NE. 0) GO TO 20
         ENDDO
         K = BXMAX
20       IF (K .GT. 4) THEN
            K1 = (K-1)*8
            BXMIN = K
            IF (K1 .LE. 9) THEN
              LBUF1 = 7
              WRITE (BUFF1(6:LBUF1),'(I1.1,A1)') K1,'x'
            ELSEIF (K1 .LE. 99) THEN
              LBUF1 = 8
              WRITE (BUFF1(6:LBUF1),'(I2.2,A1)') K1,'x'
            ELSEIF (K1 .LE. 999) THEN
              LBUF1 = 9
              WRITE (BUFF1(6:LBUF1),'(I3.3,A1)') K1,'x'
            ELSE
              LBUF1 = 10
              WRITE (BUFF1(6:LBUF1),'(I4.4,A1)') K1,'x'
            ENDIF
          ENDIF
 
25        WRITE (LUN) BUFF1(1:LBUF1)
 
          lbuf2 = 1
 
30        CONTINUE
          DO K = BXMIN,BXMAX
            IF (K .GE. BXMAX-2) THEN
C we are at the end of the bit-map,
C    N.B. BXMAX - BXMIN will be less than 128
              buff2(lbuf2) = BXMAX - BXMIN
              lbuf2 = lbuf2 + 1
              DO K1=BXMIN,BXMAX
                 buff2(lbuf2) = BITMAP(K1, J)
                 lbuf2 = lbuf2 + 1
              ENDDO
              GOTO 100
            ELSEIF (K - BXMIN .GE. 125) THEN
C we have 126 non-repeated characters
              buff2(lbuf2) = K - BXMIN
              lbuf2 = lbuf2 + 1
              DO K1=BXMIN,K
                 buff2(lbuf2) = BITMAP(K1, J)
                 lbuf2 = lbuf2 + 1
              ENDDO
              BXMIN = K+1
              IF (BXMIN .GT. BXMAX) GOTO 100
              GOTO 30
            ELSEIF (BITMAP(K, J) .EQ. BITMAP(K+1, J) .AND.
     1          BITMAP(K, J) .EQ. BITMAP(K+2, J)) THEN
C we have 2 or more repeated characters
              IF (K .gt. BXMIN) THEN
C write out non-repeated characters, if any
                buff2(lbuf2) = K - BXMIN - 1
                lbuf2 = lbuf2 + 1
                DO K1=BXMIN,K-1
                   buff2(lbuf2) = BITMAP(K1, J)
                   lbuf2 = lbuf2 + 1
                ENDDO
              ENDIF
C count the number of repeated characters, up to 127
              DO K1=K+3,MIN(BXMAX,K+127)
                IF (BITMAP(K, J) .NE. BITMAP(K1, J)) GOTO 40
              ENDDO
              K1 = BXMAX + 1
C write out repeated characters
40            CONTINUE
C! VAX version:
              Tbuf  = 257 - K1 + K
              buff2(lbuf2) = tbufb(1)
C PC version:
C!              buff2(lbuf2) = 257 - (K1 - K)
              lbuf2 = lbuf2 + 1
              buff2(lbuf2) = BITMAP(K, J)
              lbuf2 = lbuf2 + 1
              BXMIN = K1
              IF (BXMIN .GT. BXMAX) GOTO 100
              GOTO 30
            ENDIF
          ENDDO
100      WRITE (KSTR, '(I3.3)') lbuf2-1
          IF (lbuf2 .LE. 10) THEN
           WRITE (LUN) KSTR(3:3), 'W', (BUFF2(k1),k1=1,lbuf2-1)
          ELSEIF (lbuf2 .LE. 100) THEN
           WRITE (LUN) KSTR(2:3), 'W', (BUFF2(k1),k1=1,lbuf2-1)
          ELSE
           WRITE (LUN) KSTR(1:3), 'W', (BUFF2(k1),k1=1,lbuf2-1)
          ENDIF
C                                       Write out the raster line
      END DO
C                                       Turn off graphics mode.
      WRITE (LUN) ESC, '*rB'
C-----------------------------------------------------------------------
      RETURN
      END

C*GRHJ05 -- zero fill buffer
C+
      SUBROUTINE GRHJ05 (BUFSIZ,BUFFER)
C
C Arguments:
C
C BUFFER (byte array, input): (address of) the buffer.
C BUFSIZ (integer, input): number of bytes in BUFFER.
C-----------------------------------------------------------------------
      INTEGER  BUFSIZ, I
      BYTE     BUFFER(BUFSIZ), FILL
      DATA     FILL/0/
C
      DO 10 I=1,BUFSIZ
          BUFFER(I) = FILL
   10 CONTINUE
      END
