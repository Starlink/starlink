C*PSDRIV -- PGPLOT PostScript drivers
C+
      SUBROUTINE PSDRIV (IFUNC, RBUF, NBUF, CHR, LCHR, MODE)
      INTEGER IFUNC, NBUF, LCHR, MODE
      REAL    RBUF(*)
      CHARACTER*(*) CHR
C
C PGPLOT driver for PostScript devices.
C
C Version 1.2  - 1987 Aug  5 - T. J. Pearson.
C Version 1.3  - 1987 Nov 16 - add "bind" commands to prolog - TJP.
C Version 1.4  - 1988 Jan 28 - change dimensions so whole field can be
C                              plotted - TJP.
C Version 1.5  - 1988 Oct 27 - make EOF characters optional - TJP.
C Version 1.6  - 1988 Dec 15 - standard Fortran - TJP.
C Version 1.7  - 1989 Jul  5 - change color indices so most colors
C                              are black - TJP.
C Version 2.0  - 1990 Sep 10 - parameterize dimensions; correct
C                              bounding box; add color support (from
C                              D. Meier's CPdriver) - TJP.
C Version 2.1  - 1991 Nov 29 - update Document Structuring Conventions
C                              to version 3.0.
C Version 3.0  - 1992 Sep 22 - add marker support; add CPS and VCPS
C                              modes - TJP.
C Version 3.1  - 1992 Nov 12 - up to 256 colors.
C Version 3.2  - 1993 May 26 - correct error in marker support.
C Version 4.0  - 1993 Sep 20 - trap Fortran I/O errors.
C Version 4.1  - 1994 Aug  4 - make marker support optional.
C Version 5.0  - 1994 Aug 30 - support for images.
C Version 5.1  - 1994 Sep  7 - support for PGQCR.
C Version 5.2  - 1994 Oct 12 - add IDENT option.
C Version 5.3  - 1995 May  8 - recognise '-' as standard output; keep
C                              track of bounding box; use upper case
C                              for all defined commands; move
C                              showpage outside save/restore.
C Version 5.4  - 1995 Aug 19 - correct usage of PS_BBOX.
C Version 6.0  - 1995 Dec 28 - reject concurrent access.
C Version 6.1  - 1996 Apr 29 - decode environment variables using GRCTOI.
C Version 6.2  - 1996 Oct  7 - correct bounding-box error (K-G Adams);
C                              correct error in use of GCTOI (G Gonczi);
C                              suppress <0 0 C> commands (R Scharroo);
C                              allow arbitrary page size.
C Version 6.3  - 1997 Nov 14 - shorter commands for setrgbcolor and setgray.
C Version 6.4  - 1997 Nov 19 - workaround a Ghostscript bug: split long
C                              polylines into shorter segments.
C Version 6.5  - 1998 Feb 23 - support for real linewidth.
C Version 6.6  - 1998 Nov 10 - provide easy way to convert color to grey.
C Version 6.7  - 1998 Dec 12 - added #copies to header.
C
C Supported device: 
C   Any printer that accepts the PostScript page description language, 
C   eg, the LaserWriter (Apple Computer, Inc.).
C   PostScript is a trademark of Adobe Systems Incorporated.
C
C Device type code: 
C   /PS (monochrome landscape mode, long edge of paper horizontal).
C   /CPS (color landscape mode, long edge of paper horizontal).
C   /VPS (monochrome portrait mode, short edge of paper horizontal).
C   /VCPS (color portrait mode, short edge of paper horizontal).
C
C Default file name:
C   pgplot.ps
C
C Default view surface dimensions:
C   10.5 inches horizontal x  7.8 inches vertical (landscape mode),
C    7.8 inches horizontal x 10.5 inches vertical (portrait mode).
C   These dimensions can be changed with environment variables.
C
C Resolution:
C   The driver uses coordinate increments of 0.001 inch, giving an
C   ``apparent'' resolution of 1000 pixels/inch. The true resolution is
C   device-dependent; eg, on an Apple LaserWriter it is 300 pixels/inch
C   (in both dimensions). 
C
C Color capability (monochrome mode): 
C   Color indices 0-255 are supported. Color index 0 is white (erase
C   or background color), indices 1-13 are black, 14 is light grey,
C   and 15 is dark grey.
C
C Color capability (color mode):
C   Color indices 0-255 are supported. Color index 0 is white (erase
C   or background color), index 1 is black, and indices 2-15 have the
C   standard PGPLOT color assignments.
C
C Input capability: none.
C
C File format: the file contains variable length records (maximum 132
C characters) containing PostScript commands. The commands use only
C printable ASCII characters, and the file can be examined or modified 
C with a text editor. 
C
C Obtaining hardcopy: use the operating system print or copy command to
C send the file to a suitable device.
C
C Environment variables:
C
C  PGPLOT_PS_WIDTH      default  7800
C  PGPLOT_PS_HEIGHT     default 10500
C  PGPLOT_PS_HOFFSET    default   350
C  PGPLOT_PS_VOFFSET    default   250
C These variables tell PGPLOT how big an image to produce. The defaults
C are appropriate for 8.5 x 11-inch paper. The maximum dimensions of
C a PGPLOT image are WIDTH by HEIGHT, with the lower left corner offset
C by HOFFSET horizontally and VOFFSET vertically from the lower left
C corner of the paper. The units are milli-inches. The "top" of the
C paper is the edge that comes out of the printer first.
C
C  PGPLOT_IDENT
C If this variable is set, the user name, date and time are written
C in the bottom right corner of each page.
C
C  PGPLOT_PS_BBOX
C If this variable has value MAX, PGPLOT puts standard (full-page)
C bounding-box information in the header of the PostScript file. If
C the variable is unset or has some other value, PGPLOT puts the
C correct (smallest) bounding box information in the trailer of the
C PostScript file.
C
C  PGPLOT_PS_EOF
C Normally the output file does not contain special end-of-file
C characters. But if environment variable PGPLOT_PS_EOF is defined
C (with any value) PGPLOT writes a control-D job-separator character at 
C the beginning and at the end of the file. This is appropriate for
C Apple LaserWriters using the serial interface, but it may not be 
C appropriate for other PostScript devices.
C
C  PGPLOT_PS_MARKERS
C Specify "NO" to suppress use of a PostScript font for the graph
C markers; markers are then emulated by line-drawing. 
C
C Document Structuring Conventions:
C
C  The PostScript files conform to Version 3.0 of the Adobe Document 
C  Structuring Conventions (see ref.3) and to version 3.0 of the
C  encapsulated PostScript file (EPSF) format. This should allow
C  the files to be read by other programs that accept the EPSF format.
C  Note, though, that multi-page plots are not valid EPSF files. The
C  files do not contain a screen preview section.
C
C References:
C
C (1) Adobe Systems, Inc.: PostScript Language Reference Manual.
C Addison-Wesley, Reading, Massachusetts, 1985.
C (2) Adobe Systems, Inc.: PostScript Language Tutorial and Cookbook.
C Addison-Wesley, Reading, Massachusetts, 1985.
C (3) Adobe Systems, Inc.: PostScript Language Reference Manual, Second 
C Edition. Addison-Wesley, Reading, Massachusetts, 1990.
C-----------------------------------------------------------------------
      INTEGER DWD, DHT, DOFFW, DOFFH
      CHARACTER*(*) PTYPE, LTYPE, CPTYPE, CLTYPE, DEFNAM
      PARAMETER (
     : PTYPE= 'VPS   (PostScript file, portrait orientation)',
     : LTYPE= 'PS    (PostScript file, landscape orientation)',
     : CPTYPE='VCPS  (Colour PostScript file, portrait orientation)',
     : CLTYPE='CPS   (Colour PostScript file, landscape orientation)')
C     PARAMETER (PTYPE='VPS', LTYPE='PS', CPTYPE='VCPS', CLTYPE='CPS')
      PARAMETER (DEFNAM='pgplot.ps')
C -- printable paper area: in milli-inches; (WIDTH, HEIGHT) are
C    the dimensions of the printable area; OFFW, OFFH the offset from
C    the lower left corner of the paper
      PARAMETER (DWD=7800, DHT=10500, DOFFW=350, DOFFH=250)
C
      INTEGER WIDTH, HEIGHT, OFFW, OFFH
      SAVE    WIDTH, HEIGHT, OFFW, OFFH
      INTEGER  IER, I0, J0, I1, J1, L, LL, LASTI, LASTJ, UNIT, LOBUF
      SAVE                                 LASTI, LASTJ, UNIT, LOBUF
      INTEGER  CI, NPTS, NPAGE, IOERR, LFNAME
      SAVE         NPTS, NPAGE, IOERR, LFNAME
      INTEGER  STATE, NSEG
      SAVE     STATE, NSEG
      INTEGER  NXP, NYP, XORG, YORG, XLEN, YLEN, N, RGB(3)
      INTEGER  HIGH, LOW, I, K, KMAX, POSN, LD, LU
      INTEGER  BBOX(4), BB1, BB2, BB3, BB4
      SAVE     BBOX
      INTEGER  GROPTX, GRCTOI
      LOGICAL  START, LANDSC, COLOR, STDOUT
      SAVE     START,         COLOR, STDOUT
      REAL     LW
      SAVE     LW
      REAL     BBXMIN, BBXMAX, BBYMIN, BBYMAX
      SAVE     BBXMIN, BBXMAX, BBYMIN, BBYMAX
      REAL     RVALUE(0:255), GVALUE(0:255), BVALUE(0:255)
      SAVE     RVALUE,        GVALUE,        BVALUE
      CHARACTER*20  SUSER, SDATE
      CHARACTER*120 INSTR, MSG
      CHARACTER*132 OBUF
      SAVE          OBUF
      CHARACTER*255 FNAME
      SAVE          FNAME
      INTEGER       MARKER(0:31), NSYM, RAD(0:31)
      SAVE          MARKER, RAD
      REAL          MFAC
      SAVE          MFAC
      REAL          SHADE(0:15), RINIT(0:15), GINIT(0:15), BINIT(0:15)
      SAVE          SHADE,       RINIT,       GINIT,       BINIT
      CHARACTER*1   HEXDIG(0:15)
      DATA HEXDIG/'0','1','2','3','4','5','6','7',
     1            '8','9','A','B','C','D','E','F'/
      DATA SHADE /1.00, 13*0.00, 0.33, 0.67/
      DATA RINIT 
     1     / 1.00, 0.00, 1.00, 0.00, 0.00, 0.00, 1.00, 1.00,
     2       1.00, 0.50, 0.00, 0.00, 0.50, 1.00, 0.33, 0.67/
      DATA GINIT
     1     / 1.00, 0.00, 0.00, 1.00, 0.00, 1.00, 0.00, 1.00,
     2       0.50, 1.00, 1.00, 0.50, 0.00, 0.00, 0.33, 0.67/
      DATA BINIT
     1     / 1.00, 0.00, 0.00, 0.00, 1.00, 1.00, 1.00, 0.00,
     2       0.00, 0.00, 0.50, 1.00, 1.00, 0.50, 0.33, 0.67/
      DATA RAD/ 6,  1,  7,  6, 7, 5, 6, 8,
     :          7,  7,  9, 10, 9, 8, 6, 8,
     :          4,  5,  9, 12, 2, 4, 5, 7,
     :         11, 17, 22, 41, 9, 9, 9, 9/
      DATA STATE/0/
C-----------------------------------------------------------------------
C
      GOTO( 10, 20, 30, 40, 50, 60, 70, 80, 90,100,
     1     110,120,130,140,150,160,170,180,190,200,
     2     210,220,230,900,900,260,900,280,290), IFUNC
      GOTO 900
C
C--- IFUNC = 1, Return device name.-------------------------------------
C
   10 IF (MODE.EQ.1) THEN
C         -- landscape, monochrome
          CHR = LTYPE
          LCHR = LEN(LTYPE)
      ELSE IF (MODE.EQ.2) THEN
C         -- portrait, monochrome
          CHR = PTYPE
          LCHR = LEN(PTYPE)
      ELSE IF (MODE.EQ.3) THEN
C         -- landscape, color
          CHR = CLTYPE
          LCHR = LEN(CLTYPE)
      ELSE
C         -- portrait, color
          CHR = CPTYPE
          LCHR = LEN(CPTYPE)
      END IF
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
   30 RBUF(1) = 1000.0
      RBUF(2) = 1000.0
      RBUF(3) = 5
      NBUF = 3
      RETURN
C
C--- IFUNC = 4, Return misc device info. -------------------------------
C    (This device is Hardcopy, No cursor, No dashed lines, Area fill, 
C    Thick lines, QCR, Markers [optional])
C
   40 CONTINUE
      CHR = 'HNNATNQNYM'
C     -- Marker support suppressed?
      CALL GRGENV('PS_MARKERS', INSTR, L)
      IF (L.GE.2) THEN
         IF (INSTR(1:L).EQ.'NO' .OR. INSTR(1:L).EQ.'no') THEN
            CHR(10:10) = 'N'
         END IF
      END IF
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
      LANDSC = MODE.EQ.1 .OR. MODE.EQ.3
      IF (LANDSC) THEN
          RBUF(2) = HEIGHT-1
          RBUF(4) = WIDTH-1
      ELSE
          RBUF(2) = WIDTH-1
          RBUF(4) = HEIGHT-1
      END IF
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
         CALL GRWARN('a PGPLOT PostScript file is already open')
         RBUF(1) = 0
         RBUF(2) = 0
         RETURN
      END IF
C     -- Color mode?
      CALL GRGENV('PS_COLOR', INSTR, L)
      COLOR = L.GT.0 .OR. MODE.EQ.3 .OR. MODE.EQ.4
      IF (COLOR) THEN
         DO 91 CI=0,15
            RVALUE(CI) = RINIT(CI)
            GVALUE(CI) = GINIT(CI)
            BVALUE(CI) = BINIT(CI)
 91      CONTINUE
      ELSE
         DO 92 CI=0,15
            RVALUE(CI) = SHADE(CI)
            GVALUE(CI) = SHADE(CI)
            BVALUE(CI) = SHADE(CI)
 92      CONTINUE
      END IF
      DO 93 CI=16,255
         RVALUE(CI) = 0.0
         GVALUE(CI) = 0.0
         BVALUE(CI) = 0.0
 93   CONTINUE
C     -- Device dimensions
      WIDTH = DWD
      HEIGHT = DHT
      OFFW = DOFFW
      OFFH = DOFFH
      CALL GRGENV('PS_WIDTH', INSTR, L)
      LL = 1
      IF (L.GT.0) WIDTH = GRCTOI(INSTR(:L),LL)
      CALL GRGENV('PS_HEIGHT', INSTR, L)
      LL = 1
      IF (L.GT.0) HEIGHT = GRCTOI(INSTR(:L),LL)
      CALL GRGENV('PS_HOFFSET', INSTR, L)
      LL = 1
      IF (L.GT.0) OFFW = GRCTOI(INSTR(:L),LL)
      CALL GRGENV('PS_VOFFSET', INSTR, L)
      LL = 1
      IF (L.GT.0) OFFH = GRCTOI(INSTR(:L),LL)
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
            MSG = 'Cannot open output file for PostScript plot: '//
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
      LOBUF = 0
      LASTI = -1
      LASTJ = -1
      LW = 1
      NPTS = 0
      CALL GRGENV('PS_EOF', INSTR, L)
      IF (L.GT.0) CALL GRPS02(IOERR, UNIT, CHAR(4))
      CALL GRPS02(IOERR, UNIT, '%!PS-Adobe-3.0 EPSF-3.0')
      CALL GRUSER(INSTR, L)
      IF (L.GT.0) CALL GRPS02(IOERR, UNIT, '%%For: '//INSTR(1:L))
      CALL GRPS02(IOERR, UNIT, '%%Title: PGPLOT PostScript plot')
      CALL GRPS02(IOERR, UNIT, '%%Creator: PGPLOT [PSDRIV 6.6]')
      CALL GRDATE(INSTR, L)
      IF (L.GT.0) CALL GRPS02(IOERR, UNIT,
     :    '%%CreationDate: '//INSTR(1:L))
      CALL GRGENV('PS_BBOX', INSTR, L)
      CALL GRTOUP(INSTR(1:3), INSTR(1:3))
      IF (INSTR(1:3).EQ.'MAX') THEN
C        -- bounding box is based on maximum plot dimensions, not
C           actual dimensions
         CALL GRFAO('%%BoundingBox: # # # #', L, INSTR,
     :        NINT(OFFW*0.072), NINT(OFFH*0.072),
     :        NINT((WIDTH+OFFW)*0.072), NINT((HEIGHT+OFFH)*0.072))
         CALL GRPS02(IOERR, UNIT, INSTR(:L))
      ELSE
         CALL GRPS02(IOERR, UNIT, '%%BoundingBox: (atend)')
      END IF
      CALL GRPS02(IOERR, UNIT, '%%DocumentFonts: (atend)')
      CALL GRPS02(IOERR, UNIT, '%%LanguageLevel: 1')
      LANDSC = MODE.EQ.1 .OR. MODE.EQ.3
      IF (LANDSC) THEN
          CALL GRPS02(IOERR, UNIT, '%%Orientation: Landscape')
      ELSE
          CALL GRPS02(IOERR, UNIT, '%%Orientation: Portrait')
      END IF
      CALL GRPS02(IOERR, UNIT, '%%Pages: (atend)')
      CALL GRPS02(IOERR, UNIT, '%%EndComments')
      CALL GRPS02(IOERR, UNIT, '%%BeginProlog')
      CALL GRPS02(IOERR, UNIT, 
     1  '/L {moveto rlineto currentpoint stroke moveto} bind def')
      CALL GRPS02(IOERR, UNIT, 
     1  '/C {rlineto currentpoint stroke moveto} bind def')
      CALL GRPS02(IOERR, UNIT, 
     1  '/D {moveto 0 0 rlineto currentpoint stroke moveto} bind def')
      CALL GRPS02(IOERR, UNIT, '/LW {5 mul setlinewidth} bind def')
      CALL GRPS02(IOERR, UNIT, '/BP {newpath moveto} bind def')
      CALL GRPS02(IOERR, UNIT, '/LP /rlineto load def')
      CALL GRPS02(IOERR, UNIT, 
     1  '/EP {rlineto closepath eofill} bind def')
      CALL GRPS02(IOERR, UNIT, '/MB {gsave translate MFAC dup scale '//
     1 '1 setlinewidth 2 setlinecap 0 setlinejoin newpath} bind def')
      CALL GRPS02(IOERR, UNIT, '/ME /grestore load def')
      CALL GRPS02(IOERR, UNIT, '/CC {0 360 arc stroke} bind def')
      CALL GRPS02(IOERR, UNIT, '/FC {0 360 arc fill} bind def')
      CALL GRPS02(IOERR, UNIT, '/G {1024 div setgray} bind def')
      CALL GRPS02(IOERR, UNIT, '/K {3 -1 roll 1024 div 3 -1 roll 1024'//
     :     ' div 3 -1 roll 1024 div setrgbcolor} bind def')
      CALL GRPS02(IOERR, UNIT, '% Uncomment next line to convert color'
     :     //' to grey shades')
      CALL GRPS02(IOERR, UNIT, '%/K {3 -1 roll 3413 div 3 -1 roll 1739'
     :     //' div 3 -1 roll 9309 div add add setgray} bind def')
      CALL GRGENV('IDENT', INSTR, L)
      IF (L.GT.0) THEN
         CALL GRPS02(IOERR, UNIT,
     :        '/RS{findfont exch scalefont setfont moveto dup'//
     :        ' stringwidth neg exch neg exch rmoveto show} bind def')
      END IF
      CALL GRPS02(IOERR, UNIT, '%%EndProlog')
      CALL GRPS02(IOERR, UNIT, '%%BeginSetup')
      CALL GRPS02(IOERR, UNIT, '/#copies 1 def')
      CALL GRPS02(IOERR, UNIT, '%%EndSetup')
      NPAGE = 0
      RETURN
C
C--- IFUNC=10, Close workstation. --------------------------------------
C
  100 CONTINUE
      CALL GRPS02(IOERR, UNIT, ' ')
      CALL GRPS02(IOERR, UNIT, '%%Trailer')
      CALL GRGENV('PS_BBOX', INSTR, L)
      CALL GRTOUP(INSTR(1:3), INSTR(1:3))
      IF (INSTR(1:3).NE.'MAX') THEN
         CALL GRFAO('%%BoundingBox: # # # #', L, INSTR,
     :        BBOX(1), BBOX(2), BBOX(3), BBOX(4))
         CALL GRPS02(IOERR, UNIT, INSTR(:L))
      END IF
      CALL GRPS02(IOERR, UNIT, '%%DocumentFonts: ')
      CALL GRFAO('%%Pages: #', L, INSTR, NPAGE, 0, 0, 0)
      CALL GRPS02(IOERR, UNIT, INSTR(:L))
      CALL GRPS02(IOERR, UNIT, '%%EOF')
      CALL GRGENV('PS_EOF', INSTR, L)
      IF (L.GT.0) CALL GRPS02(IOERR, UNIT, CHAR(4))
      IF (IOERR.NE.0) THEN
          CALL GRWARN('++WARNING++ Error '//
     1       'writing PostScript file: file is incomplete')
          CALL GRWARN('Check for device full or quota exceeded')
          CALL GRWARN('Filename: '//FNAME(:LFNAME))
      END IF
      IF (.NOT.STDOUT) THEN
         CLOSE (UNIT, IOSTAT=IOERR)
         IF (IOERR.NE.0) THEN
           CALL GRWARN('Error closing PostScript file '//FNAME(:LFNAME))
         END IF
         CALL GRFLUN(UNIT)
      END IF
      STATE = 0
      RETURN
C
C--- IFUNC=11, Begin picture. ------------------------------------------
C
  110 CONTINUE
      LANDSC = MODE.EQ.1 .OR. MODE.EQ.3
      IF (LANDSC) THEN
         HEIGHT = RBUF(1)
         WIDTH = RBUF(2)
      ELSE
         WIDTH = RBUF(1)
         HEIGHT = RBUF(2)
      END IF
      NPAGE = NPAGE+1
      CALL GRPS02(IOERR, UNIT, ' ')
      CALL GRFAO('%%Page: # #', L, INSTR, NPAGE, NPAGE, 0, 0)
      CALL GRPS02(IOERR, UNIT, INSTR(:L))
      CALL GRPS02(IOERR, UNIT, '%%BeginPageSetup')
      CALL GRPS02(IOERR, UNIT, '/PGPLOT save def')
      CALL GRPS02(IOERR, UNIT, '0.072 0.072 scale')
      LANDSC = MODE.EQ.1 .OR. MODE.EQ.3
      IF (LANDSC) THEN
          CALL GRFAO('# # translate 90 rotate', L, INSTR, WIDTH+OFFW, 
     1               OFFH, 0, 0)
      ELSE
          CALL GRFAO('# # translate', L, INSTR, OFFW, OFFH, 0, 0)
      END IF
      CALL GRPS02(IOERR, UNIT, INSTR(:L))
      CALL GRPS02(IOERR, UNIT, '1 setlinejoin 1 setlinecap 1 LW 1')
      CALL GRPS02(IOERR, UNIT, '%%EndPageSetup')
      CALL GRPS02(IOERR, UNIT, '%%PageBoundingBox: (atend)')
      DO 111 NSYM=0,31
          MARKER(NSYM) = 0
  111 CONTINUE
      MFAC = 0.0
      BBXMIN = WIDTH
      BBYMIN = HEIGHT
      BBXMAX = 0.0
      BBYMAX = 0.0
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
         CALL GRFAO('# # C', L, INSTR, (I1-I0), (J1-J0), 0, 0)
         NSEG = NSEG+1
      ELSE
         NSEG = 1
         CALL GRFAO('# # # # L', L, INSTR, (I1-I0), (J1-J0), I0, J0)
      END IF
      LASTI = I1
      IF (NSEG.GT.200) LASTI = -1
      LASTJ = J1
      BBXMIN = MIN(BBXMIN, I0-LW*5.0, I1-LW*5.0)
      BBXMAX = MAX(BBXMAX, I0+LW*5.0, I1+LW*5.0)
      BBYMIN = MIN(BBYMIN, J0-LW*5.0, J1-LW*5.0)
      BBYMAX = MAX(BBYMAX, J0+LW*5.0, J1+LW*5.0)
      GOTO 800
C
C--- IFUNC=13, Draw dot. -----------------------------------------------
C
  130 CONTINUE
      I1 = NINT(RBUF(1))
      J1 = NINT(RBUF(2))
      CALL GRFAO('# # D', L, INSTR, I1, J1, 0, 0)
      LASTI = I1
      LASTJ = J1
      BBXMIN = MIN(BBXMIN, I1-LW*5.0)
      BBXMAX = MAX(BBXMAX, I1+LW*5.0)
      BBYMIN = MIN(BBYMIN, J1-LW*5.0)
      BBYMAX = MAX(BBYMAX, J1+LW*5.0)
      GOTO 800
C
C--- IFUNC=14, End picture. --------------------------------------------
C
  140 CONTINUE
      IF (LOBUF.NE.0) THEN
          CALL GRPS02(IOERR, UNIT, OBUF(1:LOBUF))
          LOBUF = 0
      END IF
      LANDSC = MODE.EQ.1 .OR. MODE.EQ.3
C     -- optionally write identification
      CALL GRGENV('IDENT', INSTR, L)
      IF (L.GT.0) THEN
         CALL GRUSER(SUSER, LU)
         CALL GRDATE(SDATE, LD)
         POSN = WIDTH - 1
         IF (LANDSC) POSN = HEIGHT - 1
         CALL GRFAO('('//SUSER(:LU)//' '//SDATE(:LD)//
     :        ' [#]) # # 100 /Helvetica RS',
     :        L, INSTR, NPAGE, POSN, 50, 0)
         CALL GRPS02(IOERR, UNIT, '0 G')
         CALL GRPS02(IOERR, UNIT, INSTR(1:L))
      END IF
C     -- optionally draw bounding box
      CALL GRGENV('PS_DRAW_BBOX', INSTR, L)
      IF (L.GT.0) THEN
         CALL GRFAO('0 G 0 LW newpath # # moveto', L, INSTR,
     :              NINT(BBXMIN), NINT(BBYMIN), 0, 0)
         CALL GRPS02(IOERR, UNIT, INSTR(1:L))
         CALL GRFAO('# # lineto # # lineto', L, INSTR,
     :        NINT(BBXMIN), NINT(BBYMAX), NINT(BBXMAX), NINT(BBYMAX))
         CALL GRPS02(IOERR, UNIT, INSTR(1:L))
         CALL GRFAO('# # lineto closepath stroke', L,INSTR,
     :              NINT(BBXMAX), NINT(BBYMIN), 0, 0)
         CALL GRPS02(IOERR, UNIT, INSTR(1:L))
      END IF
      CALL GRPS02(IOERR, UNIT, 'PGPLOT restore showpage')
      CALL GRPS02(IOERR, UNIT, '%%PageTrailer')
      IF (LANDSC) THEN
         BB1 = INT((WIDTH-BBYMAX+OFFW)*0.072)
         BB2 = INT((BBXMIN+OFFH)*0.072)
         BB3 = 1+INT((WIDTH-BBYMIN+OFFW)*0.072)
         BB4 = 1+INT((BBXMAX+OFFH)*0.072)
      ELSE
         BB1 = INT((BBXMIN+OFFW)*0.072)
         BB2 = INT((BBYMIN+OFFH)*0.072)
         BB3 = 1+INT((BBXMAX+OFFW)*0.072)
         BB4 = 1+INT((BBYMAX+OFFH)*0.072)
      END IF
      CALL GRFAO('%%PageBoundingBox: # # # #', L, INSTR,
     :           BB1, BB2, BB3, BB4)
      CALL GRPS02(IOERR, UNIT, INSTR(1:L))
      IF (NPAGE.EQ.1) THEN
         BBOX(1) = BB1
         BBOX(2) = BB2
         BBOX(3) = BB3
         BBOX(4) = BB4
      ELSE
         BBOX(1) = MIN(BBOX(1),BB1)
         BBOX(2) = MIN(BBOX(2),BB2)
         BBOX(3) = MAX(BBOX(3),BB3)
         BBOX(4) = MAX(BBOX(4),BB4)
      END IF
      RETURN
C
C--- IFUNC=15, Select color index. -------------------------------------
C
  150 CONTINUE
      CI = NINT(RBUF(1))
      IF (COLOR) THEN
         CALL GRFAO('# # # K', L, INSTR, NINT(1024.*RVALUE(CI)), 
     :        NINT(1024.*GVALUE(CI)), NINT(1024.*BVALUE(CI)), 0)
      ELSE
         CALL GRFAO('# G', L, INSTR, NINT(1024.*RVALUE(CI)), 0, 0, 0)
      END IF
      LASTI = -1
      GOTO 800
C
C--- IFUNC=16, Flush buffer. -------------------------------------------
C
  160 CONTINUE
      IF (LOBUF.NE.0) THEN
          CALL GRPS02(IOERR, UNIT, OBUF(1:LOBUF))
          LOBUF = 0
      END IF
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
          IF (START) THEN
              CALL GRFAO('# # BP', L, INSTR, I0, J0, 0, 0)
              START = .FALSE.
              LASTI = I0
              LASTJ = J0
          ELSE IF (NPTS.EQ.0) THEN
              CALL GRFAO('# # EP', L, INSTR, (I0-LASTI), 
     1                     (J0-LASTJ), 0, 0)
              LASTI = -1
              LASTJ = -1
          ELSE
              CALL GRFAO('# # LP', L, INSTR, (I0-LASTI), 
     1                     (J0-LASTJ), 0, 0)
              LASTI = I0
              LASTJ = J0
          END IF
          BBXMIN = MIN(BBXMIN, I0-LW*5.0)
          BBXMAX = MAX(BBXMAX, I0+LW*5.0)
          BBYMIN = MIN(BBYMIN, J0-LW*5.0)
          BBYMAX = MAX(BBYMAX, J0+LW*5.0)
          GOTO 800
      END IF
C
C--- IFUNC=21, Set color representation. -------------------------------
C
  210 CONTINUE
      IF (COLOR) THEN
          CI = RBUF(1)
          RVALUE(CI) = RBUF(2)
          GVALUE(CI) = RBUF(3)
          BVALUE(CI) = RBUF(4)
      ELSE
          CI = RBUF(1)
          RVALUE(CI) = 0.30*RBUF(2) + 0.59*RBUF(3) + 0.11*RBUF(4)
          GVALUE(CI) = RVALUE(CI)
          BVALUE(CI) = RVALUE(CI)
      END IF
      RETURN
C
C--- IFUNC=22, Set line width. -----------------------------------------
C
  220 CONTINUE
      LW = RBUF(1)
      IF (INT(LW).EQ.LW) THEN
         CALL GRFAO('# LW', L, INSTR, INT(LW), 0, 0, 0)
      ELSE
         WRITE (INSTR,'(F6.2,'' LW'')') LW
         L = 9
      END IF
      LASTI = -1
      GOTO 800
C
C--- IFUNC=23, Escape. -------------------------------------------------
C
  230 CONTINUE
      IF (LOBUF.NE.0) THEN
C         -- flush buffer first
          CALL GRPS02(IOERR, UNIT, OBUF(1:LOBUF))
          LOBUF = 0
      END IF
      CALL GRPS02(IOERR, UNIT, CHR(:LCHR))
      LASTI = -1
      RETURN
C
C--- IFUNC=26, Image.---------------------------------------------------
C
  260 CONTINUE
      N = RBUF(1)
      IF (N.EQ.0) THEN
C         -- First: setup for image
C         -- Set clipping region (RBUF(2...5))
          NXP = RBUF(2)
          NYP = RBUF(3)
          XORG = RBUF(4)
          XLEN = RBUF(5) - RBUF(4)
          YORG = RBUF(6) 
          YLEN = RBUF(7) - RBUF(6)
          BBXMIN = MIN(BBXMIN, RBUF(4), RBUF(5))
          BBXMAX = MAX(BBXMAX, RBUF(4), RBUF(5))
          BBYMIN = MIN(BBYMIN, RBUF(6), RBUF(7))
          BBYMAX = MAX(BBYMAX, RBUF(6), RBUF(7))
C      
          CALL GRPS02(IOERR, UNIT, 'gsave newpath')
          CALL GRFAO('# # moveto # 0 rlineto 0 # rlineto', L, INSTR,
     :               XORG, YORG, XLEN, YLEN)
          CALL GRPS02(IOERR, UNIT, INSTR(:L))
          CALL GRFAO('# 0 rlineto closepath clip', L, INSTR, -XLEN,
     :                0, 0, 0)
          CALL GRPS02(IOERR, UNIT, INSTR(:L))
C         -- 
          CALL GRFAO('/picstr # string def', L, INSTR, NXP, 0, 0, 0)
          CALL GRPS02(IOERR, UNIT, INSTR(:L))
          CALL GRFAO('# # 8 [', L, INSTR, NXP, NYP, 0, 0)
          CALL GRPS02(IOERR, UNIT, INSTR(:L))
          WRITE (INSTR, '(6(1PE10.3, 1X), '']'')') (RBUF(I),I=8,13)
          CALL GRPS02(IOERR, UNIT, INSTR(:67))
          IF (COLOR) THEN
              CALL GRPS02(IOERR, UNIT, 
     :      '{currentfile picstr readhexstring pop} false 3 colorimage')
          ELSE
              CALL GRPS02(IOERR, UNIT, 
     :      '{currentfile picstr readhexstring pop} image')
          END IF
      ELSE IF (N.EQ.-1) THEN
C         -- Last: terminate image
          CALL GRPS02(IOERR, UNIT, 'grestore')
      ELSE 
C         -- Middle: write N image pixels; each pixel uses 6 chars
C            in INSTR, so N must be <= 20.
          L = 0
          KMAX = 1
          IF (COLOR) KMAX = 3
          DO 262 I=1,N
              CI = RBUF(I+1)
              RGB(1) = NINT(255.0*RVALUE(CI))
              RGB(2) = NINT(255.0*GVALUE(CI))
              RGB(3) = NINT(255.0*BVALUE(CI))
              DO 261 K=1,KMAX
                  HIGH = RGB(K)/16
                  LOW  = RGB(K)-16*HIGH
                  L = L+1
                  INSTR(L:L) = HEXDIG(HIGH)
                  L = L+1
                  INSTR(L:L) = HEXDIG(LOW)
 261          CONTINUE
 262      CONTINUE
          CALL GRPS02(IOERR, UNIT, INSTR(1:L))
      END IF
      RETURN
C
C--- IFUNC=28, Marker.--------------------------------------------------
C
  280 CONTINUE
      NSYM = NINT(RBUF(1))
C     -- Output code for this marker if necessary
      IF (MARKER(NSYM).EQ.0) THEN
          IF (LOBUF.GT.0) CALL GRPS02(IOERR, UNIT, OBUF(1:LOBUF))
          LOBUF = 0
          CALL GRPS03(IOERR, NSYM, UNIT)
          MARKER(NSYM) = 1
      END IF
C     -- Output scale factor
      IF (RBUF(4).NE.MFAC) THEN
          IF (LOBUF.GT.0) CALL GRPS02(IOERR, UNIT, OBUF(1:LOBUF))
          LOBUF = 0
          MFAC = RBUF(4)
          WRITE (INSTR, '(''/MFAC '',F10.3,'' def'')') MFAC
          CALL GRPS02(IOERR, UNIT, INSTR(1:24))
      END IF
C     -- Output an instruction to draw one marker
      I1 = NINT(RBUF(2))
      J1 = NINT(RBUF(3))
      CALL GRFAO('# # M#', L, INSTR, I1, J1, NSYM, 0)
      LASTI = -1
      BBXMIN = MIN(BBXMIN, I1-MFAC*RAD(NSYM))
      BBXMAX = MAX(BBXMAX, I1+MFAC*RAD(NSYM))
      BBYMIN = MIN(BBYMIN, J1-MFAC*RAD(NSYM))
      BBYMAX = MAX(BBYMAX, J1+MFAC*RAD(NSYM))
      GOTO 800
C
C--- IFUNC=29, Query color representation.------------------------------
C
 290  CONTINUE
      CI = NINT(RBUF(1))
      NBUF = 4
      RBUF(2) = RVALUE(CI)
      RBUF(3) = GVALUE(CI)
      RBUF(4) = BVALUE(CI)
      RETURN
C
C-----------------------------------------------------------------------
C Buffer output if possible.
C
  800 IF ( (LOBUF+L+1). GT. 132) THEN
          CALL GRPS02(IOERR, UNIT, OBUF(1:LOBUF))
          OBUF(1:L) = INSTR(1:L)
          LOBUF = L
      ELSE
          IF (LOBUF.GT.1) THEN
              LOBUF = LOBUF+1
              OBUF(LOBUF:LOBUF) = ' '
          END IF
          OBUF(LOBUF+1:LOBUF+L) = INSTR(1:L)
          LOBUF = LOBUF+L
      END IF
      RETURN
C-----------------------------------------------------------------------
C Error: unimplemented function.
C
  900 WRITE (MSG,
     1  '(''Unimplemented function in PS device driver: '',I10)') IFUNC
      CALL GRWARN(MSG)
      NBUF = -1
      RETURN
C-----------------------------------------------------------------------
      END

C*GRPS03 -- PGPLOT PostScript driver, marker support
C+
      SUBROUTINE GRPS03(IOERR, NSYM, UNIT)
      INTEGER IOERR, NSYM, UNIT
C
C Write PostScript instructions for drawing graph marker number NSYM
C on Fortran unit UNIT.
C-----------------------------------------------------------------------
      CHARACTER*80 T(6)
      INTEGER I, N
C
      IF (NSYM.LT.0 .OR. NSYM.GT.31) RETURN
      GOTO (100, 101, 102, 103, 104, 105, 106, 107, 108,
     1      109, 110, 111, 112, 113, 114, 115, 116, 117,
     2      118, 119, 120, 121, 122, 123, 124, 125, 126,
     3      127, 128, 129, 130, 131) NSYM+1
C
  100 T(1)='/M0 {MB -6 -6 moveto 0 12 rlineto 12 0 rlineto'
      T(2)='0 -12 rlineto closepath stroke ME} bind def'
      N=2
      GOTO 200
  101 T(1)='/M1 {MB 0 0 1 FC ME} bind def'
      N=1
      GOTO 200
  102 T(1)='/M2 {MB 0 7 moveto 0 -14 rlineto -7 0 moveto'
      T(2)='14 0 rlineto stroke ME} bind def'
      N=2
      GOTO 200
  103 T(1)='/M3 {MB 0 6 moveto 0 -6 lineto -5 3 moveto 5 -3 lineto'
      T(2)='5 3 moveto -5 -3 lineto stroke ME} bind def'
      N=2
      GOTO 200
  104 T(1)='/M4 {MB 0 0 7 CC ME} bind def'
      N=1
      GOTO 200
  105 T(1)='/M5 {MB -5 -5 moveto 10 10 rlineto -5 5 moveto'
      T(2)='10 -10 rlineto stroke ME} bind def'
      N=2
      GOTO 200
  106 T(1)='/M6 {MB -6 -6 moveto 0 12 rlineto 12 0 rlineto'
      T(2)='0 -12 rlineto closepath stroke ME} bind def'
      N=2
      GOTO 200
  107 T(1)='/M7 {MB 0 8 moveto -7 -4 lineto 7 -4 lineto closepath'
      T(2)='stroke ME} bind def'
      N=2
      GOTO 200 
  108 T(1)='/M8 {MB 0 7 moveto 0 -14 rlineto -7 0 moveto 14 0 rlineto'
      T(2)='stroke 0 0 7 CC ME} bind def'
      N=2
      GOTO 200
  109 T(1)='/M9 {MB 0 0 1 FC 0 0 7 CC ME} bind def'
      N=1
      GOTO 200
  110 T(1)='/M10 {MB -9 9 moveto -8 7 lineto -7 3 lineto -7 -3 lineto'
      T(2)='-8 -7 lineto -9 -9 lineto -7 -8 lineto -3 -7 lineto'
      T(3)='3 -7 lineto 7 -8 lineto 9 -9 lineto 8 -7 lineto'
      T(4)='7 -3 lineto 7 3 lineto 8 7 lineto 9 9 lineto 7 8 lineto'
      T(5)='3 7 lineto -3 7 lineto  -7 8 lineto closepath stroke'
      T(6)='ME} bind def'
      N=6
      GOTO 200
  111 T(1)='/M11 {MB 0 10 moveto -6 0 lineto 0 -10 lineto 6 0 lineto'
      T(2)='closepath stroke ME} bind def'
      N=2
      GOTO 200
  112 T(1)='/M12 {MB 0 9 moveto -2 3 lineto -8 3 lineto -3 -1 lineto'
      T(2)='-5 -7 lineto 0 -3 lineto 5 -7 lineto 3 -1 lineto 8 3'
      T(3)='lineto 2 3 lineto closepath stroke ME} bind def'
      N=3
      GOTO 200
  113 T(1)='/M13 {MB 0 8 moveto -7 -4 lineto 7 -4 lineto closepath'
      T(2)='fill ME} bind def'
      N=2
      GOTO 200
  114 T(1)='/M14 {MB -2 6 moveto -2 2 lineto -6 2 lineto -6 -2 lineto'
      T(2)='-2 -2 lineto -2 -6 lineto 2 -6 lineto 2 -2 lineto'
      T(3)='6 -2 lineto 6 2 lineto 2 2 lineto 2 6 lineto closepath'
      T(4)='stroke ME} bind def'
      N=4
      GOTO 200
  115 T(1)='/M15 {MB 0 8 moveto -7 -4 lineto 7 -4 lineto closepath'
      T(2)='0 -8 moveto 7 4 lineto -7 4 lineto closepath stroke ME}'
      T(3)='bind def'
      N=3
      GOTO 200
  116 T(1)='/M16 {MB -4 -4 moveto 0 8 rlineto 8 0 rlineto 0 -8'
      T(2)='rlineto closepath fill ME} bind def'
      N=2
      GOTO 200
  117 T(1)='/M17 {MB 0 0 4.5 FC ME} bind def'
      N=1
      GOTO 200
  118 T(1)='/M18 {MB 0 9 moveto -2 3 lineto -8 3 lineto -3 -1 lineto'
      T(2)=' -5 -7 lineto 0 -3 lineto 5 -7 lineto 3 -1 lineto 8 3'
      T(3)='lineto 2 3 lineto closepath fill ME} bind def'
      N=3
      GOTO 200
  119 T(1)='/M19 {MB -12 -12 moveto 0 24 rlineto 24 0 rlineto 0 -24'
      T(2)='rlineto closepath stroke ME} bind def'
      N=2
      GOTO 200
  120 T(1)='/M20 {MB 0 0 2 CC ME} bind def'
      N=1
      GOTO 200
  121 T(1)='/M21 {MB 0 0 4 CC ME} bind def'
      N=1
      GOTO 200
  122 T(1)='/M22 {MB 0 0 5 CC ME} bind def'
      N=1
      GOTO 200
  123 T(1)='/M23 {MB 0 0 7 CC ME} bind def'
      N=1
      GOTO 200
  124 T(1)='/M24 {MB 0 0 11 CC ME} bind def'
      N=1
      GOTO 200
  125 T(1)='/M25 {MB 0 0 17 CC ME} bind def'
      N=1
      GOTO 200
  126 T(1)='/M26 {MB 0 0 22 CC ME} bind def'
      N=1
      GOTO 200
  127 T(1)='/M27 {MB 0 0 41 CC ME} bind def'
      N=1
      GOTO 200
  128 T(1)='/M28 {MB -6 2 moveto -9 0 lineto -6 -2 lineto -3 5'
      T(2)='moveto -8 0 lineto -3 -5 lineto -8 0 moveto 9 0 lineto'
      T(3)='stroke ME} bind def'
      N=3
      GOTO 200
  129 T(1)='/M29 {MB 6 2 moveto 9 0 lineto 6 -2 lineto 3 5 moveto'
      T(2)='8 0 lineto 3 -5 lineto 8 0 moveto -9 0 lineto stroke ME}'
      T(3)='bind def'
      N=3
      GOTO 200
  130 T(1)='/M30 {MB 2 6 moveto 0 9 lineto -2 6 lineto 5 3 moveto'
      T(2)='0 8 lineto -5 3 lineto 0 8 moveto 0 -9 lineto stroke ME}'
      T(3)='bind def'
      N=3
      GOTO 200
  131 T(1)='/M31 {MB 2 -6 moveto 0 -9 lineto -2 -6 lineto 5 -3'
      T(2)='moveto 0 -8 lineto -5 -3 lineto 0 -8 moveto 0 9 lineto'
      T(3)='stroke ME} bind def'
      N=3
      GOTO 200
C
  200 DO 210 I=1,N
          CALL GRPS02(IOERR, UNIT, T(I))
  210 CONTINUE
C
      END

C*GRPS02 -- PGPLOT PostScript driver, copy buffer to file
C+
      SUBROUTINE GRPS02 (IER, UNIT, S)
C
C Support routine for PSdriver: write character string S on
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
     1        GRWARN('++WARNING++ Error writing PostScript file')
      END IF
C-----------------------------------------------------------------------
      END
