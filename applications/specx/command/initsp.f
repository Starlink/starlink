*  History:
*     18 Nov 1993 (hme):
*        Resolve environment variable SPECX_INIT before opening the
*        file.
*        Replace backslashes in string constants.
*     24 Nov 1993 (hme):
*        Avoid the NAMELIST, read with * format instead. This means the
*        file specx_init.dat must change format.
*     25 Nov 1993 (hme):
*        Don't set XCLEAR true, so that an initial spectrum is assumed
*        to be in the stack X register.
*        Also don't set JTOP to 0.
*     30 Nov 1993 (hme):
*        Undo changes of 25 Nov and 24 Nov.
*     06 Dec 1993 (hme):
*        The expanded FILES include file needs DAT__SZLOC, thus we must
*        INCLUDE 'DAT_PAR' here.
*     08 Dec 1993 (hme):
*        Initialise so that auto-dumping disabled.
*     17 Dec 1993 (hme):
*        Re-order IODATA common block to avoid alignment problems.
*        In order to adapt to the new STACKCOMM, use offest 110 (not
*        108) for scan number. There is also no need for a locally
*        declared and equivalenced ISTACK, when SCAN_HEADER is declared
*        in STACKCOMM.
*     09 Jan 1994 (rp):
*        Replace PSX_GETENV with UTRNLOG
*     24 Apr 1994 (rp)
*        Remove 'DAT_PAR' for non-Unix version -- put in 'FILES'
*        Add INCLUDE for command table
*      8 May 2000 (ajc)
*        Port to Unix
*        Replace 'TYPE *' with 'PRINT *'
*        Set FILELUNS= 0 not .FALSE.
*        Add INCLUDE SPECX_FITS
*        Remove declaration of NFILE*6 and IFILE1*6
*        Initialise SHOW_BEAM and DMS
*        Remove CARRIAGECONTROL and READONLY from OPEN
*        Unused NFILE, IFILE1
C-----------------------------------------------------------------------

      SUBROUTINE INITSP

C   Routine to initialize all flags and constants used in SPECX.

      INCLUDE        'COMMAND_TABLE'
      INCLUDE        'CUBE'
      INCLUDE        'FLAGCOMM'
      INCLUDE        'IODATA'
      INCLUDE        'NEWXY'
      INCLUDE        'MAPS'
      INCLUDE        'MAPTITLES'
      INCLUDE        'NOKEEP'
      INCLUDE        'PLOT2D'
      INCLUDE        'SPECX_FITS'
      INCLUDE        'SPECX_PARS'
      INCLUDE        'STACKCOMM'
      INCLUDE        'STAKPAR'

      INCLUDE        'DAT_PAR'
C     INCLUDE        'SAE_FIX'
      INCLUDE        'FILES'

      NAMELIST /SPECX/ TOL, MAXITS, IER,
     &                 ALAT, ALONG, TIMCOR, OBSTIT,
     &                 CHARHT, XLEN, YLEN, HISTOGRAM,
     &                 IDEV, TTNN, ANDXLG,
     &                 ISUM, CONTI, CONT0, NCONT,
     &                 AX1LEN, AX2LEN, THETA,
     &                 ICAUTO, COLOUR_TABLE, AUTOGREY, OVERCONT,
     &                 GREYMIN, GREYMAX,
     &                 MVPTS, MAP_TOL, REPLACE_MAP_DATA,
     &                 IEDASK, IEDVER, IDUMP,
     &                 NXS, IREST,
     &                 INTERACTIVE, TERMDEV, PRINTDEV,
     &                 GSDNAME

      CHARACTER  FILENAME*255
      INTEGER    STATUS

      REAL*8     PI
      PARAMETER (PI=3.141592654D0)

C  Initialize the symbol table for the scan header

      CALL SPECX_INIT_TABLE

C  Number of functions

      NFPP = NFUNC

C  General program control

      DUMP_STATE = .FALSE.

C  Stack parameters

      LSTK   = 2176
      JSTK   = 4
      IDAT2  = LSTK+LHEAD

C  Baseline removal parameters

      NPR    = 0
      NPOLD  = 0
      NSOLD  = 0
      ISBFLG = 0

C  Arithmetic operations

      FACT   = 1.0
      DIV    = 1.0

C  Other reduction operations

      NBIN   = 2
      NSTART = 1
      IC1    = 0
      IC2    = 0
      ISMTH  = 1

      OFF    = 0.

C  Versatek, Anadex, VT240 and Tektronix plot parameters

      QUICK_PLOT = .FALSE.
      FILNAM = 'PLOT.000'
      JPLOT  = 0
      IPSEQ  = 0
      IMODEL = 0
      NINTSX = 10
      NINTSY = 10
      XST    = -100.
      XEND   = +100.
      YST    = -10.
      YEND   = +10.
      IPEN    = 0
      LWEIGHT = 1
      ISETSC        = .TRUE.
      FREEX         = .TRUE.
      FREEY         = .TRUE.
      CHANGE_SCALES = .FALSE.
      ERASE_PLOT    = .TRUE.
      SHOW_HEADER   = .TRUE.
      SHOW_2DHEAD   = .TRUE.
      SHOW_BEAM     = .FALSE.

C  Printer output file

      ILOUT  = 6
      IOUT   = 6
      ILOUT2 = 6
      LISTDEV      = 'T'
      SAVE_FILE    = ' '
      PRINT_OUTPUT = .FALSE.

C  Scan transfer

      IFILEX = 2
      INFLAG = 0
      OUTFLG = 0
      DO J = 1, MAX_DFILES
        FILNAMS(J)   = ' '
        ACCESS(J)    = ' '
        FILELUNS(J)  = 0
      END DO

C  1-D plots

      FCORRECT    = .FALSE.
      YAXIS_NAME  = 'T'//CHAR(92)//'dA'//CHAR(92)//
     :              'u'//CHAR(92)//'u*'//CHAR(92)//'d'
      YAXIS_UNITS = 'K'

C  Maps

      NAMEMP  = ' '
      CHANGE_PLOT = .FALSE.
      DMS = .FALSE.

      LINK(1) = 1
      LINK(2) = 2
      LINK(3) = 3

      QBEG(1) = 0.
      QEND(1) = 0.
      QBEG(2) = 0.
      QEND(2) = 0.
      QBEG(3) = -100.
      QEND(3) = 100.

      LTPOS   = 0
      LTZ     = 1
      LTNEG   = 2

      BADPIX_VAL  = -1.E2
      CLEVELS_SET = .FALSE.
      NCSET   = 0

      INTERP_X = 2
      INTERP_Y = 2
      SMOOTH   = .TRUE.

      GREYLIM(1) = GREYMIN
      GREYLIM(2) = GREYMAX

      FWHM     = 20.
      WMAX     = 25.
      MAP_INTERPOLATED    = .FALSE.
      MAP_ROTATED         = .FALSE.
      CUBE_IN_MEMORY      = .FALSE.
      NEW_CUBE_LOADED     = .FALSE.

      C5START  =  0.5
      C5ROTAT  = -1.5
      C5EXP    =  0.8
      CALL SETCOL5 (C5START, C5ROTAT, C5EXP)

C  Grid-map

      TLIM(1) = -5.0
      TLIM(2) = 20.0

C  Maps of line_parameters

      DO I = 1,4
        IPLP(I) = I
      END DO

C  Map file etc

      MAP_OPEN  = .FALSE.
      FITS_OPEN = .FALSE.

C  Book-keeping

      DO I = 1,8
        MASK(I) = 1
      END DO
      NMASK   = 1
      NAMEFD  = 'SPECX_DUMP'

C   Set scan headers to indicate position clear

      DO I=1,JSTK
        SCAN_HEADER((I-1)*LSTK+110) = -1
      END DO

C   Pick up site-specific initialization from data file
C   using NAMELIST directed read

      STATUS = 0
      CALL UTRNLOG ('SPECX_INIT', FILENAME, STATUS)

      STATUS = IGETLUN (LUN, 'Initsp', .TRUE.)
      IERR = 0
      OPEN (LUN, FILE=FILENAME, STATUS='OLD', ACCESS='SEQUENTIAL',
     &      IOSTAT=IERR)
      IF (IERR.NE.0) THEN
        PRINT *, 'Error in NAMELIST directed read: '
        PRINT *, 'Logical name was SPECX_INIT'
        PRINT *, 'Filename was ', FILENAME
        STOP 'Exiting...'
      END IF

      READ (LUN, SPECX)

      CLOSE (LUN)
      STATUS = IFREELUN (LUN)

C   Do any sums we need to on the new data

      COSLAT    = DCOS (ALAT*PI/180.D0)

C   ...and set up terminal flags

      IF (IDEV.GE.1 .AND. IDEV.LT.20) TERMINAL = .TRUE.

      RETURN
      END

C-----------------------------------------------------------------------
