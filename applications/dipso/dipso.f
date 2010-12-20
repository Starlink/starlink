       SUBROUTINE DIPSO( IENTRY )

*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
*
*
*   DIPSO
*
*   Comment out sections between "!*" flags for Starlink release.
*
*   This subroutine is called by module run_dipso. On VMS this is a dummy
*   fortran module which does nothing except call this subroutine. On
*   unix, run_dipso is a C function which sets up signal handling before
*   calling this subroutine. If a floating point exception, segmentation
*   violation, or an interupt (contol-C) occurs at any time, execution jumps
*   back to run_dipso, and this subroutine is called again with argument
*   IENTRY set to a value which indicates what sort of signal was detected.
*   IENTRY is set to zero on the initial entry.
*
*   "D" lines (lines with D in column 1) are used to mark lines which
*   are only needed for the VMS version. The VMS version should be
*   compiled with the /D_LINES qualifier.
*-----------------------------------------------------------------------


*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'MSG_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'PRM_PAR'

*  Local Constants:
      CHARACTER*1 ESC          ! A "\" character is used by some
      PARAMETER( ESC = '\\' )  ! compilers as an escape character and
                               ! therefore two are needed to be safe

*  External References:
      LOGICAL CHR_SIMLR

*  Declare stacks
       INCLUDE 'DECLARE_STKS'
       INTEGER JJ
       LOGICAL PROCEED,USENDF,CLR,EXISTS,OVERWR
       INTEGER STATUS
       CHARACTER*10 STRING1, STRING2, STRING3
       CHARACTER*80 BIGSTR
       INTEGER NCHAR1, NCHAR2, NCHAR3
       LOGICAL ICSTAT
       CHARACTER*32   sysname
       CHARACTER*32   nodename
       CHARACTER*32   release
       CHARACTER*32   machine
       CHARACTER*32   version
       CHARACTER*80   prefix
       CHARACTER*80   prefix2
       CHARACTER*80   prefix3
       CHARACTER*80   prefix4
       CHARACTER*80   sbdir
       INTEGER        plen,p2len,p3len,p4len,sblen

*  Array indices
       INTEGER PTR1, PTR2, NPT1, NPT2

       COMMON /prefix1/ prefix
       COMMON /prefix2/ plen
       COMMON /CLR_COM/ CLR

*  Declare functions
       LOGICAL USER
       LOGICAL ISBACH
       INTEGER HANDLER
       INTEGER CHR_LEN

*  Declare global variables and common blcoks used by elf.
      INCLUDE 'KARS_COM'   ! Declares common block KARS holding KAR
      INCLUDE 'KUSE_COM'   ! Declares common block KUSE holding K1 and K2

*  Initialise control-C handler
C       EXTERNAL HANDLER
C       EXTERNAL CTRLC_AST
C       INTEGER SYS$ASSIGN, SYS$QIOW
C       INTEGER CTRLCST
C       STRUCTURE /IOSTAT_BLOCK/
C          INTEGER*2 IOSTAT
C          BYTE TRANSMIT, RECEIVE, CRFILL, LFFILL, PARITY, ZERO
C       ENDSTRUCTURE
C       RECORD /IOSTAT_BLOCK/IOSB
C       INTEGER*2 INPUT_CHAN
C       INTEGER*4 CTRLCODE
C       INCLUDE '($IODEF)'
*
*   Declare additional scratch areas
*   (Basic scratch area is defined in INCLUDE block)
*
       DOUBLE PRECISION DPWORK(WORKSZ/2)
       INTEGER IWORK(WORKSZ)
       EQUIVALENCE (WORK(1),IWORK(1),DPWORK(1))
*
*   Declare data areas
       INCLUDE 'DECLARE_DATA'

       REAL XRKEEP(2), YRKEEP(2)
*
*   Strings used for parameters in CMD calls
*
       LOGICAL CMDUSE(9)
       CHARACTER*80 CMDPAR(9)
*
*   Stuff required for PDA routines
*
       INTEGER PDAPTS, IERR1, IERR2, NDEG
       DOUBLE PRECISION DPX1, DPX2, DPXBAR, DPYVAL, EPS
*
*   Declarations and common blocks for use in PLTARR
*
       INCLUDE 'DECLARE_PLTS'

*
       LOGICAL FIRSTDEVICE

*
       LOGICAL CURSOR
       LOGICAL FIRSTCURSOR
       INTEGER ASF(13)
*
*   Stuff associated with EW log file
*
       LOGICAL LOGFILE, LOGWRTE
       INTEGER EWUNIT
       CHARACTER*50 EWLOGFILE

       COMMON /EWLOG/ LOGWRTE, EWUNIT
*
*   Stuff associated with BACH files
*
       INTEGER GL, GU
       CHARACTER LINNAM*10, READIN*50
       REAL FVALUE, WAVE0, BINST, VLOW, VHIGH
       PARAMETER (C=2.9979246E+05)
*
*   Variables for PJS's ELF library
*
       COMMON /DAT2  / PJSA, PJSB, PJSP, PJSS, PJSFLX, INDXPS, IRELPS,
     : LPPS, NLPS, IABPS
       COMMON /DAT4  / PJSXV, PJSFXV, PJSARP, PJSWVF, PJSCFLX, IXVPS,
     : NXVPS, NPROFPS
       COMMON /DAT4A / ITXTPS
       COMMON /DAT5  / NDIMPS, NDI2PS, NPARPS, NUMFITPS, NCURPS, MAXFCPS
       COMMON /DEBUG / NYPS
       COMMON /VIEWALL/ IVIEW
       REAL PJSA(80), PJSB(80), PJSP(10), PJSFLX(20)
       INTEGER INDXPS(80), IRELPS(80), LPPS(20)
       REAL PJSXV(5,1000), PJSFXV(5,1000), PJSARP(5)
       REAL PJSCFLX(5)
       INTEGER IXVPS(5), NXVPS(5)
       CHARACTER*40 ITXTPS(5)
       REAL PJSWVF(5)
       INTEGER ELFPRMPT
       COMMON /IDHELF/ ELFPRMPT
*
*   Other stuff
*
       LOGICAL TZNPLT(100)
       LOGICAL LJACK
       LOGICAL ISTST
       LOGICAL DEBUG
       LOGICAL CCUR
       LOGICAL ECHO
       LOGICAL PPROMPT, TPROMPT
       LOGICAL VAL_OK
       LOGICAL USEHTX
       LOGICAL ISATTY

       INTEGER INWORK
       REAL XLIML(100), XLIMH(100)
       INTEGER IOS, IOS1, IOS2, IOS3, IOS4
       INTEGER ALASL1, ALASL2, ALASXC, ALASYC
       CHARACTER*132 ALASTXT
       INTEGER HPSTAY
       INTEGER TWO
       LOGICAL HPROT
       INTEGER DELIM, DPARSE
       INTEGER SLEN
       INTEGER NDEF
       CHARACTER TITLE*80, SUBCMD*80
       CHARACTER*80 COMSAV
       INTEGER CMDLEN
       CHARACTER COMND1*800, COMND2*800, PARAMS*800
       CHARACTER*200 RESTRING
       REAL DUMMY
       REAL vdummy(100000)
       REAL X1, X2, Y1, Y2
       INTEGER  vmsrec,vmssize
       REAL EW, SLOPE, FWHM, OVRSMP, HCMXFC
       REAL SIGMA, MARK, MARKKP1, MARKKP2
       REAL LAM0
       REAL VARRAY(MAXSTK)
       REAL VARS(3)
       INTEGER I, J, K, I1, I2
       LOGICAL NOTEND, OK

*
       CHARACTER*80 SHOWME,TSHOW
       CHARACTER*80 IHHEAD
       CHARACTER*80 IHFILE
       CHARACTER*1 KREAD(79)
       LOGICAL COMTXT
       LOGICAL SUBCHK
       LOGICAL FFOPEN
       CHARACTER*80 FFILE
       INTEGER FFUNIT
       PARAMETER (FFUNIT=29)
       LOGICAL LBOX, LKEEP
       LOGICAL ROTST, ZNTST, MRKTST, TLNTST
       LOGICAL POLTST, HSTTST
       LOGICAL ERTST
       LOGICAL DVTST
       LOGICAL PSTST, PSLST
       LOGICAL ATTST
       LOGICAL YATM
       LOGICAL WARNIT, LUSER
       LOGICAL PUSHW
       LOGICAL BEEP
       CHARACTER*1 BLEEP
       COMMON /BEEP  / BEEP
       COMMON /BLEEP / BLEEP
       LOGICAL ATFCAL
       COMMON /ATCALLED/ ATFCAL
       CHARACTER*80 IHHLP
       CHARACTER*100 DEFIN(2)
       CHARACTER ARG1*512
       CHARACTER PRMPT*20
       CHARACTER ENVNAM*30
       CHARACTER ENVVAL*256
       INTEGER PRMLEN
       COMMON /ERRCOM/ WARNIT, LUSER

*   Include LABELS for axes

       INCLUDE 'DECLARE_LBLS'
*
       COMMON /MINISV/ ISV1, ISV2

*   Common block for USERNAME (if called)
       CHARACTER*10 USERID
       COMMON /USERID/ USERID
*
*   Option to turn off 'hidden' PUSHes
       COMMON /PUSHW / PUSHW
*   Special for DJM to access FLUX results
!*
!      REAL MONKW, MONKF, MONKW1, MONKW2
!      REAL MONKFD, MONKFL, MONKFS, MONKIS
!      INTEGER MONK
!      CHARACTER*8 MONKIO
!      COMMON /DJMONK/ MONKW(100), MONKF(100), MONKW1(100), MONKW2(100),
!    : MONKQ(100), MONK, MONKFD(100), MONKFL(100),
!    : MONKFS(100), MONKIS(100), MONKIO(100)
!      COMMON /TJBATT/ NPLOTS
!*

*
*   GRIDS(8,0:100) is a 2-D array which specifies the area of the
*   selected device used for plotting
*   in normalized device co-ordinates (NDC)
*   GRIDS(1,N)  is the start of the x-axis in NDC for zone number N
*   GRIDS(2,N) is the end of the x-axis in NDC    ..  ..   ..    ..
*   GRIDS(3,N) is the start of the y-axis in NDC    ..  ..   ..    ..
*   GRIDS(4,N) is the end of the y-axis in NDC      ..  ..   ..    ..
*
*   These are set up using the command TZONE

*   LOGICAL ZONEDEF(0:100) true if zone has been defined

*
*
*   Declared for command 'SCROLLVT'
*
       INTEGER NTOP, NBOTTOM
       CHARACTER*2 CTOP, CBOTTOM
       INTEGER JTOP, JBOTTOM
       LOGICAL ANSI
*
*   Declared for commands 'LABON' and 'NLAB' which control presence of labels
*
*
*   Declared to deal with tick-labels
*
       INTEGER ITICKLABELS

       CHARACTER*1 CHECKPARAMS
*
*   Declaration for PIXY
*
!*
!   CHARACTER*60 MJSTRING
!*
*
*   Declared to deal with extra sumbols
*
*
*   Declared to set cursor position in grid window
*
       COMMON /SETCURSOR/ XCURSOR, YCURSOR

*  Ensure everything is saved between entries to this routine (e.g. caused
*  by the signal handler).

       SAVE

*   DATA statements.
       DATA CROPIT /.FALSE./
       DATA IGRIDSTYLE /1/
       DATA DEVTYP /0/
       DATA ERASEBOX/.TRUE./
       DATA LOGAXY /.FALSE./
       DATA LOGAXX /.FALSE./
       DATA FILL/.FALSE./
       DATA LABELFLAG/.TRUE./
       DATA REALSIZE /.FALSE./
       DATA FRZONE /.FALSE./
       DATA GPOS /'CC'/

       DATA MINTICKS/2*0.0/
       DATA ITICKS/2*1/

       DATA GRID/0.15, 0.95, 0.1, 0.9, 0.0, 1.0, 0.0, 1.0/
       DATA (GRIDS(N,0),N=1,8)
     : /0.15, 0.95, 0.10, 0.90, 0.00, 1.00, 0.00,  1.00/
       DATA (GRIDS(N,1),N=1,8)
     : /0.10, 0.45, 0.60, 0.90, 0.00, 0.50, 0.50, 1.00/
       DATA (GRIDS(N,2),N=1,8)
     : /0.60, 0.95, 0.60, 0.90, 0.50, 1.00, 0.50,  1.00/
       DATA (GRIDS(N,3),N=1,8)
     : /0.10, 0.45, 0.10, 0.40, 0.00, 0.50, 0.00,  0.50/
       DATA (GRIDS(N,4),N=1,8)
     : /0.60, 0.95, 0.10, 0.40, 0.50, 1.00, 0.00, 0.50/
       DATA (GRIDS(N,5),N=1,8)
     : /0.10, 0.95, 0.60, 0.90, 0.00, 1.00, 0.50, 1.00/
       DATA (GRIDS(N,6),N=1,8)
     : /0.10, 0.95, 0.10, 0.40, 0.00, 1.00, 0.00, 0.50/
       DATA (GRIDS(N,7),N=1,8)
     : /0.10, 0.45, 0.10, 0.90, 0.00, 0.50, 0.00, 1.00/
       DATA (GRIDS(N,8),N=1,8)
     : /0.60, 0.95, 0.10, 0.90, 0.50, 1.00, 0.00, 1.00/

       DATA (ZONEDEF(N),N=0,8)/9*.TRUE./
       DATA (ZONEDEF(N),N=9,100)/92*.FALSE./

       DATA (ZONECLEAR(N),N=0,8)/9*.TRUE./
       DATA (ZONECLEAR(N),N=9,100)/92*.FALSE./

       DATA FIRSTDEVICE/.TRUE./
       DATA ASF/13*0/

       DATA VLOW, VHIGH/ -800.0, 800.0/

       DATA NUMFITPS/0/, NCURPS/0/, NYPS/0/, MAXFCPS/10/
       DATA NPROFPS/0/, NPASSPS/0/, IABPS/0/

       DATA NDEF/0/

       DATA FWHM, OVRSMP, HCMXFC/0.0, 1.0, 20.0/

       DATA ANSI/.FALSE./

       DATA ITICKLABELS/1/

       DATA KAR / 1HQ, 1Hq, 1HQ, 1Hq, 1HL, 1Hl, 1Hh, 1HH, 1H?, 1H?,
     :           1HC, 1Hc, 1HW, 1Hw, 1HI, 1Hi, 1HP, 1Hp, 1HD, 1Hd,
     :           1H0, 1H1, 1H2, 1H3, 1H4, 1H5, 1H6, 1H7, 1H8, 1H9,
     :           1H., 1H-, 1H+, 1H*, 1H/, 1H=, 1H , 1H:, 1H? ,1H? /

       DATA K1/'VARIABLE ','FIXED    ','TIED TO  '/,
     :      K2/'CENTRE   ','WIDTH    ','NORM. INT','PROFILE  '/

       DATA USEHTX / .FALSE. /

*  Bell
       BLEEP = CHAR( 7 )

*  If this subroutine has been re-entered as a result of a trapped signal
*  jump to the end of the main loop.
       IF( IENTRY .EQ. 2 ) THEN
          WRITE(*,*)
          WRITE(*,*) 'Control-C detected!!'
          warnit = .true.
          GO TO 5800

       ELSE IF( IENTRY .EQ. 8 ) THEN
          WRITE(*,*)
          WRITE(*,*) 'Floating Point Exception detected!!'
          warnit = .true.
          GO TO 5800

       ELSE IF( IENTRY .EQ. 11 ) THEN
          WRITE(*,*)
          WRITE(*,*) 'Segmentation Violation detected!!'
          warnit = .true.
          GO TO 5800

       ELSE IF( IENTRY .NE. 0 ) THEN
          WRITE(*,*)
          WRITE(*,*) 'Unknown Exception detected!!'
          warnit = .true.
          GO TO 5800

       END IF

*  Initialize the global status.
      STATUS = SAI__OK

*  Defer the delivery of error messages reported by starlink software.
*  This allows them to be annulled if necessary so that the user never
*  sees them (useful if the "error" is not really an error in the
*  context of the algorithm being executed).
      CALL ERR_MARK

*
*   Initialise Control-C handler
*
C       CALL LIB$ESTABLISH(HANDLER)
C       CTRLCST = SYS$ASSIGN('SYS$INPUT',INPUT_CHAN,,)
C       IF( .NOT.CTRLCST) CALL LIB$SIGNAL(%VAL(CTRLCST))
C       CTRLCODE = IO$_SETMODE .OR. IO$M_CTRLCAST
C       CTRLCST = SYS$QIOW(,%VAL(INPUT_CHAN),%VAL(CTRLCODE),IOSB,,,
C    :                      CTRLC_AST,,,,,)
C       IF( .NOT.CTRLCST) CALL LIB$SIGNAL(%VAL(CTRLCST))
C       IF( .NOT.IOSB.IOSTAT) CALL LIB$SIGNAL(%VAL(IOSB.IOSTAT))
*
*   Miscellaneous variable initialisations
*
       USENDF = .TRUE.
       CLR = .TRUE.
       DATA NONSTK, STKLST, BSTLST/0, 0, 0/
       XLAB(1:10) = 'Wavelength'
       XLABLN = 10
       YLAB(1:4) = 'Flux'
       YLABLN = 4
       xNPOINT = 0
       NPLOTS = 0
       IFONT = 0
       TITLE(1:14) = '       (Empty)'
       IHHLP(1:1) = ' '
       LOGWRTE = .FALSE.
       LOGFILE = .FALSE.
       EWUNIT = 4
       SIGCIH = 0.0
       IHFLAG = 0
       ICFLAG = 0
       LNTYPE = 1
       LNTYPEKP = LNTYPE
       NZONEN = 0
       MARK = 1.0
       MARKKP = 1.0
       MARKSTYLE = 1
       NSIDES = 4
       MARKKP1 = MARKSTYLE
       MARKKP2 = NSIDES
       INTECHO = -1
       WARNIT = .FALSE.
       PPROMPT = .FALSE.
       HPROT = .FALSE.
       TPROMPT = .FALSE.
       LUSER = .FALSE.
       PUSHW = .TRUE.
       BEEP = .TRUE.
       POLTST = .FALSE.
       HSTTST = .TRUE.
       FFOPEN = .FALSE.
       FFILE = ' '
       RESTRING = ' '
       COMTXT = .FALSE.
       ECHO = .TRUE.
       DEBUG = .FALSE.
       LBOX = .TRUE.
       ROTST = .FALSE.
       ERTST = .FALSE.
       DVTST = .FALSE.
       PSTST = .FALSE.
       ATTST = .FALSE.
       ZNTST = .FALSE.
       MRKTST = .FALSE.
       TLNTST = .FALSE.
       TRIMX = .TRUE.
       TRIMY = .FALSE.
       SUBCHK = .TRUE.
       IPAL = 1
       IPALKP = IPAL
       EW = 0.0
       LNWIDTH = 1.0
       XLWIDTH = 1.0
       SIGEW = 0.0
       ALASL1 = 1
       ALASL2 = 0
       ALASXC = 1
       ALASYC = 2
       DO 100 I = 1, 100
          TZNPLT(I) = .FALSE.
  100  CONTINUE
       DO 200 I = 1, 2
          XRKEEP(I) = -321.654
          YRKEEP(I) = -321.654
  200  CONTINUE

*  Get the name of the operating system (VMS, UNIX, etc) in SYSNAME.
      CALL PSX_UNAME( SYSNAME, NODENAME, RELEASE, VERSION, MACHINE,
     :                STATUS )

*  Get the first command line argument. This can be a comma sperated list
*  of "keyword=value" pairs. This list is searched for values for any
*  environment variables which are required but not defined.
      CALL GETARG( 1, ARG1 )

*  Inquire if standard input has been redirected to a file
      CALL PSX_ISATTY( 0, ISATTY, STATUS )

*  Get the path to the DIPSODIR directory. This is returned in a format
*  such that a complete file specification can be formed just by
*  appending the file name (and file type on VMS) to the end of the path.
      CALL GETDIR( 'DIPSODIR', ARG1, PREFIX, PLEN, STATUS )

*  Do the same for the OWNERDIR, SPECDAT and LDIPSODIR directories.
      CALL GETDIR( 'OWNERDIR', ARG1, PREFIX2, P2LEN, STATUS )
      CALL GETDIR( 'SPECDAT', ARG1, PREFIX3, P3LEN, STATUS )
      CALL GETDIR( 'LDIPSODIR', ARG1, PREFIX4, P4LEN, STATUS )

*  Do the same for the directory containing Starlink executable files,
*  and construct a string holding the default showme command used by
*  the hypertext help system.
      CALL GETDIR( 'DIPSOSTARBIN', ARG1, SBDIR, SBLEN, STATUS )
      SHOWME = SBDIR( : SBLEN )//'showme'

*  Initialise the message filtering level to verbose, so that the user
*  sees all informational messages and error reports. This can be changed
*  by the user using the REPORTING command.
      CALL MSG_IFSET( MSG__VERB, STATUS )

*  Abort if an error has occured.
      IF( STATUS .NE. SAI__OK ) GO TO 6000

*  Translate the DIPSOPROMPT environment variable to get the DIPSO command
*  prompt string. Use ">" by default.
      CALL GTENV( "DIPSOPROMPT", ARG1, PRMPT, STATUS )
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_ANNUL( STATUS )
         PRMPT = '>'
         PRMLEN = 1
      ELSE
         PRMLEN = CHR_LEN( PRMPT )
      END IF

*  Initialisations for NCAR conversion
      NDEGREES = 0
      DEFHEIGHT = 0.025
      DO I = 1, NHTFAC
         HTFAC( I ) = 1.0
      END DO
*
*   Initialisations for PIXY
*
!*
!      MJSUNIT = 17
!      MJSTRING = ' '
!      CLOSE (MJSUNIT)
!*
*
*   Initialisations for PJS's ELF routines
*
       NDIMPS = 20
       NDI2PS = 2*NDIMPS
       NDI3PS = 3*NDIMPS
       NDI4PS = 4*NDIMPS
       CALL ELFCLR
*
*   Program starts here STARTS, BEGINS, begins
*
       CALL SETDEV(0,-1,OK)
       USERID = 'IDH'
!*
*       CALL USERNAME(USERID)
*      CALL DTOUPP(USERID)
       userid=' '
!*
*
*       INQUIRE (UNIT=6,OPENED=STREAM6)
*       IF( STREAM6 ) THEN
*          CLOSE (6,IOSTAT=IHX)
*          IF( IOSTAT .NE. 0 ) THEN
*             WRITE (*,
*     :       '(''   !*  Error closing stream 6 -'',
*     :       '' SL and SLWR may not function correctly  *!'',A)') BLEEP
*          ELSE
*             WRITE (*,'(''   !*  Closing stream 6  *!'',A)') BLEEP
*          END IF
*       END IF


*------------------------------------------------------------------------

       OPEN (UNIT=16,STATUS='OLD',
     :       FILE=prefix(1:plen)//'updates.lis',
     :    IOSTAT=IHX)
       IF( IHX .EQ. 0 ) THEN
          DO 220 IMSG = 1, 1000
             READ (16,'(A72)',ERR=250,END=250) IHHLP(1:72)
             WRITE (*,'(''   '',A)') IHHLP(1:SLEN(IHHLP))
  220     CONTINUE
       END IF
  250  CONTINUE
       CLOSE (16)

       OPEN (UNIT=16,STATUS='OLD',
     :       FILE=prefix4(1:p4len)//'lupdates.lis',
     :    IOSTAT=IHX)
       IF( IHX .EQ. 0 ) THEN
          DO 270 IMSG = 1, 1000
             READ (16,'(A72)',ERR=300,END=300) IHHLP(1:72)
             WRITE (*,'(''   '',A)') IHHLP(1:SLEN(IHHLP))
  270     CONTINUE
       END IF
  300  CONTINUE
       CLOSE (16)


*  Comment the above lines and uncomment the following lines for
*  test versions.

c       WRITE(*,*)
c       WRITE(*,*)
c       WRITE(*,*) '  This is an unofficial test version of DIPSO '//
c     :            'dated 15/3/95'
c       WRITE(*,*)
c       WRITE(*,*)

*------------------------------------------------------------------------

       IF( p2len .GT. 0 ) THEN
       OPEN (UNIT=67,STATUS='OLD',
     :       FILE=prefix2(1:p2len)//'startup.cmd',
     : IOSTAT=IHX)
       ELSE
       OPEN (UNIT=67,STATUS='OLD',
     :       FILE='startup.cmd',
     : IOSTAT=IHX)
       END IF

       IF( IHX .EQ. 0 ) THEN
          COMTXT = .TRUE.
          COMSAV = 'startup.cmd'
          IF( p2len .GT. 0 )
     :       COMSAV = prefix2(1:p2len)//'startup.cmd'
          NCCAR = SLEN(COMSAV)
          DO 350 I = 1, 9
             CMDUSE(I) = .TRUE.
  350     CONTINUE
       ELSE
          CLOSE (67)
       END IF
*
       DELIM = 0
       NOTEND = .TRUE.
       OK = .TRUE.
  400  CONTINUE
       IF( NOTEND ) THEN
         IXS = 0
         IOS = 0
         OK = .TRUE.
*
*   READ COMMANDS FROM FILE OR TERMINAL
*
          IF( COMTXT ) THEN
             IF( DELIM .NE. 1 ) THEN
  410           CONTINUE
                READ (67,'(A800)',IOSTAT=IHX) COMND1
                IF( IHX .NE. 0 ) THEN
                   COMND1 = ' '
                   IF( IHX .NE. -1 ) THEN
                      WRITE (*,'(A,A)') '   Error reading from ',
     :                COMSAV(1:NCCAR)//' command file'
                      GO TO 5000
                   ELSE
                      IF( ABS(INTECHO).GT.0 ) THEN
                         WRITE (*,'(A,A)') '   '//COMSAV(1:NCCAR),
     :                   ' command sequence completed'
                         WRITE (*,
     :                   '(''   Returning to terminal control'')')
*
                         ALASTXT = ' '
                         DO 412 I = 1, 9
                            IF( .NOT.CMDUSE(I) ) THEN
                               IF( ALASTXT .EQ. ' ' ) THEN
                                  ALASTXT(1:1) = 'P'
                                  WRITE (ALASTXT(2:2),'(I1)') I
                               ELSE
                                  J = SLEN(ALASTXT)
                                  WRITE (ALASTXT(J+1:),'(A,I1)')
     :                            ', P', I
                               END IF
                            END IF
  412                    CONTINUE
                         IF( ALASTXT .NE. ' ' ) THEN
                            WRITE (*,
     :                      '(''   Warning - command file parameters'',
     :                      '' unused '',A,A)')
     :                      ALASTXT(1:SLEN(ALASTXT)), BLEEP
*                            CLOSE (67)
                            COMTXT = .FALSE.
                            GO TO 5000
                         END IF
                      END IF
                      COMTXT = .FALSE.
*                     CLOSE (67)
                   END IF
                END IF
*
*   Block to deal with "parameters" in command procedure calls
*
                J = 1
  420           CONTINUE
                I1 = INDEX(COMND1(J:),'''P')
                I2 = INDEX(COMND1(J:),'''p')
                IF( I1 .EQ. 0 ) THEN
                   I = I2
                ELSE IF( I2 .NE. 0 ) THEN
                   I = MIN(I1,I2)
                ELSE
                   I = I1
                END IF
                IF( I .NE. 0 ) THEN
*   check to see if it is a PWRITX index
                   I = I + J - 1
                   IF( (COMND1(I+2:I+2) .EQ. 'R') .OR.
     :                (COMND1(I+2:I+2) .EQ. 'r') .OR.
     :                (COMND1(I+2:I+2) .EQ. 'G') .OR.
     :                (COMND1(I+2:I+2) .EQ. 'g') ) THEN
!                     J = I + 5
                      J = I + 2
*   check to see if it's just a `P' character preceded by a PWRITX index
*   as in, e.g. PWRITE "'KGL'P2"
                   ELSE
                      NAPOST = 0
                      DO 430 IX = 1, I
                         IF( COMND1(IX:IX) .EQ. '''') NAPOST = NAPOST+1
  430                 CONTINUE
                      TWO = 2
                      NAPOST = MOD(NAPOST,TWO)
                      IF( NAPOST .EQ. 0 ) THEN
                         J = I + 2
                      ELSE
*   it is neither a recognized PWRITE index nor just a "P"
                         I = I + J - 1
                         I = I + 3
                         IF( COMND1(I:I) .NE. '''' ) THEN
                            J = ICHAR('0')
                            K = ICHAR('9')
                            L = ICHAR(COMND1(I:I))
                            IF( L.GE.J .AND. L.LE.K ) THEN
                               WRITE (*,'(''   Illegal parameter '',A3,
     :                         '' in command file (P9 is maximum '',
     :                         ''allowed)'',A)') COMND1(I-2:I), BLEEP
                            ELSE
                               WRITE (*,'(''   Error interpreting '',
     :                         ''parameter '',A,'' in command file'',
     :                         A)')
     :                         COMND1(I-2:I), BLEEP
                            END IF
                            GO TO 5000
                         END IF
                         I = I - 1
                         READ (COMND1(I:I),'(I1)',IOSTAT=J) N
                         IF( J .NE. 0 ) THEN
                            WRITE (*,'(''   Error interpreting '',
     :                      ''parameter '',A,'' in command file'',A)')
     :                      COMND1(I-2:I), BLEEP
                            GO TO 5000
                         ELSE IF( CMDPAR(N) .EQ. ' ' ) THEN
                            WRITE (*,'(''   Parameter P'',I1,
     :                      '' in command file is undefined'',A)')
     :                      N, BLEEP
                            GO TO 5000
                         END IF
                         COMND1 = COMND1(1:I-3)//CMDPAR(N)
     :                   (1:SLEN(CMDPAR(N)))//COMND1(I+2:)
                         CMDUSE(N) = .TRUE.
                         J = 1
                      END IF
                      GO TO 420
                    END IF
                END IF
*
                IF( (COMND1(1:1) .EQ. '!') .OR.
     :              (COMND1(1:1) .EQ. '*') ) THEN
                   IF( INTECHO.GE.2 ) THEN
                      WRITE (*,'(''   Macro comment: '',A)')
     :                COMND1(2:SLEN(COMND1))
                   END IF
                   GO TO 410
                END IF
             END IF

          ELSE IF( DELIM .NE. 1 ) THEN
             CALL GETINP( 1, COMND2, PRMPT( : PRMLEN )//' ', IHX )
             IF( .NOT. ISATTY .AND. IHX .LT. 0 ) THEN
               IHX = 0
               COMND2 = 'EXIT'
             END IF
             IF( IHX .NE. 0 ) THEN
                WRITE (*,'(''   DIPSO:  error reading command line'',
     :          A)') BLEEP
                OK = .FALSE.
                GO TO 5000
             END IF
          END IF
*
*   Split off first command in string, and get parameters
*   (making a special search for the REMEMBER command)
*
          IF( COMTXT ) THEN
             CALL RECORD(COMND1,RESTRING,OK)
             IF( .NOT.OK ) THEN
                SUBCMD(1:) = 'RECORD'
                GO TO 5000
             END IF
             CALL GETCOM(COMND1,SUBCMD,PARAMS,DELIM,SUBCHK)
          ELSE
             CALL RECORD(COMND2,RESTRING,OK)
             IF( .NOT.OK ) THEN
                SUBCMD(1:) = 'RECORD'
                GO TO 5000
             END IF
             CALL GETCOM(COMND2,SUBCMD,PARAMS,DELIM,SUBCHK)
          END IF
          CMDLEN = INDEX(SUBCMD,' ') - 1
          IF( CMDLEN.LT.0) GO TO 400
          IF( ECHO .AND. COMTXT ) THEN
             IF( SUBCMD(1:1) .NE. ' ' ) THEN
                IF( ABS(INTECHO-2) .EQ. 1 ) THEN
                   WRITE (*,'(''   Macro command:'',A)')
     :             SUBCMD(1:CMDLEN)
                END IF
             END IF
          END IF

          IF( SUBCMD(1:1) .NE. '@' ) THEN
            CALL PARSE_ENV ( PARAMS, CMDLEN, ARG1 )
          END IF
*
*   NULL    No command
*
          IF( SUBCMD(1:1) .EQ. ' ' ) THEN
             CONTINUE
*
*   @ - Alternative COMRD entry
*
          ELSE IF( SUBCMD(1:1) .EQ. '@' ) THEN
             IF( COMTXT ) THEN
                WRITE (*,'(A,A)') '   '//COMSAV(1:NCCAR),
     :          ' command file contains "@" call'
                CLOSE (67)
                GO TO 5000
             END IF
             CMDLEN = CMDLEN - 1
             COMND1 = SUBCMD(2:CMDLEN+1)//' '//PARAMS(1:)
             PARAMS = COMND1
             COMND1 = ' '
             CALL PARSE_ENV ( PARAMS, CMDLEN, ARG1 )
             IF( CMDLEN.GT.0) GO TO 940
             GO TO 920
*
*   ? - Alternative HELP entry
*
          ELSE IF( SUBCMD(1:1) .EQ. '?' ) THEN
             CMDLEN = CMDLEN - 1
             PARAMS(1:CMDLEN) = SUBCMD(2:CMDLEN+1)
             GO TO 1900
*
*   ABSCAL - absolute flux calibration for IUE lores data
*   (specifically, for data extracted using the LBLFIT library)
*
          ELSE IF( SUBCMD .EQ. 'ABSCAL' ) THEN
             CALL ABSCAL(PARAMS,ASIZE1,WAVE,FLUX,NPOINT,MAXBRK,BREAKS,
     :       NBREAK,TITLE,WORV,NPLOTS,DEVTYP,MARK,IPAL,LBOX,
     :       POLTST,HSTTST,MRKTST,ROTST,SUBCHK)
             IF( .NOT.SUBCHK ) THEN
                SUBCHK = .TRUE.
                GO TO 5000
             END IF
*
*   AADD    Add current 'Y' values to stack entry 'n'
*
          ELSE IF( SUBCMD .EQ. 'ADD' ) THEN
             WRITE (*,'(''   ADD command renamed AADD'',
     :       '' (correct call mandatory)'',A)') BLEEP
             GO TO 5000
          ELSE IF( SUBCMD .EQ. 'AADD' ) THEN
             CALL DECODE('AADD',PARAMS,1,1,VARRAY,'Entry ',OK)
             IF( .NOT.OK) GO TO 5000
             NUMENT = NINT(VARRAY(1))
             IF( NUMENT.LE.0 .OR. NUMENT.GT.NONSTK ) THEN
                WRITE (*,'(''   AADD:  stack entry does not exist'')')
                GO TO 5000
             ELSE
                CALL ARITH('+',WAVE,FLUX,NPOINT,BREAK,NBREAK,
     :          XSTACK(POINTR(NUMENT)),YSTACK(POINTR(NUMENT)),
     :          STKNPT(NUMENT),BSTACK(BPOINT(NUMENT)),
     :          BSTNPT(NUMENT),WORK,IWORK,ASIZE1,MAXBRK)
             END IF
*
*   ADIV  Divide top of stack by Y
*
          ELSE IF( SUBCMD .EQ. 'DIV' ) THEN
             WRITE (*,
     :       '(''   DIV renamed ADIV (correct call mandatory)'',A)') BLEEP
             GO TO 5000
          ELSE IF( SUBCMD .EQ. 'ADIV' ) THEN
             CALL DECODE('ADIV',PARAMS,1,1,VARRAY,'Entry ',OK)
             IF( .NOT.OK) GO TO 5000
             NUMENT = NINT(VARRAY(1))
             IF( NUMENT.LE.0 .OR. NUMENT.GT.NONSTK ) THEN
                WRITE (*,'(''   ADIV:  stack entry out of range'')')
                GO TO 5000
             ELSE
                CALL ARITH('/',WAVE,FLUX,NPOINT,BREAK,NBREAK,
     :          XSTACK(POINTR(NUMENT)),YSTACK(POINTR(NUMENT)),
     :          STKNPT(NUMENT),BSTACK(BPOINT(NUMENT)),
     :          BSTNPT(NUMENT),WORK,IWORK,ASIZE1,MAXBRK)
*   Fix, 18/7/90, for fact that datasets might be in velocity space
                IF( (WORV .EQ. 1.0 .AND. WORVST(NUMENT) .NE. WORV)
     :          .OR.(WORVST(NUMENT) .EQ. 1.0 .AND. WORV .NE. 1.0) ) THEN
                   WRITE (*,
     :             '(''   ADIV:  you have divided datasets which'',
     :             '' DIPSO thinks''/''          are in velocity '',
     :             ''and wavelength space - error!'',A)') BLEEP
                   GO TO 5000
                ELSE IF( WORV .NE. WORVST(NUMENT) ) THEN
                   RATIO = WORV/WORVST(NUMENT)
                   DO I = 1, NPOINT
                      FLUX(I) = FLUX(I)/RATIO
                   ENDDO
                END IF
             END IF
*
*   AMULT    Multiply Y by top of stack
*
          ELSE IF( SUBCMD .EQ. 'MULT' ) THEN
             WRITE (*,
     :       '(''   MULT command renamed AMULT '',
     :       ''(correct call mandatory)'',A)') BLEEP
             GO TO 5000
          ELSE IF( SUBCMD .EQ. 'AMULT' ) THEN
             CALL DECODE('AMULT',PARAMS,1,1,VARRAY,'Entry ',OK)
             IF( .NOT.OK) GO TO 5000
             NUMENT = NINT(VARRAY(1))
             IF( NUMENT.LE.0 .OR. NUMENT.GT.NONSTK ) THEN
                WRITE (*,'(''   AMULT:  stack entry does not exist'')')
                GO TO 5000
             ELSE
                CALL ARITH('*',WAVE,FLUX,NPOINT,BREAK,NBREAK,
     :          XSTACK(POINTR(NUMENT)),YSTACK(POINTR(NUMENT)),
     :          STKNPT(NUMENT),BSTACK(BPOINT(NUMENT)),
     :          BSTNPT(NUMENT),WORK,IWORK,ASIZE1,MAXBRK)
             END IF
*
*   ASUB     Subtract Y from top of of stack
*
          ELSE IF( SUBCMD .EQ. 'SUB' ) THEN
             WRITE (*,
     :       '(''   SUB command renamed ASUB '',
     :       ''(correct call mandatory)'',A)') BLEEP
             GO TO 5000
          ELSE IF( SUBCMD .EQ. 'ASUB' ) THEN
             CALL DECODE('ASUB',PARAMS,1,1,VARRAY,'Entry ',OK)
             IF( .NOT.OK) GO TO 5000
             NUMENT = NINT(VARRAY(1))
             IF( NUMENT.LE.0 .OR. NUMENT.GT.NONSTK ) THEN
                WRITE (*,'(''   ASUB:  Stack entry does not exist'')')
                GO TO 5000
             ELSE
                CALL ARITH('-',WAVE,FLUX,NPOINT,BREAK,NBREAK,
     :          XSTACK(POINTR(NUMENT)),YSTACK(POINTR(NUMENT)),
     :          STKNPT(NUMENT),BSTACK(BPOINT(NUMENT)),
     :          BSTNPT(NUMENT),WORK,IWORK,ASIZE1,MAXBRK)
             END IF
*
*   ASWAP    Swap top stack entry and current entry
*
          ELSE IF( SUBCMD .EQ. 'SWAP' .OR. SUBCMD .EQ. 'ASWAP' ) THEN
             IF( SUBCMD .EQ. 'SWAP' ) THEN
                WRITE (*,
     :          '(''   SWAP command renamed ASWAP'',
     :          '' (correct call mandatory)'',A)') BLEEP
                GO TO 5000
             END IF
             CALL DECODE('ASWAP',PARAMS,0,0,VARRAY,' ',OK)
             IF( NONSTK .EQ. 0 ) THEN
                OK = .FALSE.
                WRITE (*,'(''   ASWAP: no stack entry available'')')
             ELSE IF( (STKLST+NPOINT-STKNPT(NONSTK)).GT.STKSZE .OR.
     :       (BSTLST+NBREAK-BSTNPT(NONSTK)).GT.BSTSZE ) THEN
                OK = .FALSE.
                WRITE (*,
     :          '(''   ASWAP:  ignored - stack would overflow'')')
             ELSE
                DO 3130 I = 1, NPOINT
                   WORK(I) = FLUX(I)
 3130           CONTINUE
                J = POINTR(NONSTK)
                DO 3140 I = 1, STKNPT(NONSTK)
                   FLUX(I) = YSTACK(J)
                   J = J + 1
 3140           CONTINUE
                J = POINTR(NONSTK)
                DO 3150 I = 1, NPOINT
                   YSTACK(J) = WORK(I)
                   WORK(I) = WAVE(I)
                   J = J + 1
 3150           CONTINUE
                J = POINTR(NONSTK)
                DO 3160 I = 1, STKNPT(NONSTK)
                   WAVE(I) = XSTACK(J)
                   J = J + 1
 3160           CONTINUE
                J = POINTR(NONSTK)
                DO 3170 I = 1, NPOINT
                   XSTACK(J) = WORK(I)
                   J = J + 1
 3170           CONTINUE
                STKLST = STKLST + NPOINT - STKNPT(NONSTK)
                J = NPOINT
                NPOINT = STKNPT(NONSTK)
                STKNPT(NONSTK) = J
                DO 3180 I = 1, NBREAK
                   IWORK(I) = BREAK(I)
 3180           CONTINUE
                J = BPOINT(NONSTK)
                DO 3190 I = 1, BSTNPT(NONSTK)
                   BREAK(I) = BSTACK(J)
                   J = J + 1
 3190           CONTINUE
                J = BPOINT(NONSTK)
                DO 3200 I = 1, BSTNPT(NONSTK)
                   BSTACK(J) = IWORK(I)
                   J = J + 1
 3200           CONTINUE
                BSTLST = BSTLST + NBREAK - BSTNPT(NONSTK)
                J = BSTNPT(NONSTK)
                BSTNPT(NONSTK) = NBREAK
                NBREAK = J
                PARAMS = TITLE
                TITLE = STITLE(NONSTK)
                STITLE(NONSTK) = PARAMS
                TEMP = WORV
                WORV = WORVST(NONSTK)
                WORVST(NONSTK) = TEMP
             END IF
*
*   ALASCOLS  Sets x,y columns for ALAS inputs
*
          ELSE IF( SUBCMD .EQ. 'ALASCOLS' ) THEN
             CALL DECODE('ALASCOLS',PARAMS,2,2,VARRAY,
     :       'X_column Y_column ',OK)
             IF( .NOT.OK) GO TO 5000
             ALASXC = NINT(VARRAY(1))
             ALASYC = NINT(VARRAY(2))
*
*   ALASLINS  Sets lines read in from ALAS files
*
          ELSE IF( SUBCMD .EQ. 'ALASLINS' ) THEN
             CALL DECODE('ALASLINS',PARAMS,2,2,VARRAY,'Line_1 Line_2 ',
     :       OK)
             IF( .NOT.OK) GO TO 5000
             ALASL1 = NINT(VARRAY(1))
             ALASL2 = NINT(VARRAY(2))
             IF( (ALASL2.LT.ALASL1) .AND. (ALASL2 .NE. 0) ) THEN
                WRITE (*,'(''   ALASLINS:  (Line 2)<(Line 1) - '',
     :          ''no lines will be read'',A)') BLEEP
             END IF
*
*   ALASCHK  Prints current ALAS line data
*
          ELSE IF( SUBCMD .EQ. 'ALASCHK' ) THEN
             CALL DECODE('ALASCHK',PARAMS,0,0,VARRAY,' ',OK)

*          Build and report message.
             CALL ITOCHR (ALASXC, STRING1, NCHAR1, ICSTAT)
             CALL ITOCHR (ALASYC, STRING2, NCHAR2, ICSTAT)
             CALL ITOCHR (ALASL1, STRING3, NCHAR3, ICSTAT)

             BIGSTR = '   ALASSCHK:  X column = '//STRING1(:NCHAR1)
             WRITE (*,*)BIGSTR
             BIGSTR = ' '

             BIGSTR = '              Y column = '//STRING2(:NCHAR2)
             WRITE (*,*)BIGSTR
             BIGSTR = ' '

             BIGSTR = '              Line 1 = '//STRING3(:NCHAR3)
             WRITE (*,*)BIGSTR
             BIGSTR = ' '

             IF( ALASL2.LE.0 ) THEN
                WRITE (*,'(''               Line 2 = <end of file>'')')
             ELSE
                CALL ITOCHR (ALASL2, STRING1, NCHAR1, ICSTAT)
                BIGSTR = '              Line 2 = '//STRING1(:NCHAR1)
                WRITE (*,*)BIGSTR
                BIGSTR = ' '
             END IF

*
*   ALASRD   Reads an ALAS-type file
*
          ELSE IF( SUBCMD .EQ. 'ALASRD' ) THEN
             IF( PARAMS .NE. ' ') GO TO 460

  440        CONTINUE

             CALL RDSTR( SUBCMD, 'File name', ' ', PARAMS, STATUS )
             IF( STATUS .NE. SAI__OK ) THEN
                OK = .FALSE.
                GO TO 5000
             END IF

  460        CONTINUE
             CALL SSTRIP(PARAMS)
             IF( PARAMS(1:2) .EQ. '!!' ) THEN
                WRITE (*,'(''   ALASRD:  user-induced abort'')')
                GO TO 5000
             END IF
             CLOSE (51)
             OPEN (UNIT=51,STATUS='OLD',
     :       FILE=PARAMS(1:INDEX(PARAMS,' ')),IOSTAT=IX)
             IF( IX .NE. 0 ) THEN
                WRITE (*,'(''   ALASRD:  error opening '',A)')
     :          PARAMS(1:SLEN(PARAMS))
                CLOSE (51)
                GO TO 5000
             END IF
             NPOINT = 0
             WORV = 1.0
             NLP = 0
             N1 = MIN(ALASXC,ALASYC)
             N2 = MAX(ALASXC,ALASYC)
             DO 480 I = 1, ALASL1 - 1
                READ (51,'(A)',END=520) ALASTXT(1:1)
                NLP = NLP + 1
  480        CONTINUE
             NREAD = ALASL2 - ALASL1 + 1
             IF( ALASL2.LE.0) NREAD = 40000000
             DO 500 I = 1, NREAD
                READ (51,'(A)',END=540) ALASTXT
                CALL SSTRIP(ALASTXT)
                IF( ALASTXT .NE. ' ' ) THEN
                   DO 485 II = 1, N1 - 1
                      J = 1
                      DO 482 K = 1, SLEN(ALASTXT)
                         IF( ALASTXT(K:K) .EQ. ' ') GO TO 484
                         J = J + 1
  482                 CONTINUE
                      WRITE (*,
     :                '(''   ALASRD:  error reading from file'',A)')
     :                BLEEP
                      NPOINT = 0
                      CLOSE (51)
                      GO TO 5000
  484                 CONTINUE
                      ALASTXT(1:) = ALASTXT(J:)
                      CALL SSTRIP(ALASTXT)
                      IF( ALASTXT(1:1) .EQ. ' ' ) THEN
                         WRITE (*,'(''   ALASRD:  columns specified'',

     :                   '' by ALASCOLS do not exist in the file'',A)')
     :                   BLEEP
                         CLOSE (51)
                         NPOINT = 0
                         GO TO 5000
                      END IF
  485              CONTINUE
                   READ (ALASTXT(1:INDEX(ALASTXT,' ')),*,IOSTAT=IHX)
     :             WAVE(I)
                   IF( IHX .NE. 0 ) THEN
                      WRITE (*,
     :                '(''   ALASRD:  error reading from file'')')
                      CLOSE (51)
                      NPOINT = 0
                      GO TO 5000
                   END IF
                   IF( N2 .EQ. N1 ) THEN
                      FLUX(I) = WAVE(I)
                   ELSE
                      DO 490 II = N1, N2 - 1
                         J = 1
                         DO 486 K = 1, SLEN(ALASTXT)
                            IF( ALASTXT(K:K) .EQ. ' ') GO TO 488
                            J = J + 1
  486                    CONTINUE
                         WRITE (*,
     :                   '(''   ALASRD:  error reading from file'')')
                         CLOSE (51)
                         NPOINT = 0
                         GO TO 5000
  488                    CONTINUE
                         ALASTXT(1:) = ALASTXT(J:)
                         CALL SSTRIP(ALASTXT)
                         IF( ALASTXT(1:1) .EQ. ' ' ) THEN
                            WRITE (*,
     :                      '(''   ALASRD:  insufficient columns'')')
                            CLOSE (51)
                            GO TO 5000
                         END IF
  490                 CONTINUE
                      READ (ALASTXT(1:INDEX(ALASTXT,' ')),*,IOSTAT=IHX)
     :                FLUX(I)
                      IF( IHX .NE. 0 ) THEN
                         WRITE (*,
     :                   '(''   ALASRD:  error reading from file'')')
                         NPOINT = 0
                         CLOSE (51)
                         GO TO 5000
                      END IF
                   END IF
                   NPOINT = NPOINT + 1
                END IF
  500        CONTINUE
             GO TO 560
  520        CONTINUE

*          Build and report message.
             CALL ITOCHR (NLP, STRING1, NCHAR1, ICSTAT)
             CALL ITOCHR (ALASL1, STRING2, NCHAR2, ICSTAT)

             BIGSTR= ' ALASRD:  only '//STRING1(:NCHAR1)//
     :               ' lines in file (ALASLINS Line 1 = '//
     :                 STRING2(:NCHAR2)//' )'

             WRITE (*,*)BIGSTR
             BIGSTR = ' '

             CLOSE (51)
             NPOINT = 0
             GO TO 5000
  540        CONTINUE
             IF( ALASL2.GE.1 ) THEN

*             Build and report message.
                CALL ITOCHR (NPOINT, STRING1, NCHAR1, ICSTAT)

                BIGSTR= ' ALASRD:  only '//STRING1(:NCHAR1)//
     :                  ' lines read before end-of-file encountered'
                WRITE (*,*)BIGSTR,BLEEP
                BIGSTR = ' '

             END IF
  560        CONTINUE
             IF( ALASXC.GT.ALASYC ) THEN
                IF( DEBUG ) THEN
                   WRITE (*,'(''   ALASXC > ALASYC'')')
                   WRITE (*,'(''   NPOINT:'',I5)') NPOINT
                END IF
                DO 570 I = 1, NPOINT
                   TEMP = WAVE(I)
                   WAVE(I) = FLUX(I)
                   FLUX(I) = TEMP
  570           CONTINUE
             END IF

             WORV = 1.0
             NBREAK = 1
             BREAK(1) = NPOINT
             TITLE(1:79) = PARAMS(1:INDEX(PARAMS,' '))
             PARAMS(1:) = PARAMS(INDEX(PARAMS,' '):)
             VARRAY(1) = 0.0
             CALL DECODE('ALASRD',PARAMS,0,1,VARRAY,' ',OK)
             IF( .NOT.OK ) THEN
                NPOINT = 0
                GO TO 5000
             END IF
             BRKVAL = VARRAY(1)
             CLOSE (51)
             CALL SRTBRK(ASIZE1,WAVE,FLUX,NPOINT,MAXBRK,BREAKS,NBREAK,
     :       BRKVAL,OK)
*
*   ALASWR  Writes ALAS file
*
          ELSE IF( SUBCMD .EQ. 'ALASWR' ) THEN
             DO 580 I = 1, 80
                IF( PARAMS(I:I) .NE. ' ') GO TO 620
  580        CONTINUE
  600        CONTINUE

             CALL RDSTR( SUBCMD, 'File name', ' ', PARAMS, STATUS )
             IF( STATUS .NE. SAI__OK ) THEN
                OK = .FALSE.
                GO TO 5000
             END IF

  620        CONTINUE
             CALL SSTRIP(PARAMS)
             CLOSE (55)
             OPEN (UNIT=55,FILE=PARAMS(1:INDEX(PARAMS,' ')),
     :       STATUS='NEW',IOSTAT=IX)
             IF( IX .NE. 0 ) THEN
                WRITE(*,*) '  ALASWR:  error opening output file'
                CLOSE (55)
                GO TO 5000
             ELSE
                PARAMS(1:) = PARAMS(INDEX(PARAMS,' '):)
                CALL SSTRIP(PARAMS)
                IF( PARAMS .EQ. ' ' ) THEN
                   BRKOUT = 0.0
                ELSE
                   CALL DECODE('ALASWR',PARAMS,1,1,BRKOUT,
     :             'Break_value ',OK)
                   IF( .NOT.OK ) THEN
                      CLOSE (55)
                      GO TO 5000
                   END IF
                END IF
                IHB = 1
                DO 630 IH = 1, NPOINT
!                  WRITE (55,'(F15.4,1PE15.5)',IOSTAT=IIX)
                   WRITE (55,'(1P2E15.7)',IOSTAT=IIX)
     :             WAVE(IH), FLUX(IH)
                   IF( IIX .NE. 0 ) THEN
                      WRITE (*,'(''   ALASWR:  error during write'')')
                      GO TO 640
                   END IF
                   IF( IH .EQ. BREAKS(IHB) ) THEN
                      IF( IH .NE. NPOINT ) THEN
                         XTMP = (WAVE(IH)+WAVE(IH+1))*0.5
                         WRITE (55,'(1PE15.7,E15.1)') XTMP, BRKOUT
                         IHB = IHB + 1
                      END IF
                   END IF
  630           CONTINUE
             END IF
  640        CONTINUE
             CLOSE (55)
*
*   ANGLE   Set orientation of text output with PWRITE
*
          ELSE IF( SUBCMD .EQ. 'ANGLE' ) THEN
             CALL DECODE('ANGLE',PARAMS,1,1,VARRAY(1),
     :       'Text_orientation ',OK)
             IF( .NOT.OK) GO TO 5000
             NDEGREES = NINT(VARRAY(1))
*
*   ATFIT  Fit Kurucz models to data
*
          ELSE IF( SUBCMD .EQ. 'ATFIT' ) THEN
             VARRAY(4) = 0.0
             CALL DECODE  (SUBCMD,PARAMS,3,4,VARRAY,
     :       'Teff Theta(mas) E(B-V) ',OK)
             IF( .NOT.OK) GO TO 5000
             IF( VARRAY(4) .NE. 0.0) ATFCAL = .FALSE.
             VARS(1) = VARRAY(1)
             VARS(2) = VARRAY(2)
             VARS(3) = VARRAY(3)
             CALL ATFIT
     :       (WAVE, FLUX, MAXBRK, BREAK, NBREAK,
     :       NPOINT, WORV, OK, VARS)
             IF( .NOT.OK) GO TO 5000

*  ATLASRD: Read Kurucz model
          ELSE IF( SUBCMD .EQ. 'ATLASRD' ) THEN
             IF( USENDF ) THEN
                CALL ATLASRD( 'ATLASRD', PREFIX3( : P3LEN ), PARAMS,
     :                        WORV, TITLE, STATUS )
             ELSE

*         ----------------------------------------------------------------
             VARRAY(3) = 0.0
             CALL DECODE('ATLASRD',PARAMS,2,3,VARRAY,'Teff LogG ',OK)
             IF( .NOT.OK ) THEN
                WRITE (*,'(''   ATLASRD:  DECODE error'')')
             ELSE
                WRITE (IHHEAD(1:6),'(I6)') NINT(VARRAY(1))
                ii = 1
                DO WHILE ( IHHEAD(ii:ii) .EQ. ' ' )
                  ii = ii + 1
                END DO
                LOGG = NINT(100.0*VARRAY(2))
                WRITE (IHHEAD(11:13),'(I3.3)') LOGG
                IF( VARRAY(3) .EQ. 0.0 ) THEN
                   CLOSE (7)
                   IF( p3len .GT. 0 ) THEN
                   OPEN (UNIT=7,STATUS='OLD',
     :             FORM='UNFORMATTED',IOSTAT=IHX,
     :             FILE=prefix3(1:p3len)//'f'//IHHEAD(ii:6)
     :             //'.'//IHHEAD(11:13))
                   ELSE
                   OPEN (UNIT=7,STATUS='OLD',
     :             FORM='UNFORMATTED',IOSTAT=IHX,
     :             FILE='f'//IHHEAD(ii:6)
     :             //'.'//IHHEAD(11:13))
                   END IF
                ELSE

                   CLOSE (7)
                   IF( p3len .GT. 0 ) THEN
                   OPEN (UNIT=7,STATUS='OLD',
     :             FORM='UNFORMATTED',IOSTAT=IHX,
     :             FILE=prefix3(1:p3len)//'klo'//IHHEAD(ii:6)
     :             //'.'//IHHEAD(11:13))
                   ELSE
                   OPEN (UNIT=7,STATUS='OLD',
     :             FORM='UNFORMATTED',IOSTAT=IHX,
     :             FILE='klo'//IHHEAD(ii:6)
     :             //'.'//IHHEAD(11:13))
                   END IF
                END IF
                IF( IHX .NE. 0 ) THEN
                   WRITE (*,'(''   ATLASRD:  unable to open file'')')
                   WRITE (*,'(''   (Typing USENDF and then trying '//
     :                          'again may work)'')')

                   GO TO 5000
                ELSE
                   READ (7,IOSTAT=IHX) KREAD
                   DO 645 ILOOP = 10, 30
                      TITLE(ILOOP:ILOOP) = KREAD(ILOOP-9)
  645              CONTINUE
                   READ (7,IOSTAT=IHX) KREAD
                   DO 650 ILOOP = 31, 79
                      TITLE(ILOOP:ILOOP) = KREAD(ILOOP-30)
  650              CONTINUE
                   TITLE(1:9) = ' Kurucz  '
                   IF( VARRAY(3) .NE. 0.0) TITLE(1:9) = 'Low metal'
                   WRITE (*,'(3X,A79)') TITLE(1:79)
                   READ (7,IOSTAT=IHX) NPOINT
                   NPOINT = NPOINT - 1
                   WRITE (*,'(''   ATLASRD:  number of points ='',I6)')
     :             NPOINT
                   IF( NPOINT.GT.ASIZE1 ) THEN
                      WRITE (*,'(''   only'',I6,'' read in'')') ASIZE1
                      NPOINT = ASIZE1
                   END IF
                   READ (7,ERR=660) (WAVE(I),FLUX(I),I=1,NPOINT)
                   DO 655 I = 1, NPOINT
                      FLUX(I) = FLUX(I)*3.141593
  655              CONTINUE
                   NBREAK = 1
                   BREAK(1) = NPOINT
                END IF
                WORV = 1.0
                CLOSE (7)
                GO TO 5800
  660           CONTINUE
                WRITE (*,'(''   ATLASRD:  error on read'')')
                GO TO 5000
             END IF
*         ----------------------------------------------------------------

             END IF

*  ATLIST:  List available Kurucz models
          ELSE IF( SUBCMD .EQ. 'ATLIST' ) THEN
             CALL DECODE('ATLIST',PARAMS,0,0,VARRAY,' ',OK)
             CLOSE (77)
             OPEN (UNIT=77,
     :       FILE=prefix(1:plen)//'ATLIST.DAT',
     :       STATUS='OLD',IOSTAT=IHX)
             IF( IHX .NE. 0 ) THEN
                PRINT *, '  ATLIST:  can''t open data file'
                CLOSE (77)
             ELSE
                DO 670 IH = 1, 1000
                   READ (77,'(A79)',END=680) IHHLP(2:80)
                   WRITE (*,'(A80)') IHHLP(1:80)
  670           CONTINUE
  680           CONTINUE
                CLOSE (77)
             END IF
*
*   ATNORM  -  Normalise model atmosphere to data
*
          ELSE IF( SUBCMD .EQ. 'ATNORM' ) THEN
             VARRAY(1) = 1.0
             CALL DECODE('ATNORM',PARAMS,0,1,VARRAY,' ',OK)
             IF( .NOT.OK) GO TO 5000
             IF( VARRAY(1) .NE. 1.0) VARRAY(1) = 0.0
             IF( .NOT.CURSOR .OR. NPLOTS .EQ. 0 ) THEN
                WRITE (*,'(''   ATNORM:  no plot available'')')
                GO TO 5000
             END IF
             FIRSTCURSOR = .TRUE.
             CALL SGSCURSE(I,X,Y,FIRSTCURSOR)
             YATM = .FALSE.
             IF( Y.LE.0.0 ) THEN
                YATM = .TRUE.
                Y = 10.0**Y
             END IF
             IF( X.GE.WAVE(1) ) THEN
                DO 690 I = 2, NPOINT
                   IF( WAVE(I).GE.X ) THEN
                      W1 = WAVE(I-1)
                      W2 = WAVE(I)
                      WT1 = (W2-X)/(W2-W1)
                      WT2 = 1.0 - WT1
                      FX = FLUX(I-1)*WT1 + FLUX(I)*WT2
                      YNORM = LOG10(MAX(Y,1e-30)) -
     :                        LOG10(MAX(FX/3.141593,1e-30))
                      DO 682 J = 1, NPOINT
                         IF( FLUX(J).GT.0.0 ) THEN
                            TEMP = LOG10(MAX(FLUX(J),1e-30)) + YNORM
                            IF( ABS(TEMP).LT.35.0 ) THEN
                               FLUX(J) = 10.0**TEMP
                               FLUX(J) = FLUX(J)/3.141593
                            ELSE
                               FLUX(J) = 0.0
                            END IF
                         ELSE
                            FLUX(J) = 0.0
                         END IF
  682                 CONTINUE
*
                      YNORM = YNORM*0.5
                      YNORM = YNORM + 10.39817267
                      IF( ABS(YNORM).LT.35.0 ) THEN
                         YNORM = 10.0**YNORM
                         WRITE (*,'(''   ATNORM:  R(Ro)/D(kpc) ='',
     :                   1PE15.5/
     :                   ''            Updating current arrys'')')
     :                   YNORM
                      END IF
*
                      K = 0
                      IBRK = 1
                      JBRK = 0
                      ICHK = 1
                      ITMP = 0
                      DO 684 J = 1, NPOINT
                         IF( FLUX(J) .NE. 0.0 ) THEN
                            K = K + 1
                            FLUX(K) = FLUX(J)
                            WAVE(K) = WAVE(J)
                            IF( J .EQ. BREAK(IBRK) ) THEN
                               IBRK = IBRK + 1
                               JBRK = JBRK + 1
                               IWORK(JBRK) = K
                            END IF
                            ICHK = 1
                         ELSE
                            ITMP = ITMP + 1
                            IF( ICHK .EQ. 0 ) THEN
                               IF( J .EQ. BREAK(IBRK)) IBRK = IBRK + 1
                            ELSE
                               ICHK = 0
                               JBRK = JBRK + 1
                               IWORK(JBRK) = K
                               IF( J .EQ. BREAK(IBRK)) IBRK = IBRK + 1
                            END IF
                         END IF
  684                 CONTINUE
                      IF( ITMP .NE. 0 ) THEN
                         WRITE (*,'(3X,I6,'' points lost through '',
     :                   ''rounding errors'')') ITMP
                      END IF
*
                      NBREAK = JBRK
                      NPOINT = K
                      DO 686 J = 1, NBREAK
                         BREAK(J) = IWORK(J)
  686                 CONTINUE
                      NDO = NINT(VARRAY(1))
                      VARRAY(1) = 0.0
                      IF( YATM ) THEN
                         DO 688 IATM = 1, NPOINT
                            FLUX(IATM) = LOG10(MAX(FLUX(IATM),1e-30))
  688                    CONTINUE
                      END IF
                      CALL NOBOX
                      ATTST = .TRUE.
                      IF( DEBUG ) THEN
                         WRITE (*,'(''   DEBUG/ATNORM:  NDO is'',I5)')
     :                   NDO
                      END IF
                      GO TO 2280
                   END IF
  690           CONTINUE
             END IF
*
             WRITE (*,'(''   ATNORM:  '',
     :       ''cursor out of X range of model'',A)') BLEEP
*
*   BACHRD  Read a BACH file
*
          ELSE IF( SUBCMD .EQ. 'BACHRD' ) THEN
             DO 700 I = 1, 80
                IF( PARAMS(I:I) .NE. ' ') GO TO 740
  700        CONTINUE
  720        CONTINUE

             CALL RDSTR( SUBCMD, 'File name', ' ', PARAMS, STATUS )
             IF( STATUS .NE. SAI__OK ) THEN
                OK = .FALSE.
                GO TO 5000
             END IF

  740        CONTINUE
             CLOSE(7)
             OPEN (UNIT=7,FILE=PARAMS(1:80),STATUS='OLD',
     :       IOSTAT=IOS)
             IF( IOS .NE. 0 ) THEN
                PRINT *, '  BACHRD:  error opening input file'
                GO TO 5000
             ELSE
                READ (7,'(A)',IOSTAT=IOS1) TITLE
                READ (7,*,IOSTAT=IOS2) WAVE0, DUMMY, DUMMY, FVALUE
                READ (7,*,IOSTAT=IOS3) NPOINT
                IF( NPOINT.GT.ASIZE1 ) THEN
                   PRINT *, '  BACHRD:  too many points.   First',
     :             ASIZE1, ' read in'
                   NPOINT = ASIZE1
                END IF
                READ (7,*,IOSTAT=IOS4) (WAVE(I),FLUX(I),I=1,NPOINT)
                IF( IOS1 .NE. 0 .OR. IOS2 .NE. 0 .OR. IOS3 .NE. 0 .OR.
     :          IOS4 .NE. 0 ) THEN
                   PRINT *, '  BACHRD:  error reading from file'
                   GO TO 5000
                ELSE
                   WORV = 1.0
                   NBREAK = 1
                   BREAK(1) = NPOINT
                   WRITE (*,'(''   Title:     '',A50)') TITLE(1:50)
                END IF
                CLOSE (7)
             END IF
*
*   BACHWR  Write BACH file
*
          ELSE IF( SUBCMD .EQ. 'BACHWR' ) THEN
             jj = DPARSE(PARAMS,LINNAM,' ',I)
             CALL DTOUPP(LINNAM)
             CALL SSTRIP(PARAMS)
             VARRAY(1) = VLOW
             VARRAY(2) = VHIGH
             CALL XDCODE('BACHWR',PARAMS,0,2,VARRAY,' ',OK)
             IF( .NOT.OK) GO TO 5000
             VLLOW = VARRAY(1)
             VHHIGH = VARRAY(2)
             IF( .NOT.(OK)) GO TO 5000
             CLOSE (99)
             OPEN (UNIT=99,FILE='atomic.dat',STATUS='OLD',
     :       IOSTAT=IOS1)
             IF( IOS1 .NE. 0 ) THEN
                CLOSE (99)
                WRITE (*,'(''   BACHWR:  unable to access '',
     :          ''data file'',A)') BLEEP
                GO TO 5000
             END IF
             READ (99,'(A)',IOSTAT=IOS) READIN
  760        CONTINUE
             IF( READIN .NE. LINNAM .AND. IOS .EQ. 0 ) THEN
                READ (99,'(////A)',IOSTAT=IOS) READIN
                GO TO 760
             ELSE IF( IOS .EQ. 0 ) THEN
                IF( IOS .EQ. 0 ) THEN
                   READ (99,*) WAVE0, GU, GL, FVALUE, BINST
                   READ (99,'(A)') READIN
                   CALL FINDIT(WAVE,NPOINT,(1.0+VLLOW/C)*WAVE0,I1,1)
                   CALL FINDIT(WAVE,NPOINT,(1.0+VHHIGH/C)*WAVE0,I2,0)
                   CLOSE (98)
                   OPEN (UNIT=98,FILE=LINNAM//'.DAT',
     :             STATUS='NEW')
                   I2 = MAX(I2,I1)
                   WRITE (98,'(A/G20.7,2I5,G20.7/I10)',IOSTAT=IOS1)
     :             TITLE(1:30)//READIN, WAVE0, GU, GL, FVALUE,
     :             I2 - I1
                   WRITE (98,'(G15.7,G13.5)',IOSTAT=IOS2)
     :             (WAVE(I),FLUX(I),I=I1+1,I2)
                   IF( EW .NE. 0.0 ) THEN

                      BIGSTR = ' '
                      WRITE( BIGSTR, '(''EW ='',F5.3,''; O.K.'')') EW
                      CALL GET0L( ' ', 1, .FALSE., SUBCMD, BIGSTR,
     :                            .TRUE., VAL_OK, STATUS )
                      BIGSTR = ' '

                      IF( STATUS .NE. SAI__OK ) THEN
                         OK = .FALSE.
                         GO TO 5000
                      END IF

                   ELSE
                      VAL_OK = .FALSE.
                   END IF

                   IF( .NOT. VAL_OK ) THEN
                      EW = 0.0
                      SIGEW = 0.0
                      WRITE (98,'(A/G12.4//A)',IOSTAT=IOS3) ',,,',
     :                BINST, ',,,'
                   ELSE
                      EWPSIG = EW + SIGEW/1000.0
                      EWMSIG = EW - SIGEW/1000.0
                      EWMSIG = MAX(0.0,EWMSIG)
                      WRITE (98,'(1P3E13.5/E13.5//'' ,,,'')',
     :                IOSTAT=IOS3) EW, EWPSIG, EWMSIG, BINST
                   END IF
                   CLOSE (98)
                ELSE
                   CLOSE (99)
                END IF
                IF( IOS .NE. 0 .OR. IOS1 .NE. 0 .OR. IOS2 .NE. 0 .OR.
     :          IOS3 .NE. 0 ) THEN
                   OK = .FALSE.
                   WRITE (*,'(''   BACHWR:  error on data transfer'')')
                   GO TO 5000
                END IF
                CLOSE (99)
             ELSE
                CLOSE (99)
                WRITE (*,'(''   BACHWR:  line name not recognised'')')
                GO TO 5000
             END IF
*
*   BBODY - calculates black-body curve
*
          ELSE IF( SUBCMD .EQ. 'BBODY' ) THEN
             IF( NPOINT.LE.0 ) THEN
                WRITE (*,
     :          '(''   BBODY:  no GRID available in current arrays'')')
                GO TO 5000
             END IF
             CALL DECODE('BBODY',PARAMS,1,1,TBB,'temperature ',OK)
             IF( .NOT.(OK)) GO TO 5000
             CALL BBODY(WAVE,FLUX,ASIZE1,NPOINT,TITLE,TBB,OK)
             IF( .NOT.OK ) THEN
                OK = .TRUE.
                GO TO 5000
             END IF
*
*   BEEP - Turns on 'BEEP'
*
          ELSE IF( SUBCMD .EQ. 'BEEP' ) THEN
             CALL DECODE('BEEP',PARAMS,0,0,TEMP,' ',OK)
             IF( .NOT.(OK)) GO TO 5000
             WRITE (*,'(''   BEEP:  OK!'',A)') BLEEP
             BLEEP = CHAR(7)
             BEEP = .TRUE.
*
*   BIN  -  Bin data into boxes
*
          ELSE IF( SUBCMD .EQ. 'BIN' ) THEN
             CALL DECODE('BIN',PARAMS,2,2,VARRAY,'X1 DX ',OK)
             IF( .NOT.OK) GO TO 5000
*
             K = 0
             L = 0
             JKEEP = 2
             X1 = VARRAY(1)
             DX = VARRAY(2)
             X2 = X1
             X1 = X1 - DX
*
             ITAG = 1
  780        CONTINUE
             NSUM = 0
             SUMF = 0.0
             SUMW = 0.0
             J = JKEEP - 1
             X1 = X1 + DX
             X2 = X2 + DX
*
             DO 800 I = J, NPOINT
                IF( WAVE(I).GE.X1 ) THEN
                   IF( WAVE(I).LT.X2 ) THEN
                      NSUM = NSUM + 1
                      SUMF = SUMF + FLUX(I)
                      SUMW = SUMW + WAVE(I)
                   ELSE
                      IF( NSUM .NE. 0 ) THEN
                         K = K + 1
                         WAVE(K) = SUMW/REAL(NSUM)
                         FLUX(K) = SUMF/REAL(NSUM)
                         ITAG = 1
                      ELSE IF( K .NE. 0 ) THEN
                         IF( ITAG .NE. 0 ) THEN
                            ITAG = 0
                            L = L + 1
                            BREAK(L) = K
                         END IF
                      END IF
                      JKEEP = I
                      GO TO 780
                   END IF
                END IF
  800        CONTINUE
*
             IF( NSUM .NE. 0 ) THEN
                K = K + 1
                WAVE(K) = SUMW/REAL(NSUM)
                FLUX(K) = SUMF/REAL(NSUM)
             ELSE
                L = L + 1
                BREAK(L) = K
             END IF
             IF( K.GE.2 ) THEN
                IF( WAVE(K) .EQ. WAVE(K-1)) K = K - 1
             END IF
             L = L + 1
             BREAK(L) = K
             DO 820 I = 1, 2
                IF( L.GE.2 ) THEN
                   IF( BREAK(L) .EQ. BREAK(L-1)) L = L - 1
                END IF
  820        CONTINUE
*
             NBREAK = L
             NPOINT = K
*
*   BOX     Automatic CLEAR before plotting to be switched on
*
          ELSE IF( SUBCMD .EQ. 'BOX' ) THEN
             CALL DECODE('BOX',PARAMS,0,0,VARRAY,' ',OK)
             CALL JOBOX
             LBOX = .TRUE.
             ERASEBOX = .TRUE.
*
*   CAKWR   Writes files for CAK models - NOT FOR PUBLIC
*
          ELSE IF( SUBCMD .EQ. 'CAKWR' ) THEN
             ISKIP = NINT(REAL(NPOINT)/110.0 + 1.0)
             CLOSE(38)
             OPEN (UNIT=38,FILE='DEL.LIS',STATUS='NEW',
     :       IOSTAT=IHX)
             IF( IHX .NE. 0 ) THEN
                WRITE (*,'(''   CAKWR:  could not open file'',A)')
     :          BLEEP
                CLOSE(38)
                GO TO 5000
             END IF
             WRITE (38,'(9X,1P2E11.4)')
     :       (WAVE(I), FLUX(I), I = 1, NPOINT-1, ISKIP)
             WRITE (38,'(9X,1P2E11.4)')
     :       WAVE(NPOINT), FLUX(NPOINT)
             CLOSE(38)
*
*   CDRAW   Draw a continuum using cursor
*
          ELSE IF( SUBCMD .EQ. 'CDRAW' ) THEN
             IF( USENDF ) THEN
                CALL CDRAW(PARAMS,WAVE,FLUX,ASIZE1,NPOINT,WORK,WORKSZ,
     :                     POINTR,BREAKS,MAXBRK,NBREAK,TITLE,WORV,
     :                     STKSZE,XSTACK,MAXSTK,STKNPT,NPLOTS,NONSTK,
     :                     DEVTYP,MARK,IPAL,LBOX,POLTST,HSTTST,MRKTST,
     :                     ROTST,SUBCHK,COLOUR,CURSOR)

             ELSE
                CALL OCDRAW(PARAMS,WAVE,FLUX,ASIZE1,NPOINT,WORK,WORKSZ,
     :                      POINTR,BREAKS,MAXBRK,NBREAK,TITLE,WORV,
     :                      STKSZE,XSTACK,MAXSTK,STKNPT,NPLOTS,NONSTK,
     :                      DEVTYP,MARK,IPAL,LBOX,POLTST,HSTTST,MRKTST,
     :                      ROTST,SUBCHK,COLOUR,CURSOR)

             END IF

             IF( .NOT.SUBCHK ) THEN
                SUBCHK = .TRUE.
                GO TO 5000
             END IF

*  CHEATSM:  Cheat smoothing on noisy numerical data - NOT FOR THE PUBLIC!
          ELSE IF( SUBCMD .EQ. 'CHEATSM' ) THEN
             VARRAY(1) = -1.0
             VARRAY(2) = 0.1
             CALL DECODE('CHEATSM',PARAMS,0,2,VARRAY,'X1 DY ',OK)
             IF( OK ) THEN
                CALL CHEATSM(VARRAY,WAVE,FLUX,NPOINT,OK)
             END IF
*
*   CLEAR   CLEAR the text surface
*
          ELSE IF( SUBCMD .EQ. 'CLEAR' ) THEN
             CALL DECODE('CLEAR',PARAMS,0,0,VARRAY,' ',OK)
             DO 880 I = 1, 40
                PRINT *, ' '
  880        CONTINUE
*
*   CLRBRK  -  clears break array
*
          ELSE IF( SUBCMD .EQ. 'CLRBRK' ) THEN
             CALL DECODE('CLRBRK',PARAMS,0,0,VARRAY,' ',OK)
             NBREAK = 1
             BREAKS(1) = NPOINT

* CLR: Enable/disable command line recall
          ELSE IF( SUBCMD .EQ. 'CLR' ) THEN
             CALL GET0L( PARAMS, 1, .TRUE., SUBCMD, 'Do you want to '//
     :                   'use Command Line Recall', .TRUE., CLR,
     :                   STATUS )

             IF( CLR ) THEN
                CALL MSGOUT( SUBCMD, 'Using command line recall',
     :                       .FALSE., STATUS )
             ELSE
                CALL MSGOUT( SUBCMD, 'Disabling command line recall',
     :                       .FALSE., STATUS )

             END IF

*
*   COMRD - read commands from a file
*
          ELSE IF( SUBCMD .EQ. 'COMRD' ) THEN
             IF( COMTXT ) THEN
                WRITE (*,'(A,A,A)') '   '//COMSAV(1:NCCAR),
     :          ' macro contains an (illegal) call to another'
     :          , ' command file'
                CLOSE (67)
                GO TO 5000
             END IF
             IF( PARAMS .NE. ' ') GO TO 940
  920        CONTINUE

             PARAMS = ' '
             CALL RDSTR( SUBCMD, 'Macro filename', ' ', PARAMS, STATUS )
             IF( STATUS .NE. SAI__OK ) THEN
                OK = .FALSE.
                GO TO 5000
             END IF

             IF( PARAMS .EQ. ' ') GO TO 920
             CALL SSTRIP(PARAMS)

*   BLOCK FOR "PARAMETERS"
  940        CONTINUE
             DO 960 I = 1, 9
                CMDPAR(I) = ' '
                CMDUSE(I) = .TRUE.
  960        CONTINUE
             CALL SSTRIP(PARAMS)
             I = INDEX(PARAMS,' ')
             ALASTXT = PARAMS(I:)
             PARAMS(I:) = ' '
             NCMDPAR = 0
             DO 980 I = 1, 9
                IF( ALASTXT .NE. ' ' ) THEN
                   CALL SSTRIP(ALASTXT)
                   IF( ALASTXT(1:1) .EQ. '"' ) THEN
                      K = 2
  962                 CONTINUE
                      J = INDEX(ALASTXT(K:),'"')
                      IF( J .EQ. 0 ) THEN
                         CONTINUE
                      ELSE IF( ALASTXT(J+K:J+K) .EQ. '"' ) THEN
                         K = K + J + 1
                         GO TO 962
                      ELSE
                         J = J + K - 1
                         CMDPAR(I) = ALASTXT(2:J-1)
                         CMDUSE(I) = .FALSE.
                         ALASTXT(1:) = ALASTXT(J+1:)
                         NCMDPAR = NCMDPAR + 1
                         J = INDEX(CMDPAR(I),'""')
                         DO 964 WHILE (J .NE. 0)
                            CMDPAR(I)(J:) = CMDPAR(I)(J+1:)
                            J = INDEX(CMDPAR(I),'""')
  964                    CONTINUE
                      END IF
                   ELSE
                      J = INDEX(ALASTXT,' ')
                      CMDPAR(I) = ALASTXT(1:J-1)
                      CMDUSE(I) = .FALSE.
                      ALASTXT(1:) = ALASTXT(J:)
                      NCMDPAR = NCMDPAR + 1
                   END IF
                END IF
  980        CONTINUE
             IF( NCMDPAR .EQ. 9 .AND. ALASTXT .NE. ' ' ) THEN
                WRITE (*,'(''   Command file invocation has too many'',
     :          '' parameters (n>9)'',A)') BLEEP

                GO TO 5000
             END IF
*
             CALL SSTRIP(PARAMS)
             NCCAR = SLEN(PARAMS)
             I1 = INDEX(PARAMS,' ')
             IF( I1.LT.NCCAR ) THEN
                WRITE (*,'(''   File command input:  superfluous '',
     :          ''parameters ignored'',A)') BLEEP
                PARAMS(I1:) = ' '
                NCCAR = SLEN(PARAMS)
             END IF
             I1 = INDEX(PARAMS,']')
             I2 = INDEX(PARAMS((I1+1):),'.')
             IF( I2 .EQ. 0 ) THEN
                PARAMS((NCCAR+1):) = '.cmd'
             ELSE IF( I2 .EQ. NCCAR ) THEN
                PARAMS((NCCAR+1):) = 'cmd'
             END IF
             NCCAR = SLEN(PARAMS)
             COMSAV = ' '
             COMSAV(1:NCCAR) = PARAMS(1:NCCAR)
             COMTXT = .TRUE.
             CLOSE(67)
             OPEN (UNIT=67,FILE=PARAMS(1:80),STATUS='OLD',
     :             IOSTAT=IHX)
             IF( IHX .NE. 0 ) THEN
                CLOSE (67)
                IF( p2len .GT. 0 )
     :           OPEN (UNIT=67,STATUS='OLD',IOSTAT=IHX,
     :          FILE=prefix2(1:p2len)//PARAMS(1:80))
                IF( IHX .NE. 0 ) THEN
                   CLOSE (67)
                   OPEN (UNIT=67,STATUS='OLD',IOSTAT=IHX,
     :             FILE='F'//PARAMS(1:80))
                END IF
                IF( IHX .NE. 0 ) THEN
                   WRITE (*,'(A,A)') '   Error opening ',
     :             COMSAV(1:NCCAR)//' command file'
                   CLOSE (67)
                   COMTXT = .FALSE.
                   GO TO 5000
                END IF
             END IF
             IF( DEBUG ) THEN
                WRITE (*,'(''   Opened '',A)') COMSAV(1:NCCAR)
             END IF
*
*   COMMands - give simple listing, or keyword listing
*
          ELSE IF( SUBCMD(1:4) .EQ. 'COMM' ) THEN
             CALL COMNDS( PARAMS, PREFIX2( : P2LEN ),
     :                    PREFIX4( : P4LEN ), PREFIX( : PLEN ), STATUS )

*
*   CRASH  to test condition handler...
*
          ELSE IF( SUBCMD .EQ. 'CRASH' ) THEN
             VARRAY(1) = 0.0
             CALL DECODE('CRASH',PARAMS,0,1,VARRAY,' ',OK)
             IF( .NOT.OK) GO TO 5000
             KLUNK = 0
             KLICK = 0
             ACRASH = 1.D30
             ICRASH = NINT(VARRAY(1))
             IF( ICRASH .EQ. 0) THEN
                WRITE (*,'(''   CRASH:  divide by zero'')')
                write(*,*) KLUNK/KLICK

             ELSE IF( ICRASH .EQ. 1 ) THEN
                WRITE (*,'(''   CRASH:  access violation'')')
                I = 100000000
                I = VARRAY(I)
                WRITE (*,'(''   I ='',I10)') I
             END IF
             IF( ICRASH .EQ. 2 ) THEN
                WRITE (*,'(''   CRASH:  overflow'')')
                CTEST = ACRASH*ACRASH
                WRITE (*,'(''   CTEST ='',1PD15.5)') CTEST
             END IF
             IF( ICRASH.GE.3 ) THEN
                WRITE (*,'(''   CRASH:  underflow'')')
                ACRASH = 1.0/ACRASH
                CTEST = ACRASH*ACRASH
                WRITE (*,'(''   CTEST ='',1PD15.5)') CTEST
             END IF
*
*   CREGD   Display regions selected for continuum fitting
*
          ELSE IF( SUBCMD .EQ. 'CREGD' ) THEN
             VARRAY(1) = 0.8
             CALL DECODE('CREGD',PARAMS,0,1,VARRAY,' ',OK)
             IF( DEVTYP .EQ. 0 ) THEN
                WRITE (*,'(''   CREGD:  no plotting device assigned'')')
                GO TO 5000
             END IF
             IF( NCREG.LT.1 ) THEN
                WRITE (*,
     :          '(''   CREGD:  no continuum windows assigned'')')
                GO TO 5000
             END IF
             IF( VARRAY(1).LE.0.0 .OR. VARRAY(1).GE.1.0 ) THEN
                WRITE (*,
     :          '(''   CREGD:  normalised height must be 0<h<1'',A)')
     :          BLEEP
                GO TO 5000
             END IF
             Y1 = YLIM(1) + VARRAY(1)*(YLIM(2)-YLIM(1))
             Y1 = Y1 * 10.0**IEXPY
             IF( COLOUR) CALL PPALET(IPAL)
             IF( DVTST) CALL GSLN(1)
             DO 1020 I = 1, NCREG
                CALL LINE(XLIML(I),Y1,XLIMH(I),Y1)
                CALL PLOTIT( 0, 0, 2)
                CALL SGS_FLUSH
 1020        CONTINUE
             IF( DVTST) CALL GSLN(LNTYPE)
             IF( ROTST ) THEN
                CALL IPSET(IPAL,12)
             END IF
*
*   CREGL   List continuum regions
*
          ELSE IF( SUBCMD .EQ. 'CREGL' ) THEN
             CALL XDCODE('CREGL',PARAMS,0,0,VARRAY,' ',OK)
             IF( NCREG.LE.0 ) THEN
               WRITE (*,'(''   CREGL:  no continuum windows defined'')')
             ELSE
                WRITE (*,'(''   CREGL:  '',1P2E12.4/
     :          (''           '',1P2E12.4))')
     :          (XLIML(I),XLIMH(I),I=1,NCREG)
             END IF
*
*   CREGR   Read continuum regions as input
*
          ELSE IF( SUBCMD .EQ. 'CREGR' ) THEN
             DO 1040 I = 1, MAXSTK
                VARRAY(I) = -10001.
 1040        CONTINUE
             CALL XDCODE('CREGR',PARAMS,2,MAXSTK,VARRAY,'X1 X2 ',OK)
             IF( .NOT.OK) GO TO 5000
*   CHECK REGIONS ARE O.K.
             I = 1
             NVRRY = 0
             DO 1060 WHILE (VARRAY(I) .NE. -10001.)
                NVRRY = NVRRY + 1
                I = I + 1
 1060        CONTINUE
             N2 = NVRRY/2
             NTST = NVRRY - N2*2
             ITST = 0
             DO 1080 I = 2, NVRRY
                IF( VARRAY(I-1).GE.VARRAY(I) ) THEN
                   ITST = ITST + 1
                END IF
 1080        CONTINUE
             IF( NTST .NE. 0 .OR. ITST .NE. 0 ) THEN
                ITST = 1
                PRINT *, '  CREGR: invalid arguments - no changes'
             END IF
*   SET UP REGIONS
             IF( ITST .NE. 1 ) THEN
                NCREG = 0
                DO 1090 I = 1, N2
                   NCREG = NCREG + 1
                   WORK(NCREG) = VARRAY(2*I-1)
                   WORK(NCREG+WORKSZ/4) = VARRAY(2*I)
 1090           CONTINUE
                IFAIL = 1
!               CALL M01ANF(DPWORK(1),1,NCREG,IFAIL)
                CALL HEAPSRT (NCREG,WORK)
!               IF( IFAIL .NE. 0 ) THEN
!                  WRITE (*,'(''   CREGR:  Error in M01ANF routine'')')
!                  GO TO 5000
!               END IF
                IFAIL = 1
!               CALL M01ANF(DPWORK(1+WORKSZ/4),1,NCREG,IFAIL)
                CALL HEAPSRT (NCREG,WORK(1+WORKSZ/4))
!               IF( IFAIL .NE. 0 ) THEN
!                  WRITE (*,'(''   CREGR:  Error in M01ANF routine'')')
!                  GO TO 5000
!               END IF
                DO 1100 I = NCREG, 2, -1
                   IF( WORK(I+WORKSZ/4).LE.WORK(I-1) ) THEN
                      NCREG = NCREG - 1
                      PRINT *, '  CREGR:  NCREG reduced by 1'
                   END IF
 1100           CONTINUE
                DO 1110 I = 1, NCREG
                   XLIML(I) = WORK(I)
                   XLIMH(I) = WORK(I+WORKSZ/4)
 1110           CONTINUE
             END IF
             WORK(I+WORKSZ/4-1) = WORK(I+WORKSZ/4)
*
*   CREGS   Select regions for continuum fitting
*
          ELSE IF( SUBCMD .EQ. 'CREGS' ) THEN
             NCREG = 0
             CALL SSTRIP(PARAMS)
             CCUR = .TRUE.
             IF( PARAMS .NE. ' ' ) THEN
                CCUR = .FALSE.
             ELSE IF( .NOT.CURSOR .OR. NPLOTS .EQ. 0 ) THEN
                WRITE (*,'(''   CREGS:  no plot available'')')
                GO TO 5000
             END IF

             IF( CCUR ) THEN
                CALL CPAIR(X1,Y1,X2,Y2)
             ELSE
                CALL DECODE('CREGS',PARAMS,2,2,VARRAY,'X1 X2 ',OK)
                IF( .NOT.OK ) THEN
                   OK = .TRUE.
                   GO TO 5000
                END IF
                X1 = VARRAY(1)
                X2 = VARRAY(2)
                Y1 = 1.
                Y2 = Y1
             END IF
 1120        CONTINUE
             IF( X1 .NE. X2 ) THEN
                WRITE (*,'(''   CREGS:  X1, X2 ='',1P2E12.4)')
     :          MIN(X1,X2), MAX(X1,X2)
                NCREG = NCREG + 1
                WORK(NCREG) = MIN(X1,X2)
                WORK(NCREG+WORKSZ/4) = MAX(X1,X2)
                IF( CCUR ) THEN
                   CALL CPAIR(X1,Y1,X2,Y2)
                ELSE
                   CALL SSTRIP(PARAMS)
                   IF( PARAMS .NE. ' ' ) THEN
                      CALL DECODE('CREGS',PARAMS,2,2,VARRAY,'X1 X2 ',OK)
                      IF( .NOT.OK ) THEN
                         OK = .TRUE.
                         NCREG = 0
                         WRITE (*,
     :                   '(''   CREGS:  no continuum windows set'')')
                         GO TO 5000
                      END IF
                      X1 = VARRAY(1)
                      X2 = VARRAY(2)
                   ELSE
                      X1 = 1.
                      X2 = X1
                   END IF
                END IF
                GO TO 1120
             END IF
             IF( NCREG.GT.0 ) THEN
!               CALL M01ANF(DPWORK(1),1,NCREG,IFAIL)
                CALL HEAPSRT (NCREG,WORK(1))
!               IFAIL = 0
!               CALL M01ANF(DPWORK(1+WORKSZ/4),1,NCREG,IFAIL)
                CALL HEAPSRT (NCREG,WORK(1+WORKSZ/4))
                DO 1130 I = NCREG, 2, -1
                   IF( WORK(I+WORKSZ/4).LE.WORK(I-1) ) THEN
                      NCREG = NCREG - 1
                      WORK(I+WORKSZ/4-1) = WORK(I+WORKSZ/4)
                   END IF
 1130           CONTINUE
                DO 1140 I = 1, NCREG
                   XLIML(I) = WORK(I)
                   XLIMH(I) = WORK(I+WORKSZ/4)
 1140           CONTINUE
             END IF
*
*   CSET   Select colour for ARGS plotting
*
          ELSE IF( SUBCMD .EQ. 'CSET' ) THEN
             CALL DECODE('CSET',PARAMS,1,1,VARRAY,'Index ',OK)
             IF( .NOT.OK) GO TO 5000
             IF( NINT(VARRAY(1)).LT.1 .OR. NINT(VARRAY(1)).GT.15 ) THEN
                WRITE (*,'(''   CSET:  index outside range 1-15'',A)')
     :          BLEEP
             END IF
             IPAL = VARRAY(1)
             IPALKP = IPAL
*
*   CROP   Crop the display.
*
          ELSE IF( SUBCMD .EQ. 'CROP' ) THEN
             CALL GET0L( PARAMS, 1, .TRUE., SUBCMD, 'Do you want to '//
     :                   'crop the display', .TRUE., CROPIT, STATUS )

             IF( CROPIT ) THEN
                CALL MSGOUT( SUBCMD, 'The display will be cropped',
     :                       .FALSE., STATUS )
             ELSE
                CALL MSGOUT( SUBCMD, 'The display will not be cropped',
     :                       .FALSE., STATUS )
             END IF
*
*   CROT   Rotate colour table with consecutive plots
*
          ELSE IF( SUBCMD .EQ. 'CROT' ) THEN
             CALL DECODE('CROT',PARAMS,0,0,VARRAY,' ',OK)
             ROTST = .TRUE.
             IPAL = IPALKP
*
*   CXR     Use cursor to delimit X range for plotting
*
          ELSE IF( SUBCMD .EQ. 'CXR' ) THEN
             CALL DECODE('CXR',PARAMS,0,0,VARRAY,' ',OK)
             CALL CPAIR(X1,Y1,X2,Y2)
             CALL XRSET(X1,X2)
             XRKEEP(1) = X1
             XRKEEP(2) = X2
             TRIMX = .TRUE.
*
*   CXYR    Use cursor to delimit X and Y ranges for plotting
*
          ELSE IF( SUBCMD .EQ. 'CXYR' ) THEN
             CALL DECODE('CXYR',PARAMS,0,0,VARRAY,' ',OK)
             CALL CPAIR(X1,Y1,X2,Y2)
             CALL XRSET(X1,X2)
             XRKEEP(1) = X1
             XRKEEP(2) = X2
             CALL YRSET(Y1,Y2)
             YRKEEP(1) = Y1
             YRKEEP(2) = Y2
             TRIMX = .TRUE.
             TRIMY = .TRUE.
*
*   CYR     Use cursor to delimit Y range for plotting
*
          ELSE IF( SUBCMD .EQ. 'CYR' ) THEN
             CALL DECODE('CYR',PARAMS,0,0,VARRAY,' ',OK)
             CALL CPAIR(X1,Y1,X2,Y2)
             CALL YRSET(Y1,Y2)
             YRKEEP(1) = Y1
             YRKEEP(2) = Y2
             TRIMY = .TRUE.
*
*   DEBUG  turns on debug switches
*
          ELSE IF( SUBCMD .EQ. 'DEBUG' ) THEN
             CALL SSTRIP(PARAMS)
             CALL DTOUPP(PARAMS)
             IF( PARAMS .EQ. 'ON') DEBUG = .TRUE.
             IF( PARAMS .EQ. 'OFF') DEBUG = .FALSE.
*
*   DELete  Delete a stack entry
*
          ELSE IF( SUBCMD .EQ. 'DEL' ) THEN
             IF( NONSTK .EQ. 0 ) THEN
                WRITE (*,'(''   DEL:  no stack entries present'',A)')
     :          BLEEP
                GO TO 5800
             END IF
             DO 1160 I = 1, MAXSTK
                VARRAY(I) = 1001.
 1160        CONTINUE
             I = INDEX(PARAMS,'-')
             CALL DASHIT('DEL',IXS,NV,PARAMS,MAXSTK,MAXSTK,NONSTK,
     :       VARRAY,OK)
             IF( .NOT.OK ) THEN
                OK = .TRUE.
                GO TO 5000
             END IF
             IF( NV .EQ. 0 .AND. I .EQ. 0 ) THEN
                I = 1
             ELSE
                I = 0
             END IF
             CALL XDCODE('DEL',PARAMS,I,MAXSTK-NV,VARRAY(NV+1),'Entry ',
     :       OK)
             IF( .NOT.OK ) THEN
                OK = .TRUE.
                GO TO 5000
             END IF
             IDELXS = IXS
             IDELNE = 0
             IF( DEBUG ) THEN
                WRITE (*,'(''   IXS:'',I5)') IXS
             END IF
             DO 1180 I = 1, MAXSTK
                II = NINT(VARRAY(I))
                IF( II.LT.1 .OR. II.GT.NONSTK ) THEN
                   IF( II .NE. 1001. ) THEN
                      VARRAY(I) = 1001.
                      IF( II.GT.NONSTK .AND. II.LE.MAXSTK ) THEN
                         IDELNE = IDELNE + 1
                      ELSE
                         IDELXS = IDELXS + 1
                      END IF
                   END IF
                END IF
 1180        CONTINUE
*   ORDER VARRAY INTO INCREASING ENTRY NUMBER
 1200        CONTINUE
             ITMP = 0
             DO 1220 I = 2, MAXSTK
                I1 = NINT(VARRAY(I-1))
                I2 = NINT(VARRAY(I))
                IF( I1.GT.I2 ) THEN
                   VARRAY(I-1) = REAL(I2)
                   VARRAY(I) = REAL(I1)
                   ITMP = ITMP + 1
                END IF
 1220        CONTINUE
             IF( ITMP .NE. 0) GO TO 1200
*   GET RID OF DUPLICATE ENTRIES
 1240        CONTINUE
             ITMP = 0
             DO 1260 I = 2, MAXSTK
                I1 = NINT(VARRAY(I-1))
                I2 = NINT(VARRAY(I))
                IF( I1 .EQ. I2 .AND. I2 .NE. 1001 ) THEN
                   DO 1245 J = I - 1, MAXSTK - 1
                      VARRAY(J) = VARRAY(J+1)
 1245              CONTINUE
                   VARRAY(MAXSTK) = 1001.
                   ITMP = ITMP + 1
                END IF
 1260        CONTINUE
             IF( ITMP .NE. 0) GO TO 1240
*   FIND NUMBER OF DELETIONS REQUIRED, AND
             NDO = 0
             DO 1280 I = 1, MAXSTK
                IF( NINT(VARRAY(I)) .EQ. 1001) GO TO 1300
                NDO = NDO + 1
 1280        CONTINUE
 1300        CONTINUE
             NDEL = NDO
             IF( DEBUG ) THEN
                WRITE (*,'(''  DEL numbers:''/('' '',10I5))')
     :                  (NINT(VARRAY(I)),I=1,NDEL)
             END IF

             IF( IDELXS.GT.0 ) THEN
*             Build and report message.
                CALL ITOCHR (IDELXS, STRING1, NCHAR1, ICSTAT)
                CALL ITOCHR (MAXSTK, STRING2, NCHAR2, ICSTAT)

                BIGSTR= ' DEL: '//STRING1(:NCHAR1)//' undefined (N>'//
     :                  STRING2(:NCHAR2)//') entries ignored'
                WRITE (*,*)BIGSTR,BLEEP
                BIGSTR = ' '
             END IF

             IF( IDELNE.GT.0 ) THEN

*             Build and report message.
                CALL ITOCHR (IDELNE, STRING1, NCHAR1, ICSTAT)

                BIGSTR= ' DEL: '//STRING1(:NCHAR1)//
     :          ' specified entries are already empty'
                WRITE (*,*)BIGSTR,BLEEP
                BIGSTR = ' '

             END IF

             IF( NDEL .EQ. NONSTK ) THEN
                NONSTK = 0
                STKLST = 0
                BSTLST = 0
                WRITE (*,'(''   DEL:  stack cleared'')')
             ELSE
                NEWSTK = NONSTK - NDEL
                DO 1310 I = 1, NONSTK
                   WORK(I) = REAL(I)
 1310           CONTINUE
                DO 1320 I = 1, NDEL
                   WORK(NINT(VARRAY(I))) = -1.
 1320           CONTINUE
                DO 1330 I = 1, NEWSTK
                   DO 1325 WHILE (WORK(I).LE.0.0)
                      DO 1322 J = I, NONSTK
                         WORK(J) = WORK(J+1)
 1322                 CONTINUE
 1325              CONTINUE
 1330           CONTINUE

                DO 1340 I = 1, NEWSTK
                   IF( NINT(WORK(I)) .NE. I ) THEN
                      IW = NINT(WORK(I))
                      STKNPT(I) = STKNPT(IW)
                      BSTNPT(I) = BSTNPT(IW)
                      STITLE(I) = STITLE(IW)
                      WORVST(I) = WORVST(IW)
                      IF( I .EQ. 1 ) THEN
                         POINTR(I) = 1
                         BPOINT(I) = 1
                      ELSE
                         POINTR(I) = POINTR(I-1) + STKNPT(I-1)
                         BPOINT(I) = BPOINT(I-1) + BSTNPT(I-1)
                      END IF
                      I1 = POINTR(I)
                      I2 = POINTR(IW)
                      J1 = BPOINT(I)
                      J2 = BPOINT(IW)
                      DO 1332 J = 1, STKNPT(I)
                         XSTACK(I1+J-1) = XSTACK(I2+J-1)
                         YSTACK(I1+J-1) = YSTACK(I2+J-1)
 1332                 CONTINUE
                      DO 1334 J = 1, BSTNPT(I)
                         BSTACK(J1+J-1) = BSTACK(J2+J-1)
 1334                 CONTINUE
                   END IF
 1340           CONTINUE

                NONSTK = NEWSTK
                STKLST = POINTR(NONSTK) + STKNPT(NONSTK) - 1
                BSTLST = BPOINT(NONSTK) + BSTNPT(NONSTK) - 1
             END IF

*  DESK: Spawn desk calculator
          ELSE IF( SUBCMD .EQ. 'DESK' ) THEN
             CALL MSGOUT( 'DESK', 'This command is not implemented.',
     :                    .TRUE., STATUS )

*  DEV: Select graphics device
          ELSE IF( SUBCMD .EQ. 'DEV' ) THEN

             IF( .NOT.FIRSTDEVICE ) THEN
                CALL SGS_CLSWK(IZBASE,ISTAT)
                CURSOR = .FALSE.
             END IF

*   Display list if so requested

             J = 0
 1360        CONTINUE
             IF( PARAMS .EQ. '?' .OR. CHR_SIMLR( 'H', PARAMS ) .OR.
     :           CHR_SIMLR( 'help', PARAMS ) .OR.
     :          (J .EQ. 0 .AND. PARAMS .EQ. ' ') ) THEN
                CALL SGS_WLIST(6)
                PARAMS = ' '
             END IF
 1380        CONTINUE
             IF( PARAMS .EQ. ' ' ) THEN
                CALL RDSTR( 'DEV', 'Workstation', ' ', PARAMS, STATUS )
                IF( STATUS .NE. SAI__OK ) THEN
                   OK = .FALSE.
                   GO TO 5000
                END IF
                CALL SSTRIP(PARAMS)
             END IF
             IF( PARAMS .EQ. ' ') GO TO 1380
             IF( PARAMS .EQ. '?' .OR. CHR_SIMLR( PARAMS, 'H' ) .OR.
     :           CHR_SIMLR( PARAMS ,'HELP' ) ) GO TO 1360

*   Null device

             IF( PARAMS .EQ. '0' ) THEN
                DEVTYP = 0
                NPLOTS = 0
                DVTST = .FALSE.
                CURSOR = .FALSE.
                COLOUR = .FALSE.
                XSIZ = 0.0
                YSIZ = 0.0
                FIRSTDEVICE = .TRUE.
                GO TO 1410
             END IF

*   Attempt to open SGS and GKS
             IF( FIRSTDEVICE ) THEN
                CALL SGS_OPEN(PARAMS,IZBASE,J)
             ELSE
                CALL SGS_OPNWK(PARAMS,IZBASE,J)
             END IF
*             write(*,*)firstdevice,params,izbase,j
*   If failure, flag prompt required
             PARAMS = ' '
             IF( J .EQ. 0 ) THEN
                DEVTYP = IZBASE
             ELSE IF( J .EQ. 25 .AND. (PARAMS(1:5) .EQ. 'CANON' .OR.
     :                PARAMS(1:3) .EQ. '260') ) THEN
                WRITE (*,
     :          '(''   DEV:  unable to open CANON.DAT file'',A)')
     :          BLEEP
                GO TO 1360
             ELSE IF( PARAMS(1:4) .EQ. '1100' .OR.
     :                PARAMS(1:4) .EQ. 'VERS')
     :       THEN
                WRITE (*,'(''   DEV:  unable to open Versatec file'')')
             ELSE IF( PARAMS(1:4) .EQ. '1200' .OR.
     :                PARAMS(1:4) .EQ. 'PRIN')
     :       THEN
                WRITE
     :          (*,'(''   DEV:  unable to open Printronix file'',A)')
     :          BLEEP
             ELSE
                WRITE (*,'(''   DEV:  error opening workstation'',A)')
     :          BLEEP
                GO TO 1360
             END IF
*   Match the AUTOGRAPH coordinate system to the current zone
             CALL SNX_AGWV
             CALL GQWKC(1,IERROR,ICONID,DEVTYP)
             CALL SGS_SPREC(IPREC)
*   Set up base zone
             CALL SGS_IZONE(X1,X2,Y1,Y2,XM,YM)
             XS2NDC = 1.0/Y2
             YS2NDC = 1.0/X2
             CALL SGS_SW(0.0,1.0,0.0,1.0,ISTAT)
*   Test for cursor availability
             CALL SGS_ICUAV(CURSOR)
*   Inquire extent of device display surface in metres
             CALL SGS_IZONE(XX1,XX2,YY1,YY2,XSIZ,YSIZ)
*   Change to cm
             XSIZ = 100.0*XSIZ
             YSIZ = 100.0*YSIZ
*   Set GKS aspect source flags to individual to enable setting up
*   of colour indices and polyline attributes
             DO 1400 I = 3, 13
                ASF(I) = 1
 1400        CONTINUE
             CALL GSASF(ASF)
*   Test for colour availability and Set up colour table if appropriate
             CALL GQCF(DEVTYP,IERROR,NCOLOURS,ICOLOUR,NPREDEF)
             IF( ICOLOUR .EQ. 1 ) THEN
                CALL SETCOLOURS(DEVTYP,NCOLOURS)
                COLOUR = .TRUE.
             ELSE
                COLOUR = .FALSE.
             END IF

             FIRSTDEVICE = .FALSE.
             NPLOTS = 0
             DVTST = .TRUE.
             CALL GSLN(LNTYPE)
             CALL GSLWSC(XLWIDTH)

*   Font selection

             IF( IFONT .EQ. 0 ) THEN
                IPREC = 0
                NTEXT = 0
                CALL AGPWRT(0.0,0.0,' ',0,0,0,-100)
             ELSE IF( IFONT .EQ. 1 ) THEN
                IPREC = 2
                NTEXT = 0
                CALL AGPWRT(0.0,0.0,' ',0,0,0,-100)
             ELSE IF( IFONT .EQ. 2 ) THEN
                IPREC = 2
                NTEXT = 1
                CALL AGPWRT(0.0,0.0,' ',0,0,0,+100)
             END IF

*   Line style

             IF( DVTST ) THEN
                CALL GSLN(LNTYPE)
                CALL GSLWSC (XLWIDTH)
             END IF

*   Escape for null device selection

 1410        CONTINUE

*  DIPSO: Someone's typed it in thoughtlessly...
          ELSE IF( SUBCMD .EQ. 'DIPSO' ) THEN
             WRITE (*,'(''   DIPSO: hi!'',A)') BLEEP

*  DREDden: Deredden (Galactic Law) calibrated data
          ELSE IF( SUBCMD .EQ. 'DRED' ) THEN
             IF( NPOINT.LT.1 ) THEN
                WRITE (*,'(''   DRED:  no data in current arrays'')')
                GO TO 5000
             END IF
             VARRAY(2) = 3.1
             VARRAY(3) = 0.0
             CALL XDCODE('DRED',PARAMS,1,3,VARRAY,'E(B-V) ',OK)
             IF( OK .AND. NPOINT.GE.1 ) THEN
                EBV = VARRAY(1)
                RBV = VARRAY(2)
                GALLMC = VARRAY(3)
                CALL DERED(EBV,RBV,GALLMC,WAVE,FLUX,NPOINT)
             ELSE
                WRITE (*,'(''   DRED:  error!'')')
                GO TO 5000
             END IF
*
*   DRLINE  Draws a line from user co-ords (x1,y1) to (x2,y2)
*
          ELSE IF( SUBCMD .EQ. 'DRLINE' ) THEN
             CALL DECODE('DRLINE',PARAMS,4,4,VARRAY,
     :       'xstart ystart xend yend',OK)
             IF( .NOT.OK) GO TO 5000

             IF( NPLOTS.LT.1 ) THEN
                WRITE (*,'(''   DRLINE:  no plot available'',A)') BLEEP
                GO TO 5000
             END IF

             UX1 = VARRAY(1)
             UY1 = VARRAY(2)
             UX2 = VARRAY(3)
             UY2 = VARRAY(4)
             CALL SNX_TO('SGS')
*   transform co-ords
             WX1 = SNX_AGUGX(UX1/DIVX)
             WX2 = SNX_AGUGX(UX2/DIVX)
             WY1 = SNX_AGUGY(UY1/DIVY)
             WY2 = SNX_AGUGY(UY2/DIVY)

             IF( DVTST) CALL GSLWSC(XLWIDTH)
             CALL SGS_LINE(WX1,WY1,WX2,WY2)

             CALL SGS_FLUSH
             CALL SNX_TO('NCAR')
*
*   ECHO    Echo commands read in from file (with COMRD)
*
          ELSE IF( SUBCMD .EQ. 'ECHO' ) THEN
             VARRAY(1) = -1.
             CALL DECODE('ECHO',PARAMS,0,1,VARRAY,' ',OK)
             IF( .NOT.OK) GO TO 5000
             INTECHO = NINT(VARRAY(1))
             ECHO = .TRUE.
*
*
*   ELF - this is the interface with PJS's
*   Emission Line Fitting library
*
*
          ELSE IF( SUBCMD .EQ. 'ELFINP' ) THEN

             VARRAY( 1 ) = 0.
             CALL DECODE( 'ELFINP', PARAMS, 0, 1, VARRAY, ' ', OK )
             IF( .NOT .OK ) GO TO 5000

             CALL ELFKEY( IFPS, ( NINT(VARRAY(1)) .NE. 0 ) .AND. COMTXT,
     :                    COMSAV(1:NCCAR), STATUS )
             IF( STATUS .NE. SAI__OK ) THEN
                OK = .FALSE.
                GO TO 5000
             END IF

             CALL ABLOAD(0,SUBCHK)
             IF( .NOT.SUBCHK ) THEN
                WRITE (*,
     :          '(''   ELFINP:  current fit storage is empty'')')
                SUBCHK = .TRUE.
                GO TO 5000
             END IF
          ELSE IF( SUBCMD .EQ. 'ELFOPT' ) THEN
             VARRAY(1) = 0.0
             CALL DECODE('ELFOPT',PARAMS,0,1,VARRAY,' ',OK)
             IF( .NOT.OK) GO TO 5000
             ELFPRMPT = NINT(VARRAY(1))
             CALL ACHECK(PJSA,INDXPS,IRELPS,NLPS,IMPS,SUBCHK)
             IF( .NOT.SUBCHK ) THEN
                WRITE (*,
     :          '(''   ELFOPT:  current fit storage is empty'')')
                SUBCHK = .TRUE.
                GO TO 5000
             END IF
             IF( IMPS .EQ. 0 ) THEN
                CALL LOCDAT(ASIZE1,WAVE,FLUX,WORV,NPOINT,IFPS,SUBCHK)
                IF( .NOT.SUBCHK ) THEN
                   SUBCHK = .TRUE.
                   GO TO 5000
                END IF
                IF( IFPS .EQ. 0 ) THEN
                   XFCTR = 0.0
                   CALL DECODE('ELFOPT',PARAMS,0,1,XFCTR,' ',OK)
                   IF( .NOT.OK ) THEN
                      OK = .TRUE.
                      GO TO 5000
                   END IF
                   IF( NINT(XFCTR).GT.0 ) THEN
                      PROCEED = .FALSE.
                   END IF
                   CALL ELFIT(1,PROCEED)
                   PROCEED = .TRUE.
                ELSE
                   WRITE (*,'(''   ELFOPT:  optimisation skipped'')')
                   GO TO 5000
                END IF
             ELSE
                WRITE (*,'(''   ELFOPT:  optimisation skipped'')')
                GO TO 5000
             END IF
          ELSE IF( SUBCMD .EQ. 'ELFPUSH' ) THEN
             VARRAY(1) = 0.
             VARRAY(2) = 0.
             I = INDEX(PARAMS,'-')
             IF( I.GT.0 ) THEN
                PARAMS(I:I) = ' '
             END IF
             CALL DECODE('ELFPUSH',PARAMS,0,2,VARRAY,'Line_1 Line_2 ',
     :       OK)
             IF( .NOT.OK) GO TO 5000
             N1PS = VARRAY(1)
             N2PS = VARRAY(2)
             IF( IABPS .EQ. 1 ) THEN
                CALL FPUSH(ASIZE1,MAXBRK,WAVE,FLUX,NPOINT,BREAK,NBREAK,
     :          N1PS,N2PS,SUBCHK)
                IF( .NOT.SUBCHK ) THEN
                   SUBCHK = .TRUE.
                   GO TO 5000
                END IF
             ELSE
                CALL ABLOAD(1,SUBCHK)
                IF( .NOT.SUBCHK ) THEN
                   WRITE (*,
     :             '(''   ELFPUSH:  current fit storage is empty'')')
                   SUBCHK = .TRUE.
                   GO TO 5000
                END IF
             END IF
          ELSE IF( SUBCMD .EQ. 'ELFNEWC' ) THEN
             IF( PARAMS .NE. ' ' ) THEN
                WRITE (*,
     :          '(''   ELFNEWC:  superfluous parameters ignored'',A)')
     :          BLEEP
             END IF
             CALL ELFCLR
          ELSE IF( SUBCMD .EQ. 'ELFPUSHC' ) THEN
             IF( PARAMS .NE. ' ' ) THEN
                WRITE (*,
     :          '(''   ELFPUSHC:  superfluous parameters ignored'',A)')
     :          BLEEP
             END IF
             CALL ELFSTOR
          ELSE IF( SUBCMD .EQ. 'ELFPOPC' ) THEN
             CALL DECODE('ELFPOPC',PARAMS,1,1,VARRAY,
     :       'Coefficients_stack_entry ',OK)
             IF( .NOT.OK) GO TO 5000
             NFSPS = VARRAY(1)
             CALL FCPOP(NFSPS,SUBCHK)
             IF( .NOT.SUBCHK ) THEN
                SUBCHK = .TRUE.
                GO TO 5000
             END IF
          ELSE IF( SUBCMD .EQ. 'ELFCSL' ) THEN
             IF( PARAMS .NE. ' ' ) THEN
                WRITE (*,
     :          '(''   ELFCSL:  superfluous parameters ignored'',A)')
     :          BLEEP
             END IF
             CALL FCSL
          ELSE IF( SUBCMD .EQ. 'ELFDELC' ) THEN
             DO 1420 I = 1, MAXSTK
                VARRAY(I) = 1001.
 1420        CONTINUE
             CALL DECODE('ELFDELC',PARAMS,1,MAXFCPS,VARRAY,'Entry ',OK)
             IF( .NOT.OK) GO TO 5000
             CALL FCDEL(VARRAY,MAXSTK)
          ELSE IF( SUBCMD .EQ. 'ELFWRC' ) THEN
             DO 1440 WHILE (PARAMS .EQ. ' ')

                CALL RDSTR( SUBCMD, 'File name', ' ', PARAMS, STATUS )
                IF( STATUS .NE. SAI__OK ) THEN
                   OK = .FALSE.
                   GO TO 5000
                END IF

                CALL SSTRIP(PARAMS)

 1440        CONTINUE
             CLOSE (35)
             OPEN (UNIT=35,FILE=PARAMS(1:80),STATUS='NEW',IOSTAT=IHX)
             IF( IHX .NE. 0 ) THEN
                WRITE (*,'(''   ELFWRC:  unable to open file'')')
                GO TO 5000
             END IF
             IVIEW = 1
             CALL ELFVIEW(1,NFSPS,PARAMS,IHX)
             IF( IHX .NE. 0 ) THEN
                IF( IHX .EQ. 1 ) THEN
                   WRITE (*,
     :             '(''   ELFWRC:  coefficient stack is empty'')')
                END IF
                GO TO 5000
             END IF
          ELSE IF( SUBCMD .EQ. 'ELFVUC' ) THEN
             VARRAY(2) = 0.0
             CALL DECODE('ELFVUC',PARAMS,1,2,VARRAY,
     :       'Coefficients_Stack_Entry ',OK)
             IF( .NOT.OK) GO TO 5000
             IVIEW = NINT(VARRAY(2))
             NFSPS = VARRAY(1)
             CALL ELFVIEW(0,NFSPS,PARAMS,IHX)
             IF( IHX .NE. 0 ) THEN
                IF( IHX .EQ. 1 ) THEN
                   WRITE (*,
     :             '(''   ELFVUC:  coefficient stack is empty'')')
                END IF
                GO TO 5000
             END IF
*
*   ELFSAVEC: Save entire fit STACK contents for ELFRESTC
*
          ELSE IF( SUBCMD .EQ. 'ELFSAVEC' ) THEN
             IF( NUMFITPS .EQ. 0 ) THEN
                WRITE (*,
     :          '(''   ELFSAVEC:  coefficient stack is empty'')')
                GO TO 5000
             END IF
             DO 1460 I = 1, 40
                IF( PARAMS(I:I) .NE. ' ') GO TO 1480
 1460        CONTINUE
             PARAMS = 'ELFSAVE.ESTK'
 1480        CONTINUE
             CALL SSTRIP(PARAMS)
             LLNGTH = INDEX(PARAMS,' ') - 1
             DO 1500 IL = LLNGTH, 1, -1
                LDOT = IL
                IF( PARAMS(IL:IL) .EQ. '.') GO TO 1520
 1500        CONTINUE
             LDOT = 0
 1520        CONTINUE
             LBRACE = INDEX(PARAMS,']')
             IF( LDOT.LE.LBRACE ) THEN
                PARAMS(LLNGTH+1:LLNGTH+4) = '.ESTK'
                LLNGTH = LLNGTH + 4
             END IF
             CLOSE (34)


             INQUIRE(FILE=PARAMS(1:SLEN(PARAMS)),EXIST=EXISTS)
             IF( EXISTS ) THEN

                CALL GET0L( ' ', 1, .FALSE., SUBCMD, 'Output file '//
     :                      'exists. Over-write it', .FALSE., OVERWR,
     :                      STATUS )
                IF( STATUS .NE. SAI__OK ) THEN
                   OK = .FALSE.
                   GO TO 5000
                END IF

                IF( .NOT. OVERWR ) GO TO 5000

                OPEN( UNIT=34, STATUS='OLD',
     :                FILE=PARAMS(1:SLEN(PARAMS)) )
                CLOSE( 34, STATUS='DELETE' )

             END IF

             OPEN (UNIT=34,STATUS='NEW',FILE=PARAMS(1:SLEN(PARAMS)),
     :       FORM='UNFORMATTED',IOSTAT=IHX)
             IF( IHX .NE. 0 ) THEN
                CLOSE (34)
                WRITE (*,'(''   ELFSAVEC:  unable to open '',A)')
     :          PARAMS(1:LLNGTH)
                GO TO 5000
             END IF
             CALL FCSAVE(SUBCHK)
             CLOSE (34)
             IF( .NOT.SUBCHK ) THEN
                WRITE (*,'(''   ELFSAVEC:  error saving '',A)')
     :          PARAMS(1:LLNGTH)
                GO TO 5000
             END IF
             WRITE (*,'(''   ELFSAVEC:  '',A,'' saved'')')
     :       PARAMS(1:LLNGTH)
*
*   ELFRESTC:  restore fit coefficient stack after ELFSAVEC
*
          ELSE IF( SUBCMD .EQ. 'ELFRESTC' ) THEN
             DO 1540 I = 1, 40
                IF( PARAMS(I:I) .NE. ' ') GO TO 1560
 1540        CONTINUE
             PARAMS = 'ELFSAVE.ESTK'
 1560        CONTINUE
             CALL SSTRIP(PARAMS)
             LLNGTH = INDEX(PARAMS,' ') - 1
             DO 1580 IL = LLNGTH, 1, -1
                LDOT = IL
                IF( PARAMS(IL:IL) .EQ. '.') GO TO 1600
 1580        CONTINUE
             LDOT = 0
 1600        CONTINUE
             LBRACE = INDEX(PARAMS,']')
             IF( LDOT.LE.LBRACE ) THEN
                PARAMS(LLNGTH+1:LLNGTH+4) = '.ESTK'
                LLNGTH = LLNGTH + 4
             END IF
              CLOSE(34)
              OPEN (UNIT=34,FILE=PARAMS(1:80),STATUS='OLD',
     :        FORM='UNFORMATTED',IOSTAT=IHX)
             IF( IHX .NE. 0 ) THEN
                CLOSE (34)
                WRITE (*,'(''   ELFRESTC:  unable to open '',A)')
     :          PARAMS(1:LLNGTH)
                GO TO 5000
             ELSE
                CALL FCREST(SUBCHK)
                IF( .NOT.SUBCHK ) THEN
                   CLOSE (34)
                   WRITE (*,'(''   ELFRESTC:  error restoring '',A)')
     :             PARAMS(1:LLNGTH)
                   GO TO 5000
                END IF
             END IF
             CLOSE (34)
             WRITE (*,
     :       '(''   ELFRESTC:  '',A,     '' restored;  new stack'')')
     :       PARAMS(1:LLNGTH)
             CALL FCSL
          ELSE IF( SUBCMD .EQ. 'ELFLFIX' ) THEN
             IF( .NOT.CURSOR .OR. NPLOTS .EQ. 0 ) THEN
                WRITE (*,'(''   ELFLFIX:  no plot available'')')
                GO TO 5000
             END IF
             CALL DECODE('ELFLFIX',PARAMS,1,1,VARRAY,'Line ',OK)
             IF( .NOT.OK) GO TO 5000
             IL = NINT(VARRAY(1))
             IF( IL.LE.20 .AND. IL.GT.0 ) THEN
                CALL LOCDAT(ASIZE1,WAVE,FLUX,WORV,NPOINT,IFPS,SUBCHK)
                IF( .NOT.SUBCHK ) THEN
                   SUBCHK = .TRUE.
                   GO TO 5000
                END IF
                IF( IFPS .EQ. 0 ) THEN
                   CALL FLFIX(IL)
                   CALL ABLOAD(0,SUBCHK)
                END IF
             ELSE
                WRITE (*,'(''   ELFLFIX:  line number invalid'')')
             END IF
          ELSE IF( SUBCMD .EQ. 'ELFPIN' ) THEN
             CALL DECODE('ELFPIN',PARAMS,1,1,VARRAY,
     :       'DIPSO_stack_entry ',OK)
             IF( .NOT.OK) GO TO 5000
             NSPS = VARRAY(1)
             CALL FPGET(NSPS,SUBCHK)
             IF( .NOT.SUBCHK ) THEN
                SUBCHK = .TRUE.
                GO TO 5000
             END IF
          ELSE IF( SUBCMD .EQ. 'ELFPL' ) THEN
             CALL ELFPL
          ELSE IF( SUBCMD .EQ. 'FBUGON' ) THEN
             NYPS = 1
          ELSE IF( SUBCMD .EQ. 'FBUGOFF' ) THEN
             NYPS = 0
*
*   END OF ELF INTERFACE
*

*
*   ENV  Return the value of an environment variable used within
*        DIPSO.
*
          ELSE IF( SUBCMD .EQ. 'ENV' ) THEN
             CALL GET0C( PARAMS, 1, .FALSE., SUBCMD, 'The environment '
     :                   // 'variable to display', 'DIPSODIR', ENVNAM,
     :                   STATUS )
             IF( STATUS .EQ. SAI__OK ) THEN
                CALL GTENV( ENVNAM, ARG1, ENVVAL, STATUS )
                IF( STATUS .EQ. SAI__OK ) THEN
                   WRITE(*,*) ENVVAL( : CHR_LEN( ENVVAL ) )
                ELSE
                   CALL ERR_ANNUL( STATUS )
                   WRITE(*,*) '"',ENVNAM( : CHR_LEN( ENVNAM ) ),
     :                        '" is undefined.'
                END IF
             ELSE
                OK = .FALSE.
                GO TO 5000
             END IF

*
*   ERASE   CLEAR graphics device
*
          ELSE IF( SUBCMD .EQ. 'ERASE' ) THEN
             CHECKPARAMS = PARAMS(1:1)

             CALL DECODE('ERASE',PARAMS,0,1,VARRAY,' ',OK)
             IF( .NOT.OK) GO TO 5000
             IF( DEVTYP .EQ. 0 ) THEN
                WRITE (*,'(''   ERASE:  no device assigned'',A)')
     :          BLEEP
                GO TO 5000
             END IF

             IF( CHECKPARAMS .EQ. ' ' ) THEN
                NZONERASE = 0
             ELSE
                NZONERASE = NINT(VARRAY(1))
             END IF
*   Select base zone (world coordinates are screen coordinates)
             CALL SGS_SELZ(IZBASE,ISTAT)
             IF( NZONERASE .EQ. 0 ) THEN
                CALL SGS_CLRZ
*   All zones are clear
                DO 1610 IZZ = 0, 100
                   ZONECLEAR(IZZ) = .TRUE.
 1610           CONTINUE
             ELSE
                CALL SGS_CLRBL(GRIDS(5,NZONERASE),GRIDS(6,NZONERASE),
     :          GRIDS(7,NZONERASE),GRIDS(8,NZONERASE))
                DO 1620 IZ = 0, 100
                   IF( ZONEDEF(IZ) ) THEN
*   Does this clear any new zones ?
                      IF( (GRIDS(5,IZ).GE.GRIDS(5,NZONERASE)) .AND.
     :                (GRIDS(6,IZ).LE.GRIDS(6,NZONERASE)) .AND.
     :                (GRIDS(7,IZ).GE.GRIDS(7,NZONERASE)) .AND.
     :                (GRIDS(8,IZ).LE.GRIDS(8,NZONERASE)) ) THEN
                         ZONECLEAR(IZ) = .TRUE.
                      END IF
                   END IF
 1620           CONTINUE
             END IF
             CALL SGS_FLUSH
             ERTST = .TRUE.
*
*   EW      Measure equivalent widths
*
          ELSE IF( SUBCMD .EQ. 'EW' ) THEN
             IF( NPOINT .EQ. 0 ) THEN
                WRITE (*,'(''   EW:  no data in current arrays'')')
                GO TO 5000
             END IF
             VARRAY(1) = 0.0
             CALL DECODE('EW',PARAMS,0,1,VARRAY,' ',OK)
             IF( .NOT.OK) GO TO 5000
             MODE = NINT(VARRAY(1))
             IF( .NOT.CURSOR .OR. NPLOTS .EQ. 0 ) THEN
                WRITE (*,'(''   EW:  no plot available'')')
                GO TO 5000
             END IF
             IF( LOGWRTE ) THEN
                ALASTXT(1:) = TITLE(1:)
                CALL SSTRIP (ALASTXT)
                ITMP = SLEN(ALASTXT)
*                WRITE (EWUNIT,'('' ''//'' '',A/
*     :          '' '',<ITMP>(''='')/'' '')')
*     :          ALASTXT(1:ITMP)
             END IF
             CALL EWMEAS(EW,DEVTYP,ASIZE1,WAVE,FLUX,NPOINT,MAXBRK,BREAK,
     :       NBREAK,ICFLAG,IHFLAG,NCIH,SIGCIH,ERRCIH,ERR0IH,
     :       SIGEW,WORV,MODE,SUBCHK)
             IF( .NOT.SUBCHK ) THEN
                SUBCHK = .TRUE.
                GO TO 5000
             END IF
*
*   EWERR - Change, or input, systematic errors for 'EW'
*
          ELSE IF( SUBCMD .EQ. 'EWERR' ) THEN
             CALL DECODE('EWERR',PARAMS,2,2,VARRAY,
     :       '%_continuum_error %_zero-level_error ',OK)
             IF( .NOT.(OK)) GO TO 5000
             ERRCIH = VARRAY(1)*0.01
             ERR0IH = VARRAY(2)*0.01
             IHFLAG = 0
             IF( ERRCIH .NE. 0.0 .OR. ERR0IH .NE. 0) IHFLAG = 1
*
*   EWLOG - logs EW measurements to file
*
          ELSE IF( SUBCMD .EQ. 'EWLOG' ) THEN
             CALL DTOUPP(PARAMS)
             CALL SSTRIP(PARAMS)

 1625        CONTINUE
             IF( PARAMS .EQ. ' ' ) THEN

                CALL RDSTR( SUBCMD, 'Option', ' ', PARAMS, STATUS )
                IF( STATUS .NE. SAI__OK ) THEN
                   OK = .FALSE.
                   GO TO 5000
                END IF

                CALL SSTRIP (PARAMS)
                CALL DTOUPP (PARAMS)
                GO TO 1625
             END IF

             LSTRING = INDEX(PARAMS,' ')-1
             IF( PARAMS(1:LSTRING) .EQ. 'OPEN' ) THEN

                IF( LOGFILE ) THEN
                   CALL MSG_SETC( 'FILE', EWLOGFILE )
                   CALL MSGOUT( SUBCMD, '''^FILE'' is already open.',
     :                          .TRUE., STATUS )
                   CALL GET0L( ' ', 1, .FALSE., SUBCMD, 'Close this '//
     :                         'and open a new log file', .TRUE.,
     :                         VAL_OK, STATUS )

                   IF( STATUS .NE. SAI__OK ) THEN
                      OK = .FALSE.
                      GO TO 5000
                   END IF

                   IF( .NOT. VAL_OK ) THEN
                      OK = .FALSE.
                      GO TO 5000
                   END IF

                END IF

                CLOSE (EWUNIT)
                PARAMS(1:) = PARAMS(LSTRING+1:)
                CALL SSTRIP (PARAMS)
                LSTRING = INDEX(PARAMS,' ')-1
                IF( LSTRING .EQ. 0 ) THEN
                   EWLOGFILE(1:) = 'EWLOG.DAT'
                   LSTRING = 9
                ELSE
                   EWLOGFILE(1:) = PARAMS(1:LSTRING)
                END IF
                OPEN (UNIT=EWUNIT,FILE=EWLOGFILE(1:LSTRING),
     :          STATUS='NEW',IOSTAT=IHX)
                IF( IHX .NE. 0 ) THEN
                   WRITE (*,'(''   EWLOG:  error opening '',A)')
     :             EWLOGFILE(1:LSTRING)
                   CLOSE (EWUNIT)
                   LOGFILE = .FALSE.
                   LOGWRTE = .FALSE.
                   OK = .FALSE.
                   GO TO 5000
                END IF
                LOGFILE = .TRUE.
                LOGWRTE = .TRUE.
                PARAMS(1:) = PARAMS(LSTRING+1:)
             ELSE IF( PARAMS(1:LSTRING) .EQ. 'OFF' ) THEN
                PARAMS(1:) = PARAMS(LSTRING+1:)
                IF( .NOT.LOGFILE ) THEN
                   WRITE (*,'(''   EWLOG:  there is no '',
     :             ''log file open'',A)') BLEEP
                   GO TO 5000
                END IF
                IF( .NOT.LOGWRTE ) THEN
                   WRITE (*,'(''   EWLOG:  log file is '',
     :             ''already off'',A)') BLEEP
                END IF
                LOGWRTE = .FALSE.
             ELSE IF( PARAMS(1:LSTRING) .EQ. 'ON' ) THEN
                PARAMS(1:) = PARAMS(LSTRING+1:)
                IF( .NOT.LOGFILE ) THEN
                   WRITE (*,'(''   EWLOG:  there is no '',
     :             ''log file open'',A)') BLEEP
                   GO TO 5000
                END IF
                IF( LOGWRTE ) THEN
                   WRITE (*,'(''   EWLOG:  log file is '',
     :             ''already on'',A)') BLEEP
                END IF
                LOGWRTE = .TRUE.
             ELSE IF( PARAMS(1:LSTRING) .EQ. 'CLOSE' ) THEN
                CLOSE (EWUNIT)
                PARAMS(1:) = PARAMS(LSTRING+1:)
                IF( .NOT.LOGFILE ) THEN
                   WRITE (*,'(''   EWLOG:  there is no '',
     :             ''log file open'',A)') BLEEP
                   GO TO 5000
                END IF
                LOGFILE = .FALSE.
                LOGWRTE = .FALSE.
                WRITE (*,'(''   EWLOG:  closing '',A)')
     :          EWLOGFILE(1:SLEN(EWLOGFILE))
             ELSE IF( PARAMS(1:1) .EQ. '?' .OR.
     :       PARAMS(1:3) .EQ. 'ENQ' ) THEN
                IF( .NOT.LOGFILE ) THEN
                   WRITE (*,'(''   EWLOG:  no file open'')')
                ELSE
                   WRITE (*,'(''   EWLOG:  '',A,'' is open,'')')
     :             EWLOGFILE(1:SLEN(EWLOGFILE))
                   IF( LOGWRTE ) THEN
                      WRITE (*,
     :                '(''           and being written to'')')
                   ELSE
                      WRITE (*,
     :                '(''           and is not being written to'')')
                   END IF
                END IF
                PARAMS(1:) = PARAMS(LSTRING+1:)
             ELSE
                WRITE (*,'(''   EWLOG:  option not recognised'')')
                GO TO 5000
             END IF
             IF( PARAMS .NE. ' ' ) THEN
                WRITE (*,'(''   EWLOG:  '',
     :          ''superfluous parameters ignored'',A)') BLEEP
             END IF

*  EXIT:  Quit program, save stack
          ELSE IF( SUBCMD(1:3) .EQ. 'EXI' ) THEN

*  If ANSI terminal, clear screen, reset window, home cursor
*  (VAX specific)
             IF( ANSI) WRITE (*,'($,''+'',A)') CHAR(27)//'[2J'//CHAR(27)
     :       //'[1;25r'//CHAR(27)//'[H'
             IF( FFOPEN ) THEN
                WRITE (FFUNIT,'('' ''/'' '')')
                CLOSE (FFUNIT)
             END IF

*  Close down plotting
             CALL SGS_CLOSE
             NOTEND = .FALSE.

*  Save exit stack
             IF( USENDF ) THEN
                CALL SAVE( 'EXIT', ' ', STATUS )

             ELSE
                IF( NONSTK .GT. 0 ) THEN
                   ISV1 = 1
                   ISV2 = NONSTK
                   PARAMS = 'EXIT.STK'
                   LLNGTH = 8
                   GO TO 2750
                ELSE
                   WRITE (*,'(''   EXIT:  no data on stack'',A)') BLEEP
                END IF

             END IF

*  EXPAND:  Set expansion factors for various plot components.
          ELSE IF( SUBCMD .EQ. 'EXPAND' ) THEN
             CALL DPEXPAND( 'EXPAND', PARAMS, STATUS )

*  FILL:  Derermines whether extra symbols are filled or empty
          ELSE IF( SUBCMD .EQ. 'FILL' ) THEN
             CALL DECODE('FILL',PARAMS,0,0,VARRAY,' ',OK)
             IF( .NOT.OK) GO TO 5000
             FILL = .TRUE.

*  NOFILL:  Derermines whether extra symbols are filled or empty
          ELSE IF( SUBCMD .EQ. 'NOFILL' ) THEN
             CALL DECODE('NOFILL',PARAMS,0,0,VARRAY,' ',OK)
             IF( .NOT.OK) GO TO 5000
             FILL = .FALSE.
*
*   FONT - select font for text
*
          ELSE IF( SUBCMD .EQ. 'FONT' ) THEN
*   Select character precision and font
             CALL DECODE('FONT',PARAMS,1,1,VARRAY,'FONT ',OK)
             IF( .NOT.OK) GO TO 5000
             IFONT = NINT(VARRAY(1))
             IF( IFONT .EQ. 0 ) THEN
                IPREC = 0
                NTEXT = 0
                CALL AGPWRT(0.0,0.0,' ',0,0,0,-100)
             ELSE IF( IFONT .EQ. 1 ) THEN
                IPREC = 2
                NTEXT = 0
                CALL AGPWRT(0.0,0.0,' ',0,0,0,-100)
             ELSE IF( IFONT .EQ. 2 ) THEN
                IPREC = 2
                NTEXT = 1
                CALL AGPWRT(0.0,0.0,' ',0,0,0,+100)
             ELSE
               WRITE (*,
     :         '(''   FONT:  allowed arguments are 0, 1, and 2 only'')')
               GO TO 5000
             END IF
             IF( .NOT.FIRSTDEVICE ) THEN
                CALL SGS_SPREC(IPREC)
             END IF
*
*   FORMWR  Write data into a formatted file
*
          ELSE IF( SUBCMD .EQ. 'FORMWR' ) THEN
             DO 1640 I = 1, 80
                IF( PARAMS(I:I) .NE. ' ') GO TO 1680
 1640        CONTINUE
 1660        CONTINUE

             CALL RDSTR( SUBCMD, 'File name', ' ', PARAMS, STATUS )
             IF( STATUS .NE. SAI__OK ) THEN
                OK = .FALSE.
                GO TO 5000
             END IF

 1680        CONTINUE
             OPEN (UNIT=55,FILE=PARAMS(1:80),STATUS='NEW',IOSTAT=IX)
             IF( IX .NE. 0 ) THEN
                WRITE (*,'(''   FORMWR:  error opening file'')')
                CLOSE (55)
                GO TO 5000
             ELSE
                WRITE (55,'(A80)',IOSTAT=IX) TITLE
                IF( IX .NE. 0 ) THEN
                   WRITE (*,'(''   FORMWR:  error writing title'')')
                   CLOSE (55)
                   GO TO 5000
                END IF
                JWR = 1
                DO 1690 IWR = 1, NBREAK
                   KWR = BREAK(IWR)
                   WRITE (55,'(''0'')')
                   WRITE (55,'(('' '',3(0PG13.5,1PG13.5,4X)))',
     :             IOSTAT=IX) (WAVE(I),FLUX(I),I=JWR,KWR)
                   JWR = KWR + 1
                   IF( IX .NE. 0 ) THEN
                      WRITE (*,'(''   FORMWR:  error writing data'')')
                      CLOSE (55)
                      GO TO 5000
                   END IF
 1690           CONTINUE
                CLOSE (55)
             END IF
*
*   FLUX   Calculates fluxes wrt linear 'continuum'
*
          ELSE IF( SUBCMD .EQ. 'FLUX' ) THEN
             IF( .NOT.CURSOR .OR. NPLOTS .EQ. 0 ) THEN
                WRITE (*,'(''   FLUX:  no plot available'')')
                GO TO 5000
             END IF

             CALL SSTRIP(PARAMS)
             IF( PARAMS .EQ. '0' ) THEN
                IF( FFOPEN ) THEN
                   WRITE (FFUNIT,'('' ''/'' '')')
                   CLOSE (FFUNIT)
                   FFOPEN = .FALSE.
                   WRITE (*,'(''   FLUX:  closing '',A)') FFILE
                   FFILE = ' '
                ELSE
                   WRITE (*,'(''   FLUX:  no file open'')')
                END IF
             ELSE IF( PARAMS .EQ. ' ' ) THEN
                INQUIRE (UNIT=FFUNIT,OPENED=FFOPEN)
                IF( FFOPEN ) THEN
                   IHCALL = 1
                   WRITE (*,'(''   FLUX:  using '',A)') FFILE
                   WRITE (FFUNIT,'('' ''/'' '',A/'' '')')
     :             TITLE(1:SLEN(TITLE))
                   GO TO 1740
                END IF
             ELSE
                LWORD = SLEN(PARAMS)
                DO 1700 LLWORD = LWORD, 1, -1
                   IF( PARAMS(LLWORD:LLWORD) .EQ. ']') GO TO 1710
                   IF( PARAMS(LLWORD:LLWORD) .EQ. '.') GO TO 1720
 1700           CONTINUE
 1710           CONTINUE
                PARAMS(LWORD+1:LWORD+4) = '.DAT'
                LWORD = LWORD + 4
 1720           CONTINUE
                IF( FFOPEN ) THEN
                   WRITE (*,'(''   FLUX:  closing '',A)') FFILE
                   FFOPEN = .FALSE.
                   WRITE (FFUNIT,'('' ''/'' '')')
                   CLOSE (FFUNIT)
                   FFILE = ' '
                END IF
                IHHEAD = ' '
                IHHEAD(1:) = 'KEEP'
*
*   For DJM (remove for Starlink)
*
!*
!               IF( USERID(1:3) .EQ. 'DJM' ) THEN
!                  WRITE (*,'(''   FLUX:  Do you want to keep '',
!    :             ''the file (n):'',$)')
!*
 1725              CONTINUE
!*
!                  IHHEAD = ' '
!                  READ (5,'(A5)',IOSTAT=IX) IHHEAD
!                  IF( IX .NE. 0 ) THEN
!                     WRITE (*,'(''   What??   Try again (n):'',$)')
!                     GO TO 1725
!                  END IF
!                  CALL DTOUPP(IHHEAD)
!                  CALL SSTRIP(IHHEAD)
!                  IF( IHHEAD(1:1) .EQ. ' ') IHHEAD(1:1) = 'N'
!                  IF( IHHEAD(1:1) .EQ. 'Y' ) THEN
!                     IHL = 4
!                     IHHEAD(1:IHL) = 'KEEP'
!                  ELSE IF( IHHEAD(1:1) .EQ. 'N' ) THEN
!                     IHL = 6
!                     IHHEAD(1:IHL) = 'DELETE'
!                  ELSE
!                     WRITE (*,'(''   Answer y/n (n):'',$)')
!                     GO TO 1725
!                  END IF
!               ELSE
!*
                   IHL = 4
                   IHHEAD(1:IHL) = 'KEEP'
!*
!               END IF
!*

                OPEN (UNIT=FFUNIT,FILE=PARAMS(1:LWORD),STATUS='NEW',
     :          IOSTAT=IX)
                IF( IX .NE. 0 ) THEN
                   CLOSE (FFUNIT)
                   WRITE (*,'(''   FLUX:  unable to open '',A)')
     :             PARAMS(1:LWORD)
                   FFILE = ' '
                   FFOPEN = .FALSE.
                   GO TO 5000
                ELSE
                   FFOPEN = .TRUE.
                   FFILE = PARAMS(1:LWORD)
                   CALL SSTRIP(FFILE)
*
*   Write to Dave Monk's COMMON area
*
!*
!                  MONK = 0
!                  DO 1730 MONKI = 1, 100
!                     MONKW(MONKI) = 0.0
!                     MONKF(MONKI) = 0.0
!                     MONKW1(MONKI) = 0.0
!                     MONKW2(MONKI) = 0.0
!                     MONKQ(MONKI) = 0.0
!                     MONKFD(MONKI) = 0.0
!                     MONKFL(MONKI) = 0.0
!                     MONKFS(MONKI) = 0.0
!                     MONKIS(MONKI) = 0.0
!                     MONKIO(MONKI) = ' '
!*
 1730              CONTINUE

                END IF
             END IF
             IHCALL = 0
 1740        CONTINUE
             IHCALL = IHCALL + 1
             CALL CPAIR(X1,Y1,X2,Y2)

             YMEAN = (Y1+Y2)*0.5
             DO 1760 I = 1, NPOINT
                I1 = I
                IF( WAVE(I).GT.X1) GO TO 1780
 1760        CONTINUE
 1780        CONTINUE
             DO 1800 I = I1, NPOINT
                IF( WAVE(I).GT.X2) GO TO 1820
                I2 = I
 1800        CONTINUE
 1820        CONTINUE
             IF( WAVE(I2).GT.WAVE(I1) .AND. X2 .NE. X1 ) THEN
                X0 = X1
                Y0 = Y1
                SLOPE = (Y2-Y1)/(X2-X1)
                X1 = WAVE(I1)
                Y1 = FLUX(I1) - (Y0+SLOPE*(X1-X0))
                FLXEW1 = FLUX(I1)/(Y0+SLOPE*(X1-X0))
                FFLX = 0.
                FLXEW = 0.
                DO 1830 I = I1 + 1, I2
                   X2 = WAVE(I)
                   Y2 = FLUX(I) - (Y0+SLOPE*(X2-X0))
                   FLXEW2 = FLUX(I)/(Y0+SLOPE*(X2-X0))
                   FFLX = FFLX + (X2-X1)*(Y2+Y1)/2.0
                   FLXEW = FLXEW + (X2-X1)*((FLXEW1+FLXEW2)*0.5-1.0)
                   X1 = X2
                   Y1 = Y2
                   FLXEW1 = FLXEW2
 1830           CONTINUE
                IF( WORV .NE. 0.0 ) THEN
                   FFLX = FFLX*WORV
                END IF

*
                IF( IHCALL .EQ. 1 ) THEN
                   IF( FFOPEN ) THEN
                      WRITE (FFUNIT,'('' ''/'' '',A/'' ''/
     :                ''       X1           X2         XMEAN '',
     :                ''        FLUX    EMISSION E.W.''/'' '')')
     :                TITLE(1:SLEN(TITLE))
                      IF( ABS(X2).LT.100000.0 ) THEN
                         WRITE (FFUNIT,
     :                   '('' '',3(F10.2,3X),1P,2(E12.5,2X))')
     :                   X0, X2, (0.5*(X0+X2)), FFLX, FLXEW
                      ELSE
                         WRITE (FFUNIT,'('' '',5(1P,E11.4,2X))')
     :                   X0, X2, (0.5*(X0+X2)), FFLX, FLXEW
                      END IF
*
*   More stuff for Dave's MONK Common
*
!*
!                     MONK = MONK + 1
!                     MONKW(MIN(MONK,100)) = 0.5*(X0+X2)
!                     MONKW1(MIN(MONK,100)) = X0
!                     MONKW2(MIN(MONK,100)) = X2
!                     MONKF(MIN(MONK,100)) = FFLX
!                     IF( ITMP .NE. 0 ) THEN
!                        MONKQ(MIN(MONK,100)) = -1
!                     ELSE
!                        MONKQ(MIN(MONK,100)) = 1
!                     END IF
!*
                   END IF
                   IF( ABS(X2).LT.100000.0 ) THEN
                      WRITE (*,
     :                '(''   FLUX: '',1P,E12.5/
     :                ''   (X1, X2:'',0P,2F10.2,'')'')')
     :                FFLX, X0, X2
                   ELSE
                      WRITE (*,
     :                '(''   FLUX: '',1P,E12.5/
     :                ''   (X1, X2:'',1P,2E11.4,'')'')') FFLX, X0, X2
                   END IF
                ELSE
                   IF( FFOPEN ) THEN
                      IF( ABS(X2).LT.100000.0 ) THEN
                         WRITE (FFUNIT,
     :                   '('' '',3(F10.2,3X),1P,2(E12.5,2X))')
     :                   X0, X2, (0.5*(X0+X2)), FFLX, FLXEW
                      ELSE
                         WRITE (FFUNIT,'('' '',5(1P,E11.4,2X))')
     :                   X0, X2, (0.5*(X0+X2)), FFLX, FLXEW
                      END IF
*
*   Even more MONK stuff....
*
!*
!                     MONK = MONK + 1
!                     MONKW(MIN(100,MONK)) = 0.5*(X0+X2)
!                     MONKW1(MIN(MONK,100)) = X0
!                     MONKW2(MIN(MONK,100)) = X2
!                     MONKF(MIN(100,MONK)) = FFLX
!                     IF( ITMP .NE. 0 ) THEN
!                        MONKQ(MIN(MONK,100)) = -1
!                     ELSE
!                        MONKQ(MIN(MONK,100)) = 1
!                     END IF
!*

                   END IF
                   IF( ABS(X2).LT.100000.0 ) THEN
                      WRITE (*,'(''   FLUX: '',1PE12.5/
     :                ''   (X1, X2:'',0P2F10.2,'')'')')
     :                FFLX, X0, X2
                   ELSE
                      WRITE (*,'(''   FLUX: '',1PE12.5/
     :                ''   (X1, X2:'',1P2E11.4,'')'')')
     :                FFLX, X0, X2
                   END IF
                END IF
*
                ITMP = 0
                DO 1840 I = 1, NBREAK
                   IBRK = BREAK(I)
                   IF( IBRK.GE.I1 .AND. IBRK.LT.I2 ) THEN
                      ITMP = ITMP + 1
                   END IF
 1840           CONTINUE
                IF( ITMP .NE. 0 ) THEN
                   WRITE (*,
     :             '(''   WARNING - linear interpolation used!'')')
*
                END IF
                GO TO 1740
             END IF
*
*   (End of MONK COMMON FLUX area extensions)
*

*
* FRAME Defines size & position of total plotting area available
*
*        Check xsize,ysize>0 & <current device size ??
          ELSE IF( SUBCMD .EQ. 'FRAME' ) THEN
             VARRAY(3) = 5.0
             CALL DECODE('FRAME',PARAMS,3,2,VARRAY,
     :       'Xsize(cm) Ysize(cm) Pos(1-9) ',OK)
             IF( .NOT.OK) GO TO 5000
             IF( (VARRAY(1).LT.1.0) .OR. (VARRAY(2).LT.1.0) ) THEN
                WRITE (*,
     :          '(''   FRAME:  linear dimensions are too small'',A)')
     :          BLEEP
                GO TO 5000
             END IF
             XGSIZE = VARRAY(1)*0.01
             YGSIZE = VARRAY(2)*0.01
             IGPOS = NINT(VARRAY(3))
             IF( (IGPOS.LT.1).OR.(IGPOS.GT.9) ) THEN
                WRITE (*,
     :          '(''   FRAME:  position index must be 1-9'',A)') BLEEP
                GO TO 5000
             END IF
             FRZONE=.FALSE.
* For the benefit of people who can't cope with 'TL' for top left etc.
             IF( IGPOS .EQ. 7)THEN
                GPOS='TL'
             ELSE IF( IGPOS .EQ. 8)THEN
                GPOS='TC'
             ELSE IF( IGPOS .EQ. 9)THEN
                GPOS='TR'
             ELSE IF( IGPOS .EQ. 4)THEN
                GPOS='CL'
             ELSE IF( IGPOS .EQ. 5)THEN
                GPOS='CC'
             ELSE IF( IGPOS .EQ. 6)THEN
                GPOS='CR'
             ELSE IF( IGPOS .EQ. 1)THEN
                GPOS='BL'
             ELSE IF( IGPOS .EQ. 2)THEN
                GPOS='BC'
             ELSE IF( IGPOS .EQ. 3)THEN
                GPOS='BR'
             END IF
             REALSIZE=.TRUE.
             ZNTST = .TRUE.
*
* FRZONE Define Frame within FRAME
*
          ELSE IF( SUBCMD .EQ. 'FRZONE' ) THEN
             CALL DECODE('FRZONE',PARAMS,4,4,GRIDP,
     :       'X(min) X(max) Y(min) Y(max) ',OK)
             IF( .NOT.OK) GO TO 5000
             IF( OK ) THEN
                GRIDP(5)=0.0
                FRZONE=.TRUE.
             END IF
             ZNTST = .TRUE.
*
*   GRID  sets up a wavelength GRID containing dummy Y values
*
          ELSE IF( SUBCMD .EQ. 'GRID' ) THEN
             CALL DECODE('GRID',PARAMS,3,3,VARRAY,'X1 X2 DX ',OK)
             IF( .NOT.OK) GO TO 5000
             X1 = VARRAY(1)
             X2 = VARRAY(2)
             DX = VARRAY(3)
             IF( DX .NE. 0.0 ) THEN
                NNEW = X1 - X2
                NNEW = ABS(NNEW/DX) + 1
             ELSE
                NNEW = ASIZE1 + 100
             END IF
             IF( NNEW.GT.ASIZE1 ) THEN
                WRITE (*,'(''   GRID:  too many points'',I7)') NNEW
                OK = .FALSE.
                GO TO 5000
             END IF
             IF( DX.LT.0.0) DX = -DX
             TEMP = MIN(X1,X2)
             X2 = MAX(X1,X2)
             X1 = TEMP
             WAVE(1) = X1
             FLUX(1) = 0.0
             I = 1
             NPOINT = 1
             DO 1860 I = 2, ASIZE1
                WAVE(I) = X1 + DX*(I-1)
                IF( WAVE(I).GT.X2) GO TO 1880
                NPOINT = I
                FLUX(I) = 0.0
 1860        CONTINUE
 1880        CONTINUE
             NBREAK = 1
             BREAK(NBREAK) = NPOINT
             WORV = 1.0
             TITLE = ' GRID data'
*
*   GRIDSTYLE  defines
*
          ELSE IF( SUBCMD .EQ. 'GRIDSTYLE' ) THEN
             CALL DECODE('GRIDSTYLE',PARAMS,1,1,VARRAY,'Grid_Style ',OK)
             IF( .NOT.(OK)) GO TO 5000
             ITEMP = NINT(VARRAY(1))
             IF( ITEMP.LT.1 .OR. ITEMP.GT.5 ) THEN
                WRITE (*,
     :          '(''   GRIDSTYLE:  style must be in the range 1-5'',A)')
     :          BLEEP
                GO TO 5000
             END IF
             IGRIDSTYLE = ITEMP

*  HANDLER:  Turns condition handler on/off
          ELSE IF( SUBCMD .EQ. 'HANDLER' ) THEN
             CALL GET0I( PARAMS, 1, .FALSE., 'HANDLER',
     :                   'Signal handler level', 1, LEVEL, STATUS )
             CALL HANDLEV( LEVEL, STATUS )

*  HCol:  Calculate theoretical Lyman-alpha profile
          ELSE IF( SUBCMD .EQ. 'HC' ) THEN
             VARRAY(2) = HCMXFC
             CALL DECODE('HC',PARAMS,1,2,VARRAY,'Column ',OK)
             IF( .NOT.(OK)) GO TO 5000
             HCMXFC = VARRAY(2)
             IF( VARRAY(1).LE.30.0) VARRAY(1) = 10.0**VARRAY(1)
             CALL LALPHA(VARRAY(1),HCMXFC,WAVE,FLUX,NPOINT,BREAK,NBREAK)
*
*   HELP    Give help on available commands
*
          ELSE IF( SUBCMD .EQ. 'HELP' ) THEN
 1900        CONTINUE

             CALL XHELP( SHOWME, PARAMS, USEHTX, STATUS )
             IF( .NOT. USEHTX ) THEN

C ----------------------------------------------------------------
             CALL SSTRIP(PARAMS)
             CALL DTOUPP(PARAMS)
             LLNGTH = INDEX(PARAMS,' ') - 1
             IF( LLNGTH.LE.0 ) THEN
                CALL HELP1
             ELSE
                DO 1910 I = 100, 1, -1
                   IF( PARAMS(I:I) .NE. ' ') GO TO 1920
 1910           CONTINUE
                I = 0
 1920           CONTINUE
                IEND = I
                IF( IEND.GT.LLNGTH ) THEN
                   WRITE (*,
     :             '(''   HELP:  superfluous parameters ignored'',A)')
     :             BLEEP
                END IF
                IF( PARAMS(1:LLNGTH) .EQ. 'COMRD' ) THEN
                   LLNGTH = 1
                   PARAMS(1:LLNGTH) = '@'
                   WRITE (*,'(''   HELP - looking for "@" help'',A)')
     :             BLEEP
                ELSE IF( PARAMS(1:LLNGTH) .EQ. '?' ) THEN
                   LLNGTH = 4
                   PARAMS(1:LLNGTH) = 'HELP'
                ELSE IF( PARAMS(1:LLNGTH) .EQ. 'Q' .OR. PARAMS(1:LLNGTH)
     :           .EQ. 'QU' .OR. PARAMS(1:LLNGTH) .EQ. 'QUIT' ) THEN
                   LLNGTH = 4
                   PARAMS(1:LLNGTH) = 'Q(U)'
                END IF
*
                OPEN (UNIT=77,
     :             FILE=prefix(1:plen)//'help.lis',
     :          STATUS='OLD',IOSTAT=IHX)
                IF( IHX .NE. 0 ) THEN
                   WRITE (*,'(''   HELP:  unable to open file'')')
                ELSE
                   DO 1925 I = 1, 100000
                      IHHLP = ' '
                      READ (77,'(A80)',END=1930) IHHLP(1:80)
                      MLNGTH = INDEX(IHHLP,' ')
                      IF( IHHLP(1:1) .EQ. '@' ) THEN
                         MLNGTH = LLNGTH
                      END IF
                      IF( IHHLP(1:MLNGTH) .EQ. PARAMS(1:LLNGTH) ) THEN
                         WRITE (*,'(''   HELP: '',A)') IHHLP(1:70)
                         WRITE (*,'(''     '')')
                         DO 1922 II = 1, 100000
                            READ (77,'(A80)',END=1940) PARAMS(1:80)
                            IF( PARAMS(1:3) .NE. ' ' ) THEN
                               WRITE (*,'(24X,''   -------'')')
                               GO TO 1940
                            END IF
                            IF( MOD(II,20) .EQ. 0 ) THEN
                               CALL MSG_BLANK( STATUS )
                               CALL GET0L( ' ', 1, .FALSE., 'HELP',
     :                                     'Continue', .TRUE., VAL_OK,
     :                                     STATUS )
                               CALL MSG_BLANK( STATUS )
                               IF( STATUS .NE. SAI__OK ) THEN
                                  OK = .FALSE.
                                  GO TO 5000
                               END IF

                               IF( .NOT. VAL_OK ) GO TO 1940
                               WRITE (*,'('' '',A)') PARAMS(81:159)

                            END IF

                            WRITE (*,'('' '',A79)') PARAMS(1:79)
                            PARAMS(81:160) = PARAMS(1:80)
 1922                    CONTINUE
                         GO TO 1940
                      END IF
 1925              CONTINUE
 1930              CONTINUE
                END IF
*
*   Look for help in LDIPSODIR if unsuccessful
*
                CLOSE (77)
                IF( p4len .GT. 0 ) THEN
                OPEN (UNIT=77,
     :            FILE=prefix4(1:p4len)//'lhelp.lis',
     :          STATUS='OLD',IOSTAT=IHX)
                ELSE
                OPEN (UNIT=77,
     :            FILE='lhelp.lis',
     :          STATUS='OLD',IOSTAT=IHX)
                END IF
                IF( IHX .NE. 0 ) THEN
                   GO TO 1938
                ELSE
                   DO 1935 I = 1, 100000
                      IHHLP = ' '
                      READ (77,'(A80)',END=1938) IHHLP(1:80)
                      MLNGTH = INDEX(IHHLP,' ')
                      IF( IHHLP(1:1) .EQ. '@' ) THEN
                         MLNGTH = LLNGTH
                      END IF
                      IF( IHHLP(1:MLNGTH) .EQ. PARAMS(1:LLNGTH) ) THEN
                         WRITE (*,'(''   HELP: '',A)') IHHLP(1:70)
                         WRITE (*,'(''     '')')
                         DO 1932 II = 1, 100000
                            READ (77,'(A80)',END=1940) PARAMS(1:80)
                            IF( PARAMS(1:3) .NE. ' ' ) THEN
                               WRITE (*,'(24X,''   -------'')')
                               GO TO 1940
                            END IF
                            IF( MOD(II,20) .EQ. 0 ) THEN

                               CALL GET0L( ' ', 1, .FALSE., 'HELP',
     :                                     'Continue', .TRUE., VAL_OK,
     :                                     STATUS )
                               IF( STATUS .NE. SAI__OK ) THEN
                                  OK = .FALSE.
                                  GO TO 5000
                               END IF

                               IF( .NOT. VAL_OK ) GO TO 1940
                               WRITE (*,'('' '',A)') PARAMS(81:159)

                            END IF

                            WRITE (*,'('' '',A79)') PARAMS(1:79)
                            PARAMS(81:160) = PARAMS(1:80)
 1932                    CONTINUE
                         GO TO 1940
                      END IF
 1935              CONTINUE
                END IF
 1938           CONTINUE
                WRITE (*,'(''   HELP:  command not found'')')
 1940           CONTINUE
                CLOSE (77)
             END IF

C ----------------------------------------------------------------
             END IF
*
*   HIST    Plots to be done in histogram form
*
          ELSE IF( SUBCMD .EQ. 'HIST' ) THEN
             CALL DECODE('HIST',PARAMS,0,0,VARRAY,' ',OK)
             CALL SETHIS
             POLTST = .FALSE.
             HSTTST = .TRUE.
             MRKTST = .FALSE.
             NXPLTS = 0
*
*   HPROT    Rotate between HIST and POLY
*
          ELSE IF( SUBCMD .EQ. 'HPROT' ) THEN
             VARRAY(1) = 0.0
             CALL DECODE('HPROT',PARAMS,0,1,VARRAY,' ',OK)
             HPROT = .TRUE.
             IF( VARRAY(1) .NE. 0.0) HPSTAY = 1
*
*   INTEGRATE  Trapezoidal integration of data
*
          ELSE IF( SUBCMD .EQ. 'INTEGRATE' ) THEN
             IF( NPOINT.LE.1 ) THEN
                WRITE (*,'(''   INTEGRATE:  no data to integrate'',
     :          A)') BLEEP
                GO TO 5000
             END IF
             CALL DECODE('INTEGRATE',PARAMS,0,0,VARRAY,' ',OK)
             CALL INTEGRATE(NPOINT,WAVE,FLUX,VARRAY(1))
             WRITE (*,'(''   INTEGRATE:  sum ='',1PE12.5)') VARRAY(1)
*
*   IUECOR  corrects for aging in IUE sensitivity
*
          ELSE IF( SUBCMD .EQ. 'IUECOR' ) THEN
             IF( NPOINT.LT.1 ) THEN
                WRITE (*,'(''   IUECOR:  no data in current arrays'',
     :          A)') BLEEP
                OK = .FALSE.
                GO TO 500
             END IF
             VARRAY(4) = 3.0
             CALL DECODE (SUBCMD, PARAMS, 3, 4, VARRAY,
     :       'Camera Year Day ',OK)
             IF( OK ) THEN
                IYR = NINT(VARRAY(2))
                IF( IYR.GT.1900) IYR = IYR - 1900
                IF( (IYR.GE.78) .AND. (IYR.LE.89) ) THEN
                   IF( (VARRAY(3).GE.1.0) .AND.
     :             (VARRAY(3).LE.367.0) ) THEN
                      IF( 4*(IYR/4) .EQ. IYR ) THEN
                         EPOCH = REAL(IYR) + (VARRAY(3)-1.0)/366.0
                      ELSE
                         EPOCH = REAL(IYR) + (VARRAY(3)-1.0)/365.0
                      END IF
                      IAPER = NINT(VARRAY(4))
                      IF( (IAPER.GE.1) .AND. (IAPER.LE.3) ) THEN
                         ICAM = NINT(VARRAY(1))
                         IF( ICAM .EQ. 1 ) THEN
                            WRITE (*,'(''   IUECOR:  no corrections '',
     :                      ''available for LWP camera'',A)') BLEEP
                            OK = .FALSE.
                         ELSE IF( ICAM .EQ. 2 ) THEN
                            CALL LWRCOR
     :                      (NPOINT, EPOCH, IAPER, WAVE, FLUX, OK)
                         ELSE IF( ICAM .EQ. 3 ) THEN
                            CALL SWPCOR
     :                      (NPOINT, EPOCH, IAPER, WAVE, FLUX, OK)
                         ELSE IF( ICAM .EQ. 4 ) THEN
                            WRITE (*,'(''   IUECOR:  no corrections '',
     :                      ''available for SWR camera'',A)') BLEEP
                            OK = .FALSE.
                         ELSE
                            WRITE (*,
     :                      '(''   IUECOR:  invalid camera '',
     :                      ''(must be 1-4)'',A)') BLEEP
                            OK = .FALSE.
                         END IF
                      ELSE
                         WRITE (*,
     :                   '(''   IUECOR:  invalid aperture '',
     :                   ''(must be 1-3)'',A)') BLEEP
                         OK = .FALSE.
                      END IF
                   ELSE
                      WRITE (*,
     :                '(''   IUECOR:  invalid day number'',A)') BLEEP
                      OK = .FALSE.
                   END IF
                ELSE
                   WRITE (*,'(''   IUECOR:  year is outside '',
     :             ''permitted range (1978-89)'',A)') BLEEP
                   OK = .FALSE.
                END IF
             END IF
*
*   XINV    Replaces X values with 1/X values
*
          ELSE IF( SUBCMD .EQ. 'INVX' .OR. SUBCMD .EQ. 'XINV' ) THEN
             IF( SUBCMD .EQ. 'INVX' ) THEN
                WRITE (*,'(''   INVX command renamed XINV'',A)') BLEEP
                SUBCMD = 'XINV'
                WRITE (*,'(''   (command executed)'')')
             END IF
             CALL DECODE('XINV',PARAMS,0,0,VARRAY,' ',OK)
             IF( NPOINT.LT.1 ) THEN
                WRITE (*,'(''   XINV:  no points!'',A)') BLEEP
             ELSE
                DO 1950 I = 1, NPOINT
                   WAVE(I) = 1.0/WAVE(I)
 1950           CONTINUE
             END IF
*
*   YINV   Replaces Y values with 1/Y values
*
          ELSE IF( SUBCMD .EQ. 'INVY' .OR. SUBCMD .EQ. 'YINV' ) THEN
             IF( SUBCMD .EQ. 'INVY' ) THEN
                WRITE (*,'(''   INVY command renamed YINV'',A)') BLEEP
                SUBCMD = 'YINV'
                WRITE (*,'(''   (command executed)'')')
             END IF
             CALL DECODE('YINV',PARAMS,0,0,VARRAY,' ',OK)
             IF( NPOINT.LT.1 ) THEN
                WRITE (*,'(''   YINV:  no points!'',A)') BLEEP
             ELSE
                DO 1960 I = 1, NPOINT
                   FLUX(I) = 1.0/FLUX(I)
 1960           CONTINUE
             END IF
*
*   INTERSTELLAR OPTIONS
*
          ELSE IF( (SUBCMD .EQ. 'ISATM') .OR. (SUBCMD .EQ. 'ISCALC')
     :             .OR. (SUBCMD .EQ. 'ISCOG') .OR. (SUBCMD .EQ. 'ISINP')
     :             .OR. (SUBCMD .EQ. 'ISOPT') ) THEN
             I = WORKSZ/8
             ISTST = ISBACH(SUBCMD,PARAMS,OK,WORKSZ,MAXSTK,NONSTK,
     :       WORK(1),WORK(I+1),WORK(2*I+1),WORK(3*I+1),
     :       WORK(4*I+1),WORK(5*I+1),WORK(6*I+1),WORK(7*I+1),STATUS)
             IF( .NOT.OK) GO TO 5000
*
*   LABON - switch on graph-labelling
*
          ELSE IF( SUBCMD .EQ. 'LABON' ) THEN
             CALL DECODE('LABON',PARAMS,0,1,VARRAY,' ',OK)
             LABELFLAG = .TRUE.
*
*   LBLFIT  -  profile fitting to IUEDR Line-By-Line spectrum
*
          ELSE IF( SUBCMD .EQ. 'LBLFIT' ) THEN
             IF( .NOT.CURSOR ) THEN
                WRITE (*,'(''   LBLFIT:  '',
     :          ''interactive plotting device required'')')
                GO TO 5000
             END IF
             IF( NONSTK.GE.MAXSTK ) THEN
                WRITE (*,
     :          '(''   LBLFIT:  no STACK space to store results'')')
                GO TO 5000
             END IF
             CALL SSTRIP(PARAMS)
             LLNGTH = INDEX(PARAMS,' ') - 1
             IF( LLNGTH.LE.0 ) THEN
 1970           CONTINUE

                CALL RDSTR( SUBCMD, 'File name', ' ', PARAMS, STATUS )
                IF( STATUS .NE. SAI__OK ) THEN
                   OK = .FALSE.
                   GO TO 5000
                END IF

                CALL SSTRIP(PARAMS)
                LLNGTH = INDEX(PARAMS,' ') - 1
                IF( LLNGTH .EQ. 0) GO TO 1970
             END IF
             CALL DEFTYP(PARAMS,'LBL',OK)
             LLNGTH = MAX(50,SLEN(PARAMS))
             SUBCHK = .TRUE.
             MEMDEV = DEVTYP
             DEFIN(1) = XLAB
             DEFIN(2) = YLAB
             MEMXLN = XLABLN
             MEMYLN = YLABLN
             MEMZONE = NZONEN
             CALL LBLRUN
     :       (WORKSZ,STKSZE,MAXSTK,NONSTK,BSTSZE,
     :       XSTACK,YSTACK,BSTACK,BSTNPT,POINTR,STKNPT,
     :       BPOINT,STITLE,WORK,PARAMS(1:LLNGTH),DEVTYP,SUBCHK)

             NPLOTS = 0
             DVTST = .TRUE.
             TRIMY = .TRUE.
             XLABLN = MEMXLN
             YLABLN = MEMYLN
             NZONEN = MEMZONE
             IF( COLOUR) CALL PPALET(IPAL)
             DO 1980 I = 1, 8
                GRID(I) = GRIDS(I,NZONEN)
 1980        CONTINUE
             ZNTST = .TRUE.
             XLAB = DEFIN(1)(1:XLABLN)
             YLAB = DEFIN(2)(1:YLABLN)
             TRIMX = .TRUE.
             DEVTYP = MEMDEV
             IF( DVTST) CALL GSLN(LNTYPE)
             IF( POLTST) CALL SETPOL
             IF( HSTTST) CALL SETHIS
             IF( MRKTST) CALL SETMAR
             CALL NXY
             IF( XRKEEP(1) .NE. -321.654) CALL XMINST(XRKEEP(1))
             IF( XRKEEP(2) .NE. -321.654) CALL XMAXST(XRKEEP(2))
             IF( YRKEEP(1) .NE. -321.654) CALL YMINST(YRKEEP(1))
             IF( YRKEEP(2) .NE. -321.654) CALL YMAXST(YRKEEP(2))
             IF( .NOT.SUBCHK ) THEN
                SUBCHK = .TRUE.
                GO TO 5000
             END IF
*
*   LIST  -  List 'current' contents at terminal
*
          ELSE IF( SUBCMD .EQ. 'LIST' ) THEN
             VARRAY(1) = REAL(NPOINT)/20.0 + 1.0
             CALL DECODE('LIST',PARAMS,0,1,VARRAY,' ',OK)
             IF( .NOT.OK) GO TO 5000
             ISKIP = NINT(VARRAY(1))
             WRITE (*,'(''   LIST - break points:''/(5I10))')
     :       (BREAK(I),I=1,NBREAK)
             WRITE (*,'(0P,I10,1P,E15.3,1P,E15.5)')
     :       (I,WAVE(I),FLUX(I),I=1,NPOINT-1,ISKIP)
             WRITE (*,'(0P,I10,1P,E15.3,1P,E15.5)')
     :       NPOINT,WAVE(NPOINT),FLUX(NPOINT)

*  LOGAXX:  X axis in log space?
          ELSE IF( SUBCMD .EQ. 'LOGAXX' ) THEN
             CALL GET0L( PARAMS, 1, .FALSE., SUBCMD, 'Plot X on a '//
     :                   'log10 scale', .FALSE., LOGAXX, STATUS )

*  LOGAXY:  Y axis in log space?
          ELSE IF( SUBCMD .EQ. 'LOGAXY' ) THEN
             CALL GET0L( PARAMS, 1, .FALSE., SUBCMD, 'Plot Y on a '//
     :                   'log10 scale', .FALSE., LOGAXY, STATUS )

*  LOGX:  Convert 'X' array to log10 values
          ELSE IF( SUBCMD .EQ. 'LOGX' .OR. SUBCMD .EQ. 'XLOG' ) THEN
             IF( SUBCMD .EQ. 'XLOG' ) THEN
                WRITE (*,'(''   XLOG command renamed LOGX'',A)') BLEEP
                SUBCMD = 'LOGX'
                WRITE (*,'(''   (command executed)'')')
             END IF
             CALL DECODE('LOGX',PARAMS,0,0,VARRAY,' ',OK)
             IF( NPOINT.LT.1 ) THEN
                WRITE (*,'(''   LOGX:  no points!'',A)') BLEEP
             ELSE
                ITMP = 0
                DO 1990 I = 1, NPOINT
                   WVI = WAVE(I)
                   IF( WVI.GT.0.0 ) THEN
                      WVI = LOG10(MAX(1e-30,WVI))
                   ELSE
                      ITMP = ITMP + 1
                   END IF
                   WORK(I) = WVI
 1990           CONTINUE
                IF( ITMP .EQ. 0 ) THEN
                   DO 1995 I = 1, NPOINT
                      WAVE(I) = WORK(I)
 1995              CONTINUE
                ELSE
                   WRITE (*,'(''   LOGX:'',I5,
     :             '' negative values - no logs taken'')') ITMP
                END IF
             END IF
*
*   LOGY    Convert 'Y' array to log10 values
*
          ELSE IF( SUBCMD .EQ. 'LOGY' .OR. SUBCMD .EQ. 'YLOG' ) THEN
             IF( SUBCMD .EQ. 'YLOG' ) THEN
                WRITE (*,'(''   YLOG command renamed LOGY'',A)') BLEEP
                SUBCMD = 'LOGY'
                WRITE (*,'(''   (command executed)'')')
             END IF
             CALL DECODE('LOGY',PARAMS,0,0,VARRAY,' ',OK)
             IF( NPOINT.LT.1 ) THEN
                WRITE (*,'(''   LOGY:  no data!'',A)') BLEEP
             ELSE
                IBRK = 1
                JBRK = 0
                ITMP = 0
                ICHK = 1
                J = 0
                IF( BREAK(1) .EQ. 0) IBRK = 2
                DO 2000 I = 1, NPOINT
                   IF( JBRK.LT.MAXBRK ) THEN
                      FLXI = FLUX(I)
                      IF( FLXI.GT.0.0 ) THEN
                         J = J + 1
                         FLUX(J) = LOG10(MAX(1e-30,FLXI))
                         WAVE(J) = WAVE(I)
                         IF( I .EQ. BREAK(IBRK) ) THEN
                            JBRK = JBRK + 1
                            IBRK = IBRK + 1
                            IWORK(JBRK) = J
                         END IF
                         ICHK = 1
                      ELSE
                         ITMP = ITMP + 1
                         IF( I .EQ. BREAK(IBRK)) IBRK = IBRK + 1
                         IF( ICHK .NE. 0 ) THEN
                            ICHK = 0
                            JBRK = JBRK + 1
                            IWORK(JBRK) = J
                         END IF
                      END IF
                   ELSE
                      WRITE (*,'(''   LOGY:  maximum number of '',
     :                ''breaks exceeded'')')
                      GO TO 2010
                   END IF
 2000           CONTINUE
 2010           CONTINUE
                NPOINT = J
                NBREAK = JBRK
                DO 2020 I = 1, NBREAK
                   BREAK(I) = IWORK(I)
 2020           CONTINUE
                IF( NBREAK .EQ. 0 .OR. BREAK(NBREAK) .NE. NPOINT ) THEN
                   NBREAK = NBREAK + 1
                   BREAK(NBREAK) = NPOINT
                END IF
                IF( ITMP .NE. 0 ) THEN
                   WRITE (*,'(''   LOGY:'',I6,'' negative points '',
     :             ''thrown away'')') ITMP
                END IF
             END IF
*
*   LWEIGHT - increases line `weight' (on appropriate devices)
*
          ELSE IF( SUBCMD .EQ. 'LWEIGHT' ) THEN
             CALL DECODE('LWEIGHT',PARAMS,1,1,VARRAY,'Weight ',OK)
             IF( .NOT.OK) GO TO 5000
             IF( NINT(VARRAY(1)).LT.1 .OR. NINT(VARRAY(1)).GT.5 ) THEN
                WRITE (*,'(''   LWEIGHT:  weight must be 1-5'',A)')
     :          BLEEP
             ELSE
                XLWIDTH = VARRAY(1)
                LNWIDTH = VARRAY(1)
             END IF
*
*   MARK    Plots to be done with symbols
*
          ELSE IF( SUBCMD .EQ. 'MARK' ) THEN
             CALL DECODE('MARK',PARAMS,0,0,MARK,' ',OK)
             IF( .NOT.(OK)) GO TO 5000
             POLTST = .FALSE.
             HSTTST = .FALSE.
             CALL SETMAR
*
*   AMAX    Finds maximum of current and stack entry Y values
*
          ELSE IF( SUBCMD .EQ. 'MAX' .OR. SUBCMD .EQ. 'AMAX' ) THEN
             IF( SUBCMD .EQ. 'MAX' ) THEN
                WRITE (*,'(''   MAX command renamed AMAX '',
     :          ''(correct call mandatory)'',A)') BLEEP

                GO TO 5000
             END IF
             CALL DECODE('AMAX',PARAMS,1,1,VARRAY,'Entry ',OK)
             IF( .NOT.OK) GO TO 5000
             NUMENT = NINT(VARRAY(1))
             IF( NUMENT.LE.0 .OR. NUMENT.GT.NONSTK ) THEN
                WRITE (*,'(''   AMAX:  stack entry does not exist'')')
             ELSE
                CALL ARITH('MAX',WAVE,FLUX,NPOINT,BREAK,NBREAK,
     :          XSTACK(POINTR(NUMENT)),YSTACK(POINTR(NUMENT)),
     :          STKNPT(NUMENT),BSTACK(BPOINT(NUMENT)),
     :          BSTNPT(NUMENT),WORK,IWORK,ASIZE1,MAXBRK)
             END IF
*
*   MBOL - idh one-off
*
          ELSE IF( SUBCMD .EQ. 'MBOL' ) THEN
             CALL DECODE('MBOL',PARAMS,0,0,VARRAY,' ',OK)
             CALL INTEGRATE(NPOINT,WAVE,FLUX,VARRAY(1))
             VARRAY(1) = -2.5*LOG10(MAX(1e-30,VARRAY(1)))
             WRITE (*,'(''   MBOL:'',1PE12.5)') VARRAY(1)
*
*   MEAN    calculate mean value of data
*
          ELSE IF( SUBCMD .EQ. 'MEAN' ) THEN
             CALL DECODE('MEAN',PARAMS,0,0,VARRAY,' ',OK)
             IF( NPOINT.GT.1 ) THEN
                VALIH = 0.0
                SIGMAM = 0.0
                DO 2030 I = 1, NPOINT
                   VALIH = VALIH + FLUX(I)
 2030           CONTINUE
                VALIH = VALIH/REAL(NPOINT)
                DO 2040 I = 1, NPOINT
                   SIGMAM = SIGMAM + (FLUX(I)-VALIH)**2
 2040           CONTINUE
                SIGMAM = SQRT(SIGMAM/REAL(NPOINT-1))
                WRITE (*,'(''   MEAN:  Mean, s.d.  '',1P2E13.5)')
     :           VALIH, SIGMAM
             ELSE IF( NPOINT .EQ. 1 ) THEN
                WRITE (*,
     :          '(''   MEAN:  Only one point, X ='',1PE13.5)')
     :          FLUX(1)
*   IF( USERID(1:3) .EQ. 'IPG' .OR. USERID(1:3) .EQ. 'IDH' ) THEN
*   IF( GRIFFU3 .EQ. 0.0 ) THEN
*   GRIFFU3 = FLUX(1)
*   ELSE IF( GRIFFB3 .EQ. 0.0 ) THEN
*   GRIFFB3 = FLUX(1)
*   ELSE IF( GRIFFB2 .EQ. 0.0 ) THEN
*   GRIFFB2 = FLUX(1)
*   ELSE IF( GRIFFV1 .EQ. 0.0 ) THEN
*   GRIFFV1 = FLUX(1)
*   END IF
*   END IF
             ELSE
                WRITE (*,'(''   MEAN:  No data'')')
             END IF
*
*   MERGE   Merge 2 STACK entries - result in 'current' arrays
*
          ELSE IF( SUBCMD .EQ. 'MERGE' ) THEN
             VARRAY(5) = 0.0
             CALL DECODE('MERGE',PARAMS,4,5,VARRAY,
     :       'Entry1 Entry2 Wt1 Wt2 ',OK)
             IF( OK ) THEN
                IHTAG = 0
                IF( NINT(VARRAY(1)).LE.0 .OR. NINT(VARRAY(1)).GT.NONSTK)
     :          THEN
                   IHTAG = 1
                   WRITE (*,
     :             '(''   MERGE:  entry 1 out of STACK range'')')
                   GO TO 5000
                END IF
                IF( NINT(VARRAY(2)).LE.0 .OR. NINT(VARRAY(2)).GT.NONSTK)
     :          THEN
                   IHTAG = 1
                   WRITE (*,
     :             '(''   MERGE:  entry 2 out of STACK range'')')
                   GO TO 5000
                END IF
                IF( NINT(VARRAY(1)) .EQ. NINT(VARRAY(2)) ) THEN
                   IHTAG = 1
                   WRITE (*,'(''   MERGE:  identical entries!'')')
                   GO TO 5000
                END IF
                IF( VARRAY(3).LE.0.0 ) THEN
                   IHTAG = 1
                   WRITE (*,'(''   MERGE:  Wt1 must be positive'')')
                   GO TO 5000
                END IF
                IF( VARRAY(4).LE.0.0 ) THEN
                   IHTAG = 1
                   WRITE (*,'(''   MERGE:  Wt2 must be positive'')')
                   GO TO 5000
                END IF
                NPT1 = STKNPT(NINT(VARRAY(1)))
                NPT2 = STKNPT(NINT(VARRAY(2)))
                PTR1 = POINTR(NINT(VARRAY(1)))
                PTR2 = POINTR(NINT(VARRAY(2)))
                X11 = XSTACK(PTR1)
                X12 = XSTACK(PTR1-1+NPT1)
                X21 = XSTACK(PTR2)
                X22 = XSTACK(PTR2-1+NPT2)
                IF( X11.GE.X12 ) THEN
                   IHTAG = 1
                   WRITE (*,'(''   MERGE:  entry1 data not ranked in'',
     :             '' increasing X order'')')
                   GO TO 5000
                END IF
                IF( X21.GE.X22 ) THEN
                   IHTAG = 1
                   WRITE (*,'(''   MERGE:  entry2 data not ranked in'',
     :             '' increasing X order'')')
                   GO TO 5000
                END IF
             ELSE
                IHTAG = 1
                GO TO 5000
             END IF
             IF( IHTAG .EQ. 0 ) THEN
                CALL IHMRG(VARRAY,STKSZE,XSTACK,YSTACK,MAXSTK,POINTR,
     :          STKNPT,BPOINT,BSTSZE,BSTACK,BSTNPT,ASIZE1,WAVE,
     :          FLUX,TITLE,MAXBRK,BREAK,NPOINT,NBREAK,WORKSZ,WORK)
                WORV = WORVST(NINT(VARRAY(1)))
                IF( WORV .EQ. 0.0) WORV = 1.0
             END IF
*
*   AMIN    Finds minimum of current and stack entry Y values
*
          ELSE IF( SUBCMD .EQ. 'MIN' .OR. SUBCMD .EQ. 'AMIN' ) THEN
             IF( SUBCMD .EQ. 'MIN' ) THEN
                WRITE (*,'(''   MIN command renamed AMIN '',
     :          ''(correct call mandatory)'',A)') BLEEP
                GO TO 5000
             END IF
             CALL DECODE('AMIN',PARAMS,1,1,VARRAY,'Entry ',OK)
             IF( .NOT.OK) GO TO 5000
             NUMENT = NINT(VARRAY(1))
             IF( NUMENT.LE.0 .OR. NUMENT.GT.NONSTK ) THEN
                WRITE (*,'(''   AMIN:  stack entry does not exist'')')
             ELSE
                CALL ARITH('MIN',WAVE,FLUX,NPOINT,BREAK,NBREAK,
     :          XSTACK(POINTR(NUMENT)),YSTACK(POINTR(NUMENT)),
     :          STKNPT(NUMENT),BSTACK(BPOINT(NUMENT)),
     :          BSTNPT(NUMENT),WORK,IWORK,ASIZE1,MAXBRK)
             END IF
*
*   MODE - calculates mode of X values in a sampled continuous distribution
*
          ELSE IF( SUBCMD .EQ. 'MODE' ) THEN
             VARRAY(1) = 0.0
             CALL DECODE('MODE',PARAMS,0,1,VARRAY,' ',OK)
             IF( .NOT.OK) GO TO 5000
             CALL MODEST(OK,WAVE,NPOINT,WORK(1),WORK(NPOINT+1),
     :       WORK(NPOINT+NPOINT/5+1),NINT(VARRAY(1)))
             IF( .NOT.OK ) THEN
                OK = .TRUE.
                GO TO 5000
             END IF
*
*   MOMENT   Calculates first and second moments of P Cygni profile
*
          ELSE IF( SUBCMD .EQ. 'MOM' .OR. SUBCMD .EQ. 'MOMENT' ) THEN
             CALL DECODE('MOMENT',PARAMS,2,2,VARRAY,'Wav0 V(inf) ',OK)
             IF( .NOT.OK) GO TO 5000
             WAV0 = VARRAY(1)
             VINF = VARRAY(2)
             IF( .NOT.(OK)) GO TO 5000
             CALL IHMMNT(WAVE,FLUX,WAV0,VINF,ASIZE1,NPOINT,SUBCHK)
             IF( .NOT.SUBCHK ) THEN
                SUBCHK = .TRUE.
                GO TO 5000
             END IF
*
*   MONGOWR   write MONGO compatible file
*
          ELSE IF( SUBCMD .EQ. 'MONGOWR' ) THEN
             CALL MONGOWR(WAVE,FLUX,ASIZE1,NPOINT,BREAKS,MAXBRK,NBREAK,
     :                    PARAMS, SUBCHK, STATUS )
             IF( .NOT.SUBCHK ) THEN
                SUBCHK = .TRUE.
                GO TO 5000
             END IF
*
*   MROT    Rotate symbols ('Marks')
*
          ELSE IF( SUBCMD .EQ. 'MROT' ) THEN
             CALL DECODE('MROT',PARAMS,0,0,VARRAY,' ',OK)
             MRKTST = .TRUE.
             MARK = MARKKP
             WRITE (*,'(''   MROT:  not currently implemented'',A)')
     :       BLEEP
             GO TO 5000
*
*   MSET - Sets Marker style (or whatever you want to call it)
*
          ELSE IF( SUBCMD .EQ. 'MSET' ) THEN
             CALL DECODE('MSET',PARAMS,1,2,VARRAY,'Style Nvert ',OK)
             IF( .NOT.(OK)) GO TO 5000
             IF( VARRAY(2).LT.2.6 ) THEN
                WRITE (*,'(''   MSET:  symbol must have at least'',
     :          '' 3 vertices'',A)') BLEEP
                GO TO 5000
             ELSE IF( VARRAY(1).LT.0.6 .OR. VARRAY(1).GT.4.4 ) THEN
                WRITE (*,
     :          '(''   MSET:  style must be in the range 1-4'',A)')
     :          BLEEP
                GO TO 5000
             END IF
             NSIDES = NINT(VARRAY(2))
             MARKSTYLE = NINT(VARRAY(1))
*
*   NBox    Switch off automatic CLEAR before plots
*
          ELSE IF( SUBCMD .EQ. 'NB' ) THEN
             CALL DECODE('NB',PARAMS,0,0,VARRAY,' ',OK)
             CALL NOBOX
             LBOX = .FALSE.
             ERASEBOX = .FALSE.
*
*   NEBCONT  Calculates nebular continuum
*
          ELSE IF( SUBCMD .EQ. 'NEBCONT' ) THEN
             IF( NPOINT.LT.1 ) THEN
                WRITE (*,'(''   NEBCONT:  no data in current arrays'')')
                GO TO 5000
             END IF
 2060        CONTINUE
             CALL SSTRIP(PARAMS)
             LLENS = SLEN(PARAMS)
             IF( LLENS.LT.1 ) THEN

                CALL RDSTR( SUBCMD, 'File name', ' ', PARAMS, STATUS )
                IF( STATUS .NE. SAI__OK ) THEN
                   OK = .FALSE.
                   GO TO 5000
                END IF
                GO TO 2060

             END IF

             IF( WORV .NE. 1.0 ) THEN
                WRITE (*,
     :          '(''   NEBCONT:  data are not in wavelength space'')')
                GO TO 5000
             END IF
             IF( NONSTK.GE.MAXSTK ) THEN
                WRITE (*,
     :          '(''   NEBCONT:  no room on stack for results'')')
                GO TO 5000
             END IF
             IF( WAVE(1).LT.10.0 .OR. WAVE(NPOINT).LT.100.0 .OR. WAVE(1)
     :           .GE.WAVE(NPOINT) ) THEN

                CALL MSG_SETR( 'V1', WAVE( 1 ) )
                CALL MSG_SETR( 'VN', WAVE( NPOINT ) )
                CALL MSGOUT( SUBCMD, 'X range is ^V1 - ^V2', .FALSE.,
     :                       STATUS )

                CALL GET0L( ' ', 1, .FALSE., SUBCMD, 'Do you wish to '//
     :                      'continue', .TRUE., VALUE, STATUS )
                IF( STATUS .NE. SAI__OK ) THEN
                   OK = .FALSE.
                   GO TO 5000
                END IF

                OK = .TRUE.
                IF( VAL_OK ) THEN
                   GO TO 2080
                ELSE
                   GO TO 5000
                END IF

             END IF

 2080        CONTINUE
             LLENS = INDEX(PARAMS,' ') - 1
             OPEN (UNIT=54,FILE=PARAMS(1:LLENS),STATUS='OLD',
     :       IOSTAT=IHX,ACCESS='SEQUENTIAL')
             IF( IHX .NE. 0 ) THEN
                CLOSE (54)
                IF( p2len .GT. 0 ) THEN
                OPEN (UNIT=54,STATUS='OLD',IOSTAT=IHX,
     :          FILE=prefix2(1:p2len)//PARAMS(1:LLENS),
     :          ACCESS='SEQUENTIAL')
                ELSE
                OPEN (UNIT=54,STATUS='OLD',IOSTAT=IHX,
     :          FILE=PARAMS(1:LLENS),
     :          ACCESS='SEQUENTIAL')
                END IF
                IF( IHX .NE. 0 ) THEN
                   WRITE (*,'(''   NEBCONT:  unable to open file'')')
                   CLOSE (54)
                   GO TO 5000
                END IF
             END IF
             IUNIT = 54
             PARAMS = PARAMS(LLENS+1:)
             CALL SSTRIP(PARAMS)
             VARRAY(1) = 0.0
             VARRAY(2) = 0.0
             VARRAY(3) = 0.0
             CALL DECODE('NEBCONT',PARAMS,0,3,VARRAY,' ',OK)
             IF( .NOT.OK) GO TO 5000
             MODE1 = NINT(VARRAY(1))
             MODE2 = NINT(VARRAY(2))
             MODE3 = NINT(VARRAY(3))

             SUBCHK = .TRUE.
             CALL NEBSET(IUNIT,WAVE,FLUX,ASIZE1,NPOINT,BREAK,MAXBRK,
     :                   NBREAK,MODE1,MODE2,MODE3,SUBCHK,STATUS)
             CLOSE (54)
             IF( .NOT.SUBCHK ) THEN
                SUBCHK = .TRUE.
                GO TO 5000
             END IF
*
*   NLAB - switch off graph-labelling
*
          ELSE IF( SUBCMD .EQ. 'NLAB' ) THEN
             CHECKPARAMS = PARAMS(1:1)
             CALL DECODE('NLAB',PARAMS,0,1,VARRAY,' ',OK)
             LABELFLAG = .FALSE.
             IF( .NOT.OK) GO TO 5000
             IF( CHECKPARAMS .EQ. ' ' ) THEN
             ELSE
*               NLABEL = NINT(VARRAY(1))
             END IF
*
*   NOBEEP - turns off beep
*
          ELSE IF( SUBCMD .EQ. 'NOBEEP' ) THEN
             CALL DECODE('NOBEEP',PARAMS,0,0,TEMP,' ',OK)
             IF( .NOT.(OK)) GO TO 5000
             BEEP = .FALSE.
             BLEEP = ' '
*
*   NODEBUG   turns off DEBUG
*
          ELSE IF( SUBCMD .EQ. 'NODEBUG' ) THEN
             DEBUG = .FALSE.
*
*   NROT   Stop rotating everything
*
          ELSE IF( SUBCMD .EQ. 'NROT' ) THEN
             CALL DECODE('NROT',PARAMS,0,0,VARRAY,' ',OK)
             ROTST = .FALSE.
             MRKTST = .FALSE.
             TLNTST = .FALSE.
             IPAL = IPALKP
             MARK = MARKKP
             LNTYPE = LNTYPEKP
             IF( COLOUR) CALL PPALET(IPAL)
             IF( DVTST) CALL GSLN(LNTYPE)
*
*   NCROT  Stop rotating colour table
*
          ELSE IF( SUBCMD .EQ. 'NCROT' ) THEN
             CALL DECODE('NCROT',PARAMS,0,0,VARRAY,' ',OK)
             ROTST = .FALSE.
             IPAL = IPALKP
             IF( COLOUR) CALL PPALET(IPAL)
*
*   NECHO  No echo from COMRD file
*
          ELSE IF( SUBCMD .EQ. 'NECHO' ) THEN
             CALL DECODE('NECHO',PARAMS,0,0,VARRAY,' ',OK)
             INTECHO = -1
*
*   NHPROT  Stop rotating line style
*
          ELSE IF( SUBCMD .EQ. 'NHPROT' ) THEN
             CALL DECODE('NHPROT',PARAMS,0,0,VARRAY,' ',OK)
             HPROT = .FALSE.
*
*   NMROT  Stop rotating symbols
*
          ELSE IF( SUBCMD .EQ. 'NMROT' ) THEN
             CALL DECODE('NMROT',PARAMS,0,0,VARRAY,' ',OK)
             MRKTST = .FALSE.
             MARK = MARKKP
             CALL SETMAR
*
*   NTROT  Stop rotating line attributes
*
          ELSE IF( SUBCMD .EQ. 'NTROT' ) THEN
             CALL DECODE('NTROT',PARAMS,0,0,VARRAY,' ',OK)
             TLNTST = .FALSE.
             LNTYPE = LNTYPEKP
             IF( DVTST ) THEN
                CALL PLOTIT( 0, 0, 2)
                CALL SGS_FLUSH
                CALL GSLN(LNTYPE)
             END IF
*
*   NUMOFF     Stops axis numbering
*
          ELSE IF( SUBCMD .EQ. 'NUMOFF' ) THEN
             CALL DECODE
     :       ('NUMOFF',PARAMS,0,0,VARRAY,' ',OK)
             NONUM=.TRUE.
*
*   NUMON     restores axis numbering
*
          ELSE IF( SUBCMD .EQ. 'NUMON' ) THEN
             CALL DECODE
     :       ('NUMON',PARAMS,0,0,VARRAY,' ',OK)
             NONUM=.FALSE.
*
*   NX      Remove x constraints on plot
*
          ELSE IF( SUBCMD .EQ. 'NX' ) THEN
             CALL DECODE('NX',PARAMS,0,0,VARRAY,' ',OK)
             CALL NX
             XRKEEP(1) = -321.654
             XRKEEP(2) = -321.654
*
*   NXY    Remove X and Y constraints on plot
*
          ELSE IF( SUBCMD .EQ. 'NXY' ) THEN
             CALL DECODE('NXY',PARAMS,0,0,VARRAY,' ',OK)
             CALL NXY
             DO 2100 I = 1, 2
                XRKEEP(I) = -321.654
                YRKEEP(I) = -321.654
 2100        CONTINUE
             TRIMY = .FALSE.
*
*   NY      Remove Y constraints on plotting
*
          ELSE IF( SUBCMD .EQ. 'NY' ) THEN
             CALL DECODE('NY',PARAMS,0,0,VARRAY,' ',OK)
             CALL NY
             YRKEEP(1) = -321.654
             YRKEEP(2) = -321.654
             TRIMY = .FALSE.
*
*   PAUSE - pause (e.g. if driven from .CMD file)
*
          ELSE IF( SUBCMD .EQ. 'PAUSE' ) THEN
             CALL DECODE('PAUSE',PARAMS,0,0,VARRAY,' ',OK)
             WRITE (*,'('' '',A)') BLEEP
             READ( *, '(A)', ERR=6000 ) PARAMS(1:10)
             CALL DTOUPP(PARAMS)
             CALL SSTRIP(PARAMS)
             IF( PARAMS(1:1) .EQ. 'Q' ) THEN
                OK = .FALSE.
                GO TO 5000
             END IF
*
*   PFit    Fit polynomial to selected continuum regions
*
          ELSE IF( SUBCMD .EQ. 'PF' ) THEN

             CALL DECODE('PFIT',PARAMS,1,1,VARRAY,'Degree ',OK)
             IF( .NOT.OK) GO TO 5000

             IF( NONSTK .EQ. 0 ) THEN
                WRITE (*,'(''   PF:  PUSH data first!'')')
                OK = .FALSE.
                GO TO 5000
             END IF

             IF( OK ) THEN
                KPLUS1 = VARRAY(1) + 1.01
                MAXPTS = ((WORKSZ/2)-KPLUS1*(KPLUS1+3))/6
                PDAPTS = 0
                DO 2110 I = 1, NCREG
                   CALL FINDIT(XSTACK(POINTR(NONSTK)),STKNPT(NONSTK),
     :             XLIML(I),I1,1)
                   CALL FINDIT(XSTACK(POINTR(NONSTK)),STKNPT(NONSTK),
     :             XLIMH(I),I2,0)
                   DO 2105 J=POINTR(NONSTK)+I1,POINTR(NONSTK)+I2-1
                      PDAPTS = PDAPTS + 1
                      IF( PDAPTS.GT.MAXPTS ) THEN
                         WRITE (*,'(''   PF:  no of points ='',I5/
     :                   ''        max allowed ='',I5)')
     :                   PDAPTS, MAXPTS
                         GO TO 5000
                      END IF
                      DPWORK(PDAPTS) = XSTACK(J)
                      DPWORK(PDAPTS+MAXPTS) = YSTACK(J)
 2105              CONTINUE
 2110           CONTINUE
                DO 2120 I = 2*MAXPTS + 1, 2*MAXPTS + PDAPTS
                   DPWORK(I) = 1.0D0
 2120           CONTINUE
                IFAIL = 0
                IF( PDAPTS .EQ. 0 ) THEN
                   WRITE (*,'(''   PF:  no continuum points'')')
                   GO TO 5000
                END IF

                EPS = 0.0
                IERR2 = 0
                CALL PDA_DPOLFT( PDAPTS, DPWORK(1), DPWORK(MAXPTS+1),
     :                           DPWORK(2*MAXPTS+1), KPLUS1-1, NDEG,
     :                           EPS, DPWORK(3*MAXPTS+1), IERR1,
     :                           DPWORK(4*MAXPTS+1), IERR2 )

                IF( IERR1 .NE. 1 ) THEN
                   WRITE (*,'(''   PF: PDA_DPOLFT error - IERR ='',I2)')
     :                    IERR1
                   GO TO 5000

                ELSE
                   IF( NDEG .NE. KPLUS1-1 ) THEN
                     WRITE (*,'(''   PF: Warning: could only fit '//
     :                      'polynomial of degree '',I2)') NDEG
                   END IF

                   DPX1 = DPWORK(1)
                   DPX2 = DPWORK(PDAPTS)
                   X1 = DPX1
                   X2 = DPX2
                   CALL FINDIT(XSTACK(POINTR(NONSTK)),STKNPT(NONSTK),
     :             X1, I1,1)
                   CALL FINDIT(XSTACK(POINTR(NONSTK)),STKNPT(NONSTK),
     :             X2, I2,0)
                   NPOINT = 0
                   J = WORKSZ/2 + 1 - KPLUS1
                   IH = 0
                   RMSUM = 0.0

                   DO 2130 I = POINTR(NONSTK)+I1, POINTR(NONSTK)+I2-1
                      NPOINT = NPOINT + 1
                      WAVE(NPOINT) = XSTACK(I)

                      CALL PDA_DP1VLU( NDEG, 0, DBLE( XSTACK(I) ),
     :                                 DPYVAL, DPXBAR,
     :                                 DPWORK(4*MAXPTS+1), IERR2 )
                      FLUX(NPOINT) = DPYVAL

                      IF( ABS(WAVE(NPOINT)-DPWORK(IH+1)).LT.1.E-3 ) THEN
                         IH = IH + 1
                         OMC = DPWORK(MAXPTS+IH) - DPYVAL
                         RMSUM = RMSUM + (OMC/DPYVAL)**2
                      END IF
                      ICFLAG = 1
 2130              CONTINUE
                   NCIH = IH
                   SIGCIH = SQRT(RMSUM/REAL(NCIH))
                   PRINT '(2X,A,F6.2,A)',
     :             ' PF - RMS scatter of data about fit is',
     :             SIGCIH*100.0, '%'
                   PRINT '(2X,A,I5,A)', ' (from', NCIH, ' points)'
                   NBREAK = 1
                   BREAK(1) = NPOINT
                   WRITE (TITLE(20:22),'(I3)') NINT(VARRAY(1))
                   TITLE(1:19) = ' Poly Fit, degree ='
                   TITLE(23:) = ' '
                END IF
             END IF
*
*   PIXY - read PhotoIonization XY data (for MJS)
*   also uses MJSUNIT, MJSTRING
*
!*
!         ELSE IF( SUBCMD .EQ. 'PIXY' ) THEN
!            IF( PARAMS .NE. ' ' ) THEN
 2140           CONTINUE
!               CALL SSTRIP (PARAMS)
!               CALL DTOUPP(PARAMS)
!               CLOSE (MJSUNIT)
!               MJSL = SLEN(PARAMS)
!               MJSTRING(1:) = PARAMS(1:MJSL)
!               MJSL = MJSL + 1
!               IHX = 1
!               MJSUM = 1
!               DO 2150 WHILE (IHX .NE. 0 .AND. MJSUM.LT.100)
!                  WRITE (MJSTRING(MJSL:),'(I3.3)') MJSUM
!                  OPEN (UNIT=MJSUNIT,FILE=MJSTRING(1:(MJSL+2)),
!    :             STATUS='OLD',IOSTAT=IHX)
!                  MJSUM = MJSUM + 1
 2150           CONTINUE
!               IF( IHX .NE. 0 ) THEN
!                  WRITE (*,
!    :             '(''   PIXY:  unable to open file '',A,
!    :             ''.DAT nnn for nnn<100'',A)')
!    :             MJSTRING(1:(MJSL-1)),BLEEP
!                  CLOSE (MJSUNIT)
!                  MJSTRING = ' '
!                  OK = .FALSE.
!                  IHX = 0
!                  GO TO 5000
!               ELSE
!                  MJSUM = MJSUM - 1
!                  WRITE (*,'(''   PIXY:  opened file '',A,''.DAT'')')
!    :             MJSTRING(1:(MJSL+2))
!               END IF
!            ELSE IF( MJSTRING .EQ. ' ' ) THEN
 2160           CONTINUE
!               WRITE (*,'(''   PIXY:  enter file root: '',$)')
!               READ (*,'(A)') PARAMS
!               IF( PARAMS .NE. ' ') GO TO 2140
!               GO TO 2160
!            END IF
!
 2180        CONTINUE
!            READ (MJSUNIT,'(A)',IOSTAT=IHX) TITLE
!            IF( IHX .EQ. 0 ) THEN
!               READ (MJSUNIT,*) N
!               DO 2190 I = 1, N
!                  READ (MJSUNIT,*) WAVE(I), FLUX(I), FLUX(I+N)
!                  WAVE(I+N) = WAVE(I)
 2190           CONTINUE
!               NBREAK = 2
!               NPOINT = 2*N
!               BREAK(1) = N
!               BREAK(2) = NPOINT
!            ELSE
!               WRITE (*,
!               '(''   PIXY:  file '',A,''.DAT completed'')')
!    :          MJSTRING(1:SLEN(MJSTRING))
!               CLOSE (MJSUNIT)
!               MJSUM = MJSUM + 1
!               WRITE (MJSTRING(MJSL:),'(I3.3)') MJSUM
!               OPEN (UNIT=MJSUNIT,FILE=MJSTRING(1:(MJSL+2)),
!    :          STATUS='OLD',IOSTAT=IHX)
!               IF( IHX .NE. 0 ) THEN
!                  WRITE (*,
!                  '(''   PIXY:  unable to open '',A,''.DAT'')')
!    :             MJSTRING(1:(MJSL+2))
!                  CLOSE (MJSUNIT)
!                  MJSTRING = ' '
!                  IHX = 0
!                  GO TO 5000
!               ELSE
!                  WRITE (*,'(''   PIXY:  opened '',A,''.DAT'')')
!    :             MJSTRING(1:MJSL+2))
!                  GO TO 2180
!               END IF
!            END IF
!*
*
*   PLOTINV  Y-axis to be inverted
*
          ELSE IF( SUBCMD .EQ. 'INV' .OR. SUBCMD .EQ. 'PLOTINV' ) THEN
             IF( SUBCMD .EQ. 'INV' ) THEN
                WRITE (*,'(''   INV command renamed PLOTINV'',A)') BLEEP
                SUBCMD = 'PLOTINV'
                WRITE (*,'(''   (command executed)'')')
             END IF
             CALL DECODE('PLOTINV',PARAMS,0,0,VARRAY,' ',OK)
             CALL INVERT
*
*   PLOTREV Reverse x-axis
*
          ELSE IF( SUBCMD .EQ. 'REV' .OR. SUBCMD .EQ. 'PLOTREV' ) THEN
             IF( SUBCMD .EQ. 'REV' ) THEN
                WRITE (*,'(''   REV command renamed PLOTREV'',A)') BLEEP
                SUBCMD = 'PLOTREV'
                WRITE (*,'(''   (command executed)'')')
             END IF
             CALL DECODE('PLOTREV',PARAMS,0,0,VARRAY,' ',OK)
             CALL REVERS
*
*   PM      Plot
*
          ELSE IF( SUBCMD .EQ. 'PM' ) THEN
             NZONEN1 = NZONEN
             DO 2200 I = 1, MAXSTK
                VARRAY(I) = -100.0
 2200        CONTINUE
             NVR = 1
             IXS = 0
             IF( PARAMS .EQ. ' ' ) THEN
                IF( .NOT.PPROMPT ) THEN
                   PARAMS(1:1) = '0'
                ELSE

 2205              CONTINUE
                   CALL RDSTR( SUBCMD, 'Entries', ' ', PARAMS,
     :                         STATUS )
                   IF( STATUS .NE. SAI__OK ) THEN
                      OK = .FALSE.
                      GO TO 5000
                   END IF

                   IF( PARAMS .EQ. ' ' ) THEN
                      CALL MSGOUT( SUBCMD, ' You have set PPROMPT '//
     :                             'true. Try again...', .TRUE.,
     :                             STATUS )
                      GO TO 02205
                   END IF


                END IF
             END IF
             DO 2220 I = 1, MAXSTK
                CALL SSTRIP(PARAMS)
                IF( PARAMS .EQ. ' ') GO TO 2240
                II1 = INDEX(PARAMS,' ')
                II2 = INDEX(PARAMS,'-')
                IF( II2 .EQ. 0) II2 = 1000
                IF( II2.LT.II1 ) THEN
                   PARAMS((II2+1):) = PARAMS(II2:)
                   PARAMS(II2:II2) = ' '
                END IF
                CALL XDCODE('PM',PARAMS,1,1,VARRAY(NVR),' ',OK)
                IF( .NOT.OK ) THEN
                   OK = .TRUE.
                   GO TO 5000
                END IF
                CALL SSTRIP(PARAMS)
                IF( INDEX(PARAMS,'-') .EQ. 1 ) THEN
                   IHHEAD = ' '
                   WRITE (IHHEAD,'(I5)',IOSTAT=IHX) NINT(VARRAY(NVR))
                   IF( IHX .NE. 0 ) THEN
                      WRITE (*,
     :                '(''   PM:  error in entry specification'',A)')
     :                BLEEP
                      GO TO 5000
                   END IF
                   IHHEAD(6:8) = ' - '
                   PARAMS = PARAMS(2:SLEN(PARAMS))
                   CALL XDCODE('PM',PARAMS,1,1,VARRAY(NVR),' ',OK)
                   IF( .NOT.OK ) THEN
                      OK = .TRUE.
                      GO TO 5000
                   END IF
                   WRITE (IHHEAD(9:13),'(I5)',IOSTAT=IHX)
     :             NINT(VARRAY(NVR))
                   IF( IHX .NE. 0 ) THEN
                      WRITE (*,
     :                '(''   PM:  error in entry specification'',A)')
     :                BLEEP
                      GO TO 5000
                   END IF
                   CALL DASHIT('PM',IERR,NV,IHHEAD(1:14),(MAXSTK-NVR+1),
     :             MAXSTK,NONSTK,VARRAY(NVR),OK)
                   IF( NV .EQ. 0 ) THEN
                      VARRAY(NVR) = -100.0
                      INULL = INULL + 1
                   END IF
                   IXS = IXS + IERR
                   IF( .NOT.OK ) THEN
                      OK = .TRUE.
                      GO TO 5000
                   END IF
                   NVR = NVR + NV
                ELSE
                   INVR = NINT(VARRAY(NVR))
                   IF( INVR .GE. 1 .AND. INVR .LE. MAXSTK ) THEN

                      IF( INVR .GT. NONSTK .OR.
     :                    STKNPT( INVR ) .LT. 1 ) THEN

                         WRITE (*,'(''   PM:  entry'',I3,'' is '//
     :                          'empty'',A)') INVR, BLEEP

                      END IF

                   ELSE IF( INVR .LT. 0 .OR. INVR .GT. MAXSTK ) THEN
                      IXS = IXS + 1
                   END IF

                   NVR = NVR + 1
                END IF

                IF( NVR.GT.MAXSTK ) THEN
                  WRITE (*,'(''   PM:  too many entries specified'',A)')
     :            BLEEP
                  GO TO 5000
                END IF
 2220        CONTINUE
 2240        CONTINUE
             NDO = 0
             DO 2260 I = 1, MAXSTK
                IF( VARRAY(I).LT.0.0) GO TO 2280
                NDO = NDO + 1
 2260        CONTINUE
 2280        CONTINUE
             IF( DEVTYP .EQ. 0 ) THEN
                WRITE (*,'(''   PM:  no plotting device assigned'',A)')
     :          BLEEP
                ATTST = .FALSE.
                GO TO 5000
             END IF
             LKEEP = .FALSE.
             IF( LBOX) LKEEP = .TRUE.

             IF( LBOX .OR. DVTST .OR. ERTST .OR. ZNTST ) THEN
                IF( .NOT.ATTST ) THEN
                   CALL JOBOX
                   IF( HSTTST) CALL SETHIS
                   IF( POLTST) CALL SETPOL
                   IF( .NOT.HSTTST .AND. .NOT.POLTST) CALL SETMAR
                   NXPLTS = 0
                END IF
             END IF

             IF( ROTST .AND. .NOT.ATTST .AND.
     :       (LBOX .OR. DVTST .OR. ERTST .OR. ZNTST) ) THEN
                IPAL = IPALKP
             END IF
             IF( MRKTST .AND. .NOT.ATTST .AND.
     :       (LBOX .OR. DVTST .OR. ERTST .OR. ZNTST) ) THEN
                MARK = MARKKP
             END IF
             IF( TLNTST .AND. .NOT.ATTST .AND.
     :       (LBOX .OR. DVTST .OR. ERTST .OR. ZNTST) ) THEN
                LNTYPE = LNTYPEKP
             END IF
             ATTST = .FALSE.
             JPLTS = 1
             DO 2300 J = 1, NDO
                IF( JPLTS .EQ. 2) CALL NOBOX
                IF( HPROT ) THEN
                   IF( NXPLTS.GT.0 ) THEN
                      JJ = NXPLTS/2
                      JJ = JJ*2
                      JJ = NXPLTS - JJ
                      IF( HPSTAY .EQ. 1 ) THEN
                         JJ = 1
                      END IF
                      IF( (JJ .EQ. 0 .AND. POLTST) .OR.
     :                (JJ .EQ. 1 .AND. HSTTST) ) THEN
                         CALL SETPOL
                      ELSE IF( (JJ .EQ. 1 .AND. POLTST) .OR.
     :                (JJ .EQ. 0 .AND. HSTTST) ) THEN
                         CALL SETHIS
                      END IF
                   ELSE
!                     IF( POLTST) CALL SETPOL
!                     IF( HSTTST) CALL SETHIS
                      IF( POLTST ) THEN
                         CALL SETPOL
                      ELSE IF( HSTTST)  THEN
                         CALL SETHIS
                      ELSE
                         CALL SETMAR
                      END IF
                   END IF
                END IF

                IF( TLNTST ) THEN
                   IF( DVTST) CALL GSLN(LNTYPE)
                END IF
                IF( MRKTST ) THEN
                   CALL SETMAR
                END IF
                IF( ROTST ) THEN
                   IF( COLOUR ) THEN
                      CALL PPALET(IPAL)
                   END IF
                END IF
                I = NINT(VARRAY(J))
                IF( I .EQ. 0 ) THEN
                   IF( NPOINT.LE.1 ) THEN
                      WRITE (*,'(''   PM:  '',
     :                ''nothing to plot in default arrays'',A)')
     :                BLEEP
                   ELSE
*   Find mid-point of plot for initial cursor position (if required)
                      XCURSOR = XLIM1
                      YCURSOR = YLIM1

                      CALL PLTARR
     :                (WAVE, FLUX, NPOINT, BREAK, NBREAK, TITLE,
     :                2*ASIZE1, WORK(1), WORK((WORKSZ/2+1)))
                      IF( PLTCAL.GT.0 ) THEN
                         JPLTS = JPLTS + 1
                         NPLOTS = NPLOTS + 1
                         NXPLTS = NXPLTS + 1
                         IF( ROTST .AND. COLOUR ) THEN
                            IPAL = IPAL + 1
                            IF( IPAL .EQ. 16) IPAL = 1
                         END IF
                      ELSE
                         WRITE (*,'(''   '',A,'':  no data exist '',
     :                   ''in current arrays over X range in force'',
     :                   A)') SUBCMD(1:SLEN(SUBCMD)), BLEEP
                         IF( .NOT.LBOX ) THEN
                            WRITE (*,
     :                      '(''   (Remember, BOX is switched off!)'')')
                         END IF
                      END IF
                   END IF
                ELSE IF( I.LE.NONSTK .AND. I.GE.1 ) THEN
                   IF( STKNPT(I).GE.1 ) THEN
*   Find mid-point of plot for initial cursor position (if required)
                      XCURSOR = XLIM1
                      YCURSOR = YLIM1
                      CALL PLTARR
     :                (XSTACK(POINTR(I)), YSTACK(POINTR(I)),
     :                STKNPT(I), BSTACK(BPOINT(I)), BSTNPT(I),
     :                STITLE(I),
     :                2*ASIZE1, WORK(1), WORK((WORKSZ/2+1)))
                      IF( PLTCAL.GT.0 ) THEN
                         JPLTS = JPLTS + 1
                         NPLOTS = NPLOTS + 1
                         NXPLTS = NXPLTS + 1
                         IF( ROTST .AND. COLOUR ) THEN
                            IPAL = IPAL + 1
                            IF( IPAL .EQ. 16) IPAL = 1
                         END IF
                      ELSE
                         WRITE (*,'(''   '',A,'':  no data exist in '',
     :                   ''stack entry'',I3,'' over X range in force'',
     :                   A)') SUBCMD(1:SLEN(SUBCMD)), I, BLEEP
                      END IF
                   END IF
                END IF
                IF( TLNTST ) THEN
                   LNTYPE = LNTYPE + 1
                   IF( LNTYPE .EQ. 6) LNTYPE = 1
                END IF
                IF( MRKTST ) THEN
                   MARK = MARK + 1.0
                   IF( NINT(MARK) .EQ. 11) MARK = 1.0
                END IF
 2300        CONTINUE

             IF( ERTST .OR. DVTST .OR. ZNTST ) THEN
                CALL NOBOX
                ERTST = .FALSE.
                DVTST = .FALSE.
                ZNTST = .FALSE.
             END IF
             IF( PSTST) GO TO 2420
             IF( LKEEP ) THEN
                CALL JOBOX
                LBOX = .TRUE.
             END IF
             IF( IXS .NE. 0 ) THEN
                WRITE (*,'(''   PM:'',I4,''  invalid entries'',A)')
     :          IXS, BLEEP
                IXS = 0
             END IF
*
*   POLY    Plotting to be done in polynomial style
*
          ELSE IF( SUBCMD .EQ. 'POLY' ) THEN
             CALL DECODE('POLY',PARAMS,0,0,VARRAY,' ',OK)
             CALL SETPOL
             MRKTST = .FALSE.
             HSTTST = .FALSE.
             POLTST = .TRUE.
             NXPLTS = 0
*
*   POP    Copy stack entry into current arrays
*
          ELSE IF( SUBCMD .EQ. 'POP' ) THEN
             CALL DECODE('POP',PARAMS,1,1,VARRAY,'Entry ',OK)
             IF( .NOT.OK ) THEN
                OK = .TRUE.
                GO TO 5000
             END IF
             J = VARRAY(1)
             IF( J.GE.1 .AND. J.LE.NONSTK ) THEN
                I1 = POINTR(J)
                DO 2310 I = 1, STKNPT(J)
                   WAVE(I) = XSTACK(I1)
                   FLUX(I) = YSTACK(I1)
                   I1 = I1 + 1
 2310           CONTINUE
                NPOINT = STKNPT(J)
                I1 = BPOINT(J)
                DO 2320 I = 1, BSTNPT(J)
                   BREAK(I) = BSTACK(I1)
                   I1 = I1 + 1
 2320           CONTINUE
                NBREAK = BSTNPT(J)
                TITLE = STITLE(J)
                WORV = WORVST(J)
                WRITE (*,'(''   POPped'',I3,'': '',A60)') J, TITLE(1:60)
             ELSE
                WRITE (*,
     :          '(''   POP:  stack entry'',I4,'' does not exist'',A)')
     :          J, BLEEP
                OK = .FALSE.
             END IF

*  PPROMPT:  Force prompting on PM
          ELSE IF( SUBCMD .EQ. 'PPROMPT' ) THEN
             CALL GET0L( PARAMS, 1, .FALSE., SUBCMD, 'Should the PM '//
     :                   'command prompt', PPROMPT, PPROMPT, STATUS )
             IF( PPROMPT ) THEN
                CALL MSGOUT( SUBCMD, 'On', .FALSE., STATUS )
             ELSE
                CALL MSGOUT( SUBCMD, 'Off', .FALSE., STATUS )
             END IF

*  PS:  Plot Spectrum (for hires data)
          ELSE IF( SUBCMD .EQ. 'PS' ) THEN
             VARRAY(3) = 0.0
             VARRAY(4) = 0.0
             TSTVAL = -365.4321
             DO 2360 I = 5, MAXSTK
                VARRAY(I) = TSTVAL
 2360        CONTINUE
             CALL DECODE('PS',PARAMS,2,MAXSTK,VARRAY,'X1 X2 ',OK)
             IF( .NOT.OK) GO TO 5000
             X1 = VARRAY(1)
             X2 = VARRAY(2)

             NV4 = NINT(VARRAY(4))
             IF( NV4 .EQ. 0 ) THEN
                W1V4 = WAVE(1)
                W2V4 = WAVE(NPOINT)
             ELSE IF( NV4.GE.1 .AND. NV4.LE.NONSTK ) THEN
                INDEX1 = POINTR(NV4)
                INDEX2 = INDEX1 + STKNPT(NV4) - 1
                W1V4 = XSTACK(INDEX1)
                W2V4 = XSTACK(INDEX2)
             ELSE
*             Build and report message.
                CALL ITOCHR (NV4, STRING1, NCHAR1, ICSTAT)
                BIGSTR= '  PS:  stack entry'//STRING1(:NCHAR1)//
     :                  ' does not exist'

                WRITE (*,*)BIGSTR
                BIGSTR = ' '
                GO TO 5000
             END IF
             IF( X1.GT.W2V4 .OR. X2.LT.W1V4 ) THEN
                WRITE (*,'(''   PS:  no data in given X range'')')
                GO TO 5000
             END IF
             DX = VARRAY(3)
             IF( DX .EQ. 0.0) DX = X2 - X1
             PSTST = .TRUE.
             PSLST = .FALSE.
*
             NZONE = 5
             NZONEN1 = 5
             DO 2380 JK = 1, 8
                GRID(JK) = GRIDS(JK,NZONEN1)
 2380        CONTINUE
             CALL JOBOX
             NN = -1
             NDO = 1
*
             DO 2400 I = 1, MAXSTK - 3
                VARRAY(I) = VARRAY(I+3)
                IF( VARRAY(I) .NE. TSTVAL) NDO = NDO + 1
 2400        CONTINUE
*
 2420        CONTINUE
             IF( .NOT.(PSLST) ) THEN
                NN = NN + 1
                IF( NN .NE. 0 ) THEN

                   IF( NN .EQ. 1 ) THEN
                      WRITE (*,'(''   PS:  hit return to continue, '',
     :                ''QPS to stop> '',$)')
                   ELSE
                      WRITE (*,
     :                '('' '',30X,''Continue/QPS> '',$)')
                   END IF
                   IHHEAD = ' '
                   READ (*,'(A10)',ERR=6000) IHHEAD(1:10)
                   CALL SSTRIP(IHHEAD)
                   CALL DTOUPP(IHHEAD)
                   IF( IHHEAD(1:1) .EQ. ' ' .OR.
     :                 IHHEAD(1:1) .EQ. 'C' ) THEN
                      X1 = X1 + DX
                      X2 = X2 + DX
                      IF( ((NV4 .EQ. 0) .AND. (WAVE(NPOINT).LE.X1)) .OR.
     :                ((NV4 .NE. 0) .AND.
     :                (XSTACK(POINTR(NV4)+STKNPT(NV4)-1).LE.X1)))
     :                 THEN
                         WRITE (*,
     :                  '(''   PS:  no data in frame - ending'')')
                         X1 = X1 - DX
                         X2 = X2 - DX
                         PSTST = .FALSE.
                         GO TO 5800
                      END IF
                      CALL XRSET(X1,X2)
                      IF( (NV4 .EQ. 0) .AND. (WAVE(NPOINT).LE.X2) .OR.
     :                (NV4 .NE. 0) .AND.
     :                (XSTACK(POINTR(NV4)+STKNPT(NV4)-1).LE.X2))
     :                 THEN
                         WRITE (*,'(''   PS:  last frame'')')
                         PSLST = .TRUE.
                      END IF
                      IF( NZONE .EQ. 6 ) THEN
                         NZONE = 5
                         NZONEN1 = 5
                         DO 2422 JK = 1, 8
                            GRID(JK) = GRIDS(JK,NZONEN1)
 2422                    CONTINUE
                         CALL JOBOX
                         IF( DEVTYP .EQ. 1200) CALL FRAME
                      ELSE
                         NZONE = 6
                         NZONEN1 = 6
                         DO 2424 JK = 1, 8
                            GRID(JK) = GRIDS(JK,NZONEN1)
 2424                    CONTINUE
                         CALL JOBOX
                      END IF
                      GO TO 2280
                   ELSE IF( IHHEAD(1:3) .NE. 'QPS' ) THEN
                      NN = NN - 1
                      GO TO 2420
                   END IF
                ELSE
                   CALL XRSET(X1,X2)
                   GO TO 2280
                END IF
             END IF
             PSTST = .FALSE.
*
*   PUSH    Push current X, Y onto stack
*
          ELSE IF( SUBCMD .EQ. 'PUSH' ) THEN
             CALL DECODE('PUSH',PARAMS,0,0,VARRAY,' ',OK)
             IF( NPOINT.LE.0 ) THEN
                WRITE (*,'(''   PUSH: no data in current arrays'')')
                GO TO 5000
             END IF
             CALL UPUSH(ASIZE1,WAVE,FLUX,NPOINT,MAXBRK,BREAK,NBREAK,
     :       TITLE,WORV,OK)
             IF( .NOT.(OK) ) THEN
                OK = .TRUE.
                GO TO 5000
             ELSE IF( WORV .NE. 0 ) THEN
                WORVST(NONSTK) = WORV
             ELSE
                WORVST(NONSTK) = 1.0
             END IF
*
*   PWRITE   Write text on screen
*
          ELSE IF( SUBCMD .EQ. 'PWRITE' ) THEN
             IF( .NOT.SUBCHK ) THEN
                WRITE (*,'(''   PWRITE:  closing quotes added'',A)')
     :          BLEEP
                SUBCHK = .TRUE.
             END IF
             IF( NPLOTS .EQ. 0 ) THEN
                WRITE (*,'(''   PWRITE:  no plot available'',A)')
     :          BLEEP
                GO TO 5000
             END IF
             IF( DEVTYP .EQ. 0 ) THEN
                WRITE (*,
     :          '(''   PWRITE:  no plotting device assigned'',A)')
     :          BLEEP
                GO TO 5000
             END IF
             CALL PWRITIT(PARAMS,ALASTXT,SUBCMD(1:CMDLEN),XS2NDC,YS2NDC,
     :       DEFHEIGHT,HTFAC( PWRITE ),NDEGREES,IFONT,LNTYPE,
     :       MAXSTK,VARRAY,OK,STATUS)
             IF( .NOT.OK) GO TO 5000
*
*   QAREA   Find out GRAFX device bounds
*
          ELSE IF( SUBCMD .EQ. 'QAREA' ) THEN
             CALL DECODE('QAREA',PARAMS,1,1,VARRAY,'Zone_no. ',OK)
             IF( .NOT.(OK)) GO TO 5000
             IZONE = NINT(VARRAY(1))
             IF( ZONEDEF(IZONE) ) THEN
                GRIDX = (GRIDS(2,IZONE)-GRIDS(1,IZONE))*XSIZ
                GRIDY = (GRIDS(4,IZONE)-GRIDS(3,IZONE))*YSIZ
                GRAPHX = (GRIDS(6,IZONE)-GRIDS(5,IZONE))*XSIZ
                GRAPHY = (GRIDS(8,IZONE)-GRIDS(7,IZONE))*YSIZ
                WRITE (*,'(''   QAREA:'')')
                ALASTXT = ' '
                WRITE (ALASTXT,
     :          '(''   Grid window: '',F6.3,'','',F6.3, '' to'',
     :          F6.3,'','',F6.3,'';  '',F6.1,'' by '',F5.1,'' cm'')',
     :          IOSTAT=IHX)
     :          (GRIDS(N,IZONE),N=1,4), GRIDX, GRIDY
                IF( IHX .NE. 0 ) THEN
                   ALASTXT = ' '
                   WRITE (ALASTXT,
     :             '(''   Grid window: '',F6.3,'','',F6.3,'' to'',
     :             F6.3,'','',F6.3,'';  size not defined'')',
     :             IOSTAT=IHX)
     :             (GRIDS(N,IZONE),N=1,4)
                END IF
                WRITE (*,'(A)') ALASTXT(1:SLEN(ALASTXT))
                ALASTXT = ' '
                WRITE (ALASTXT,
     :          '(''   Graph window:'',F6.3,'','',F6.3, '' to'',
     :          F6.3,'','',F6.3,'';  '',F6.1,'' by '',F5.1,'' cm'')',
     :          IOSTAT=IHX)
     :          (GRIDS(N,IZONE),N=5,8), GRAPHX, GRAPHY
                IF( IHX .NE. 0 ) THEN
                   ALASTXT = ' '
                   WRITE (ALASTXT,
     :             '(''   Graph window:'',F6.3,'','',F6.3,'' to'',
     :             F6.3,'','',F6.3,'';  size not defined'')',
     :             IOSTAT=IHX)
     :             (GRIDS(N,IZONE),N=5,8)
                END IF
                WRITE (*,'(A)') ALASTXT(1:SLEN(ALASTXT))
             ELSE
                WRITE (*,'(''   QAREA:  zone not defined'',A)') BLEEP
             END IF
*
*   QSMooth Apply a quick gaussian smooth to Y
*
          ELSE IF( SUBCMD .EQ. 'QSM' ) THEN
             CALL DECODE('QSM',PARAMS,1,1,SIGMA,'Sigma ',OK)
             IF( .NOT.(OK)) GO TO 5000
             DO 2440 I = 1, NPOINT
                WORK(I) = FLUX(I)
 2440        CONTINUE
             CALL SMOOTH(WAVE,WORK,FLUX,NPOINT,SIGMA)
*
*   Quit    Quit program
*
          ELSE IF( SUBCMD .EQ. 'Q' .OR. SUBCMD(1:2) .EQ. 'QU' .OR.
     :    SUBCMD(1:4) .EQ. 'LOGO' ) THEN

*   If ANSI terminal, clear screen, reset window, home cursor
*   (VAX specific)
             IF( ANSI) WRITE (*,'($,''+'',A)') CHAR(27)//'[2J'//CHAR(27)
     :       //'[1;25r'//CHAR(27)//'[H'
             IF( FFOPEN ) THEN
                WRITE (FFUNIT,'('' ''/'' '')')
                CLOSE (FFUNIT)
             END IF

*   Close down plotting

             CALL SGS_CLOSE

             NOTEND = .FALSE.

*  RDCAT:  Read data from a catalogue into the current arrays.
          ELSE IF( SUBCMD .EQ. 'RDCAT' ) THEN
             CALL RDCAT( PARAMS, WORV, TITLE, STATUS )

*  READ:  Read data into the current arrays.
          ELSE IF( SUBCMD .EQ. 'READ' ) THEN
             IF( USENDF ) THEN
                CALL READ( 'READ', PARAMS, WORV, TITLE, STATUS )
             ELSE

*         ----------------------------------------------------------------
             DO 12460 I = 1, 40
                IF( PARAMS(I:I) .NE. ' ') GO TO 12500
12460        CONTINUE
12480        CONTINUE

             CALL RDSTR( SUBCMD, 'File name', ' ', PARAMS, STATUS )
             IF( STATUS .NE. SAI__OK ) THEN
                OK = .FALSE.
                GO TO 5000
             END IF

12500        CONTINUE
              OPEN (UNIT=9,FILE=PARAMS(1:80),STATUS='OLD',
     :        FORM='UNFORMATTED',IOSTAT=IOS)
             IF( IOS .EQ. 0 ) THEN
                 READ (9,IOSTAT=IOS1) TITLE
                IF( IOS1 .EQ. 0 ) THEN
                   WRITE (*,'(''   READ - title:  '',A50)') TITLE(1:50)
                    READ (9,IOSTAT=IOS1) NBREAK, (BREAK(I),I=1,NBREAK)
                   IF( IOS1 .EQ. 0 ) THEN
                      NPOINT = BREAK(NBREAK)
                      IF( NPOINT.GT.ASIZE1 .OR. NPOINT.LE.0 ) THEN
                         WRITE (*,'(''   READ:  input error'')')
                         NPOINT = 0
                         GO TO 5000
                      END IF
                       READ (9,IOSTAT=IOS1) (WAVE(I),FLUX(I),I=1,NPOINT)
                   END IF
                END IF
                IF( IOS1 .NE. 0 ) THEN
                   WRITE (*,'(''   READ:  error on input'')')
                   NPOINT = 0
                   GO TO 5000
                END IF
             ELSE
                WRITE (*,'(''   READ:  error opening file'')')
                GO TO 5000
             END IF
             IF( IOS1 .NE. 0 .OR. IOS2 .NE. 0 .OR. IOS3 .NE. 0 .OR.
     :           IOS .NE. 0)
     :       THEN
                OK = .FALSE.
             END IF
              READ (9,IOSTAT=IOS) WORV
             IF( IOS .NE. 0 ) THEN
*   Earlier versions of WRITE did not write a WORV value, so for these input
*   files a value of 1.0 (i.e. a wavelength x-scale) must be assumed.
                WORV = 1.0
                WRITE (*,'(''   READ:  assumed WORV=1'',A)') BLEEP
             END IF
              CLOSE (9)
*         ----------------------------------------------------------------

             END IF

*  RECALL: Recall a RECORDed command string
          ELSE IF( SUBCMD .EQ. 'RECALL' ) THEN
             CALL DECODE('RECALL',PARAMS,0,0,VARRAY,' ',OK)
             IF( RESTRING .EQ. ' ' ) THEN
                WRITE (*,'(''   RECALL:  no RECORDed string'',A)') BLEEP
             ELSE
                CALL SSTRIP(RESTRING)
                WRITE (*,'(''   RECALLed string: '',A)')
     :          RESTRING(1:SLEN(RESTRING))
             END IF

*  RECORD: Record a command string.
          ELSE IF( SUBCMD .EQ. 'RECORD' ) THEN
             CALL RECORD( 'RECORD '//PARAMS, RESTRING, OK )

*  REPLAY: Replay a REMEMBERed command string
          ELSE IF( SUBCMD .EQ. 'REPLAY' ) THEN
             CALL DECODE('REPLAY',PARAMS,0,0,VARRAY,' ',OK)
             IF( .NOT.OK) GO TO 5000
             IF( RESTRING .EQ. ' ' ) THEN
                WRITE (*,
     :          '(''   REPLAY:  no RECORDed command string'',A)')
     :          BLEEP
                GO TO 5000
             END IF
             IF( COMTXT ) THEN
                COMND1 = RESTRING(1:SLEN(RESTRING))//','//COMND1
             ELSE
                COMND2 = RESTRING(1:SLEN(RESTRING))//','//COMND2
             END IF
             DELIM = 1
             IF( DEBUG ) THEN
                WRITE (*,'('' DELIM, COMND2: '',I2,'' '',A)') DELIM,
     :          COMND2(1:SLEN(COMND2))
             END IF

*  REPORTING: Set current MSG message filtering level
          ELSE IF( SUBCMD .EQ. 'REPORTING' ) THEN
             CALL REPORTING( 'REPORTING', PARAMS, STATUS )

*
*  RESTORE:  Restore stacks
          ELSE IF( SUBCMD .EQ. 'RESTORE' ) THEN
             KPTNST = NONSTK + 1
             IF( USENDF ) THEN
                CALL RESTORE( 'RESTORE', PARAMS, STATUS )
             ELSE

*         ----------------------------------------------------------------
             CALL SSTRIP(PARAMS)
             LLNGTH = SLEN(PARAMS)
             LBRACE = INDEX(PARAMS,']')
             IF( LLNGTH.LE.LBRACE ) THEN
                PARAMS(LLNGTH+1:LLNGTH+8) = 'SAVE.STK'
             END IF
             LLNGTH = SLEN(PARAMS)
             DO 2520 IL = LLNGTH, 1, -1
                IF( PARAMS(IL:IL) .EQ. ']') GO TO 2540
                IF( PARAMS(IL:IL) .EQ. '.') GO TO 2560
 2520        CONTINUE
 2540        CONTINUE
             PARAMS(LLNGTH+1:LLNGTH+4) = '.STK'
             LLNGTH = LLNGTH + 4
 2560        CONTINUE
!!             CALL DTOUPP(PARAMS)
              OPEN (UNIT=37,FILE=PARAMS(1:80),STATUS='OLD',
     :         FORM='UNFORMATTED',IOSTAT=IHX)
             IF( IHX .NE. 0 ) THEN
                 CLOSE (37)
                WRITE (*,'(''   RESTORE:  unable to open '',A)')
     :            PARAMS(1:LLNGTH)
                GO TO 5000
             END IF
             CALL IHRSTR(sysname,vmsrec,NONSTK,STKSZE,BSTSZE,
     :                     MAXSTK,XSTACK,YSTACK,
     :                     BSTACK,BSTNPT,POINTR,
     :                     STKNPT,BPOINT,STITLE,
     :                  WORVST,STKLST,BSTLST,SUBCHK)
             CLOSE (37)
             IF( .NOT.SUBCHK ) THEN
                CLOSE (37)
                WRITE (*,'(''   RESTORE:  error restoring '',A)')
     :            PARAMS(1:LLNGTH)
                GO TO 5000
             END IF
             WRITE (*,'(''   RESTORE:  data restored from '',A)')
     :       PARAMS(1:LLNGTH)
*         ----------------------------------------------------------------

             END IF

             VARRAY(1) = 1.0
             VARRAY(2) = NONSTK
             KP2 = NONSTK
             lunit = 6
             GO TO 2820

*  RETITLE:  Change TITLE on stack
          ELSE IF( SUBCMD .EQ. 'RETITLE' ) THEN
             IF( PARAMS .EQ. ' ' ) THEN
                CALL DECODE('RETITLE',PARAMS,1,1,VARRAY,'Stack_entry ',
     :          OK)
                IF( .NOT.OK) GOTO 5000
             ELSE
                CALL SSTRIP(PARAMS)
                LBRK = INDEX(PARAMS,' ')
                IHHLP = PARAMS(1:LBRK)
                PARAMS = PARAMS((LBRK+1):)
                CALL DECODE('RETITLE',IHHLP,1,1,VARRAY,'Stack_entry ',
     :          OK)
                IF( .NOT.OK) GOTO 5000
             END IF
             NTRY = NINT(VARRAY(1))
             IF( NTRY.LT.1 .OR. NTRY.GT.NONSTK ) THEN

*             Build and report message.
                CALL ITOCHR (NTRY, STRING1, NCHAR1, ICSTAT)

                BIGSTR= '   RETITLE:  Stack entry '//STRING1(:NCHAR1)//
     :          ' does not exist'

                WRITE (*,*)BIGSTR
                BIGSTR = ' '

                GOTO 5000
             END IF
             IF( PARAMS .EQ. ' ' .AND. TPROMPT ) THEN

                CALL RDSTR( SUBCMD, 'Title', ' ', PARAMS, STATUS )
                IF( STATUS .NE. SAI__OK ) THEN
                   OK = .FALSE.
                   GO TO 5000
                END IF

             END IF
             STITLE(NTRY) = PARAMS(1:80)
             PARAMS = PARAMS(81:)
             IF( PARAMS .NE. ' ' ) THEN
                WRITE (*,
     :          '(''   RETITLE:  only the first 80 characters '',
     :          '' accepted'',A)') BLEEP
             END IF
             IF( .NOT.SUBCHK ) THEN
                WRITE (*,'(''   RETITLE:  closing quotes added'')')
                SUBCHK = .TRUE.
                GOTO 5000
             END IF

*  RXR:  Restrict x-range of data
          ELSE IF( SUBCMD .EQ. 'RR' .OR. SUBCMD .EQ. 'RXR' ) THEN
             IF( SUBCMD .EQ. 'RR' ) THEN
                WRITE (*,'(''   RR command renamed RXR'',A)') BLEEP
                SUBCMD = 'RXR'
                WRITE (*,'(''   (command executed)'')')
             END IF
             CALL DECODE('RXR',PARAMS,2,2,VARRAY,'X1 X2 ',OK)
             IF( .NOT.(OK)) GO TO 5000
             XMAXX = MAX(VARRAY(1),VARRAY(2))
             XMINN = MIN(VARRAY(1),VARRAY(2))
             VARRAY(1) = XMINN
             VARRAY(2) = XMAXX
             CALL FINDIT(WAVE,NPOINT,VARRAY(1),I1,1)
             CALL FINDIT(WAVE,NPOINT,VARRAY(2),I2,0)
             IF( WAVE(1).GT.WAVE(NPOINT) ) THEN
                DO 2570 I = 1, NPOINT
                   IF( WAVE(I).LT.XMAXX ) THEN
                      I1 = I
                      DO 2562 J = I1, NPOINT
                         IF( WAVE(J).LT.XMINN ) THEN
                            I2 = J - 1
                            GO TO 2580
                         END IF
 2562                 CONTINUE
                      I2 = NPOINT
                      GO TO 2580
                   END IF
 2570           CONTINUE
                I1 = NPOINT
                I2 = NPOINT
             END IF
 2580        CONTINUE
             I1 = I1 + 1
             J = I1
             DO 2600 I = 1, I2 - I1 + 1
                WAVE(I) = WAVE(J)
                FLUX(I) = FLUX(J)
                J = J + 1
 2600        CONTINUE
             NPOINT = I2 - I1 + 1
             J = 1
             I2 = 1
 2620        CONTINUE
             IF( (BREAK(J)-I1+1).LT.NPOINT ) THEN
                IF( (BREAK(J)-I1+1).GT.1 ) THEN
                   BREAK(I2) = BREAK(J) - I1 + 1
                   I2 = I2 + 1
                END IF
                J = J + 1
                GO TO 2620
             END IF
             BREAK(I2) = NPOINT
             NBREAK = I2
*
*   RYR -- cut out Y values outside designated range
*
*   Arguments -- Ymin Ymax (Xmin Xmax)
          ELSE IF( SUBCMD .EQ. 'RYR' ) THEN
             VARRAY(3) = MIN(WAVE(1),WAVE(NPOINT))
             VARRAY(4) = MAX(WAVE(1),WAVE(NPOINT))
             CALL DECODE('RYR',PARAMS,2,4,VARRAY,'Ymin Ymax Xmin Xmax ',
     :                   OK)
             IF( .NOT.OK ) THEN
                OK = .TRUE.
                GO TO 5000
             END IF
             TEMP = VARRAY(1)
             VARRAY(1) = MIN(VARRAY(1),VARRAY(2))
             VARRAY(2) = MAX(VARRAY(2),TEMP)
             TEMP = VARRAY(3)
             VARRAY(3) = MIN(VARRAY(3),VARRAY(4))
             VARRAY(4) = MAX(VARRAY(4),TEMP)
             DO 2640 I = 1, NPOINT
                IF( WAVE(I).GE.VARRAY(3) .AND. WAVE(I).LE.VARRAY(4))
     :          THEN
                   IF( FLUX(I).LT.VARRAY(1) .OR. FLUX(I).GT.VARRAY(2))
     :             THEN
                      FLUX(I) = -642.135
                   END IF
                END IF
 2640        CONTINUE
             TSTVAL = -642.135
             CALL SRTBRK(ASIZE1,WAVE,FLUX,NPOINT,MAXBRK,BREAK,NBREAK,
     :       TSTVAL,OK)
             IF( .NOT.OK ) THEN
                OK = .TRUE.
                GO TO 5000
             END IF

*  SAVE: Save stack contents.
          ELSE IF( SUBCMD .EQ. 'SAVE' ) THEN
             IF( USENDF ) THEN
                CALL SAVE( 'SAVE', PARAMS, STATUS )
             ELSE

*         ----------------------------------------------------------------
             IF( NONSTK .EQ. 0 ) THEN
                WRITE (*,'(''   SAVE:  stack is empty'')')
                GO TO 5000
             END IF
!!!             CALL DTOUPP(PARAMS)
             CALL SSTRIP(PARAMS)
             MLNGTH = INDEX(PARAMS,'-')
             IF( MLNGTH .NE. 0 ) THEN
                DO 2650 I = MLNGTH - 1, 1, -1
                   IF( PARAMS(I:I) .NE. ' ' ) THEN
                      NLNGTH = I
                      GO TO 2660
                   END IF
 2650           CONTINUE
                NLNGTH = 0
 2660           CONTINUE
                OLNGTH = INDEX(PARAMS,' ') - 1
                IF( OLNGTH.GE.NLNGTH ) THEN
                   PARAMS(10:) = PARAMS(1:)
                   PARAMS(1:8) = 'SAVE.STK'
                END IF
             END IF
             DO 2680 I = 40,1,-1
                IF( PARAMS(I:I) .NE. ' ') GO TO 2700
 2680        CONTINUE
             PARAMS(1:8) = 'SAVE.STK'
             GO TO 2701
 2700        CONTINUE
             I=INDEX(PARAMS,' ')
             params = params(1:i-1)//'.STK'//params(i:)
 2701        CONTINUE
!!!             CALL DTOUPP(PARAMS)
             CALL SSTRIP(PARAMS)
             LLNGTH = INDEX(PARAMS,' ') - 1
             VARRAY(1) = 0.0
             VARRAY(2) = 0.0
             MLNGTH = INDEX(PARAMS,'-')
             IF( MLNGTH .NE. 0 ) THEN
                PARAMS(MLNGTH:MLNGTH) = ' '
             END IF
             CALL DECODE('SAVE',PARAMS(LLNGTH+1:),0,2,VARRAY,
     :       'Entry_1 Entry_2 ',OK)
             IF( .NOT.OK ) THEN
                OK = .TRUE.
                GO TO 5000
             END IF
             ISV1 = NINT(VARRAY(1))
             ISV2 = NINT(VARRAY(2))
             IF( ISV1.LT.0 ) THEN
                WRITE (*,'(''   SAVE:  Entry_1 = 1 assumed'')')
                ISV1 = 1
             ELSE IF( ISV1 .EQ. 0 ) THEN
                ISV1 = 1
             ELSE IF( ISV1.GT.NONSTK ) THEN
                WRITE (*,
     :          '(''   SAVE:  Entry_1 larger than max. stack entry'')')
                GO TO 5000
             END IF
             IF( ISV2.LT.0 ) THEN

*             Build and report message.
                CALL ITOCHR (NONSTK, STRING1, NCHAR1, ICSTAT)
                BIGSTR= '   SAVE:  Entry2 = '//STRING1(:NCHAR1)//
     :          ' assumed'
                WRITE (*,*)BIGSTR
                BIGSTR = ' '
                ISV2 = NONSTK
             ELSE IF( ISV2 .EQ. 0 ) THEN
                ISV2 = NONSTK
             ELSE IF( ISV2.GT.NONSTK ) THEN
                ISV2 = NONSTK
             END IF
             IF( ISV1.GT.ISV2 ) THEN
                WRITE (*,'(''   SAVE:  Entry_1 greater than Entry_2'')')
                GO TO 5000
             END IF
             DO 2720 IL = LLNGTH, 1, -1
                LDOT = IL
                IF( PARAMS(IL:IL) .EQ. '.') GO TO 2740
 2720        CONTINUE
             LDOT = 0
 2740        CONTINUE
             LBRACE = INDEX(PARAMS,']')
             IF( LDOT.LE.LBRACE ) THEN
                PARAMS(LLNGTH+1:LLNGTH+4) = '.STK'
                LLNGTH = LLNGTH + 4
             END IF
             CALL SSTRIP(PARAMS)
             LBRK = INDEX(PARAMS,' ')
             IF( PARAMS(LBRK:) .NE. ' ' ) THEN
                PARAMS(LBRK:) = ' '
                WRITE (*,'(''   SAVE:  superfluous text ignored'')')
                IF( BEEP) WRITE (*,'(A)') BLEEP
             END IF

*    Entry point for EXIT

 2750        CONTINUE
             CLOSE(37)
             IF( SUBCMD( : 3 ) .EQ. 'EXI' ) THEN
                OPEN (UNIT=37,FILE=PARAMS(1:80),STATUS='UNKNOWN',
     :                FORM='UNFORMATTED',IOSTAT=IHX)
             ELSE
                OPEN (UNIT=37,FILE=PARAMS(1:80),STATUS='NEW',
     :                FORM='UNFORMATTED',IOSTAT=IHX)
             END IF
             IF( IHX .NE. 0 ) THEN
                CLOSE (37)
                WRITE (*,'(''   SAVE:  unable to open '',A)')
     :            PARAMS(1:LLNGTH)
                GO TO 5000
             END IF
             CALL IHSVAL(SUBCHK)
             CLOSE (37)
             IF( .NOT.SUBCHK ) THEN
                WRITE (*,'(''   SAVE:  error saving '',A)')
     :          PARAMS(1:LLNGTH)
                GO TO 5000
             END IF
             WRITE (*,'(''   SAVE:  '',A,'' saved'')') PARAMS(1:LLNGTH)

*             Build and report message.
                CALL ITOCHR (ISV1, STRING1, NCHAR1, ICSTAT)
                CALL ITOCHR (ISV2, STRING2, NCHAR2, ICSTAT)
                BIGSTR= '          (Entries'//STRING1(:NCHAR1)//
     :          ' - '//STRING2(:NCHAR2)//')'
                WRITE (*,*)BIGSTR
                BIGSTR = ' '
*         ----------------------------------------------------------------
             END IF

*  SCROLLVT:  Set scrolling region on ANSI terminal
          ELSE IF( SUBCMD .EQ. 'SCROLLVT' ) THEN
             CHECKPARAMS = PARAMS(1:1)
             CALL DECODE('SCROLLVT',PARAMS,0,2,VARRAY,' ',OK)
             IF( .NOT.OK) GO TO 5000
             IF( CHECKPARAMS .NE. ' ' ) THEN
                NTOP = VARRAY(1)
                NBOTTOM = VARRAY(2)
             ELSE
                NTOP = 1
                NBOTTOM = 25
             END IF
*   Validate (top line is 1, bottom is 25, minimum 2 lines)
             IF( NTOP.GE.1 .AND. NTOP.LE.25 .AND. NBOTTOM.GE.1 .AND.
     :       NBOTTOM.LE.25 .AND. (NBOTTOM-NTOP).GT.1 ) THEN
                WRITE (CTOP,'(I2)') NTOP
                IF( CTOP(:1) .EQ. ' ' ) THEN
                   JTOP = 2
                ELSE
                   JTOP = 1
                END IF
                WRITE (CBOTTOM,'(I2)') NBOTTOM
                IF( CBOTTOM(:1) .EQ. ' ' ) THEN
                   JBOTTOM = 2
                ELSE
                   JBOTTOM = 1
                END IF
*   Output escape sequences
                WRITE (*,'($,''+'',A)') CHAR(27)//'[2J'//CHAR(27)
     :          //'['//CTOP(JTOP:)
     :          //';'//CBOTTOM(JBOTTOM:)
     :          //'r'//CHAR(27)
     :          //'['//CTOP(JTOP:)//';1H'
                ANSI = .TRUE.
             ELSE
                WRITE (*,
     :         '(''   SCROLLVT:  lines outside range (1-25)'',A)')
     :         BLEEP
             END IF
*
*   SDOCONT  Calculate continuum for 128220
*
          ELSE IF( SUBCMD .EQ. 'SDOCONT' ) THEN
             IF( NPOINT.LT.100 ) THEN
                WRITE (*,'(''   SDOCONT:  insufficient data'')')
                GO TO 5000
             ELSE IF( WAVE(1).LT.1100.0 .OR. WAVE(1).GT.2000.0 ) THEN
                WRITE (*,'(''   SDOCONT:  not SWP??'')')
             ELSE
                CALL SDOCONT(WAVE,FLUX,NPOINT,WORK)
             END IF
*
*   SDOFIT   Fit profiles to 128220
*
          ELSE IF( SUBCMD .EQ. 'DACFIT' .OR. SUBCMD .EQ. 'DACCALC'
     :    .OR. SUBCMD .EQ. 'DACALC' ) THEN
             IF( SUBCMD .EQ. 'DACALC') SUBCMD = 'DACCALC'
             IF( NPOINT.LE.0 ) THEN
                WRITE (*,'(''   '',A,'':  no data in current arrays'')')
     :          SUBCMD(1:SLEN(SUBCMD))
                OK = .FALSE.
                GO TO 5000
             END IF
             DO 2760 I = 1, MAXSTK
                VARRAY(I) = 0.0
 2760        CONTINUE
             CALL DECODE(SUBCMD(1:SLEN(SUBCMD)),PARAMS,0,
     :       MAXSTK,VARRAY,'Lambda(blue) ',OK)
             IF( .NOT.OK) GO TO 5000
             CALL DACFIT(WAVE,FLUX,NPOINT,VARRAY,MAXSTK,DEVTYP,
     :       NPLOTS,CURSOR,TITLE,OK,INWORK,SUBCMD,
     :       ASIZE1,MAXBRK,BREAKS,NBREAK)
             IF( .NOT.OK ) THEN
                OK = .TRUE.
                GO TO 5000
             END IF
*
*   SSD - draw lines showing doublets on SDO plots
*
          ELSE IF( (SUBCMD .EQ. 'SSD') .OR. (SUBCMD .EQ. 'DDRAW') ) THEN
             DO 2780 I = 1, MAXSTK
                VARRAY(I) = 0.0
 2780        CONTINUE
             CALL DECODE('SUBCMD(1:SLEN(SUBCMD))',PARAMS,0,MAXSTK,
     :       VARRAY,'Pstn. ',OK)
             IF( .NOT.CURSOR .OR. NPLOTS .EQ. 0 ) THEN
                WRITE (*,'(''   '',A,'':  no plot available'',A)')
     :          SUBCMD(1:SLEN(SUBCMD)), BLEEP
                GO TO 5000
             END IF
             IF( .NOT.OK) GO TO 5000
             XMID = (XLIM1+XLIM2)*0.5
             CALL SSDRAW(VARRAY,MAXSTK)
*
*   SHELL   Spawn the shell
*
          ELSE IF( SUBCMD .EQ. 'SHELL' ) THEN
C           IF( sysname .EQ. 'VMS' ) THEN
C             CALL DECODE('SHELL',PARAMS,0,0,VARRAY,' ',OK)
C             IHX = LIB$SPAWN(,,,,,,,,,,'shell>$ ',)
C           ELSE
              CALL SYSEXE( PARAMS, 0, STATUS )
C           END IF
*
*   SL(WR) List (or file) stack entries
*
          ELSE IF( SUBCMD .EQ. 'SL' .OR. SUBCMD .EQ. 'SLWR' ) THEN
             VARRAY(1) = 1.0
             VARRAY(2) = REAL(MAX(NONSTK,1))
             IF( SUBCMD .EQ. 'SL' ) THEN
                lunit=6
             ELSE
                LUNIT=32
             END IF
 2800        CONTINUE
             I = INDEX(PARAMS,'-')
             IF( I .NE. 0 ) THEN
                PARAMS(I:I) = ' '
                GO TO 2800
             END IF
             IF( SUBCMD .EQ. 'SL' ) THEN
                CALL XDCODE('SL',PARAMS,0,2,VARRAY,' ',OK)
                IF( .NOT.OK) GO TO 5000
             ELSE
                CALL XDCODE('SLWR',PARAMS,0,2,VARRAY,' ',OK)
                IF( .NOT.OK) GO TO 5000
             END IF
             IF( .NOT.OK) GO TO 5000
             KPTNST = MAX(1,NINT(VARRAY(1)))
             KP2 = MAX(1,NINT(VARRAY(2)))
             IF( KP2.LT.KPTNST ) THEN
                I = KPTNST
                KPTNST = KP2
                KP2 = I
             END IF
 2820        CONTINUE
C             CLOSE (6)
             IX=0
             IF( SUBCMD .EQ. 'SLWR' ) THEN
                OPEN (UNIT=lunit,FILE='stack.lis',STATUS='NEW',
     :                IOSTAT=IX)

C             ELSE
C                OPEN (UNIT=6,FILE='TT:',STATUS='NEW',IOSTAT=IX)
             END IF
             IF( IX .NE. 0 ) THEN
                IF( SUBCMD .EQ. 'SLWR' ) THEN
                   WRITE (*,'(''   SLWR:  unable to open stack.lis'')')
                   CLOSE (lunit)
C                ELSE
C                   WRITE (*,
C    :                  '(''   SL:  unable to open TT as FOR006'')')
C                   CLOSE (6)
                END IF
                GO TO 5000
             END IF
             IF( SUBCMD .EQ. 'SLWR' ) THEN
                IPMAX = 80
             ELSE
                IPMAX = 42
             END IF
             WRITE (lunit,
     :       '(''       I    N     X1         X2              '',
     :       ''TITLE'')')
             IF( VARRAY(1).LT.0.1 .OR. VARRAY(2).LT.0.1 ) THEN
                IF( NPOINT .NE. 0 ) THEN

                   WRITE (lunit,'('' Current'',I6,1X,G11.5,1X,G11.5,'//
     :                    '1X,A)') NPOINT, WAVE(1), WAVE(NPOINT),
     :                    TITLE(1:IPMAX)
                    CONTINUE
                ELSE
                   WRITE (lunit,
     :             '('' Current    0       '',
     :             ''                        (empty)'')')
                END IF
             END IF
             IF( VARRAY(2) .NE. 0 ) THEN
                IF( NONSTK .EQ. 0 ) THEN
                   WRITE (lunit,
     :             '('' Stack      0       '',
     :             ''                        (empty)'')')
                ELSE
                   DO 2825 I = MIN(NONSTK,KPTNST), MIN(NONSTK,KP2)
                      WRITE (lunit,'(I8,I6,1X,2G11.4,1X,A)') I,
     :                STKNPT(I), XSTACK(POINTR(I)),
     :                XSTACK(POINTR(I)+STKNPT(I)-1), STITLE(I)(1:IPMAX)
                      IF( MOD((I-KPTNST+1),25) .EQ. 0 .AND.
     :                    I .NE. NONSTK) THEN
                         IF( SUBCMD .EQ. 'SL' .AND. ISATTY ) THEN
                            WRITE (lunit,
     :                      '(''   SL:  hit return for more>'',$)')
                            IHHLP = ' '
                            READ (*,'(A)',ERR=6000) IHHLP(1:10)
                            CALL SSTRIP(IHHLP)
                            CALL DTOUPP(IHHLP)
                            IF( IHHLP(1:1) .EQ. 'Q') GO TO 2830
                         END IF
                      END IF
 2825              CONTINUE
                   IF( KPTNST.GT.NONSTK .OR. KP2.GT.NONSTK ) THEN
                      WRITE (lunit,'(''      (Last entry)'')')
                   END IF
 2830              CONTINUE
                   IF( lunit .NE. 6 ) CLOSE (lunit)
                   IF( .NOT.SUBCHK ) THEN
                      SUBCHK = .TRUE.
                      GO TO 5000
                   END IF
                END IF
             END IF
*
*   SCREENRD  - read data from terminal
*
          ELSE IF( SUBCMD .EQ. 'SCREENRD' ) THEN
             BRKTMP = -13579.2468
             CALL DECODE('SCREENRD',PARAMS,0,1,BRKTMP,' ',OK)
             IF( .NOT.OK) GO TO 5000
             NPOINT = 0
             TITLE = ' Data input from terminal'
             NBREAK = 0
             WRITE(*,*) '   SCREENRD:  '//ESC//' to terminate input'
             DO 2860 I = 1, 1000000
                PARAMS = ' '
 2840           CONTINUE

                BIGSTR = ' '
                WRITE (BIGSTR,'(I3)') NPOINT + 1
                CALL RDSTR( SUBCMD, BIGSTR(:3), ' ', PARAMS, STATUS )
                BIGSTR = ' '

                IF( STATUS .NE. SAI__OK ) THEN
                   OK = .FALSE.
                   GO TO 5000
                END IF

                IF( PARAMS .EQ. ' ') GO TO 2840
                IF( PARAMS .EQ. ESC) GO TO 2870

                CALL DECODE('SCREENRD',PARAMS,2,2,VARRAY,'X_val Y_val ',
     :          OK)
                IF( .NOT.OK ) THEN
                   OK = .TRUE.
                   GO TO 2840
                END IF
                IF( VARRAY(2) .EQ. BRKTMP ) THEN
                   NBREAK = NBREAK + 1
                   BREAKS(NBREAK) = NPOINT
                ELSE
                   NPOINT = NPOINT + 1
                   WAVE(NPOINT) = VARRAY(1)
                   FLUX(NPOINT) = VARRAY(2)
                END IF
 2860        CONTINUE
 2870        CONTINUE
 2880        CONTINUE
             NBREAK = NBREAK + 1
             BREAKS(NBREAK) = NPOINT
             WRITE (*,'(''   SCREENRD:'',I3,'' points input'')') NPOINT
             WORV = 1.0
*
*   SMooth  Apply a gaussian smooth to Y
*
          ELSE IF( SUBCMD .EQ. 'SM' ) THEN
             CALL DECODE('SM',PARAMS,1,1,SIGMA,'Sigma ',OK)
             IF( .NOT.(OK)) GO TO 5000
             DO 2900 I = 1, NPOINT
                WORK(I) = FLUX(I)
 2900        CONTINUE
             CALL IHSMTH(WAVE,WORK,FLUX,NPOINT,SIGMA)
*
*   SNIP   Cut out bits of spectrum
*
          ELSE IF( SUBCMD .EQ. 'SNIP' ) THEN
             IF( PARAMS .EQ. ' ' ) THEN
                IF( .NOT.CURSOR .OR. NPLOTS .EQ. 0 ) THEN
                   WRITE (*,'(''   SNIP:  no plot available'')')
                   GO TO 5000
                END IF
             END IF
             IF( NPOINT .EQ. 0 ) THEN
                WRITE (*,'(''   SNIP:  no data in current arrays'')')
                GO TO 5000
             END IF
             CALL SNIP(ASIZE1,WAVE,FLUX,NPOINT,MAXBRK,BREAKS,NBREAK,
     :       PARAMS,SUBCHK)
             IF( .NOT.SUBCHK) GO TO 5000

*  SP0WR: Write a SPECTRUM format '0' file
          ELSE IF( SUBCMD .EQ. 'SP0WR' ) THEN
             IF( USENDF  ) THEN
                CALL SP0WR( 'SP0WR', PARAMS, TITLE, STATUS )
             ELSE

*         ----------------------------------------------------------------
             IF( NPOINT .EQ. 0 ) THEN
                WRITE (*,'(''   SP0WR:  no data in current arrays'')')
                GO TO 5000
             END IF

             CALL SSTRIP(PARAMS)
             LBRK = INDEX(PARAMS,' ')
             IF( PARAMS(LBRK:) .NE. ' ' ) THEN
                WRITE (*,'(''   SP0WR:  superfluous text ignored'')')
                PARAMS(LBRK:) = ' '
                IF( BEEP) WRITE (*,'(A)') BLEEP
             END IF
             CALL OSP0WR(PARAMS(1:80),TITLE(1:79), IHHEAD(1:79),WORKSZ,
     :                   WORK,NPOINT,WAVE,FLUX,NBREAK,BREAK,SUBCHK,
     :                   STATUS )
             IF( .NOT.SUBCHK ) THEN
                SUBCHK = .TRUE.
                GO TO 5000
             END IF
*         ----------------------------------------------------------------

             END IF

*  SP2WR:  Output formatted SPECTRUM file
          ELSE IF( SUBCMD .EQ. 'SP2WR' ) THEN
             IF( USENDF ) THEN
                CALL MSGOUT(SUBCMD,'Command not available in NDF '//
     :                      'mode.', .TRUE., STATUS )
                GO TO 5000
             END IF

             IF( NPOINT .EQ. 0 ) THEN
                WRITE (*,'(''   SP2WR:  no data in current arrays'')')
                GO TO 5000
             END IF

             CALL SSTRIP(PARAMS)
             LBRK = INDEX(PARAMS,' ')
             IF( PARAMS(LBRK:) .NE. ' ' ) THEN
                WRITE (*,'(''   SP2WR:  superfluous text ignored'')')
                PARAMS(LBRK:) = ' '
                IF( BEEP) WRITE (*,'(A)') BLEEP
             END IF
             CALL SP2WR(PARAMS(1:80),TITLE(1:79),IHHEAD(1:79),WORKSZ,
     :                  WORK,NPOINT,WAVE,FLUX,NBREAK,BREAK,SUBCHK,
     :                  STATUS)
             IF( .NOT.SUBCHK ) THEN
                SUBCHK = .TRUE.
                GO TO 5000
             END IF

*  SP0RD: Read a SPECTRUM format '0' file
          ELSE IF( SUBCMD .EQ. 'SP0RD' .OR. SUBCMD .EQ. 'PANICRD' ) THEN
             IF( USENDF  ) THEN

                IF( SUBCMD .EQ. 'SP0RD' ) THEN
                   CALL SP0RD( 'SP0RD', PARAMS, WORV, TITLE, STATUS )
                ELSE
                   CALL MSGOUT(SUBCMD,'Command not available in NDF '//
     :                         'mode.', .TRUE., STATUS )
                END IF

             ELSE

*         ----------------------------------------------------------------
             LJACK = .FALSE.
             IF( SUBCMD .EQ. 'OPANICRD') LJACK = .TRUE.
             DO 2920 I = 1, 80
                IF( PARAMS(I:I) .NE. ' ') GO TO 2960
 2920        CONTINUE
 2940        CONTINUE

             CALL RDSTR( SUBCMD, 'File name', ' ', PARAMS, STATUS )
             IF( STATUS .NE. SAI__OK ) THEN
                OK = .FALSE.
                GO TO 5000
             END IF

 2960        CONTINUE
             CALL SSTRIP(PARAMS)
             IF( PARAMS(1:2) .EQ. '!!' ) THEN
                WRITE (*,'(''   SP0RD:  user-induced abort'')')
                GO TO 5000
             END IF
!!!!             CALL DTOUPP(PARAMS)
             I1 = SLEN(PARAMS)
             I2 = INDEX(PARAMS,' ') - 1
             IF( PARAMS .NE. ' ' .AND. I1 .NE. I2 ) THEN
                WRITE (*,
     :          '(''   SP0RD:  superfluous parameters ignored'',A)')
     :          BLEEP
                PARAMS(INDEX(PARAMS,' '):) = ' '
             END IF
             IHX = 0
               OPEN (UNIT=7,FILE=PARAMS(1:80),
     :               STATUS='OLD',
     :               IOSTAT=IHX,ACCESS='SEQUENTIAL',
     :               FORM='UNFORMATTED',
     :               ERR=2970)
               IF( IHX .NE. 0 ) THEN
 2970             CONTINUE
                  WRITE (*,'(''   SP0RD:  unable to open '',A)')
     :            PARAMS(1:SLEN(PARAMS))
                  CLOSE (7)
                  GO TO 5000
               ELSE
                  IUNIT = 7
                  CALL OSP0RD(ASIZE1,WAVE,FLUX,
     :              NPOINT,MAXBRK,BREAK,NBREAK,
     :              TITLE,WORV,IUNIT,LJACK,SUBCHK)
                  IF( .NOT.SUBCHK ) THEN
                     SUBCHK = .TRUE.
                     GO TO 5000
                  END IF
               END IF
*         ----------------------------------------------------------------

             END IF

* SP1RD:  Read a SPECTRUM format '1' file
          ELSE IF( SUBCMD .EQ. 'SP1RD' ) THEN
             IF( USENDF ) THEN
                CALL MSGOUT(SUBCMD,'Command not available in NDF '//
     :                      'mode.', .TRUE., STATUS )
                GO TO 5000
             END IF

             DO 2980 I = 1, 80
                IF( PARAMS(I:I) .NE. ' ') GO TO 3020
 2980        CONTINUE
 3000        CONTINUE

             CALL RDSTR( SUBCMD, 'File name', ' ', PARAMS, STATUS )
             IF( STATUS .NE. SAI__OK ) THEN
                OK = .FALSE.
                GO TO 5000
             END IF

 3020        CONTINUE
             CALL SSTRIP(PARAMS)
             OPEN (UNIT=7,FILE=PARAMS(1:80),STATUS='OLD',
     :       IOSTAT=IHX,ACCESS='SEQUENTIAL',ERR=3030)
             IF( IHX .NE. 0 ) THEN
 3030           CONTINUE
                WRITE (*,'(''   SP1RD:  unable to open '',A)')
     :          PARAMS(1:SLEN(PARAMS))
                CLOSE (7)
                GO TO 5000
             ELSE
                IUNIT = 7
                CALL SP1RD(ASIZE1,WAVE,FLUX,NPOINT,MAXBRK,BREAK,NBREAK,
     :           TITLE,WORV,IUNIT,SUBCHK)
                IF( .NOT.SUBCHK ) THEN
                   SUBCHK = .TRUE.
                   GO TO 5000
                END IF
             END IF
*
*   SP2RD  -  Read a SPECTRUM format '2' file
*
          ELSE IF( SUBCMD .EQ. 'SP2RD' ) THEN
             IF( USENDF ) THEN
                CALL MSGOUT(SUBCMD,'Command not available in NDF '//
     :                      'mode.', .TRUE., STATUS )
                GO TO 5000
             END IF
             DO 3040 I = 1, 80
                IF( PARAMS(I:I) .NE. ' ') GO TO 3080
 3040        CONTINUE
 3060        CONTINUE

             CALL RDSTR( SUBCMD, 'File name', ' ', PARAMS, STATUS )
             IF( STATUS .NE. SAI__OK ) THEN
                OK = .FALSE.
                GO TO 5000
             END IF

 3080        CONTINUE
             CALL SSTRIP(PARAMS)
             OPEN (UNIT=7,FILE=PARAMS(1:80),STATUS='OLD',
     :       IOSTAT=IHX,ACCESS='SEQUENTIAL',ERR=3090)

             IF( IHX .NE. 0 ) THEN
 3090           CONTINUE
                WRITE (*,'(''   SP2RD:  unable to open '',A)')
     :          PARAMS(1:SLEN(PARAMS))
                CLOSE (7)
                GO TO 5000
             ELSE
                IUNIT = 7
                CALL SP2RD(ASIZE1,WAVE,FLUX,NPOINT,MAXBRK,BREAK,NBREAK,
     :          TITLE,WORV,IUNIT,SUBCHK)
                IF( .NOT.SUBCHK ) THEN
                   SUBCHK = .TRUE.
                   GO TO 5000
                END IF
             END IF
*
*   SQRTX  takes square root of X
*
          ELSE IF( SUBCMD .EQ. 'SQRTX' ) THEN
             CALL DECODE('SQRTX',PARAMS,0,0,VARRAY,' ',OK)
             TSTVAL = -1.0
             NTST = 0
             DO 3100 I = 1, NPOINT
                IF( WAVE(I).GE.0.0 ) THEN
                   WAVE(I) = SQRT(WAVE(I))
                ELSE
                   FLUX(I) = TSTVAL
                   NTST = NTST + 1
                END IF
 3100        CONTINUE
             IF( NTST.GT.0 ) THEN

*             Build and report message.
                CALL ITOCHR (NTST, STRING1, NCHAR1, ICSTAT)
                BIGSTR= '  SQRTX: '//STRING1(:NCHAR1)//
     :                  ' negative points lost'
                WRITE (*,*)BIGSTR,BLEEP
                BIGSTR = ' '

                CALL SRTBRK(ASIZE1,WAVE,FLUX,NPOINT,MAXBRK,BREAK,NBREAK,
     :          TSTVAL,OK)
             END IF
*
*   SQRTY  takes square root of Y
*
          ELSE IF( SUBCMD .EQ. 'SQRTY' ) THEN
             CALL DECODE('SQRTY',PARAMS,0,0,VARRAY,' ',OK)
             TSTVAL = -1.0
             NTST = 0
             DO 3120 I = 1, NPOINT
                IF( FLUX(I).GE.0.0 ) THEN
                   FLUX(I) = SQRT(FLUX(I))
                ELSE
                   FLUX(I) = TSTVAL
                   NTST = NTST + 1
                END IF
 3120        CONTINUE
             IF( NTST.GT.0 ) THEN

*             Build and report message.
                CALL ITOCHR (NTST, STRING1, NCHAR1, ICSTAT)
                BIGSTR= '  SQRTY: '//STRING1(:NCHAR1)//
     :                  ' negative points lost'
                WRITE (*,*)BIGSTR,BLEEP
                BIGSTR = ' '

                CALL SRTBRK(ASIZE1,WAVE,FLUX,NPOINT,MAXBRK,BREAK,NBREAK,
     :          TSTVAL,OK)
             END IF
*
*   STAROT - stellar rotational broadening
*
          ELSE IF( SUBCMD .EQ. 'STAROT' ) THEN
             CALL DECODE('STAROT',PARAMS,1,1,VARRAY,'Velocity ',OK)
             IF( VARRAY(1).LT.0.0 ) THEN
                WRITE (*,
     :          '(''   STAROT:  rotational velocity must be +ve'')')
                OK = .FALSE.
             END IF
             IF( .NOT.OK) GO TO 5000
             TSTVAL = -123.456
             NTEST = 0
             VELROT = VARRAY(1)
             CALL IHROT(WAVE,FLUX,WORK,NPOINT,NTEST,TSTVAL,VELROT)
             IF( NTEST.GT.0 ) THEN
                CALL SRTBRK(ASIZE1,WAVE,FLUX,NPOINT,MAXBRK,BREAK,NBREAK,
     :          TSTVAL,OK)
             END IF
*
*   STATUS   tells status of program
*
          ELSE IF( SUBCMD .EQ. 'STATUS' ) THEN
             CALL DECODE('STATUS',PARAMS,0,0,VARRAY,' ',OK)
             WRITE (*,'(''   STATUS:  Device -'',I5)') DEVTYP
             IF( POLTST ) THEN
                WRITE (*,'(''            Plot type - POLY'')')
             ELSE IF( HSTTST ) THEN
                WRITE (*,'(''            Plot type - HIST'')')
             ELSE IF( MRKTST ) THEN
                WRITE (*,'(''            Plot type - MARK'',
     :          I2,'','',I3)') MARKSTYLE, NSIDES
                WRITE (*,'(''                        FILL'',L2)')
     :          FILL
             END IF
             IF( POLTST .OR. HSTTST ) THEN
                WRITE (*,'(''            Line type -'',I2)') LNTYPE
             END IF
             IF( COLOUR ) THEN
                WRITE (*,'(''            Colour -'',I3)') IPAL
             END IF
             IHHEAD = ' '
             IF( ROTST .AND. COLOUR ) THEN
                IHHEAD(1:) = 'COLOUR'
             END IF
             IF( TLNTST .AND. (POLTST .OR. HSTTST) ) THEN
                IF( IHHEAD .NE. ' ' ) THEN
                   IHHEAD((SLEN(IHHEAD)+1):) = ', LINE TYPE'
                ELSE
                   IHHEAD(1:) = 'LINE TYPE'
                END IF
             END IF
             IF( .NOT.POLTST .AND. .NOT.HSTTST ) THEN
                IF( MRKTST ) THEN
                   IF( IHHEAD .NE. ' ' ) THEN
                      IHHEAD((SLEN(IHHEAD)+1):) = ', MARK'
                   ELSE
                      IHHEAD(1:) = 'MARK'
                   END IF
                END IF
             END IF
             IF( IHHEAD .NE. ' ' ) THEN
                WRITE (*,'(''            Rotating '',A)')
     :          IHHEAD(1:SLEN(IHHEAD))
             END IF
             IF( XRKEEP(1) .NE. -321.654 .AND.
     :           XRKEEP(2) .NE. -321.654 ) THEN
                WRITE (*,'(''            X range -'',4P2E12.3)')
     :          XRKEEP(1), XRKEEP(2)
             ELSE IF( XRKEEP(1) .EQ. -321.654 .AND.
     :                XRKEEP(2) .EQ. -321.654)
     :       THEN
                WRITE (*,
     :          '(''            X range - autoscaled,'',
     :          '' autoscaled'')')
             ELSE IF( XRKEEP(1) .EQ. -321.654 ) THEN
                WRITE (*,
     :          '(''            X range - autoscaled,'',4PE12.3)')
     :          XRKEEP(2)
             ELSE IF( XRKEEP(2) .EQ. -321.654 ) THEN
                WRITE (*,
     :          '(''            X range -'',4PE12.3,'', autoscaled'')')
     :          XRKEEP(1)
             END IF
             IF( YRKEEP(1) .NE. -321.654 .AND.
     :           YRKEEP(2) .NE. -321.654 ) THEN
                WRITE (*,'(''            Y range -'',4P2E12.3)')
     :          YRKEEP(1), YRKEEP(2)
             ELSE IF( YRKEEP(1) .EQ. -321.654 .AND.
     :                YRKEEP(2) .EQ. -321.654)
     :       THEN
                WRITE (*,
     :          '(''            Y range - autoscaled,'',
     :          '' autoscaled'')')
             ELSE IF( YRKEEP(1) .EQ. -321.654 ) THEN
                WRITE (*,
     :          '(''            Y range - autoscaled,'',4PE12.3)')
     :          YRKEEP(2)
             ELSE IF( YRKEEP(2) .EQ. -321.654 ) THEN
                WRITE (*,
     :          '(''            Y range -'',4PE12.3,'', autoscaled'')')
     :          YRKEEP(1)
             END IF
             IF( TRIMX ) THEN
                IF( TRIMY ) THEN
                   WRITE (*,'(''            XTrimmed, YTrimmed'')')
                ELSE
                   WRITE (*,'(''            XTrimmed, YJustified'')')
                END IF
             ELSE IF( TRIMY ) THEN
                WRITE (*,'(''            XJustified, YTrimmed'')')
             ELSE
                WRITE (*,'(''            XJustified, YJustified'')')
             END IF
             WRITE (*,'(''            Zone -'',I3)') NZONEN
             IF( LBOX ) THEN
                WRITE (*,'(''            Box - ON'')')
             ELSE
                WRITE (*,'(''            Box - OFF'')')
             END IF
             IF( NPOINT .EQ. 0 ) THEN
                WRITE (*,'(''            Current arrays are empty'')')
             ELSE
                WRITE (*,
     :          '(''0           Current arrays:''/
     :          ''           "'',A''"''/
     :          ''            N ='',
     :          I10,
     :          '', WorV ='',G10.3/
     :          ''            X range:'',
     :          G12.5,'' - '',G12.5)')
     :          TITLE(1:MIN(60,SLEN(TITLE))), NPOINT, WORV,
     :          WAVE(1), WAVE(NPOINT)
             END IF
*
*   TBALL   Tracker ball to be used to control ARGS cursor
*   Not applicable to GKS version
*
*   ELSE IF( SUBCMD .EQ. 'TBALL' ) THEN
*   CALL DECODE('TBALL',PARAMS,0,0,VARRAY,' ',OK)
*   CALL TCURAT(1,5,1)
*   CONTST = .FALSE.
*
*   TENX   Converts 'X' values to 10**X
*
          ELSE IF( SUBCMD .EQ. 'TENX' .OR. SUBCMD .EQ. 'XTEN' ) THEN
             IF( SUBCMD .EQ. 'XTEN' ) THEN
                WRITE (*,'(''   XTEN command renamed TENX'',A)') BLEEP
                SUBCMD = 'TENX'
                WRITE (*,'(''   (command executed)'')')
             END IF
             CALL DECODE('TENX',PARAMS,0,0,VARRAY,' ',OK)
             IF( NPOINT.LE.0 ) THEN
                WRITE (*,'(''   TENX:  no points!'',A)') BLEEP
             ELSE
                DO 3210 I = 1, NPOINT
                   WAVE(I) = 10.0**WAVE(I)
 3210           CONTINUE
             END IF
*
*   TENY    Converts 'Y' values to 10**Y
*
          ELSE IF( SUBCMD .EQ. 'TENY' .OR. SUBCMD .EQ. 'YTEN' ) THEN
             IF( SUBCMD .EQ. 'YTEN' ) THEN
                WRITE (*,'(''   YTEN command renamed TENY'',A)')
                SUBCMD = 'TENY'
                WRITE (*,'(''   (command executed)'')')
             END IF
             CALL DECODE('TENY',PARAMS,0,0,VARRAY,' ',OK)
             IF( NPOINT.LE.0 ) THEN
                WRITE (*,'(''   TENY:  no points!'',A)') BLEEP
             ELSE
                DO 3220 I = 1, NPOINT
                   FLUX(I) = 10.0**FLUX(I)
 3220           CONTINUE
             END IF
*
*   TICKS controls major and minor tick marks
*

*   TICKS dx dy nx ny

*   dx,dy  1st 2 arguments control major tick spacing (one relating to each axis)

*   If argument is negative   -- no major ticks
*   ..  ..         blank/zero -- reset to default ticks (decided by NCAR)
*   ..  ..     ..  above zero -- specifies  tick SPACING directly

*   nx,ny   Second 2 arguments control no. of minor ticks (one for each axis)
*
*   If argument is less than 1 -- no minor ticks
*   ..  ..     .. greater ...  -- specifies no of minor ticks directly
*   Default set to 0 for each axis (ie no minor tick between major ticks)

          ELSE IF( SUBCMD .EQ. 'TICKS' ) THEN
             TOLER = 1.E-35
             VARRAY(1) = 0.
             VARRAY(2) = 0.
             VARRAY(3) = 0.
             VARRAY(4) = 0.
             CALL DECODE('TICKS',PARAMS,0,4,VARRAY,' ',OK)
             IF( .NOT.OK) GO TO 5000
*   Major ticks
*   X-axis
             IF( VARRAY(1).LT.0.0 ) THEN
*   No ticks
                ITICKS(1) = 0
             ELSE IF( VARRAY(1).LT.TOLER ) THEN
*   Default argument - revert to autotick
                ITICKS(1) = 1
             ELSE
*   User specified tick
                ITICKS(1) = 2
                MAJTICKS(1) = VARRAY(1)
             END IF
*   Y-axis
             IF( VARRAY(2).LT.0.0 ) THEN
*   No ticks
                ITICKS(2) = 0
             ELSE IF( VARRAY(2).LT.TOLER ) THEN
*   Default argument - revert to autotick
                ITICKS(2) = 1
             ELSE
*   User specified tick
                ITICKS(2) = 2
                MAJTICKS(2) = VARRAY(2)
             END IF

*   Minor ticks

             MINTICKS(1) = REAL(NINT(VARRAY(3)))
             MINTICKS(2) = REAL(NINT(VARRAY(4)))

             IF( MINTICKS(1).GT.0) MINTICKS(1) = MINTICKS(1) - 1
             IF( MINTICKS(2).GT.0) MINTICKS(2) = MINTICKS(2) - 1

*
*   TICKLABELS
*
          ELSE IF( SUBCMD .EQ. 'TICKLABELS' ) THEN
             CALL DECODE('TICKLABELS',PARAMS,1,1,VARRAY,
     :       'Tick_Label_Style ',OK)
             IF( .NOT.(OK)) GO TO 5000
             ITICKLABELS = NINT(VARRAY(1))

*  TITLE/TADD :  Change (or add to) TITLE
          ELSE IF( SUBCMD .EQ. 'TITLE' .OR. SUBCMD .EQ. 'TADD' ) THEN

             IF( PARAMS .EQ. ' ' .AND. TPROMPT ) THEN
                CALL RDSTR( SUBCMD, 'String', ' ', PARAMS, STATUS )
                IF( STATUS .NE. SAI__OK ) GO TO 5000

                IF( PARAMS .NE. ' ' ) THEN

                   I = INDEX( PARAMS, '"' )
                   IF( I .EQ. 0 ) THEN
                      CALL CHR_PREFX( '"', PARAMS, J )
                      CALL CHR_APPND( '"', PARAMS, J )
                   ELSE
                      J = INDEX( PARAMS( I + 1 : ), '"' )

                      IF( J .EQ. 0 ) THEN
                         J = SLEN( PARAMS )
                         CALL CHR_APPND( '"', PARAMS, J )
                         CALL MSGOUT( SUBCMD, 'Closing quotes added',
     :                                .TRUE., STATUS )
                         IF( STATUS .NE. SAI__OK ) GO TO 5000

                      ELSE
                         J = SLEN( PARAMS )
                      END IF

                   END IF

                   IF( COMTXT ) THEN
                      CALL CHR_PREFX( 'TITLE '//PARAMS( 1 : J ), COMND1,
     :                                I )
                   ELSE
                      CALL CHR_PREFX( 'TITLE '//PARAMS( 1 : J ), COMND2,
     :                                I )
                   END IF

                   DELIM = 1
                   GO TO 400

                END IF

             END IF

             IF( SUBCMD .EQ. 'TITLE' ) THEN
                TITLE = PARAMS( 1 : 80 )
             ELSE
                I = SLEN( TITLE )
                CALL CHR_APPND( PARAMS, TITLE, I )
             END IF

             PARAMS = PARAMS( 81 : )
             IF( SLEN( PARAMS ) .GT. 80 ) THEN
                CALL MSGOUT( SUBCMD, 'Only the first 80 characters '//
     :                       'accepted', .TRUE., STATUS )
             END IF

             IF( .NOT.SUBCHK ) THEN
                CALL MSGOUT( SUBCMD, 'Closing quotes added', .FALSE.,
     :                       STATUS )
                GO TO 5000
             END IF

*  TSWAP: Swap title into current array
          ELSE IF( SUBCMD .EQ. 'TSWAP' ) THEN
             CALL DECODE( 'TSWAP', PARAMS, 1, 1, VARRAY, 'Entry ', OK )
             IF( .NOT. OK ) GO TO 5000
             NUMENT = NINT( VARRAY(1 ))
             IF( NUMENT .LE. 0 .OR. NUMENT .GT. NONSTK ) THEN
                CALL MSG_SETI( 'I', NUMENT )
                CALL MSGOUT( 'TSWAP', 'Stack entry ^I does not exist',
     :                       .TRUE., STATUS )
                GO TO 5000
             ELSE
                TITLE = STITLE( NUMENT )
                CALL MSGOUT( 'TSWAP', TITLE, .FALSE., STATUS )
             END IF

*  TSTRIP: Strip leading blanks from title
          ELSE IF( SUBCMD .EQ. 'TSTRIP' ) THEN
             CALL DECODE( 'TSTRIP', PARAMS, 0, 0, VARRAY, ' ', OK )
             IF( .NOT.OK ) GO TO 5000
             CALL SSTRIP( TITLE )

*  TLINE: Change line attributes
          ELSE IF( SUBCMD .EQ. 'TLINE' ) THEN
             CALL DECODE('TLINE',PARAMS,1,1,VARRAY(1),'Linetype ',OK)
             IF( .NOT.OK) GO TO 5000
             IF( NINT(VARRAY(1)).LT.1 .OR. NINT(VARRAY(1)).GT.5 ) THEN
                WRITE (*,'(''   TLINE:  line type must be 1-5'',A)')
     :          BLEEP
             ELSE
                LNTYPE = NINT(VARRAY(1))
                LNTYPEKP = LNTYPE
                IF( DVTST ) THEN
                   CALL PLOTIT( 0, 0, 2)
                   CALL SGS_FLUSH
                   IF( DVTST) CALL GSLN (LNTYPE)
                END IF
             END IF
*
*   TOFLAMBDA  converts from Nu, F(Nu)
*
          ELSE IF( SUBCMD .EQ. 'TOFLAMBDA' ) THEN
             CALL DECODE('TOFLAMBDA',PARAMS,0,0,VARRAY,' ',OK)
             IF( .NOT.OK) GO TO 5000
             CLIGHT = 2.997925E+18
             CLLOG = LOG10(max(1e-30,CLIGHT))
             TSTVAL = 654321.789
             NLOST = 0
             DO 3240 I = 1, NPOINT
                FLUX(I) = LOG10(MAX(1e-30,FLUX(I))) +
     :                   2.0*LOG10(max(1e-30,WAVE(I))) - CLLOG
                IF( ABS(FLUX(I)).GT.36.0 ) THEN
                   FLUX(I) = TSTVAL
                   NLOST = NLOST + 1
                ELSE
                   FLUX(I) = 10.0**FLUX(I)
                   WAVE(I) = CLLOG - LOG10(max(1e-30,WAVE(I)))
                   IF( ABS(WAVE(I)).GT.36.0 ) THEN
                      FLUX(I) = TSTVAL
                      NLOST = NLOST + 1
                   ELSE
                      WAVE(I) = 10.0**WAVE(I)
                   END IF
                END IF
 3240        CONTINUE
             IF( NLOST.GT.0 ) THEN
                WRITE (*,'(''   TOFLAMBDA:'',
     :          I4,'' point(s) lost through overflows'',A)')
     :          NLOST, BLEEP
                CALL SRTBRK(ASIZE1,WAVE,FLUX,NPOINT,MAXBRK,BREAK,NBREAK,
     :          TSTVAL,OK)
                IF( .NOT.OK) GO TO 5000
             END IF
*
*   TOFNU  converts to Nu, F(Nu)
*
          ELSE IF( SUBCMD .EQ. 'TOFNU' ) THEN
             CALL DECODE('TOFNU',PARAMS,0,0,VARRAY,' ',OK)
             IF( .NOT.OK) GO TO 5000
             CLIGHT = 2.997925E+18
             CLLOG = LOG10(max(1e-30,CLIGHT))
             NLOST = 0
             TSTVAL = 654312.789
             DO 3260 I = 1, NPOINT
                WAVE(I) = LOG10(max(1e-30,WAVE(I)))
                FLUX(I) = LOG10(max(1e-30,FLUX(I))) +
     :                     2.0*WAVE(I) - CLLOG
                IF( ABS(FLUX(I)).GT.36.0 ) THEN
                   FLUX(I) = TSTVAL
                   NLOST = NLOST + 1
                ELSE
                   FLUX(I) = 10.0**FLUX(I)
                   WAVE(I) = CLLOG - WAVE(I)
                   IF( ABS(WAVE(I)).GT.36.0 ) THEN
                      FLUX(I) = TSTVAL
                      NLOST = NLOST + 1
                   ELSE
                      WAVE(I) = 10.0**WAVE(I)
                   END IF
                END IF
 3260        CONTINUE
             IF( NLOST.GT.0 ) THEN
                WRITE (*,'(''   TOFNU:'',
     :          I4,'' point(s) lost through overflows'',A)')
     :          NLOST, BLEEP
                CALL SRTBRK(ASIZE1,WAVE,FLUX,NPOINT,MAXBRK,BREAK,NBREAK,
     :          TSTVAL,OK)
                IF( .NOT.OK) GO TO 5000
             END IF
*
*   TOV  Convert X from wavelength to velocity
*
          ELSE IF( SUBCMD .EQ. 'TOV' ) THEN
             CALL DECODE('TOV',PARAMS,1,1,LAM0,'Wave0 ',OK)
             IF( .NOT.OK) GO TO 5000
             IF( OK .AND. LAM0 .NE. 0.0 ) THEN
                WORV = LAM0/C
                DO 3270 I = 1, NPOINT
                   WAVE(I) = (WAVE(I)/LAM0-1.0)*C
 3270           CONTINUE
             END IF
             IF( DEBUG ) THEN
                WRITE (*,'(''   DEBUG/TOV/WORV:  '',1PE15.5)') WORV
             END IF
*
*   TOW  Convert X from velocity to wavelength
*
          ELSE IF( SUBCMD .EQ. 'TOW' ) THEN
             LAM0 = WORV*C
             IF( WORV .NE. 1.0 ) THEN
                CALL DECODE('TOW',PARAMS,0,1,LAM0,' ',OK)
                IF( .NOT.OK) GO TO 5000
             ELSE
*   This branch should only occur when an old-style (i.e. lacking a WORV value)
*   EQWID file is read in.
                CALL DECODE('TOW',PARAMS,1,1,LAM0,' ',OK)
                IF( .NOT.OK) GO TO 5000
             END IF
             IF( OK ) THEN
                WORV = 1.0
                DO 3280 I = 1, NPOINT
                   WAVE(I) = (WAVE(I)/C+1.0)*LAM0
 3280           CONTINUE
             END IF
             IF( DEBUG ) THEN
                WRITE (*,'(''   DEBUG/TOW/WORV:  '',1PE15.5)') WORV
             END IF
*
*   TPORT   Set up a GRAFX zone limits
*
          ELSE IF( SUBCMD .EQ. 'TPORT' ) THEN

             GRIL = 0.13
             GRIR = 0.98
             GRIB = 0.1
             GRIT = 0.95

             VARRAY(6) = GRIL
             VARRAY(7) = GRIR
             VARRAY(8) = GRIB
             VARRAY(9) = GRIT

             CALL DECODE('TPORT',PARAMS,5,9,VARRAY,
     :       'Zone_no. X(min) X(max) Y(min) Y(max) ',OK)
             IF( .NOT.OK) GO TO 5000
             IF( NINT(VARRAY(1)).GT.100 .OR. NINT(VARRAY(1)).LE.8 ) THEN
                WRITE (*,'(''   TPORT:  zone number must be 9-100'')')
                GO TO 5000
             ELSE
                DO 3290 I = 2, 9
                   IF( VARRAY(I).LT.0.0 .OR. VARRAY(I).GT.1.0 ) THEN
                      WRITE (*,'(''   TPORT:  '',
     :                ''graph window limits must be in the range 0-1'',
     :                A)') BLEEP

                      GO TO 5000
                   END IF
 3290           CONTINUE
                IF( OK ) THEN
                   NZONEN = NINT(VARRAY(1))
                   GRIDS(5,NZONEN) = VARRAY(2)
                   GRIDS(6,NZONEN) = VARRAY(3)
                   GRIDS(7,NZONEN) = VARRAY(4)
                   GRIDS(8,NZONEN) = VARRAY(5)
                   GRIL = VARRAY(6)
                   GRIR = VARRAY(7)
                   GRIB = VARRAY(8)
                   GRIT = VARRAY(9)
                   ZONEDEF(NZONEN) = .TRUE.
                   CHECKPARAMS = PARAMS(1:1)
*   Work out NCAR default graph window
                   GRIDS(1,NZONEN) = GRIDS(5,NZONEN)
     :             + (GRIDS(6,NZONEN)-GRIDS(5,NZONEN))*GRIL
                   GRIDS(2,NZONEN) = GRIDS(5,NZONEN)
     :             + (GRIDS(6,NZONEN)-GRIDS(5,NZONEN))*GRIR
                   GRIDS(3,NZONEN) = GRIDS(7,NZONEN)
     :             + (GRIDS(8,NZONEN)-GRIDS(7,NZONEN))*GRIB
                   GRIDS(4,NZONEN) = GRIDS(7,NZONEN)
     :             + (GRIDS(8,NZONEN)-GRIDS(7,NZONEN))*GRIT

                END IF
             END IF
*
*   ZONEMM   Set up plotting zone limits
*
          ELSE IF( SUBCMD .EQ. 'ZONEMM' ) THEN

             GRIL = 0.15
             GRIR = 0.95
             GRIB = 0.15
             GRIT = 0.95

             TSTVAL = -123.5678
             DO 3300 I = 6, 9
                VARRAY(I) = TSTVAL
 3300        CONTINUE

             CALL DECODE('ZONEMM',PARAMS,5,9,VARRAY,
     :       'Zone_no. X(min) X(max) Y(min) Y(max) ',OK)
             IF( .NOT.OK) GO TO 5000

             IF( NINT(VARRAY(1)).GT.100 .OR. NINT(VARRAY(1)).LE.8 ) THEN
                WRITE(*,'(''   ZONEMM:  zone number must be 9-100'',A)')
     :          BLEEP
                GO TO 5000
             END IF

             DX = VARRAY(3) - VARRAY(2)
             DY = VARRAY(5) - VARRAY(4)
             IF( DX.LE.0.0 .OR. DY.LE.0.0 ) THEN
                WRITE (*,'(''   ZONEMM:  '',
     :          ''graph window occupies zero area'',A)') BLEEP
                GO TO 5000
             END IF
             IF( VARRAY(6) .EQ. TSTVAL) VARRAY(6) = VARRAY(2) + 0.1*DX
             IF( VARRAY(7) .EQ. TSTVAL) VARRAY(7) = VARRAY(3) - 0.05*DX
             IF( VARRAY(8) .EQ. TSTVAL) VARRAY(8) = VARRAY(4) + 0.1*DY
             IF( VARRAY(9) .EQ. TSTVAL) VARRAY(9) = VARRAY(5) - 0.05*DY
             DX = VARRAY(7) - VARRAY(6)
             DY = VARRAY(9) - VARRAY(8)
             IF( DX.LE.0.0 .OR. DY.LE.0.0 ) THEN
                WRITE (*,'(''   ZONEMM:  '',
     :          ''grid window occupies zero area'',A)') BLEEP
                GO TO 5000
             END IF

             XMILLIM = 1000.0*XM
             YMILLIM = 1000.0*YM
             DO 3320 I = 2, 9
                IF( ((I .EQ. 2 .OR. I .EQ. 3 .OR. I .EQ. 6 .OR.
     :                I .EQ. 7) .AND. (VARRAY(I).LT.0.0 .OR.
     :                VARRAY(I).GT.XMILLIM)) .OR. ((I .EQ. 4 .OR.
     :                I .EQ. 5 .OR. I .EQ. 8 .OR. I .EQ. 9) .AND.
     :              (VARRAY(I).LT.0.0 .OR. VARRAY(I).GT.YMILLIM)) ) THEN
                   WRITE (*,'(''   ZONEMM:  beyond device limits '',A)')
     :             BLEEP
                   GO TO 5000

                END IF
 3320        CONTINUE

             NZONEN = NINT(VARRAY(1))
             GRIDS(5,NZONEN) = VARRAY(2)/XMILLIM
             GRIDS(6,NZONEN) = VARRAY(3)/XMILLIM
             GRIDS(7,NZONEN) = VARRAY(4)/YMILLIM
             GRIDS(8,NZONEN) = VARRAY(5)/YMILLIM
             GRIL = VARRAY(6)/XMILLIM
             GRIR = VARRAY(7)/XMILLIM
             GRIB = VARRAY(8)/YMILLIM
             GRIT = VARRAY(9)/YMILLIM
             ZONEDEF(NZONEN) = .TRUE.
             CHECKPARAMS = PARAMS(1:1)

             GRIDS(1,NZONEN) = GRIDS(5,NZONEN)
     :       + (GRIDS(6,NZONEN)-GRIDS(5,NZONEN))*GRIL
             GRIDS(2,NZONEN) = GRIDS(5,NZONEN)
     :       + (GRIDS(6,NZONEN)-GRIDS(5,NZONEN))*GRIR
             GRIDS(3,NZONEN) = GRIDS(7,NZONEN)
     :       + (GRIDS(8,NZONEN)-GRIDS(7,NZONEN))*GRIB
             GRIDS(4,NZONEN) = GRIDS(7,NZONEN)
     :       + (GRIDS(8,NZONEN)-GRIDS(7,NZONEN))*GRIT

*  TPROMPT:  .true. forces prompt for title
          ELSE IF( SUBCMD .EQ. 'TPROMPT' ) THEN
             CALL GET0L( PARAMS, 1, .FALSE., SUBCMD, 'Should titles '//
     :                   ' be prompted for', TPROMPT, TPROMPT, STATUS )
             IF( TPROMPT ) THEN
                CALL MSGOUT( SUBCMD, 'On', .FALSE., STATUS )
             ELSE
                CALL MSGOUT( SUBCMD, 'Off', .FALSE., STATUS )
             END IF

*  TROT:  Rotate line attributes
          ELSE IF( SUBCMD .EQ. 'TROT' ) THEN
             CALL DECODE('TROT',PARAMS,0,0,VARRAY,' ',OK)
             TLNTST = .TRUE.
             LNTYPE = LNTYPEKP
*
*   TURN/XREV  Replace X(1 to N) by X(N to 1)
*
          ELSE IF( SUBCMD .EQ. 'XREV' ) THEN
             CALL DECODE('XREV',PARAMS,0,0,VARRAY,' ',OK)
             I = (NPOINT+1)/2
             DO 3360 J = 1, I
                TEMP = WAVE(J)
                WAVE(J) = WAVE(NPOINT-J+1)
                WAVE(NPOINT-J+1) = TEMP
                TEMP = FLUX(J)
                FLUX(J) = FLUX(NPOINT-J+1)
                FLUX(NPOINT-J+1) = TEMP
 3360        CONTINUE
             I = (NBREAK+1)/2
             DO 3380 J = 1, I
                ITMP = BREAKS(J)
                BREAKS(J) = BREAKS(NBREAK-J+1)
                BREAKS(NBREAK-J+1) = ITMP
 3380        CONTINUE
             DO 3400 J = 1, NBREAK - 1
                BREAKS(J) = NPOINT - BREAKS(J+1)
 3400        CONTINUE
             BREAKS(NBREAK) = NPOINT
*
*   TWEIGHT - increases plot `weight' (on appropriate devices)
*
          ELSE IF( SUBCMD .EQ. 'TWEIGHT' ) THEN
             CALL DECODE('TWEIGHT',PARAMS,1,1,VARRAY,'Weight ',OK)
             IF( .NOT.OK) GO TO 5000
             IF( NINT(VARRAY(1)).LT.1 .OR. NINT(VARRAY(1)).GT.5 ) THEN
                WRITE (*,'(''   TWEIGHT:  weight must be 1-5'',A)')
     :          BLEEP
             ELSE
                LNWIDTH = VARRAY(1)
             END IF
*
*   TZONE   Select NCAR zone for plots
*
          ELSE IF( SUBCMD .EQ. 'TZONE' ) THEN
             CALL DECODE('TZONE',PARAMS,1,1,VARRAY,'Zone_no. ',OK)
             IF( .NOT.(OK)) GO TO 5000
**Release old working zone
             CALL SGS_RELZ(IZW)
             NZONEN = NINT(VARRAY(1))
             IF( ZONEDEF(NZONEN) ) THEN
                DO 3410 JK = 1, 8
                   GRID(JK) = GRIDS(JK,NZONEN)
 3410           CONTINUE
             ELSE
                WRITE (*,'(''   TZONE:  zone not defined'')')
                OK = .FALSE.
                GO TO 5000
             END IF
             IF( DEVTYP .EQ. 1200 .AND. NZONEN.GT.0 ) THEN
*   allow one multiuse zone
                IF( NZONEN .NE. 99 ) THEN
                   IF( TZNPLT(NZONEN) ) THEN
                      CALL SGS_CLRZ
                      DO 3412 I = 1, 100
                         TZNPLT(I) = .FALSE.
 3412                 CONTINUE
                   END IF
                   TZNPLT(NZONEN) = .TRUE.
                END IF
             END IF
             ZNTST = .TRUE.
             REALSIZE = .FALSE.
             FRZONE = .FALSE.
*
*   UBVRD  Read (at terminal) U,B,V mags & create flux file
*
          ELSE IF( SUBCMD .EQ. 'UBVRD' ) THEN
             VARRAY(4) = 50.0
             CALL DECODE('UBVRD',PARAMS,3,4,VARRAY,'U B V ',OK)
             IF( .NOT.OK) GO TO 5000
             TITLE = ' '
             TITLE = 'UBV Fluxes:'
             WRITE (TITLE(12:17),'(F6.2)') VARRAY(1)
             TITLE(18:18) = ','
             WRITE (TITLE(19:24),'(F6.2)') VARRAY(2)
             TITLE(25:25) = ','
             WRITE (TITLE(26:31),'(F6.2)') VARRAY(3)
             CALL IHUBV(VARRAY,ASIZE1,WAVE,FLUX,MAXBRK,BREAK,NPOINT,
     :       NBREAK)
             WORV = 1.0

*  USEHTX: Select HELP format to use.
          ELSE IF( SUBCMD .EQ. 'USEHTX' ) THEN
             CALL GET0L( PARAMS, 1, .TRUE., SUBCMD, 'Do you want to '//
     :                   'use hypertext help', .TRUE., USEHTX, STATUS )
             IF( USEHTX ) THEN
                CALL MSGOUT( SUBCMD, 'Using hypertext help', .FALSE.,
     :                       STATUS )
             ELSE
                CALL MSGOUT( SUBCMD, 'Using plain text help', .FALSE.,
     :                       STATUS )
             END IF

             CALL GET0C( PARAMS, 2, .TRUE., SUBCMD, 'Type the showme '//
     :                   'command with full path', SHOWME, TSHOW,
     :                   STATUS )

             IF( TSHOW .NE. SHOWME ) THEN
                CALL MSG_SETC( 'SHOWME', TSHOW )
                CALL MSGOUT( SUBCMD, 'Using ''^SHOWME'' for the '//
     :                       'Starlink ''showme'' command.', .FALSE.,
     :                       STATUS )
                SHOWME = TSHOW
             END IF


*  USENDF: Select data format to use.
          ELSE IF( SUBCMD .EQ. 'USENDF' ) THEN
             CALL GET0L( PARAMS, 1, .TRUE., SUBCMD, 'Do you want to '//
     :                   'use NDF data files', .TRUE., USENDF, STATUS )
             IF( USENDF ) THEN
                CALL MSGOUT( SUBCMD, 'Using NDF data files', .FALSE.,
     :                       STATUS )
             ELSE
                CALL MSGOUT( SUBCMD, 'Using native DIPSO data files',
     :                       .FALSE., STATUS )
             END IF

*  USSPRD:  Reads native dipso ULDA output from USSP
          ELSE IF( SUBCMD .EQ. 'USSPRD' ) THEN
             IF( USENDF ) THEN
                CALL MSGOUT(SUBCMD,'Command not available in NDF '//
     :                      'mode.', .TRUE., STATUS )
                GO TO 5000
             END IF

 3420        CONTINUE
             CALL SSTRIP(PARAMS)
             I = INDEX(PARAMS,' ')

             IF( I .EQ. 1 ) THEN

                CALL RDSTR( SUBCMD, 'File name', ' ', PARAMS, STATUS )
                IF( STATUS .NE. SAI__OK ) THEN
                   OK = .FALSE.
                   GO TO 5000
                END IF

                GO TO 3420

             END IF

             I = I - 1
             IHFILE = PARAMS(1:MIN(I,80))
             I = MAX(INDEX(IHFILE,']'),INDEX(IHFILE,':'))
             I = MAX(1,I)
             I = INDEX(IHFILE(I:),'.')

             IF( I .EQ. 0 ) THEN
                 IHFILE(INDEX(IHFILE,' '):) = '.ULD'
             END IF
               CLOSE (9)
               OPEN (UNIT=9,FILE=IHFILE,STATUS='OLD',
     :              FORM='UNFORMATTED',IOSTAT=IHX)

             IF( IHX .NE. 0 ) THEN
                WRITE (*,'(''   USSPRD:  unable to open '',A)')
     :          IHFILE(1:SLEN(IHFILE))
                CLOSE (9)
                GO TO 5000
             END IF

             PARAMS(1:) = PARAMS(INDEX(PARAMS,' '):)
             CALL SSTRIP(PARAMS)
             PARAMS = '-251 '//PARAMS
             VARRAY(2) = -251.0
             CALL DECODE('USSPRD',PARAMS,1,2,VARRAY,' ',OK)
             IF( .NOT.OK ) THEN
                OK = .TRUE.
                GO TO 5000
             END IF
             EPSMIN = VARRAY(2)

             IF( EPSMIN.LE.-1599.99 ) THEN
                WRITE (*,
     :          '(''   USSPRD:  specified Epsmin would '',
     :          ''allow input of saturated data'',A)') BLEEP
                WRITE (*,'(''            Use +ve Epsmin'')')
                CLOSE (9)
                GO TO 5000
             ELSE IF( NINT(EPSMIN).LE.0 ) THEN

                 READ (9,IOSTAT=IHX) TITLE
                 READ (9,IOSTAT=J) NBREAK, (BREAK(I),I=1,NBREAK)
                 IHX = MAX(J,IHX)
                 NPOINT = BREAK(NBREAK)
                 READ (9,IOSTAT=J) (WAVE(I),FLUX(I),IWORK(I),I=1,NPOINT)
                 IHX = MAX(J,IHX)

                IF( IHX .NE. 0 ) THEN
                   WRITE (*,'(''   USSPRD:  error reading data'',A)')
     :             BLEEP
                   CLOSE (9)
                   NPOINT = 0
                   TITLE = '(Empty)'
                   GO TO 5000
                END IF
                WORV = 1.0
                TSTVAL = -1234.9876
                NDUFF = 0
                DO 3430 I = 1, NPOINT
                   IF( IWORK(I).LT.EPSMIN ) THEN
                      NDUFF = NDUFF + 1
                      FLUX(I) = TSTVAL
                   END IF
 3430           CONTINUE
                IF( NDUFF.GT.0 ) THEN
                   CALL SRTBRK(ASIZE1,WAVE,FLUX,NPOINT,MAXBRK,BREAK,
     :             NBREAK,TSTVAL,OK)
                END IF
                IF( .NOT.OK ) THEN
                   OK = .TRUE.
                   WRITE (*,'(''   USSPRD:  error sorting data'',A)')
     :             BLEEP
                   NPOINT = 0
                   TITLE = '(Empty)'
                   GO TO 5000
                END IF

                CALL SSTRIP(TITLE)
                TITLE = 'ULDA '//TITLE
                WRITE (*,'(''   USSPRD:  read '',A)')
     :          TITLE(1:MIN(60,SLEN(TITLE)))
             ELSE
                IF( NONSTK+2.GT.MAXSTK ) THEN
                   WRITE (*,
     :             '(''  USSPRD:  insufficient stack '',
     :             ''entries available for storage'',A)') BLEEP
                   CLOSE (9)
                   GO TO 5000
                END IF
                N1 = NONSTK + 1
                N2 = NONSTK + 2
                 READ (9,IOSTAT=IHX) STITLE(N1)
                CALL SSTRIP(STITLE(N1))
                STITLE(N2) = 'ULDA eps: '//STITLE(N1)
                STITLE(N1) = 'ULDA flx: '//STITLE(N1)
                 READ (9,IOSTAT=J) ITBRK, (IWORK(K),K=1,ITBRK)
                itbrk=iwork(1)
                do i=2,itbrk+1
                  iwork(i)=iwork(i+1)
                end do
                IWORK(1) = IWORK(ITBRK)
                ITBRK = 1
                STKNPT(N1) = IWORK(1)
                STKNPT(N2) = IWORK(1)
                BSTNPT(N1) = 1
                BSTNPT(N2) = 1
                WORVST(N1) = 1.0
                WORVST(N2) = 1.0
                IHX = MAX(J,IHX)
                IF( IHX .EQ. 0 ) THEN
                   IF( STKLST+2*IWORK(I).GT.STKSZE ) THEN
                      WRITE (*,
     :                '(''   USSPRD:  insufficient stack '',
     :                ''space for storage'',A)') BLEEP
                       CLOSE (9)
                      GO TO 5000
                   ELSE
                      BPOINT(N1) = BSTLST + 1
                      BPOINT(N2) = BSTLST + 2
                      BSTACK(BSTLST+1) = STKNPT(N1)
                      BSTACK(BSTLST+2) = STKNPT(N2)
                   END IF
                END IF

                 READ (9,IOSTAT=J) (WORK(I),WORK(I+STKNPT(N1)),
     :           IWORK(I+2*STKNPT(N1)),I=1,STKNPT(N1))
!!!                IHX = MAX(J,IHX)
                 CLOSE (9)
                IF( IHX .NE. 0 ) THEN
                   WRITE (*,'(''   USSPRD:  error reading file'',A)')
     :             BLEEP
                   GO TO 5000
                END IF

                BSTLST = BSTLST + 2
                POINTR(N1) = STKLST + 1
                POINTR(N2) = POINTR(N1) + STKNPT(N1)
                STKLST = POINTR(N2) + STKNPT(N2) - 1
                NONSTK = NONSTK + 2
                NPSTK = STKNPT(N1)
                DO 3440 I = 1, NPSTK
                   XSTACK(POINTR(N1)+I-1) = WORK(I)
                   XSTACK(POINTR(N2)+I-1) = WORK(I)
                   YSTACK(POINTR(N1)+I-1) = WORK(STKNPT(N1)+I)
                   YSTACK(POINTR(N2)+I-1) = IWORK(2*STKNPT(N1)+I)
 3440           CONTINUE

                WRITE (*,
     :          '(''   USSPRD: filling stack entries'',
     :          I3,'','',I3)') N1, N2
             END IF

*
*   USSPCLIP  -  clips spectrum using USSP epsilon data
*
          ELSE IF( SUBCMD .EQ. 'USSPCLIP' ) THEN
             VARRAY(3) = 0.0
             VARRAY(4) = 0.0
             VARRAY(5) = 1E+30
             CALL DECODE('USSPCLIP',PARAMS,2,5,VARRAY,'Epsmin n1 ',OK)
             IF( VARRAY(3) .EQ. 0.0) VARRAY(3) = VARRAY(2) + 1.0

             N1 = NINT(VARRAY(2))
             N2 = NINT(VARRAY(3))
             W1 = VARRAY(4)
             W2 = VARRAY(5)
             MINEPS = NINT(VARRAY(1))

             IF( N1.LE.0 .OR. N1.GT.NONSTK ) THEN
                WRITE (*,
     :          '(''   USSPCLIP:  stack entry'',I3,
     :          '' does not exist'',A)') N1, BLEP
                GO TO 5000
             ELSE IF( N2.LE.0 .OR. N2.GT.NONSTK ) THEN
                WRITE (*,
     :          '(''   USSPCLIP:  stack entry'',I3,
     :          '' does not exist'',A)') N2, BLEEP
                GO TO 5000
             ELSE IF( STKNPT(N1) .NE. STKNPT(N2) ) THEN
                WRITE (*,
     :          '(''   USSPCLIP:  stack entries'',I3,'','',
     :          I3,'' do not match'',A)') N1, N2, BLEEP
                GO TO 5000
             ELSE IF( W1.GE.W2 ) THEN
                WRITE (*,'(''   USSPCLIP:  '',
     :          ''no legal wavelength limits'',A)') BLEEP
                GO TO 5000
             END IF

             I1 = POINTR(N1) - 1
             I2 = POINTR(N2) - 1
             TSTVAL = -123.4567
             DO 3460 I = 1, STKNPT(N1)
                WAVE(I) = XSTACK(I1+I)
                FLUX(I) = YSTACK(I1+I)
                IF( NINT(YSTACK(I2+I)).LE.MINEPS ) THEN
                   IF( WAVE(I).GE.W1 .AND. WAVE(I).LE.W2 ) THEN
                      FLUX(I) = TSTVAL
                   END IF
                END IF
 3460        CONTINUE
             NPOINT = STKNPT(N1)
             NBREAK = 1
             BREAK(1) = NPOINT
             TITLE = STITLE(N1)
             CALL SRTBRK(ASIZE1,WAVE,FLUX,NPOINT,MAXBRK,BREAK,NBREAK,
     :       TSTVAL,OK)
             IF( NPOINT .EQ. 0 ) THEN
                TITLE = '(Empty)'
                WRITE (*,'(''   USSPCLIP:   ALL data rejected!'',A)')
     :          BLEEP
                GO TO 5000
             ELSE IF( .NOT.OK ) THEN
                NPOINT = 0
                TITLE = '(Empty)'
                WRITE (*,'(''   USSPCLIP:  error sorting data'',A)')
     :          BLEEP
                OK = .TRUE.
                GO TO 5000
             ELSE
                WRITE (*,'(''   USSPCLIP:  current arrays replaced'')')
             END IF
*
*   VCORR  Corrects for system redshift
*
          ELSE IF( SUBCMD .EQ. 'VSH' ) THEN
             WRITE (*,'(''   VSH:  replaced by VCORR command''/
     :       ''         (different arguments - use HELP)'',A)') BLEEP
             GO TO 5000
          ELSE IF( SUBCMD .EQ. 'VCORR' ) THEN
             VARRAY(2) = 1.0
             CALL DECODE('VCORR',PARAMS,1,2,VARRAY,'Velocity ',OK)
             IF( .NOT.OK) GO TO 5000
             VELCRR = 1.0 + VARRAY(1)/2.997925E+05
             IF( NINT(VARRAY(2)) .EQ. 1 ) THEN
                DO 3470 I = 1, NPOINT
                   WAVE(I) = WAVE(I)/VELCRR
 3470           CONTINUE
             ELSE IF( NINT(VARRAY(2)) .EQ. -1 ) THEN
                DO 3475 I = 1, NPOINT
                   WAVE(I) = WAVE(I)/VELCRR
                   FLUX(I) = FLUX(I)*VELCRR
 3475           CONTINUE
             ELSE IF( NINT(VARRAY(2)) .EQ. 2 ) THEN
                DO 3480 I = 1, NPOINT
                   WAVE(I) = WAVE(I)*VELCRR
 3480           CONTINUE
             ELSE IF( NINT(VARRAY(2)) .EQ. -2 ) THEN
                DO 3485 I = 1, NPOINT
                   WAVE(I) = WAVE(I)*VELCRR
                   FLUX(I) = FLUX(I)/VELCRR
 3485           CONTINUE
             END IF
*
*    WCORR    Fix wavelengths
*
          ELSE IF( SUBCMD .EQ. 'WCORR' ) THEN
             CALL DECODE ('WCORR',PARAMS,2,2,VARRAY,'WC0 WC1 ',OK)
             WC0 = VARRAY(1)
             WC1 = VARRAY(2)
             J = 1
             L = 0
             DO I = 1, NPOINT
                IF( BREAKS(J).LT.I ) THEN
                   J = J + 1
                   L = L + 1
                END IF
                L = L + 1
                WAVE(I) = WAVE(I) + WC0 + WC1*REAL(L)
             ENDDO

*  WRITE:  Write data from the current arrays.
          ELSE IF( SUBCMD .EQ. 'WRITE' ) THEN
             IF( USENDF ) THEN
                CALL WRITE( 'WRITE', PARAMS, WORV, TITLE, STATUS )
             ELSE

*         ----------------------------------------------------------------
             DO 3500 I = 1, 40
                IF( PARAMS(I:I) .NE. ' ') GO TO 3540
 3500        CONTINUE
 3520        CONTINUE

             CALL RDSTR( SUBCMD, 'File name', ' ', PARAMS, STATUS )
             IF( STATUS .NE. SAI__OK ) THEN
                OK = .FALSE.
                GO TO 5000
             END IF

 3540        CONTINUE
             OPEN (UNIT=18,FILE=PARAMS(1:80),STATUS='NEW',
     :         FORM='UNFORMATTED',IOSTAT=IOS)
             IF( IOS .NE. 0 ) THEN
                WRITE (*,
     :              '(''   WRITE:  unable to open file'')')
                CLOSE (18)
                GO TO 5000
             END IF
             WRITE (18,IOSTAT=IOS) TITLE
             IF( IOS .EQ. 0 ) THEN
                WRITE (18,IOSTAT=IOS) NBREAK,
     :                     (BREAK(I),I=1,NBREAK)
              IF( IOS .EQ. 0 ) THEN
                   WRITE (18,IOSTAT=IOS)
     :                     (WAVE(I),FLUX(I),I=1,NPOINT)
                   IF( IOS .EQ. 0 ) THEN
                      WRITE (18,IOSTAT=IOS) WORV
                   END IF
                END IF
             END IF
             IF( IOS .NE. 0 ) THEN
                WRITE (*,
     :          '(''   WRITE:  error writing to file'')')
                CLOSE (18)
                GO TO 5000
             END IF
             CLOSE (18)
*         ----------------------------------------------------------------

             END IF
*
*   XDEC  take decimal part of X arrays
*
          ELSE IF( SUBCMD .EQ. 'XDEC' ) THEN
             CALL DECODE('XDEC',PARAMS,0,0,VARRAY,' ',OK)
             IF( .NOT.OK) GO TO 5000
             DO 3550 I = 1, NPOINT
                WAVE(I) = WAVE(I) - INT(WAVE(I))
 3550        CONTINUE
*
*   XDIV  divide X arrays by a constant
*
          ELSE IF( SUBCMD .EQ. 'XDIV' ) THEN
             CALL DECODE('XDIV',PARAMS,1,1,XFCTR,'Divisor ',OK)
             IF( .NOT.(OK)) GO TO 5000
             XFCTR = 1./XFCTR
             DO 3560 I = 1, NPOINT
                WAVE(I) = WAVE(I)*XFCTR
 3560        CONTINUE
*
*   XJust   X range to be justified
*
          ELSE IF( SUBCMD .EQ. 'XJ' ) THEN
             CALL DECODE('XJ',PARAMS,0,0,VARRAY,' ',OK)
             TRIMX = .FALSE.

*
*   XLABEL   Label X axis
*
          ELSE IF( SUBCMD .EQ. '5XLABEL' .OR. SUBCMD .EQ. 'XLAB' ) THEN
             XLABLN = MIN(80,MAX(1,SLEN(PARAMS)))
             XLAB = PARAMS(1:XLABLN)
             IF( .NOT.SUBCHK ) THEN
                WRITE (*,'(''   XLABEL:  closing quotes added'')')
                SUBCHK = .TRUE.
                GO TO 5000
             END IF
*
*   XMIN    Set X minimum for plotting
*
          ELSE IF( SUBCMD .EQ. 'XMIN' ) THEN
             CALL DECODE('XMIN',PARAMS,1,1,VARRAY,'X(min) ',OK)
             IF( .NOT.(OK)) GO TO 5000
             CALL XMINST(VARRAY(1))
             XRKEEP(1) = VARRAY(1)
*
*   XMAX    Set X maximum for plotting
*
          ELSE IF( SUBCMD .EQ. 'XMAX' ) THEN
             CALL DECODE('XMAX',PARAMS,1,1,VARRAY,'X(max) ',OK)
             IF( .NOT.(OK)) GO TO 5000
             CALL XMAXST(VARRAY(1))
             XRKEEP(2) = VARRAY(1)
*
*   XMULT   Multiply X values by a constant (i.e. change units)
*
          ELSE IF( SUBCMD .EQ. 'XMULT' ) THEN
             CALL DECODE('XMULT',PARAMS,1,1,XFCTR,'Factor ',OK)
             IF( .NOT.(OK)) GO TO 5000
             DO 3580 I = 1, NPOINT
                WAVE(I) = WAVE(I)*XFCTR
 3580        CONTINUE
*
*   XRange  Set X range for plotting
*
          ELSE IF( SUBCMD .EQ. 'XR' ) THEN
             CALL DECODE('XR',PARAMS,2,2,VARRAY,'X1 X2 ',OK)
             IF( .NOT.(OK)) GO TO 5000
             CALL XRSET(VARRAY(1),VARRAY(2))
             XRKEEP(1) = VARRAY(1)
             XRKEEP(2) = VARRAY(2)
*
*   XADD  Add constant to X
*
          ELSE IF( SUBCMD .EQ. 'XSH' .OR. SUBCMD .EQ. 'XADD' ) THEN
             IF( SUBCMD .EQ. 'XSH' ) THEN
                WRITE (*,'(''   XSH command renamed XADD'',A)') BLEEP
                SUBCMD = 'XADD'
                WRITE (*,'(''   (command executed)'')')
             END IF
             CALL DECODE('XADD',PARAMS,1,1,X1,'Constant ',OK)
             IF( .NOT.(OK)) GO TO 5000
             DO 3600 I = 1, NPOINT
                WAVE(I) = WAVE(I) + X1
 3600        CONTINUE
*
*   XABS  takes ABS(X)
*
          ELSE IF( SUBCMD .EQ. 'XABS' ) THEN
             CALL DECODE('XABS',PARAMS,0,0,VARRAY,' ',OK)
             DO 3620 I = 1, NPOINT
                WAVE(I) = ABS(WAVE(I))
 3620        CONTINUE
*
*   XINT  takes INT(X)
*
          ELSE IF( SUBCMD .EQ. 'XINT' ) THEN
             CALL DECODE('XINT',PARAMS,0,0,VARRAY,' ',OK)
             DO 3640 I = 1, NPOINT
                WAVE(I) = INT(WAVE(I))
 3640        CONTINUE
*
*   XNINT  takes NINT(X)
*
          ELSE IF( SUBCMD .EQ. 'XNINT' ) THEN
             CALL DECODE('XNINT',PARAMS,0,0,VARRAY,' ',OK)
             DO 3660 I = 1, NPOINT
                WAVE(I) = NINT(WAVE(I))
 3660        CONTINUE
*
*   XSQRD  squares X values
*
          ELSE IF( SUBCMD .EQ. 'XSQRD' ) THEN
             CALL DECODE('XSQRD',PARAMS,0,0,VARRAY,' ',OK)
             DO 3670 I = 1, NPOINT
                WAVE(I) = WAVE(I)**2
 3670        CONTINUE
*
*   YABS takes ABS(Y)
*
          ELSE IF( SUBCMD .EQ. 'YABS' ) THEN
             CALL DECODE('YABS',PARAMS,0,0,VARRAY,' ',OK)
             DO 3680 I = 1, NPOINT
                FLUX(I) = ABS(FLUX(I))
 3680        CONTINUE
*
*   YINT  takes INT(Y)
*
          ELSE IF( SUBCMD .EQ. 'YINT' ) THEN
             CALL DECODE('YINT',PARAMS,0,0,VARRAY,' ',OK)
             DO 3700 I = 1, NPOINT
                FLUX(I) = INT(FLUX(I))
 3700        CONTINUE
*
*   YNINT  takes NINT(Y)
*
          ELSE IF( SUBCMD .EQ. 'YNINT' ) THEN
             CALL DECODE('YNINT',PARAMS,0,0,VARRAY,' ',OK)
             DO 3720 I = 1, NPOINT
                FLUX(I) = NINT(FLUX(I))
 3720        CONTINUE
*
*   XSORT - sorts into increasing X
*
          ELSE IF( SUBCMD .EQ. 'XSORT' ) THEN
             CALL DECODE('XSORT',PARAMS,0,0,VARRAY,' ',OK)
             IF( .NOT.OK) OK = .TRUE.
             IF( NPOINT .EQ. 0 ) THEN
                WRITE (*,
     :          '(''   XSORT:  no data in current arrays'',       A)')
     :          BLEEP
             ELSE IF( NPOINT.GT.1 ) THEN
                CALL XSORT(NPOINT,WAVE,FLUX)
                NBREAK = 1
                BREAKS(NBREAK) = NPOINT
             END IF
*
*   XSUB  subtract constant from X
*
          ELSE IF( SUBCMD .EQ. 'XSUB' ) THEN
             CALL DECODE('XSUB',PARAMS,1,1,X1,'Constant ',OK)
             IF( .NOT.(OK)) GO TO 5000
             DO 3740 I = 1, NPOINT
                WAVE(I) = WAVE(I) - X1
 3740        CONTINUE
*
*   YSUB subtract a constant from Y
*
          ELSE IF( SUBCMD .EQ. 'YSUB' ) THEN
             CALL DECODE('YSUB',PARAMS,1,1,Y1,'Constant ',OK)
             IF( .NOT.(OK)) GO TO 5000
             DO 3760 I = 1, NPOINT
                FLUX(I) = FLUX(I) - Y1
 3760        CONTINUE
*
*   XTrim   X range to be trimmed
*
          ELSE IF( SUBCMD .EQ. 'XT' ) THEN
             CALL DECODE('XT',PARAMS,0,0,VARRAY,' ',OK)
             TRIMX = .TRUE.
*
*   XValue  Obtain X value from cursor
*
          ELSE IF( SUBCMD .EQ. 'XV' ) THEN
             CALL DECODE('XV',PARAMS,0,0,VARRAY,' ',OK)
             IF( .NOT.CURSOR .OR. NPLOTS .EQ. 0 ) THEN
                WRITE (*,'(''   XV:  no plot available'')')
                GO TO 5000
             END IF
             IHCALL = 0
             FIRSTCURSOR = .TRUE.
 3780        CONTINUE
             IHCALL = IHCALL + 1
             CALL SGSCURSE(I,X1,X2,FIRSTCURSOR)
             FIRSTCURSOR = .FALSE.
             IF( IHCALL .EQ. 1) XTEMP = 2.0*X1

             IF( X1 .EQ. 0.0 ) THEN
                WRITE (*,'(''   XV:  cursor error'')')
                GO TO 5000
             END IF
             IF( ABS((X1-XTEMP)/X1).GT.1.E-6 ) THEN
                IF( IHCALL .EQ. 1 ) THEN
                   WRITE (*,'(''   XV: '',4PE12.5)') X1
                   XTEMP = X1
                ELSE
                   WRITE (*,'(A,4PE12.5)') '+ '//CHAR(13)//CHAR(10)
     :             //'  XV: ', X1
                   XTEMP = X1
                END IF
                GO TO 3780
             END IF
*
*   XYSWAP  swaps X and Y arrays
*
          ELSE IF( SUBCMD .EQ. 'XYSWAP' ) THEN
             CALL DECODE('XYSWAP',PARAMS,0,0,DUMMY,' ',OK)
             IF( NPOINT .EQ. 0 ) THEN
                WRITE (*,'(''   XYSWAP:  '',
     :          ''no data present in current arrays'',A)') BLEEP
                GO TO 5000
             END IF
             DO 3800 I = 1, NPOINT
                TEMP = WAVE(I)
                WAVE(I) = FLUX(I)
                FLUX(I) = TEMP
 3800        CONTINUE
*
*   XYValue Obtain X and Y values from cursor
*
          ELSE IF( SUBCMD .EQ. 'XYV' ) THEN
             CALL DECODE('XYV',PARAMS,0,0,VARRAY,' ',OK)
             IF( .NOT.CURSOR .OR. NPLOTS .EQ. 0 ) THEN
                WRITE (*,'(''   XYV:  no plot available'')')
                GO TO 5000
             END IF
             IHCALLS = 0
             FIRSTCURSOR = .TRUE.
 3820        CONTINUE
             IHCALLS = IHCALLS + 1
             CALL SGSCURSE(I,X1,X2,FIRSTCURSOR)
             FIRSTCURSOR = .FALSE.
             IF( IHCALLS .EQ. 1 ) THEN
                XTEMP = 2.0*X1
                YTEMP = 2.0*X2
             END IF

             IF( X1 .EQ. 0.0 .OR. X2 .EQ. 0.0 ) THEN
                WRITE (*,'(''   XYV:  cursor error'')')
                GO TO 5000
             END IF
             IF( ABS((X1-XTEMP)/X1).GT.1.E-6 .OR. ABS((X2-YTEMP)/X2)
     :       .GT.1.E-6 ) THEN
                IF( IHCALLS .EQ. 1 ) THEN
                   WRITE (*,
     :             '(''   XV: '',4PE12.5,''  YV: '',1PE12.5)')
     :             X1, X2
                   XTEMP = X1
                   YTEMP = X2
                ELSE
                   WRITE (*,'(A,4PE12.5,A,1PE12.5)') '+ '//CHAR(13)
     :             //CHAR(10)//'  XV: ', X1, '  YV: ', X2
                   XTEMP = X1
                   YTEMP = X2
                END IF
                GO TO 3820
             END IF
*
*   YADD    Add a constant to y-values
*
          ELSE IF( SUBCMD .EQ. 'CADD' .OR. SUBCMD .EQ. 'YADD' ) THEN
             IF( SUBCMD .EQ. 'CADD' ) THEN
                WRITE (*,'(''   CADD command renamed YADD'',A)') BLEEP
                WRITE (*,'(''   (command executed)'')')
                SUBCMD = 'YADD'
             END IF
             CALL DECODE('YADD',PARAMS,1,1,CONST,'Constant ',OK)
             IF( .NOT.(OK)) GO TO 5000
             DO 840 I = 1, NPOINT
                FLUX(I) = FLUX(I) + CONST
  840        CONTINUE
*
*   YDEC  take decimal part of Y arrays
*
          ELSE IF( SUBCMD .EQ. 'YDEC' ) THEN
             CALL DECODE('YDEC',PARAMS,0,0,VARRAY,' ',OK)
             IF( .NOT.OK) GO TO 5000
             DO 3825 I = 1, NPOINT
                FLUX(I) = FLUX(I) - INT(FLUX(I))
 3825        CONTINUE
*
*   YDIV    Divide y-values bby a constant
*
          ELSE IF( SUBCMD .EQ. 'CDIV' .OR. SUBCMD .EQ. 'YDIV' ) THEN
             IF( SUBCMD .EQ. 'CDIV' ) THEN
                WRITE (*,'(''   CDIV command renamed YDIV'',A)') BLEEP
                SUBCMD = 'YDIV'
                WRITE (*,'(''   (command executed)'')')
             END IF
             CALL DECODE('YDIV',PARAMS,1,1,CONST,'Constant ',OK)
             IF( CONST .EQ. 0.0 ) THEN
               WRITE (*,'(''   YDIV:  division by zero not possible'')')
                OK = .FALSE.
             END IF
             IF( .NOT.(OK)) GO TO 5000
             DO 860 I = 1, NPOINT
                FLUX(I) = FLUX(I)/CONST
  860        CONTINUE
*
*   YMULT   Multiply Y by a constant
*
          ELSE IF( SUBCMD .EQ. 'CMULT' .OR. SUBCMD .EQ. 'YMULT' ) THEN
             IF( SUBCMD .EQ. 'CMULT' ) THEN
                WRITE (*,'(''   CMULT renamed YMULT'',A)') BLEEP
                SUBCMD = 'YMULT'
                WRITE (*,'(''   (command executed)'')')
             END IF
             CALL DECODE('YMULT',PARAMS,1,1,CONST,'Constant ',OK)
             IF( .NOT.(OK)) GO TO 5000
             DO 900 I = 1, NPOINT
                FLUX(I) = FLUX(I)*CONST
  900        CONTINUE
*
*   YJust   Y range to be justified
*
          ELSE IF( SUBCMD .EQ. 'YJ' ) THEN
             CALL DECODE('YJ',PARAMS,0,0,VARRAY,' ',OK)
             TRIMY = .FALSE.
*
*   YLABEL   Label Y axis
*
          ELSE IF( SUBCMD .EQ. 'YLABEL' .OR. SUBCMD .EQ. 'YLAB' ) THEN
             YLABLN = MIN(80,MAX(1,SLEN(PARAMS)))
             YLAB = PARAMS(1:YLABLN)
             IF( .NOT.SUBCHK ) THEN
                WRITE (*,'(''   YLABEL:  closing quotes added'')')
                SUBCHK = .TRUE.
                GO TO 5000
             END IF
*
*   YMAX    Set Y maximum for plotting
*
          ELSE IF( SUBCMD .EQ. 'YMAX' ) THEN
             CALL DECODE('YMAX',PARAMS,1,1,VARRAY,'Y(max) ',OK)
             IF( .NOT.(OK)) GO TO 5000
             CALL YMAXST(VARRAY(1))
             YRKEEP(2) = VARRAY(1)
*
*   YMIN    Set Y minimum for plotting
*
          ELSE IF( SUBCMD .EQ. 'YMIN' ) THEN
             CALL DECODE('YMIN',PARAMS,1,1,VARRAY,'Y(min) ',OK)
             IF( .NOT.(OK)) GO TO 5000
             CALL YMINST(VARRAY(1))
             YRKEEP(1) = VARRAY(1)
*
*   YRange  Set Y range for plotting
*
          ELSE IF( SUBCMD .EQ. 'YR' ) THEN
             CALL DECODE('YR',PARAMS,2,2,VARRAY,'Y1 Y2 ',OK)
             IF( .NOT.(OK)) GO TO 5000
             CALL YRSET(VARRAY(1),VARRAY(2))
             YRKEEP(1) = VARRAY(1)
             YRKEEP(2) = VARRAY(2)
*
*   YSQRD   square Y values
*
          ELSE IF( SUBCMD .EQ. 'YSQRD' ) THEN
             CALL DECODE('YSQRD',PARAMS,0,0,VARRAY,' ',OK)
             DO 3827 I = 1, NPOINT
                FLUX(I) = FLUX(I)**2
 3827        CONTINUE
*
*   YTrim   Y range to be trimmed
*
          ELSE IF( SUBCMD .EQ. 'YT' ) THEN
             CALL DECODE('YT',PARAMS,0,0,VARRAY,' ',OK)
             TRIMY = .TRUE.
*
*   YValue  Obtain Y value from cursor
*
          ELSE IF( SUBCMD .EQ. 'YV' ) THEN
             VARRAY(1) = -654.321
             CALL DECODE('YV',PARAMS,0,1,VARRAY,' ',OK)
             IF( VARRAY(1) .NE. -654.321 ) THEN
                WAVX = VARRAY(1)
                IF( WAVX.LT.MIN(WAVE(1),WAVE(NPOINT)) .OR.
     :          WAVX.GT.MAX(WAVE(1),WAVE(NPOINT)) ) THEN
                   WRITE (*,
     :             '(''   YV:  X is outside range of current arrays'',
     :             A)') BLEEP
                   GO TO 5000
                END IF
                LOOP1 = 1
                LOOP2 = NPOINT
                LOOP3 = 1
                IF( WAVE(NPOINT).LT.WAVE(1) ) THEN
                   LOOP1 = NPOINT
                   LOOP2 = 1
                   LOOP3 = -1
                END IF
                DO 3830 I = LOOP1, LOOP2, LOOP3
                   IF( WAVE(I).GE.WAVX ) THEN
                      J = I - LOOP3
                      J = MIN(J,NPOINT)
                      J = MAX(1,J)
                      DWX = WAVE(I) - WAVE(J)
                      DW2 = WAVX - WAVE(J)
                      WT1 = DW2/DWX
                      WT2 = 1.0 - WT1
                      FLUXX = FLUX(I)*WT1 + FLUX(J)*WT2
                      WRITE (*,'(''   YV: '',1PE12.5)') FLUXX
                      GO TO 5800
                   END IF
 3830           CONTINUE
                WRITE (*,'(''   YV:  X not found'',A)') BLEEP
                GO TO 5000
             ELSE IF( .NOT.CURSOR .OR. NPLOTS .EQ. 0 ) THEN
                WRITE (*,'(''   YV:  no plot available'')')
                GO TO 5000
             ELSE
                IHCALL = 0
                FIRSTCURSOR = .TRUE.
 3840           CONTINUE
                IHCALL = IHCALL + 1
                CALL SGSCURSE(I,X1,X2,FIRSTCURSOR)
                FIRSTCURSOR = .FALSE.
                IF( IHCALL .EQ. 1) YTEMP = 2.0*X2

                IF( X2 .EQ. 0.0 ) THEN
                   WRITE (*,'(''   YV:  cursor error'')')
                   GO TO 5000
                END IF
                IF( ABS((X2-YTEMP)/X2).GT.1.E-6 ) THEN
                   IF( IHCALL .EQ. 1 ) THEN
                      WRITE (*,'(''   YV: '',1PE12.5)') X2
                      YTEMP = X2
                   ELSE
                      WRITE (*,'(A,1PE12.5)') '+ '//CHAR(13)//CHAR(10)
     :                //'  YV: ', X2
                      YTEMP = X2
                   END IF
                   GO TO 3840
                END IF
             END IF
*
*   YXN  Calculate FLUX * WAVE**N (for RESC)
*
          ELSE IF( SUBCMD .EQ. 'YXN' ) THEN
             IF( NPOINT.LT.1 ) THEN
                WRITE (*,'(''   YXN:  no data'')')
                GO TO 5000
             ELSE IF( WORV .NE. 1.0 ) THEN
                WRITE (*,
     :          '(''   YXN:  data are not in wavelength space'')')
                GO TO 5000
             END IF
             VARRAY(1) = 4.0
             CALL DECODE('YXN',PARAMS,1,1,VARRAY,'Power ',OK)
             IF( .NOT.OK) GO TO 5000
             POWER = VARRAY(1)
             CALL YXN(ASIZE1,WAVE,FLUX,NPOINT,MAXBRK,BREAK,NBREAK,POWER)
*
*   ZANSTRA  Calculate Zanstra temperatures
*
          ELSE IF( SUBCMD .EQ. 'ZANSTRA' ) THEN
             IF( .NOT.CURSOR .OR. NPLOTS .EQ. 0 ) THEN
                WRITE (*,'(''   ZANSTRA:  plot of data required'')')
                GO TO 5000
             END IF
             VARRAY(4) = 0.0
             CALL DECODE('ZANSTRA',PARAMS,3,4,VARRAY,
     :       'Line F(obs) T(neb) ',OK)
             IF( .NOT.OK) GO TO 5000
             TEMP = VARRAY(3)
             VARRAY(3) = VARRAY(4)
             VARRAY(4) = TEMP
             IF( OK ) THEN
                IF( NINT(VARRAY(1)) .NE. 1640 .AND. NINT(VARRAY(1))
     :           .NE. 4471 .AND. NINT(VARRAY(1)) .NE. 4686 .AND.
     :          NINT(VARRAY(1)) .NE. 4861 .AND. NINT(VARRAY(1))
     :           .NE. 5876 ) THEN
                   WRITE (*,
     :             '(''   ZANSTRA:  line must be 1640, 4471,'',
     :             '' 4686, 4861 or 5876'')')
                   GO TO 5000
                END IF
                IF( VARRAY(2).LE.0.0 ) THEN
                   VARRAY(2) = 10.0**VARRAY(2)
                   IF( VARRAY(2) .EQ. 0.0 ) THEN
                      WRITE (*,
     :                '(''   ZANSTRA:  F(obs) too small - underflow'')')
                      GO TO 5000
                   END IF
                ELSE IF( VARRAY(3).LT.0.0 ) THEN
                   WRITE (*,
     :             '(''   ZANSTRA:  E(B-V) cannot be negative'')')
                   GO TO 5000
                ELSE IF( VARRAY(4).LE.0.0 ) THEN
                   WRITE(*,'(''   ZANSTRA:  T(neb) must be positive'')')
                   GO TO 5000
                END IF
                CALL ZANSTRA(OK,VARRAY)
                IF( .NOT.OK ) THEN
                   OK = .TRUE.
                   GO TO 5000
                END IF
             END IF
*
*   IPGWR
*
       ELSE IF( SUBCMD .EQ. 'IPGWR' ) THEN
          CLOSE(12)
          WRITE (12,
     :    '(10('' '',F10.1,F10.3/)/'' '')')
     :    WAVE(1), FLUX(1), (WAVE(I),FLUX(I),I=1,NPOINT)
*   ELSE IF( SUBCMD .EQ. 'IPGWR' ) THEN
*   CALL SSTRIP(PARAMS)
*   CALL DTOUPP(PARAMS)
*   IF( PARAMS .NE. ' ' ) THEN
*   CLOSE (12)
*   OPEN (UNIT=12,STATUS='NEW',FILE=PARAMS(1:SLEN(PARAMS)),
*   :                IOSTAT=IHX)
*   IF( IHX .NE. 0 ) THEN
*   WRITE (*,'(''   Unable to open '',A)')
*   :                     PARAMS(1:SLEN(PARAMS))
*   GO TO 5000
*   END IF
*   END IF
*   WRITE (12,'('' ''/'' '',A/)') TITLE(1:SLEN(TITLE))
*
*   ZANSTRA INTEGRALS
*   ZSUM1 = 0.0
*   ZSUM2 = 0.0
*   I = 1
*   J = 1
*   DO 3860 WHILE (WAVE(I).LT.912.0)
*   J = J + 1
*   I = J - 1
*   IF( WAVE(J).LT.504.0 ) THEN
*   ZSUM1 = ZSUM1 + (WAVE(J)-WAVE(I))
*   :                     *0.5*(WAVE(J)*FLUX(J)+WAVE(I)*FLUX(I))
*   ZSUM2 = ZSUM2 + (WAVE(J)-WAVE(I))
*   :                     *0.5*(WAVE(J)*FLUX(J)+WAVE(I)*FLUX(I))
*   ELSE IF( WAVE(I).LT.504.0 ) THEN
*   ZSUM1 = ZSUM1 + (504.0-WAVE(I))
*   :                     *0.5*(504.0*FLUX(I)+WAVE(I)*FLUX(I))
*   ZSUM2 = ZSUM2 + (WAVE(J)-WAVE(I))
*   :                     *0.5*(WAVE(J)*FLUX(J)+WAVE(I)*FLUX(I))
*   ELSE IF( WAVE(J).LT.912.0 ) THEN
*   ZSUM2 = ZSUM2 + (WAVE(J)-WAVE(I))
*   :                     *0.5*(WAVE(J)*FLUX(J)+WAVE(I)*FLUX(I))
*   ELSE IF( WAVE(I).LT.912.0 ) THEN
*   ZSUM2 = ZSUM2 + (912.0-WAVE(I))
*   :                     *0.5*(912.0*FLUX(I)+WAVE(I)*FLUX(I))
*   END IF
 3860        CONTINUE
*   IF( ZSUM1.GT.0.0) ZSUM1 = LOG10(max(1e-30,ZSUM1)) - 2.0
*   IF( ZSUM2.GT.0.0) ZSUM2 = LOG10(max(1e-30,ZSUM2)) - 2.0
*
*
*   WRITE (12,'('' ''/'' '',10X,F15.5/       '' '',10X,F15.5)')
*   :               ZSUM1, ZSUM2
*
*   GRIFFU3 = GRIFFU3 - GRIFFB2 - 1.093 + 0.634
*   GRIFFB3 = GRIFFB3 - GRIFFV1 + 0.710 - 0.122
*   WRITE (12,
*   :  '('' ''/'' '',10X,F15.5/       '' '',10X,F15.5/'' '',10X,F15.5)'
*   :  ) GRIFFV1, GRIFFB3, GRIFFU3
*
*   GRIFFV1 = 0.0
*   GRIFFB2 = 0.0
*   GRIFFB3 = 0.0
*   GRIFFU3 = 0.0
*
*   FOURIER (SJB) COMMANDS:
*   FTRANS, FTFILTER, FTINV,
*   PDGRAM, PDGWINDOW, PDGPEAK,
*   XCORR, INTERP
          ELSE IF( SUBCMD .EQ. 'FTRANS' .OR. SUBCMD .EQ. 'FTFILTER' .OR.
     :    SUBCMD .EQ. 'FTINV' .OR. SUBCMD .EQ. 'PDGRAM' .OR.
     :    SUBCMD .EQ. 'PDGWINDOW' .OR. SUBCMD .EQ. 'PDGPEAK' .OR.
     :    SUBCMD .EQ. 'XCORR' .OR. SUBCMD .EQ. 'INTERP' ) THEN
          IF( NPOINT .EQ. 0 ) THEN
             WRITE (*,'('' '',A,'':  no data in current arrays'',A)')
     :       SUBCMD(1:SLEN(SUBCMD)), BLEEP
             GO TO 5000
          END IF
             CALL FCALLER(SUBCMD(1:CMDLEN),PARAMS,ASIZE1,MAXBRK,WAVE,
     :       FLUX,NPOINT,BREAK,NBREAK,TITLE,OK)
             IF( .NOT.OK) GO TO 5000
*
*   ROE IR COMMANDS:
*   CGSREAD, SPECTRE, BARS, PRINT, COADD, ADAMSPEC
*
          ELSE IF( SUBCMD .EQ. 'ADAMSPEC' .OR. SUBCMD .EQ. 'BARS' .OR.
     :    SUBCMD .EQ. 'COADD' .OR. SUBCMD .EQ. 'CGSREAD' .OR.
     :    SUBCMD .EQ. 'PRINT' .OR. SUBCMD .EQ. 'SPECTRE' ) THEN
             CALL IRUSER(SUBCMD(1:CMDLEN),PARAMS,ASIZE1,MAXBRK,WAVE,
     :       FLUX,NPOINT,BREAK,NBREAK,TITLE,OK)
             IF( .NOT.OK) GO TO 5000
*
*   User Commands
*
          ELSE IF( USER(SUBCMD(1:CMDLEN),PARAMS,ASIZE1,MAXBRK,WAVE,FLUX,
     :    NPOINT,BREAK,NBREAK,TITLE,OK) ) THEN

*   Found user command

             IF( .NOT.OK) GO TO 5000
             IF( NPOINT.LT.0 ) THEN
                WRITE (*,'(''   USER error - NPT:'',I8)') NPOINT
                NPOINT = 0
             ELSE IF( NPOINT.GT.ASIZE1 ) THEN
                WRITE (*,'(''   USER error - NPT:'',I8)') NPOINT
                WRITE (*,'(''   (Maximum value allowed:'',I8,'')'')')
     :          ASIZE1
                NPOINT = ASIZE1
             END IF
             IF( NBREAK.LT.0 ) THEN
                WRITE (*,'(''   USER error - NBREAK:'',I8)') NBREAK
                NBREAK = 0
                NPOINT = 0
             ELSE IF( NBREAK.GT.MAXBRK ) THEN
                WRITE (*,'(''   USER error - NBRK:'',I8)') NBREAK
                WRITE (*,'(''   (Maximum value allowed:'',I8,'')'')')
     :          MAXBRK
             END IF
             IF( NPOINT.GT.0 .AND. NBREAK.GT.0 ) THEN
                IF( BREAK(NBREAK) .NE. NPOINT ) THEN
                   WRITE (*,
     :             '(''   USER warning - BREAK/DATA mismatch'')')
                   BREAK(NBREAK) = NPOINT
                END IF
             END IF
          ELSE
             WRITE (*,'(''   Illegal command:'',A20)')
     :       SUBCMD(1:CMDLEN)
 5000        CONTINUE
             IF( .NOT.OK ) THEN
                WRITE (*,'(''   Error attempting to obey '',A)')
     :          SUBCMD(1:SLEN(SUBCMD))
             END IF
             IF( COMTXT ) THEN
                WRITE (*,'(''   Returning to terminal control'')')
                COMTXT = .FALSE.
                CLOSE (67)
             END IF
             IF( BEEP ) THEN
                WRITE (*,'(''   Command line aborted'',a)') BLEEP
             ELSE
                WRITE (*,'(''   Command line aborted'')')
             END IF
             DELIM = 0
             OK = .TRUE.
          END IF
 5800     CONTINUE
          IF( WARNIT ) THEN
             WRITE (*,
     :       '(''   WARNING:  condition handler invoked by '',     A)')
     :       SUBCMD(1:CMDLEN)
             WARNIT = .FALSE.
             GO TO 5000
          END IF
          IF( IOS .NE. SAI__OK ) CALL ERR_FLUSH ( ios )

*  CHeck for GKS/SGS errors.
          CALL GKS_GSTAT( STATUS )

*  If a parameter requested abort was given ("!!"), flush the error,
*  close down SGS, save the stack, and abort.
          IF( STATUS .EQ. PAR__ABORT ) THEN
             CALL ERR_REP( 'DIPSO_ERR1','Shutting down DIPSO...',
     :                     STATUS )
             CALL ERR_FLUSH( STATUS )
             CALL SGS_CLOSE
             CALL SAVE( 'EXIT', ' ', STATUS )
             GO TO 6000

*  If any other error has been reported (other than PAR__ABORT), just flush
*  it and continue.
          ELSE IF( STATUS .NE. SAI__OK ) THEN
             CALL ERR_FLUSH( STATUS )
          END IF

*  Give a blank line to separate the dialogue of the previous command
*  from that of the next.
c          WRITE(*,*)

*  Go back for the next command.
          GO TO 400

       END IF

       CALL SGS_CLOSE

       GO TO 6000

 6000  CONTINUE

*  Release the error stack. Any remaining deferred error messages will be
*  delivered to the user.
       CALL ERR_RLSE

       END
