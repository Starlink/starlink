      SUBROUTINE ABLINE
C+
C     A B L I N E
C
C     This routine does interactive analysis of absorption lines in
C     spectra.
C
C     The user designates a segment of the input spectrum to
C     be analysed in each pass.  First a continuum is fitted to this
C     region, using only wavelength subsegments selected graphically
C     by the user (ie. ignoring the absorption line in question and any
C     other nearby lines or spikes).  In addition to this selection,
C     iterative rejection of discrepant points is performed.  The
C     functional form of the continuum is a polynomial of degree
C     specified by the user (0 - 7).  Alternatively, if a precomputed
C      continuum spectrum is available, it can be used instead.
C
C     The user specifies the wavelength limits of the interval
C     containing the line itself: the median wavelength and equivalent
C     width of the absorption line are calculated.
C
C     The routine finishes up each segment with a hard copy plot showing
C     the data, continuum and wavelength limits of the line, with a
C     printout of results.
C
C     Command line parameters -
C
C     SPECTRUM    Name of the file containing the spectrum with
C                 lines to be fitted
C     CONTIN      File containing precomputed continuum, if one is
C                 to be used.
C     SIG         Multiple of sigma for continuum point rejection
C     ITN         Number of iterations for continuum point rejection
C     DEG         Degree of polynomial for continuum fit
C     WIDTH       Wavelength range to display at one time
C     CONTOUT     Output continuum file name, if one is written.
C                 A new file, the same as SPECTRUM except for the data,
C                 is created.
C     CMD         The command in the main menu.
C     LINENAME    The name of a line to be fitted.
C     COMMENT     A comment for a hardcopy
C
C     Command keywords -
C
C     OLDCONT     Set if a precomputed continuum is to be used.
C     LIMIT       LIMIT is set if the limits of a line are to be
C                 taken as the limits indicated with the cursor.
C                 Otherwise, the program will look for the points
C                 within the indicated limits where the data drops
C                 below the continuum.
C     NEWCONT     Set if the continuum constructed during the run
C                 is to be written to a file.
C     HARDCOPY    True if hardcopy of soft plot to be made.
C
C     User variables -  (">" input, "<" output)
C
C     (>) SOFT    (Character) Device/type for soft plots
C     (>) HARD    (Character)   "     "    "  hard  "
C     (<) TVXST   (Numeric)  )
C     (<) TVXEN   (Numeric)  ) Used to set the soft plot
C     (<) TVHIGH  (Numeric)  ) parameters so that
C     (<) TVLOW   (Numeric)  ) routines such as CCUR
C     (<) XSTART  (Numeric)  ) know what the display
C     (<) XEND    (Numeric)  ) limits for the currently
C     (<) HIGH    (Numeric)  ) displayed plot have
C     (<) LOW     (Numeric)  ) been set to.
C     (<) TVFILE  (Character))
C     (<) TVCOLOR (Numeric)  )
C                                                    JGR   Jan 1985
C     Modified:
C
C     02 Oct 1985  KS / AAO.  Changed to use standard Figaro parameter
C                  parameter handling.  Style of prompts etc modified
C                  to look more like a standard Figaro routine, and a
C                  more menu-like mode of operation introduced.
C     23 Jul 1986  KS / AAO. Output format used for hard copy modified
C                  slightly to print numbers with more significant figs.
C     04 Sep 1986  KS / AAO. Output format to terminal also modified to
C                  output with more precision.
C     24 Mar 1988  KS / AAO. Modified for GKS version of PGPLOT. HARDFILE
C                  parameter no longer supported, unfortunately.
C     13 Dec 1991  HME / UoE, Starlink. Replace GRSETCOL by PGSCI.
C     03 Sep 1992  HME / UoE, Starlink. Remove all TABs. Open help
C                  file with lowercase name.
C                  PGASK is banned from ADAM, commented out.
C     30 Oct 1992  HME / UoE, Starlink. Initialise returned value of
C                  SPLITSUM.
C     25 Jan 1993  HME / UoE, Starlink. Put PGASK back.
C     22 Jul 1993  HME / UoE, Starlink. Convert to DSA. Disuse GKD.
C                  Disuse PAR_Q*, new parameters LINENAME, HARDCOPY,
C                  COMMENT, CMD.  Call PAR_ABORT.
C     15 Feb 1995  HME / UoE, Starlink. Declare Q single precision,
C                  because it is.
C     05 Apr 1995  HME / UoE, Starlink. No longer use NAG.
C     15 Feb 1996  HME / UoE, Starlink. Convert to FDA:
C                  Avoid _NAMED_ routines.
C     24 Jul 1996  MJCL / Starlink, UCL.  Mod to FORMAT labelled 44
C                  to be shorter than 72 characters for Linux port.
C     23 Feb 2001  ACD / UoE, Starlink. Removed technically illegal
C                  jumps into IF blocks.
C     2005 June 10 MJC / Starlink  Use CNF_PVAL for pointers to
C                  mapped data.
C     2005 Aug 15  TIMJ / JACH Force integer array indices
C+
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions used
C
      LOGICAL DSA_SAME_DATA,PAR_ABORT
      INTEGER PGBEGIN,ICH_LEN,ICH_ENCODE
      INTEGER DSA_TYPESIZE
      REAL GEN_ELEMF
C
C     Local variables
C
      REAL XVALS(2048),YVALS(2048),DELTLAM,CENLAM,XMIN,XMAX,A,
     :YMIN,YMAX,DELTY,XCST(10),XCEND(10),XCUR,YCUR,CVALS(2048),XL,XH,
     :SUM,XMED,WLAM,XRANGL(2),XRANGH(2),YRANGE(2),ACH,ABS,
     :SPLITSUM,XSIGU,XSIGL,WIDTH,SIGN,ASYM,VALUE,Q
C
      DOUBLE PRECISION
     :   DUMMY,
     :   EPS,
     :   XFIT(2048),
     :   YFIT(2048),
     :   WFIC(2048),
     :   RFIT(2048),
     :   XOLD,
     :   SNAG(8),
     :   PNAG,
     :   ANAG2(3*2048+3*8),
     :   QLIM
C
      INTEGER
     :   NPOL,
     :   NDEG,
     :   LESPEC,
     :   STATUS,
     :   LECONT,
     :   DIMS(10),NDIM,DIMC(10),NCHAN,
     :   IPTRS,IPTRC,IPTRX,CHCEN,CHDELP,CHDELM,NSEG,
     :   NCONTSS,NCST(10),NCEND(10),NFIT,I,J,LEOBJ,JNEW,NQ,NITN,NREJ,
     :   NL,NH,NTEM,JTEM,LENAM,JST,JEND,NINT,JCST,JCEND,
     :   LEHARDF,IGNORE,INVOKE,NEXT,IFAIL,
     :   IFAIL2,LDAY,LDATE,LHOUR,
     :   PGSTAT,
     :   SLOT,
     :   TPTR,
     :   WSLOT
C
      CHARACTER*64
     :   SPEC,
     :   CONT,
     :   LABEL(2),
     :   SOFTDEV,OBJ,LINNAM,
     :   HARDDEV,HARDFIL,CONTO,COMENT
C
      CHARACTER  CURC*1,DAT*20,TIM*12,TEXT*100,
     :COMMAND*8,STRING*80,DAY*9
C
      LOGICAL
     :   QREPLY,
     :   LIMIT,
     :   CONTOPN,
     :   CONTOLD,CONTWR,
     :   OUOPEN,
     :   FAULT,PLTOPN,
     :   SELECT,PLTTED,WARNED
C
C     PGPLOT colour values
C
      INTEGER GREEN, RED, WHITE, BLUE
      PARAMETER (WHITE=1, RED=2, GREEN=3, BLUE=4)
C
C     Initial values
C
      PLTOPN=.FALSE.             ! True if soft copy plot open
      FAULT=.FALSE.              ! True if error occurs
      CONTOPN=.FALSE.            ! True when continuum spectrum file
                                 ! opened
      PLTTED=.FALSE.             ! True if a soft plot has been made
      OUOPEN=.FALSE.             ! True when output continuum file open
      SELECT=.FALSE.             ! True if subsegments selected.
      LIMIT=.FALSE.              ! .TRUE. : cutoff equiv width sum at
                                 ! designated limits.  .FALSE. : cutoff
                                 ! at first channel exceeding continuum
      CONTWR=.FALSE.             ! True when a continuum file to be written.
C
C     Initialise.
C
      STATUS=0
      CALL DSA_OPEN(STATUS)
C
C     Program won't work without a soft device, so get specification
C     for that first.
C
      CALL VAR_GETCHR('SOFT',0,0,SOFTDEV,STATUS)
      IF (STATUS.NE.0) THEN
         CALL PAR_WRUSER('Cannot obtain device/type for soft plot',
     :                                                        STATUS)
         FAULT=.TRUE.
         GO TO 500
      END IF
C
C     Get user specified parameters for whole run.
C       Start by getting spectrum file name and opening it.
C
      CALL DSA_INPUT('SPEC','SPECTRUM',STATUS)
      CALL DSA_GET_ACTUAL_NAME('SPEC',SPEC,STATUS)
      IF (STATUS.NE.0) GO TO 500
      LESPEC=ICH_LEN(SPEC)
      CALL DSA_DATA_SIZE('SPEC',1,NDIM,DIMS,NCHAN,STATUS)
      IF (STATUS.NE.0) GO TO 500
      NCHAN=DIMS(1)
C
C     Get workspace.  First, we need space for the continuum spectrum.
C
      CALL DSA_GET_WORK_ARRAY( NCHAN, 'FLOAT', IPTRC, WSLOT, STATUS )
      IF (STATUS.NE.0) GO TO 500
C
C     Continuum spectrum - either new or old file.
C     First find if old or new.
C
      CALL PAR_RDKEY('OLDCONT',.FALSE.,CONTOLD)
      IF (PAR_ABORT()) GO TO 500
C
      IF (CONTOLD) THEN
C
C        Old continuum file to be read.  Prompt for name and
C        open it.
C
         CALL DSA_INPUT('CONT','CONTIN',STATUS)
         CALL DSA_GET_ACTUAL_NAME('CONT',CONT,STATUS)
         IF (STATUS.NE.0) GO TO 500
         LECONT=ICH_LEN(CONT)
         IF (DSA_SAME_DATA('SPEC','CONT',STATUS)) THEN
            CALL PAR_WRUSER(
     :        'Continuum file cannot be same as input spectrum',STATUS)
            FAULT=.TRUE.
            GO TO 500
         END IF
         CALL DSA_DATA_SIZE('CONT',1,NDIM,DIMC,IGNORE,STATUS)
         IF (STATUS.NE.0) GO TO 500
         IF (DIMC(1).NE.NCHAN) THEN
            CALL PAR_WRUSER(
     :         'Continuum data has different length to spectrum',STATUS)
            FAULT=.TRUE.
            GO TO 500
         END IF
         CONTOPN=.TRUE.
C
C           Read in continuum file data.
C           The continuum data are copied into the work space. Possibly
C           the modified data will be written later on, possibly into
C           the same file. So here it is best to unmap the data after
C           copy.
C
         CALL DSA_MAP_DATA('CONT','READ','FLOAT',TPTR,SLOT,STATUS)
         IF (STATUS.NE.0) GO TO 500
         CALL GEN_MOVE(DSA_TYPESIZE('FLOAT',STATUS)*NCHAN,
     :                 %VAL(CNF_PVAL(TPTR)),%VAL(CNF_PVAL(IPTRC)))
         CALL DSA_UNMAP(SLOT,STATUS)
         IF (STATUS.NE.0) GO TO 500
C
C        Use default values for continuum parameters
C
         NQ=4
         Q=2.25
         NPOL=1
      ELSE
C
C        No existing continuum file to be used, so set continuum to
C        zero.
C
         CALL GEN_FILL(DSA_TYPESIZE('FLOAT',STATUS)*NCHAN,
     :                  0,%VAL(CNF_PVAL(IPTRC)))
C
C        Get the other continuum fitting parameters
C
         CONT=' '
         CALL PAR_RDVAL('SIG',0.,100.,2.25,' ',Q)
         CALL PAR_RDVAL('ITN',0.,100.,4.,'Iterations',VALUE)
         NQ=VALUE
         CALL PAR_RDVAL('DEG',0.,7.,1.,' ',VALUE)
         NPOL=VALUE
         IF (PAR_ABORT()) GO TO 500
      END IF
C
C     Get file name for hardcopy plots
C
      CALL VAR_GETCHR('HARD',0,0,HARDDEV,STATUS)
      IF(STATUS.NE.0)THEN
         CALL PAR_WRUSER(
     :         'Cannot obtain device type for hardcopy plot',STATUS)
         FAULT=.TRUE.
         GO TO 500
      END IF
      HARDFIL = HARDDEV
      LEHARDF=ICH_LEN(HARDFIL)
C
C     Map spectrum data file, to enable access to any of its elements
C
      CALL DSA_MAP_DATA('SPEC','READ','FLOAT',IPTRS,SLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Map input spectrum's X data, for access to wavelength scale
C     First display annotations to X axis, for user to check.
C
      LABEL(1)='Not specified'
      LABEL(2)=LABEL(2)
      CALL DSA_GET_AXIS_INFO('SPEC',1,2,LABEL,1,DUMMY,STATUS)
      CALL PAR_WRUSER('Input spectrum X axis has label: '//
     :                         LABEL(2)(:ICH_LEN(LABEL(2))),STATUS)
      CALL PAR_WRUSER('                      and units: '//
     :                         LABEL(1)(:ICH_LEN(LABEL(1))),STATUS)
C
C     Map X data for spectrum
C
      CALL DSA_MAP_AXIS_DATA('SPEC',1,'READ','FLOAT',IPTRX,SLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Find and print minimum and maximum values of X (wavelength) for
C     entire spectrum
C
      XMIN=GEN_ELEMF(%VAL(CNF_PVAL(IPTRX)),1)
      XMAX=GEN_ELEMF(%VAL(CNF_PVAL(IPTRX)),NCHAN)
      WRITE (STRING,8,IOSTAT=IGNORE) XMIN,XMAX
    8 FORMAT('Input spectrum X (wavelength) axis ranges from',F10.3,
     :                                                 '  to',F10.3)
      CALL PAR_WRUSER(STRING,STATUS)
C
C     Get the last of the parameters from the user
C
      CALL PAR_RDKEY('LIMIT',.TRUE.,LIMIT)
      CALL PAR_RDVAL('WIDTH',0.001,XMAX-XMIN,(XMAX-XMIN)/5.,
     :   LABEL(1),DELTLAM)
      IF (PAR_ABORT()) GO TO 500
C
      CALL PAR_RDKEY('NEWCONT',.FALSE.,CONTWR)
      IF (PAR_ABORT()) GO TO 500
      IF (CONTWR) THEN
         CALL DSA_OUTPUT('OUTPUT','CONTOUT','SPEC',0,0,STATUS)
         CALL DSA_GET_ACTUAL_NAME('OUTPUT',CONTO,STATUS)
         IF (STATUS.NE.0) GO TO 500
      END IF
C
C       Following values indicate no continuum fit so far
C
      JCST=-1
      JCEND=-1
C
C       Initial opening of soft plotting device
C
      PGSTAT=PGBEGIN(0,SOFTDEV,1,1)
      IF(PGSTAT.NE.1)THEN
        CALL PAR_WRUSER(
     :           'Cannot open graphics device for soft plot',STATUS)
        FAULT=.TRUE.
        GO TO 500
      END IF
      PLTOPN=.TRUE.
C
      CALL PAR_WRUSER(' ',STATUS)
      CALL PAR_WRUSER('ABLINE main command ''menu''.'//
     :          '  Use ''HELP'' for assistance.',STATUS)
    5 CONTINUE
C
C     Main menu.  At this point one of a number of different
C     commands may be input.  Most are concerned with changing
C     the values of parameters already set.  Others are used to
C     determine the continuum over a specified range, to
C     change the wavelength range displayed, to delimit a line,
C     or to initiate a fit.
C
C       Possible responses are:
C
C       SIG     xxx     set new value for Q
C       DEG     nnn      "   "    "    "  NPOL
C       ITN     nnn      "   "    "    "  NQ
C       LIMIT            "   "    "    "  LIMIT
C       NOLIM            "   "    "    "   "
C       WIDTH   xxx      "   "    "    "  DELTLAM
C       CONT          fit continuum
C       FIT           analyse a line and produce hard plot
C       QUIT          exit program
C       HELP (?)      output help text
C       RECONT        repeat continuum fit, but with same subsections
C       xxxxx         new centre wavelength: begin line analysis
C
C         Note that some of these are handled by ABGETCOM itself.
C
      CALL ABGETCOM(XMIN,XMAX,COMMAND,Q,NPOL,NQ,DELTLAM,LIMIT,CENLAM)
C
C     If a new plot is required, produce it. (This happens when a
C       new center wavelength or a new width is specified.)
C
      IF ((COMMAND.EQ.'XXXX').OR.(COMMAND.EQ.'WIDTH')) THEN
C
C        Get X values for desired portion of spectrum into XVALS; allowing
C        for end effects, to ensure don't go outside data space.
C        CHDELM is element number in SPEC.X.DATA of first element put into
C        XVALS - hence pointer to get corresponding Z.DATA
C
C        First get channel numbers corresponding to CENLAM and
C        CENLAM +/- DELTLAM
C
         CHCEN=(NCHAN*CENLAM-CENLAM-NCHAN*XMIN+XMAX)/(XMAX-XMIN)
         A=CENLAM-DELTLAM/2.
         CHDELM=(NCHAN*A-A-NCHAN*XMIN+XMAX)/(XMAX-XMIN)
         A=CENLAM+DELTLAM/2.
         CHDELP=(NCHAN*A-A-NCHAN*XMIN+XMAX)/(XMAX-XMIN)
C
C        Check if segment specified by CENLAM, DELTLAM is entirely
C        within range.  If not, move centre so that it is.  Illegal to
C        have DELTLAM more than entire wavelength (X) range of
C        input spectrum.
C
    9    IF(CHDELP.LE.NCHAN)GO TO 10
         CHDELP=CHDELP-1
         CHCEN =CHCEN -1
         CHDELM=CHDELM-1
         GO TO 9
   10    IF(CHDELM.GE.1)GO TO 11
         CHDELP=CHDELP+1
         CHCEN =CHCEN +1
         CHDELM=CHDELM+1
         GO TO 10
   11    IF(CHDELP.GT.NCHAN)THEN
            CALL PAR_WRUSER(
     :          'Wavelength interval for line analysis exceeds total '
     :                                     //'spectrum length!',STATUS)
            GO TO 5
         END IF
C
C           Load X (wavelength) and data values of the selected segment
C           into arrays for analysis.
C           NSEG = number of channels in the segment.
C
         NSEG = CHDELP - CHDELM + 1
         IF(NSEG.GT.2048)THEN
            CALL PAR_WRUSER('Number of channels in segment exceeds '
     :                  //'limit of 2048',STATUS)
            GO TO 5
         END IF
         CALL CHUNK(%VAL(CNF_PVAL(IPTRX)),NCHAN,XVALS,CHDELM,NSEG,
     :              STATUS)
         IF(STATUS.NE.0)THEN
   12       CALL PAR_WRUSER(
     :             'Index out of range in extraction of segment from '
     :                            //'wavelength or data array',STATUS)
            FAULT=.TRUE.
            GO TO 500
         END IF
         CALL CHUNK(%VAL(CNF_PVAL(IPTRS)),NCHAN,YVALS,CHDELM,NSEG,
     :              STATUS)
         IF(STATUS.NE.0) THEN
            CALL PAR_WRUSER(
     :             'Failure extracting segment from '
     :                            //'wavelength or data array',STATUS)
            FAULT=.TRUE.
            GO TO 500
         END IF

C
C        Initialise plotting on soft device, if not already open
C
         IF (.NOT.PLTOPN) THEN
            PGSTAT=PGBEGIN(0,SOFTDEV(:ICH_LEN(SOFTDEV))//'/APPEND',1,1)
            IF(PGSTAT.NE.1)THEN
              CALL PAR_WRUSER(
     :           'Cannot open graphics device for soft plot',STATUS)
              FAULT=.TRUE.
              GO TO 500
            END IF
            PLTOPN=.TRUE.
         END IF
         CALL PGASK(.FALSE.)
         CALL PGSCI(WHITE)
C
C        Find minimum and maximum Y (data) values to be plotted in
C        this segment.
C
         YMIN=YVALS(1)
         YMAX=YVALS(1)
         DO I = 2,NSEG
            IF(YVALS(I).LT.YMIN)YMIN=YVALS(I)
            IF(YVALS(I).GT.YMAX)YMAX=YVALS(I)
         END DO
C
C        Actual YMIN and YMAX set to give 20% blank space at top and
C        bottom of plot.   Complete plot initialisation.
C
         DELTY=YMAX-YMIN
         YMAX=YMAX+0.2*DELTY
         YMIN=YMIN-0.2*DELTY
         CALL PGENV(XVALS(1),XVALS(NSEG),YMIN,YMAX,0,0)
C
C        Plot the spectrum in histogram style
C
         CALL PGBIN(NSEG,XVALS,YVALS,.TRUE.)
         CALL PGUPDT(2)
C
C        and plot the corresponding continuum section as a line plot
C
         CALL CHUNK(%VAL(CNF_PVAL(IPTRC)),NCHAN,CVALS,CHDELM,NSEG,
     :              STATUS)
         IF(STATUS.NE.0)THEN
            CALL PAR_WRUSER(
     :          'Index out of range in extraction of segment from '
     :            //'continuum file',STATUS)
            FAULT=.TRUE.
           GO TO 500
         END IF
         CALL PGSCI(RED)
         CALL PGLINE(NSEG,XVALS,CVALS)
         CALL PGUPDT(2)
         CALL PGSCI(WHITE)
         SELECT=.FALSE.
         PLTTED=.TRUE.
C
C     End of data plotting section. (Commands 'XXXX' or 'WIDTH')
C
      END IF
C
C     See if continuum to be fitted.  ('CONT' command)
C
      IF (COMMAND.EQ.'CONT') THEN
C
C       If continuum is to be fitted here, use cursor to read in start
C       and end X values for subsegments.  Starting values in XCST and
C       end values in XCEND.  Number of active continuum subsegments
C       =NCONTSS
C
         IF (.NOT.PLTOPN) THEN
            CALL PAR_WRUSER('No wavelength range selected yet.',STATUS)
            GO TO 5
         END IF
      END IF
   21 CONTINUE
      IF (COMMAND.EQ.'CONT') THEN
         NCONTSS=1
         DO I=1,10
            XCST(I)=0.
            XCEND(I)=0.
         END DO
         XCUR=XVALS(1)
         YCUR=0.5*(YMAX+YMIN)
         CALL PAR_WRUSER(
     :       'Selection of subsegments for continuum fitting:',STATUS)
         CALL PAR_WRUSER(
     :       'Up to 10 such subsegments can be specified',STATUS)
         CALL PAR_WRUSER(
     :       'Press any key except Q to read cursor.',STATUS)
         CALL PAR_WRUSER(
     :       'Press Q to terminate subsegment selection',STATUS)
   14    STRING='Use cursor to indicate left hand edge of subsegment'
         INVOKE=ICH_ENCODE(STRING,FLOAT(NCONTSS),53,0,NEXT)
         CALL PAR_WRUSER(STRING(:NEXT),STATUS)
         CALL PGCURSE(XCUR,YCUR,CURC)
         IF (CURC.EQ.'Q'.OR.CURC.EQ.'q') THEN
            NCONTSS=NCONTSS-1
            IF (NCONTSS.LT.1) THEN
               CALL PAR_WRUSER('You have not selected any subsegments',
     :                                                          STATUS)
               GO TO 5
            ELSE
               GO TO 17
            END IF
         END IF
         XCST(NCONTSS)=XCUR
         STRING='And now the right hand edge of subsegment'
         INVOKE=ICH_ENCODE(STRING,FLOAT(NCONTSS),43,0,NEXT)
         CALL PAR_WRUSER(STRING(:NEXT),STATUS)
         CALL PGCURSE(XCUR,YCUR,CURC)
         XCEND(NCONTSS)=XCUR
         IF(CURC.EQ.'Q'.OR.CURC.EQ.'q') GO TO 17
         NCONTSS=NCONTSS+1
         IF(NCONTSS.GT.10)THEN
            NCONTSS=10
         ELSE
            GO TO 14
         END IF
   17    CONTINUE
C
C          'CONT' option continues in the next section..
C
      END IF
      IF ((COMMAND.EQ.'CONT').OR.(COMMAND.EQ.'RECONT')) THEN
C
C          This section continues the 'CONT' operation, but is also the
C          entry point for the 'RECONT' operation.
C
         IF ((COMMAND.EQ.'RECONT').AND.(.NOT.SELECT)) THEN
            CALL PAR_WRUSER('No continuum subsegments selected yet',
     :                                                       STATUS)
            CALL PAR_WRUSER('To select segments, use the CONT command',
     :                                                         STATUS)
            GO TO 5
         END IF
C
C       X values in XCST and XCEND do not correspond to exact channels
C       in spectrum.  Derive arrays NCST and NCEND containing exact
C       channel numbers corresponding (as nearly as possible) to XCST
C       and XCEND values.  Channel numbers as appropriate to XVALS and
C       YVALS and NOT original spectrum
C
         DO I =1,10
            NCST(I)=0
            NCEND(I)=0
         END DO
         DO I=1,NCONTSS
            NCST(I)=(NSEG*XCST(I)-XCST(I)-NSEG*XVALS(1)+XVALS(NSEG))/
     :                                         (XVALS(NSEG)-XVALS(1))
            NCEND(I)=(NSEG*XCEND(I)-XCEND(I)-NSEG*XVALS(1)+XVALS(NSEG))/
     :                                          (XVALS(NSEG)-XVALS(1))
            IF(NCST(I).LT.1)NCST(I)=1
            IF(NCEND(I).GT.NSEG)NCEND(I)=NSEG
            IF(NCST(I).GE.NCEND(I))THEN
               CALL PAR_WRUSER(
     :        'Starting value of subsegment >= end value! Start again!',
     :                                                           STATUS)
               GO TO 21
            END IF
         END DO
C
C       Pack X and Y values for selected continuum subsegments into
C       arrays XFIT and YFIT.  Subsegments butted up to each other,
C       ie. no gaps.  Total number of channels included = NFIT
C       (This mess necessary because NAG routine cannot be instructed
C       to ignore any of its input data points.)
C
         NFIT=0
         I=1                  ! Loop over number of subsegments
         J=NCST(1)            ! Preset for loop through channels of each
   18    NFIT=NFIT+1          ! Top of       "     "       "
         XFIT(NFIT)=XVALS(J)
         YFIT(NFIT)=YVALS(J)
         J=J+1
         IF(J.LE.NCEND(I))GO TO 18
         I=I+1
         IF(I.GT.NCONTSS)GO TO 19
         JNEW=NCST(I)
         IF(JNEW.LT.J)THEN
            STRING=
     :        'Subsegment overlap: truncating lower end of subsegment'
            INVOKE=ICH_ENCODE(STRING,FLOAT(I),56,0,NEXT)
            CALL PAR_WRUSER(STRING(:NEXT),STATUS)
            IF(NCEND(I).LE.J)THEN
               CALL PAR_WRUSER(
     :               'Totally overlapping continuum subsegments! Start '
     :               //'again!',STATUS)
               GO TO 21
            END IF
            GO TO 18
         END IF
         J=JNEW
         GO TO 18
C
C       Final check that X values in XFIT are strictly increasing -
C       (To catch supposedly impossible case before it gets to NAG fit)
C
   19    XOLD=XFIT(1)
         DO J=2,NFIT
            IF(XFIT(J).LE.XOLD)THEN
               CALL PAR_WRUSER(
     :          'X values reversed in subsegment!  Start again!',STATUS)
               GO TO 21
            END IF
            XOLD=XFIT(J)
         END DO
C
C       Initialise for call to NAG routine for polynomial fitting.
C       WFIC(J=1,NFIT) = 1 for all J is what NAG routine calls weight.
C       Can't be zero so is fictitious "weight", all = 1.
C
   23    DO J=1,2048
            WFIC(J)=1.D+00
         END DO
         NREJ=0
         NITN=0
   25    CALL PAR_WRUSER(' ',STATUS)
         CALL PAR_WRUSER(
     :          'RMS residuals of polynomial fits to continuum',STATUS)
         STRING='Successive lines are iterations with'
         INVOKE=ICH_ENCODE(STRING,Q,38,7,NEXT)
         STRING(NEXT:)=' sigma rejection of points.'
         CALL PAR_WRUSER(STRING(:NEXT+26),STATUS)
         CALL PAR_WRUSER(
     :       'Columns are degree of polynomial, 0 - 7, then'
     :       //' number of points in fit,',STATUS)
         CALL PAR_WRUSER('and number rejected',STATUS)
         CALL PAR_WRUSER(' ',STATUS)
         CALL PAR_WRUSER(
     :      '     0      1      2      3      4      5      6      7'
     :      //'    NFIT   NREJ ',STATUS)
C
C       Call NAG routine to do polynomial fit to (remaining) continuum
C       data points
C
   26    CONTINUE
         IFAIL2=0
         EPS=0D0
         CALL PDA_DPOLFT(NFIT,XFIT,YFIT,WFIC,NPOL,NDEG,EPS,RFIT,
     :      IFAIL,ANAG2,IFAIL2)
         IF (NDEG.NE.NPOL.OR.IFAIL.NE.1.OR.IFAIL2.NE.0) THEN
            FAULT=.TRUE.
            GO TO 500
         END IF
         DO I=1,8
            SNAG(I)=0D0
         END DO
         SNAG(NPOL+1)=EPS
         WRITE (STRING,27,IOSTAT=IGNORE)SNAG,NFIT,NREJ
   27    FORMAT(8F7.2,2I6)
         CALL PAR_WRUSER(STRING(:70),STATUS)
         NITN=NITN+1
         IF(NITN.GT.NQ)GO TO 28
C
C        Go through YFIT point by point calculating residuals wrt continuum
C        fit; delete data point if residual exceeds specified tolerance.
C
         QLIM=DBLE(Q)*SNAG(NPOL+1)
         J=1
   47    CONTINUE
         IFAIL2=0
         CALL PDA_DP1VLU(NPOL,0,XFIT(J),PNAG,DUMMY,ANAG2,IFAIL2)
         IF(IFAIL2.NE.0)THEN
            FAULT=.TRUE.
            GO TO 500
         END IF
C
C        Pack down arrays XFIT, YFIT to eliminate rejected points.
C        Again necessary because NAG routine can't ignore any input data
C        points.  Then jump back for another iteration.
C
         IF(ABS(YFIT(J)-PNAG).GT.QLIM)THEN
            DO I=J+1,NFIT
               XFIT(I-1)=XFIT(I)
               YFIT(I-1)=YFIT(I)
            END DO
            NFIT=NFIT-1
            NREJ=NREJ+1
            J=J-1
         END IF
         J=J+1
         IF(J.LE.NFIT)GO TO 47
         GO TO 26
C
C       Iterative continuum fit completed
C
   28   CONTINUE
C
C       Find channel numbers JST and JEND (on scale of XVALS and YVALS)
C       of first and last channels actually used in final continuum fit.
C       Allows for rejection of data points at ends of range.
C
        DO I=1,NSEG
           IF(XVALS(I).EQ.XFIT(1))THEN
              JST=I
              GO TO 30
           END IF
        END DO
        CALL PAR_WRUSER('Failed to find value of JST',STATUS)
        FAULT=.TRUE.
        GO TO 500
   30   DO I=1,NSEG
           IF(XVALS(I).EQ.XFIT(NFIT))THEN
              JEND=I
              GO TO 46
           END IF
        END DO
        CALL PAR_WRUSER('Failed to find value of JEND',STATUS)
        FAULT=.TRUE.
        GO TO 500
C
C       Evaluate XY arrays of continuum fit,between channels JST and JEND.
C       Put into array CVALS (channels corresponding to XVALS, YVALS)
C
   46   DO I=JST,JEND
           IFAIL2=0
           CALL PDA_DP1VLU(NPOL,0,DBLE(XVALS(I)),PNAG,DUMMY,ANAG2,
     :        IFAIL2)
           CVALS(I)=SNGL(PNAG)
           IF(IFAIL2.NE.0)THEN
              GO TO 500
           END IF
        END DO
C
C       And now soft plot it
C
        CALL PGSCI(RED)
        CALL PGLINE(NSEG,XVALS,CVALS)
        CALL PGUPDT(2)
        CALL PGSCI(WHITE)
        SELECT=.TRUE.
C
C       Write this section of continuum fit to the continuum array.
C
        CALL CHUNKWRITE(%VAL(CNF_PVAL(IPTRC)),NCHAN,CHDELM+JST-1,
     :      CVALS,NSEG,JST,JEND-JST+1,STATUS)
C
C          Remember which channels we've just fitted.
C
        JCST=JST
        JCEND=JEND
C
C          End of fit continuum section ('CONT' command)
C
      END IF
C
C     Analyse a line.  This is the 'FIT' option. Get cursor
C     indication of range defining absorption line itself, and
C     calculate wavelength and equivalent width.
C     XL,XH = X values of low and high side of the range.
C
      IF (COMMAND.EQ.'FIT') THEN
C
C           Check for a plot
C
         IF (.NOT.PLTOPN) THEN
            CALL PAR_WRUSER('No wavelength range selected yet.',STATUS)
            GO TO 5
         END IF
C
C        Label the plot with name of the line
C
         CALL PAR_CNPAR('LINENAME')
         CALL PAR_RDCHAR('LINENAME',' ',LINNAM)
         IF (PAR_ABORT()) GO TO 500
         LENAM=ICH_LEN(LINNAM)
         IF(LENAM.NE.0)CALL PGLABEL(' ',' ',LINNAM(1:LENAM))
   32    CALL PAR_WRUSER(
     :      'Definition of wavelength range of absorption line itself',
     :                                                          STATUS)
         CALL PAR_WRUSER(
     :      'Use cursor to indicate left hand edge of line range',
     :                                                          STATUS)
         CALL PAR_WRUSER(
     :      'Press any key to read cursor position',STATUS)
         XCUR=XVALS(INT(NSEG/2.))
         YCUR=0.5*(YMAX+YMIN)
         CALL PGCURSE(XCUR,YCUR,CURC)
         XL=XCUR
         CALL PAR_WRUSER('And now the right hand edge',STATUS)
         CALL PGCURSE(XCUR,YCUR,CURC)
         XH=XCUR
C
C        Convert line range limits to nearest integer NL and NH
C        (channel no. scale of XVALS, YVALS) and test for obvious faults
C
         NL=NINT((NSEG*XL-XL-NSEG*XVALS(1)+XVALS(NSEG))/(XVALS(NSEG)-
     :                                                     XVALS(1)))
         NH=NINT((NSEG*XH-XH-NSEG*XVALS(1)+XVALS(NSEG))/(XVALS(NSEG)-
     :                                                     XVALS(1)))
         IF(NH.LE.NL.OR.NL.LT.1.OR.NH.GT.NSEG)THEN
            CALL PAR_WRUSER(
     :         'Bad wavelength range specification.  Try again',STATUS)
            GO TO 32
         END IF
C
C        If LIMIT = .FALSE. revise NL and/or NH as required so that they
C        give channel nearest to line centre for which data exceeds
C        continuum, unless this channel is outside original NL - NH
C        range, in which case leave limit at NL (NH) specified by cursor.
C
         IF(LIMIT)GO TO 36
         NTEM=(NL+NH)/2.
         JTEM=NL+1
         DO I =NTEM,JTEM,-1
           IF(YVALS(I).GT.CVALS(I))THEN
              NL=I
              GO TO 37
           END IF
         END DO
   37    JTEM=NH-1
         DO I=NTEM,JTEM
            IF(YVALS(I).GT.CVALS(I))THEN
               NH=I
               GO TO 36
            END IF
         END DO
C
C        Compute median XMED of 1 - YVALS/CVALS between NL and NH.
C        The detailed calculation is done by SPLITSUM
C
   36    SUM=0.
         SIGN=1.0
         ACH=(XVALS(NSEG)-XVALS(1))/(NSEG-1.)
         WARNED=.FALSE.
         DO I=NL,NH
            IF (CVALS(I).EQ.0.) THEN
               IF (.NOT.WARNED) THEN
                  CALL PAR_WRUSER(
     :              'Warning! One or more continuum values are zero',
     :                                                       STATUS)
                END IF
                WARNED=.TRUE.
            ELSE
               SUM=SUM+1.-YVALS(I)/CVALS(I)
            END IF
         END DO
         IF(SUM.LT.0.)THEN
            SUM=-SUM        ! Set up for emission line case
            SIGN=-1.0
            CALL PAR_WRUSER(
     :          'Negative area below continuum. Treat as an emission '
     :                                                //'line',STATUS)
         END IF
         XMED=SPLITSUM(SUM,0.5,YVALS,CVALS,XVALS,NSEG,NL,NH,SIGN,STATUS)
   54    CONTINUE
         IF(STATUS.NE.0)THEN
           CALL PAR_WRUSER(
     :        'Median (or median +/- sigma) calculation failed!',STATUS)
           CALL PAR_WRUSER(
     :        'Use of unscrunched data is a possible cause,',STATUS)
           CALL PAR_WRUSER(
     :        'as is the failure to set a continuum in this region.',
     :                                                        STATUS)
           GO TO 5
         END IF
C
C        Now find X values for which area up to this value = 0.1587 and
C        0.8413 of the total.  For a Gaussian these give +/- 1 standard
C        deviation from the median.  Compute width and asymmetry parameters.
C
         XSIGL=SPLITSUM(SUM,.1587,YVALS,CVALS,XVALS,NSEG,NL,NH,
     :                                                   SIGN,STATUS)
         IF(STATUS.NE.0)GO TO 54
         XSIGU=SPLITSUM(SUM,.8413,YVALS,CVALS,XVALS,NSEG,NL,NH,
     :                                                   SIGN,STATUS)
         IF(STATUS.NE.0)GO TO 54
         WIDTH=1.1775*(XSIGU-XSIGL)
         ASYM=(XSIGU+XSIGL)*0.5 - XMED
         ASYM=ASYM*100./WIDTH
C
C        Calculate equivalent width WLAM in units of wavelength (XVALS
C        data) by scaling SUM by dispersion.  Display results.
C
         WLAM=SUM*ACH
         STRING='Wavelength = '
         INVOKE=ICH_ENCODE(STRING,XMED,14,7,NEXT)
         STRING(NEXT:)=', Equivalent width = '
         INVOKE=ICH_ENCODE(STRING,WLAM,NEXT+21,7,NEXT)
         CALL PAR_WRUSER(STRING(:NEXT),STATUS)
         STRING='Line width parameter = '
         INVOKE=ICH_ENCODE(STRING,WIDTH,24,7,NEXT)
         CALL PAR_WRUSER(STRING(:NEXT),STATUS)
         STRING='Asymmetry parameter  = '
         INVOKE=ICH_ENCODE(STRING,ASYM,24,7,NEXT)
         CALL PAR_WRUSER(STRING(:NEXT),STATUS)
C
C        Finish up soft plot by drawing vertical lines showing line range
C        limits, then sign off soft plot. Adjust X values of vertical
C        lines so they are at edges of bins, not centre, to better show
C        full range/area included.
C
         XRANGL(1)=XVALS(NL)-0.5*ACH
         XRANGL(2)=XRANGL(1)
         YRANGE(1)=YMIN
         YRANGE(2)=YMAX
         CALL PGLINE(2,XRANGL,YRANGE)
         XRANGH(1)=XVALS(NH)+0.5*ACH
         XRANGH(2)=XRANGH(1)
         CALL PGLINE(2,XRANGH,YRANGE)
C
C        See if a hardcopy plot is required.  If so, close soft plot.
C
         CALL PAR_CNPAR('HARDCOPY')
         CALL PAR_RDKEY('HARDCOPY',.FALSE.,QREPLY)
         IF (PAR_ABORT()) GO TO 500
         IF (QREPLY) THEN
            CALL PGEND
            PLTOPN=.FALSE.
C
C           Hard plot; data, continuum fit, lines to indicate range fitted,
C           and Y = 0 axis
C
            CALL PAR_WRUSER('Beginning hard copy plot',STATUS)
            PGSTAT=PGBEGIN(0,HARDFIL,1,2)
            CALL PGSETC(1.5)
            CALL PGENV(XVALS(1),XVALS(NSEG),YMIN,YMAX,0,0)
            IF(LENAM.NE.0)CALL PGLABEL(' ',' ',LINNAM(1:LENAM))
            CALL PGBIN(NSEG,XVALS,YVALS,.TRUE.)
            CALL PGLINE(NSEG,XVALS,CVALS)
            YRANGE(2)=YMIN+0.3*(YMAX-YMIN)
            CALL PGLINE(2,XRANGL,YRANGE)
            CALL PGLINE(2,XRANGH,YRANGE)
            XRANGL(1)=XVALS(1)
            XRANGL(2)=XVALS(NSEG)
            YRANGE(1)=0.
            YRANGE(2)=0.
            CALL PGLINE(2,XRANGL,YRANGE)
C
C           Output printed quantities describing the fit. Have to do as
C           graphics, in order to get on the same page.
C
            CALL PAR_CNPAR('COMMENT')
            CALL PAR_RDCHAR('COMMENT',' ',COMENT)
            IF (PAR_ABORT()) GO TO 500
            CALL PGSETC(1.5)
            CALL PGENV(1.,100.,1.,17.,0,-2)
            CALL GEN_TIME(6,DAY,LDAY,DAT,LDATE,TIM,LHOUR)
            CALL PGTEXT(1.,16.,DAY(:LDAY)//' '//DAT(:LDATE)//' at '
     :                                               //TIM(:LHOUR))
            CALL PGTEXT(1.,15.,'Input spectrum name: '//SPEC(:LESPEC))
C
            CALL DSA_OBJECT_NAME('SPEC',OBJ,STATUS)
            LEOBJ=ICH_LEN(OBJ)
            CALL PGTEXT(1.,14.,'Object name: '//OBJ(1:LEOBJ))
C
            IF(CONTOLD)THEN
              CALL PGTEXT(1.,13.,
     :         'Precomputed continuum used, from file '//CONT(:LECONT))
            ELSE IF (CONTWR) THEN
              CALL PGTEXT(1.,13.,'Continuum will be written to file '
     :                                        //CONTO(:ICH_LEN(CONTO)))
            END IF
            IF (((NH+NL)/2.GE.JCST).AND.((NH+NL)/2.LE.JCEND)) THEN
              WRITE(TEXT,42)NPOL
   42         FORMAT(I5)
              CALL PGTEXT(1.,12.,
     :          'Degree of polynomial fitted to continuum = '//TEXT(:5))
              WRITE(TEXT,42)NQ
              CALL PGTEXT(1.,11.,
     :                  'Number of iterations for continuum point'
     :                                      //' rejection = '//TEXT(:5))
              WRITE(TEXT,43)Q
   43         FORMAT(F9.3)
              CALL PGTEXT(1.,10.,
     :                  'Multiple of sigma for discrepant point'
     :                     //' rejection = '//TEXT(:9))
              WRITE(TEXT,43)SNAG(NPOL+1)
              CALL PGTEXT(1.,9.,
     :                       'Final rms of remaining continuum about '
     :                                     //'fit = '//TEXT(:9))
              WRITE(TEXT,44)NFIT,NREJ,NCONTSS
   44         FORMAT('Final continuum fit to',I4,' points;',I4,
     :        ' points rejected.',I4,' subsegments used')
              CALL PGTEXT(1.,8.,TEXT(1:76))
            END IF
            I=NH-NL+1
            WRITE(TEXT,42)I
            CALL PGTEXT(1.,6.,'Number of channels used in line itself ='
     :                                                     //TEXT(1:5))
            IF(LIMIT)THEN
              CALL PGTEXT(1.,5.,
     :                    'Line evaluation cutoff at cursor positions')
            ELSE
              CALL PGTEXT(1.,5.,
     :                   'Line evaluation cutoff at first channel'
     :                                        //' exceeding continuum')
            END IF
            WRITE(TEXT,56)XMED,WIDTH
   56       FORMAT('Median wavelength = ',
     :                          G13.6,14X,'Line width = ',G13.6)
            CALL PGTEXT(1.,3.,TEXT(1:73))
            WRITE(TEXT,45)WLAM,ASYM
   45       FORMAT('Equivalent width = ',G13.6,15X,'Asymmetry = ',G13.6)
            CALL PGTEXT(1.,2.,TEXT(1:74))
            IF (COMENT.NE.' ') THEN
               CALL PGTEXT(1.,1.,COMENT(:ICH_LEN(COMENT)))
            END IF
C
C           Close hardcopy graphics, re-open soft device
C
            CALL PGEND
            CALL PAR_WRUSER('Hardcopy plot output to '
     :                                 //HARDFIL(:LEHARDF),STATUS)
            PGSTAT=PGBEGIN(0,SOFTDEV(:ICH_LEN(SOFTDEV))//'/APPEND',1,1)
            IF(PGSTAT.NE.1)THEN
               CALL PAR_WRUSER(
     :           'Cannot re-open graphics device for soft plot',STATUS)
               FAULT=.TRUE.
               GO TO 500
            END IF
            CALL PGASK(.FALSE.)
            CALL PGWINDOW(XVALS(1),XVALS(NSEG),YMIN,YMAX)
            CALL PGVSTAND
            PLTOPN=.TRUE.
         END IF
C
C        Finished!  End of line analysis.  (Command 'FIT')
C
      END IF
C
C     The QUIT option will take us outof ABLINE, anything else
C     and we want to be back at the command node.
C
      IF (COMMAND.NE.'QUIT') GO TO 5
C
C     This is the way out.  See if we have to write out the
C       continuum to a file.  If we do, find out the file name.  If
C       it is the same as the original continuum file, just rewrite
C       the data in it.  Otherwise, create a new file - by making
C       a copy of the original spectrum file - and write the
C       continuum data out to it.
C
      IF (CONTWR) THEN
         CALL DSA_MAP_DATA('OUTPUT','UPDATE','FLOAT',TPTR,SLOT,STATUS)
         IF (STATUS.NE.0) GO TO 500
         CALL GEN_MOVE(DSA_TYPESIZE('FLOAT',STATUS)*NCHAN,
     :                 %VAL(CNF_PVAL(IPTRC)),%VAL(CNF_PVAL(TPTR)))
      END IF
C
C     Tidy up
C
  500 CONTINUE
C
C       If a plot was made at all, set the user variables describing
C       it, so routines such as XCUR and CCUR will work on it.
C
      IF (PLTTED) THEN
         CALL VAR_SETNUM('TVXST',0,0,XVALS(1),STATUS)
         CALL VAR_SETNUM('TVXEN',0,0,XVALS(NSEG),STATUS)
         CALL VAR_SETNUM('TVHIGH',0,0,YMAX,STATUS)
         CALL VAR_SETNUM('TVLOW',0,0,YMIN,STATUS)
         CALL VAR_SETNUM('XSTART',0,0,XVALS(1),STATUS)
         CALL VAR_SETNUM('XEND',0,0,XVALS(NSEG),STATUS)
         CALL VAR_SETNUM('HIGH',0,0,YMAX,STATUS)
         CALL VAR_SETNUM('LOW',0,0,YMIN,STATUS)
         CALL VAR_SETCHR('TVFILE',0,0,SPEC,STATUS)
         CALL VAR_SETNUM('TVCOLOR',0,0,FLOAT(WHITE),STATUS)
      END IF
C
C       Release workspace and close down any open or mapped files
C
      CALL DSA_CLOSE(STATUS)
C
      END
