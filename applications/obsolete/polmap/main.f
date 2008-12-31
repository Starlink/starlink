C
C         #####    ####   #       #    #    ##    #####
C         #    #  #    #  #       ##  ##   #  #   #    #
C         #    #  #    #  #       # ## #  #    #  #    #
C         #####   #    #  #       #    #  ######  #####
C         #       #    #  #       #    #  #    #  #
C         #        ####   ######  #    #  #    #  #
C
C+
C                            P O L M A P 
C 
C                      by T. J. Harries 1994
C                        tjh@st-and.uk.ac 
C
C POLMAP is an interactive linear spectropolarimetry analysis package.  
C 
C
C- 
      IMPLICIT NONE 
      INCLUDE 'PSX_ERR'
C 
C First, define the variables.... 
C 
C The array sizes are parameters... 
C 
      INCLUDE 'array_size.inc' 
C
C Declare various EXTERNALs so as not to confuse compilers when
C INTRINSICs have the same name.
C
      EXTERNAL MERGE
C 
C Logical unit numbers. The comfile unit number is the starting value.
C If nested comfiles are used then the the unit number is 
C incremented. A maximum of 10 can be open at once hence the comfile 
C logical units cover comfile_lu_st -> comfile_lu+9
C
      INTEGER COMFILE_LU_ST,IO_LU,OUT_LU,IN_LU
      PARAMETER(COMFILE_LU_ST=20,IO_LU=31)
C
C The logical units for input and output
C
      PARAMETER(OUT_LU=6,IN_LU=5)
C
C
      INTEGER I
C
C
C The Stokes parameters stack variables...
C 
      REAL STK_LAMBDA(MAXPTS,MAXSPEC)
      REAL STK_STOKES_I(MAXPTS,MAXSPEC)
      REAL STK_STOKES_Q(MAXPTS,MAXSPEC)
      REAL STK_STOKES_QV(MAXPTS,MAXSPEC)
      REAL STK_STOKES_U(MAXPTS,MAXSPEC)
      REAL STK_STOKES_UV(MAXPTS,MAXSPEC)
      INTEGER STK_NPTS(MAXSPEC)
      INTEGER TOP_STK
C
C The current array stokes parameters...
C
      REAL LAMBDA(MAXPTS)
      REAL STOKES_I(MAXPTS)
      REAL STOKES_Q(MAXPTS)
      REAL STOKES_QV(MAXPTS)
      REAL STOKES_U(MAXPTS)
      REAL STOKES_UV(MAXPTS)
      INTEGER NPTS
C
C The command parser parameter outputs
C
      INTEGER NPARAMS
      INTEGER MAXPARAMS
      PARAMETER(MAXPARAMS = 100)
      REAL PARAMS(MAXPARAMS)
C
C
C
C
C Misc. integers
C
      INTEGER SP,CM
C
C Character strings...
C
      CHARACTER*80 FILENAME
      CHARACTER*80 INP,CPARAM
      INTEGER NHIST,MAXHIST,NCOMM
      PARAMETER(MAXHIST=20)
      CHARACTER*80 COMM_HIST(MAXHIST),DROPPED
      CHARACTER*80 STK_TITLE(MAXSPEC),TITLE
      CHARACTER*10 CMD,CNUM
      CHARACTER*80 XLAB,ILAB,TLAB,PLAB,PFLAB
C
C The plotting ranges...
C
      DOUBLE PRECISION ACOEFF(20) 
      INTEGER NCOEFF
C
      REAL TMAX,TMIN
      REAL PMAX,PMIN
      REAL IMAX,IMIN
      REAL WMIN,WMAX
      REAL QMAX,QMIN
      REAL UMAX,UMIN
      REAL PFMAX,PFMIN
C
C The continuum zones...
C
      REAL CONT_ST(10)
      REAL CONT_EN(10)
      INTEGER NZONES
C
C Statistical means from ptheta command
C
      REAL STAT_Q_MEAN
      REAL STAT_U_MEAN
C
C The command file logical unit counter...
C
      INTEGER COMFILE_LU
C
C The comma flags and strings
C
      LOGICAL COMMA
C
      CHARACTER*80 TEMPSTR
C
C The plotting flags
C
      LOGICAL QUJOIN_DOTS
      LOGICAL QUARROW
      REAL ARROWSIZE
      LOGICAL PFLUX
      LOGICAL QU_TRIPLOT
      LOGICAL BOX
      LOGICAL LROT
      LOGICAL CROT
      LOGICAL WAUTOLIM
      LOGICAL IAUTOLIM
      LOGICAL PAUTOLIM
      LOGICAL TAUTOLIM
      LOGICAL QAUTOLIM
      LOGICAL UAUTOLIM
      LOGICAL PFAUTOLIM
      LOGICAL T_HI
      LOGICAL T_LOW
      LOGICAL POLY,HIST,MARK
C
C Status flags...
C
      LOGICAL OK
C
      LOGICAL COMFILE
C
C
      INTEGER LCOL,LSTYLE,PSTYLE
C
C
      INTEGER STATUS
C
      CHARACTER*(80) TRANS
      LOGICAL FSTR
C
C Initilize the variables to their default startup values...
C
      TOP_STK = 0
      NHIST=0
      OK = .TRUE.
      BOX = .TRUE.
      WAUTOLIM = .TRUE.
      PFLUX = .FALSE.
      IAUTOLIM = .TRUE.
      PAUTOLIM = .TRUE.
      QAUTOLIM = .TRUE.
      UAUTOLIM = .TRUE.
      PFAUTOLIM = .TRUE.
      POLY=.FALSE.
      HIST=.TRUE.
      MARK=.FALSE.
      LROT = .FALSE.
      CROT=.FALSE.
      LSTYLE = 1
      LCOL = 1
      PSTYLE = 1
      QUJOIN_DOTS = .FALSE.
      QUARROW = .FALSE.
      ARROWSIZE=1.
      COMMA = .FALSE.
      COMFILE = .FALSE.
      PFLUX=.FALSE.
      QU_TRIPLOT=.FALSE.
      TAUTOLIM = .TRUE.
      T_HI=.FALSE.
      T_LOW=.FALSE.
      TMAX = 180.
      TMIN = 0.
      PMIN = 0.
      PMAX = 100.
      XLAB='Wavelength (\\A)'
      ILAB='Stokes I'
      PFLAB='Polarized Flux'
      TLAB='\\gh (degrees)'
      PLAB='Polarization (%)'
      CMD=' '
      TEMPSTR=' '
      COMFILE_LU = 0
      STAT_Q_MEAN=0.
      STAT_U_MEAN=0.
C
      CALL PSX_GETENV('POLMAP_DIR',TRANS,STATUS)
      IF (STATUS.EQ.PSX__NOENV) THEN
      CALL WR_ERROR('Cannot translate environment variable POLMAP_DIR',
     &               OUT_LU)
      ENDIF
C
C Write up the message of the day...
C
      CALL MOTD(IO_LU,TRANS,OUT_LU)
C
C Initialization file
C
      OPEN(UNIT=COMFILE_LU_ST,ERR=555,FILE='~/.polmap',FORM='FORMATTED',
     &STATUS='OLD')
      WRITE(OUT_LU,*) 'Running initialization file...'
      COMFILE_LU=COMFILE_LU_ST
      COMFILE=.TRUE.
 555  CONTINUE


C
C The main loop. The command is input, parsed and the appropriate 
C routine is called. The loop finishes when the quit command is called.
C
      DO WHILE(CMD.NE.'quit')
C
C Don't input a new command if the last command had a comma in it.
C
       IF (.NOT.COMMA) THEN
C
C If a comfile is open then read a string from that, otherwise read
C from the terminal.
C
        IF (COMFILE) THEN
         READ(COMFILE_LU,'(a)',ERR = 67,END = 68) INP
        ELSE
 111       CONTINUE
         WRITE(CNUM,'(I5)') NHIST+1
         CALL SSTRIP(CNUM)
         I=INDEX(CNUM,' ')
         WRITE(OUT_LU,'(1X,A1,A,A3,$)') '(',CNUM(1:(I-1)),')> '
         READ(IN_LU,'(A80)',ERR=111) INP
        ENDIF
       ENDIF
C
C If the last command had a comma, take the next bit of the string and
C cut it to the next comma, if there is one.
C
       IF (COMMA) THEN
        INP = TEMPSTR
       ENDIF
       CM = INDEX(INP,',')
       IF (CM.EQ.0) THEN
          COMMA = .FALSE.
         ELSE
          TEMPSTR = INP((CM+1):)
          INP = INP(:(CM-1))
          COMMA = .TRUE.
       ENDIF
C
C INP holds the command. Add this to the command history.
C
       CALL SSTRIP(INP)
       IF (INP.EQ.' ') GOTO 999
C
C
       NHIST=NHIST+1
       IF (NHIST.LE.MAXHIST) THEN
        COMM_HIST(NHIST)=INP
       ELSE
        DROPPED=COMM_HIST(1)
        DO I=1,MAXHIST-1
         COMM_HIST(I)=COMM_HIST(I+1)
       ENDDO
       COMM_HIST(MAXHIST)=INP
       ENDIF
C
C Now parse the input string into a command and parameters
C
C
       IF (INP(1:1).EQ.'!') THEN
         INP=INP(2:)
         READ(INP,*,ERR=998) NCOMM
         IF (NHIST.LE.MAXHIST) THEN
            I=NCOMM
           ELSE
            I=MAXHIST-(NHIST-NCOMM)
         ENDIF
         IF ((I.GT.0).and.(I.LT.MAXHIST)) THEN
           INP=COMM_HIST(I)
           IF (NHIST.LE.MAXHIST) THEN
            COMM_HIST(NHIST)=INP
           ELSE
            COMM_HIST(MAXHIST)=INP
           ENDIF
           CALL PARSE (INP,CMD,PARAMS,NPARAMS,CPARAM,FSTR,OK,OUT_LU)
           ELSE
          CALL WR_ERROR('Event not found',OUT_LU)
          IF (NHIST.LE.MAXHIST) THEN
           NHIST=NHIST-1
          ELSE
           DO I=MAXHIST,2,-1
            COMM_HIST(I)=COMM_HIST(I-1)
           ENDDO
           COMM_HIST(1)=DROPPED
           NHIST=NHIST-1
          ENDIF
          GOTO 999
         ENDIF
        ELSE
         CALL PARSE (INP,CMD,PARAMS,NPARAMS,CPARAM,FSTR,OK,OUT_LU)
        ENDIF
C
C If the command is comfile and we're not reading from a comfile then
C open it.
C
       IF (CMD.EQ.'comfile') THEN
        IF (.NOT.COMMA) THEN
        IF (COMFILE_LU.LT.(COMFILE_LU_ST+9)) THEN
         CALL SSTRIP(CPARAM)
         SP = INDEX(CPARAM,' ')
         SP = SP-1
         FILENAME = CPARAM(:SP)
         IF (FILENAME((SP-3):SP).NE.'.cmd') THEN
           FILENAME = FILENAME(1:SP)//'.cmd'
         ENDIF
         IF (COMFILE_LU.EQ.0) THEN
           COMFILE_LU = COMFILE_LU_ST
          ELSE
           COMFILE_LU = COMFILE_LU+1
         ENDIF
         OPEN(UNIT = COMFILE_LU,FILE = FILENAME,STATUS = 'OLD',
     &                           FORM = 'FORMATTED',ERR = 66)
         OK = .FALSE.
         COMFILE = .TRUE.
        ELSE
         CALL WR_ERROR('Too many comfiles open',OUT_LU)
         OK = .FALSE.
         COMFILE = .FALSE.
         DO I = COMFILE_LU_ST+9,COMFILE_LU_ST,-1
          CLOSE(I)
         ENDDO
        ENDIF
        ELSE
         CALL WR_ERROR('Comfile must have own command line',OUT_LU)
        ENDIF
        ENDIF
C        
C Now go through all known commands and see if we find a match, call the
C appropriate command. Easy to add additional commands from here.
C
       IF (OK) THEN
         IF  (CMD.EQ.'retitle') THEN
         CALL  RETITLE(NPARAMS,CPARAM,PARAMS,STK_TITLE,
     &                 STK_NPTS,OUT_LU,IN_LU)
C
         ELSE IF (CMD.EQ.'isfit') THEN
         CALL ISFIT2(CPARAM,STOKES_I,STOKES_Q,STOKES_QV,STOKES_U,
     &                 STOKES_UV,TITLE,
     &                 LAMBDA,NPTS,OUT_LU)
C
         ELSE IF (CMD.EQ.'font') THEN
           CALL FONT(NPARAMS,PARAMS,OUT_LU)
C
         ELSE IF (CMD.EQ.'page') THEN
           CALL PGPAGE
C
         ELSE IF (CMD.EQ.'setline') THEN
           CALL SETLINE(NPARAMS,PARAMS,LSTYLE,OUT_LU)
C
         ELSE IF (CMD.EQ.'setcolour') THEN
           CALL SETCOLOUR(NPARAMS,PARAMS,LCOL,OUT_LU)
C
         ELSE IF (CMD.EQ.'setheight') THEN
           CALL POL_SCH(NPARAMS,PARAMS,OUT_LU)
C
         ELSE IF (CMD.EQ.'setsymb') THEN
           CALL SETSYMB(NPARAMS,PARAMS,PSTYLE,OUT_LU)
C
         ELSE IF (CMD.EQ.'swap') THEN
           CALL SWAP(NPARAMS,PARAMS,TITLE,LAMBDA,STOKES_I,STOKES_Q,
     &                STOKES_QV,STOKES_U,
     &             STOKES_UV,NPTS,
     &             STK_TITLE,STK_LAMBDA,STK_STOKES_I,STK_STOKES_Q,
     &             STK_STOKES_QV,STK_STOKES_U,STK_STOKES_UV,
     &             STK_NPTS,TOP_STK,OUT_LU)
C
         ELSE IF (CMD.EQ.'edit') THEN
         CALL EDIT(NPARAMS,PARAMS,STOKES_I,STOKES_Q,STOKES_QV,
     &                 STOKES_U,STOKES_UV,LAMBDA,NPTS,OUT_LU)
C
         ELSE IF (CMD.EQ.'iquadd') THEN
         CALL IQUADD(NPARAMS,PARAMS,STOKES_I,STOKES_Q,STOKES_QV,
     &                  STOKES_U,STOKES_UV,NPTS,OUT_LU)
C
         ELSE IF (CMD.EQ.'text') THEN
           CALL TEXT(NPARAMS,PARAMS,CPARAM,FSTR,OUT_LU)
C
         ELSE IF (CMD.EQ.'serkthru') THEN
          CALL SERKTHRU(NPARAMS,PARAMS,STOKES_I,STOKES_Q,STOKES_QV,
     &                STOKES_U,STOKES_UV,LAMBDA,TITLE,NPTS,OUT_LU)
C
         ELSE IF (CMD.EQ.'integ') THEN
          CALL  INTEG(NPARAMS,PARAMS,STOKES_I,LAMBDA,NPTS,OUT_LU)
C
         ELSE IF (CMD.EQ.'cremove') THEN
          CALL CREMOVE(NPARAMS,PARAMS,STOKES_I,STOKES_Q,STOKES_QV,
     &                 STOKES_U,STOKES_UV,LAMBDA,NPTS,OUT_LU)
C
         ELSE IF (CMD.EQ.'counts') THEN
          CALL MEAN_COUNTS(STOKES_I,NPTS,OUT_LU)
C
         ELSE IF (CMD.EQ.'maxpol') THEN
          CALL MAXPOL(NZONES,CONT_ST,CONT_EN,NPTS,STOKES_I,STOKES_Q,
     &                  STOKES_QV,STOKES_U,STOKES_UV,LAMBDA,OUT_LU)
C
         ELSE IF (CMD.EQ.'rvel') THEN
           CALL RV(NPARAMS,PARAMS,LAMBDA,NPTS,OUT_LU)
C
         ELSE IF (CMD.EQ.'qsm') THEN
         CALL  QSM(NPARAMS,PARAMS,STOKES_I,STOKES_Q,STOKES_QV,STOKES_U,
     &               STOKES_UV,LAMBDA,NPTS,OUT_LU)
C
         ELSE IF (CMD.EQ.'quplot') THEN
           CALL QUPLOT(NPARAMS,PARAMS,TITLE,QUJOIN_DOTS,
     &STOKES_I,STOKES_Q,STOKES_U,STOKES_QV,STOKES_UV,
     &LAMBDA,NPTS,BOX,QMAX,QMIN,UMAX,UMIN,QAUTOLIM,UAUTOLIM,
     &WMAX,WMIN,WAUTOLIM,LCOL,LSTYLE,CROT,PSTYLE,QUARROW,
     &ARROWSIZE,OUT_LU)
C
         ELSE IF (CMD.EQ.'quarrow') THEN
          QUARROW=.TRUE.
          QUJOIN_DOTS=.TRUE.
C
         ELSE IF (CMD.EQ.'noquarrow') THEN
          QUARROW=.FALSE.
C
         ELSE IF (CMD.EQ.'motd') THEN
          CALL MOTD(IO_LU,TRANS,OUT_LU)
C
         ELSE IF (CMD.EQ.'qujoin') THEN
          QUJOIN_DOTS = .TRUE.
C
         ELSE IF (CMD.EQ.'arrowstyle') THEN
          CALL ARROWSTYLE(PARAMS,NPARAMS,ARROWSIZE,OUT_LU)
C
         ELSE IF (CMD.EQ.'noqujoin') THEN
          QUJOIN_DOTS = .FALSE.
          QUARROW=.FALSE.
C
         ELSE IF (CMD.EQ.'quswap') THEN
          CALL QUSWAP(STOKES_I,STOKES_Q,STOKES_QV,
     &                 STOKES_U,STOKES_UV,LAMBDA,NPTS,OUT_LU)
C
         ELSE IF (CMD.EQ.'icadd') THEN
          CALL ICADD(NPARAMS,PARAMS,STOKES_I,NPTS,OUT_LU)
C
         ELSE IF (CMD.EQ.'icmult') THEN
            CALL ICMULT(NPARAMS,PARAMS,NPTS,STOKES_I,STOKES_Q,
     &                  STOKES_QV,STOKES_U,STOKES_UV,'*',OUT_LU)
C
         ELSE IF (CMD.EQ.'icdiv') THEN
            CALL ICMULT(NPARAMS,PARAMS,NPTS,STOKES_I,STOKES_Q,
     &                  STOKES_QV,STOKES_U,STOKES_UV,'/',OUT_LU)
C
         ELSE IF (CMD.EQ.'polmap') THEN
          CALL WR_ERROR('Hi',OUT_LU)
C
         ELSE IF (CMD.EQ.'poly') THEN
          POLY = .TRUE.
          HIST = .FALSE.
          MARK = .FALSE.
C
         ELSE IF (CMD.EQ.'hist') THEN
          HIST = .TRUE.
          POLY = .FALSE.
          MARK = .FALSE.
C
         ELSE IF (CMD.EQ.'mark') THEN
          MARK = .TRUE.
          POLY = .FALSE.
          HIST = .FALSE.
C
         ELSE IF (CMD.EQ.'thi') THEN
          T_HI=.TRUE.
          T_LOW=.FALSE.
C
         ELSE IF (CMD.EQ.'tlow') THEN
          T_HI=.FALSE.
          T_LOW=.TRUE.
C
         ELSE IF (CMD.EQ.'tfree') THEN
          T_HI=.FALSE.
          T_LOW=.FALSE.
C
         ELSE IF (CMD.EQ.'paper') THEN
          CALL SET_PAPER(NPARAMS,PARAMS,OUT_LU)
C
         ELSE IF (CMD.EQ.'qupoint') THEN
           CALL QUPOINT(NPARAMS,PARAMS,OUT_LU)
C
         ELSE IF (CMD.EQ.'rdalas') THEN
          CALL RDALAS(CPARAM,TITLE,LAMBDA,STOKES_I,STOKES_Q,STOKES_QV,
     &                STOKES_U,STOKES_UV,NPTS,IO_LU,OUT_LU)
C
         ELSE IF (CMD.EQ.'wralas') THEN
          CALL WRALAS(CPARAM,TITLE,LAMBDA,STOKES_I,STOKES_Q,STOKES_QV,
     &                STOKES_U,STOKES_UV,NPTS,IO_LU,OUT_LU)
C
         ELSE IF (CMD.EQ.'pwrite') THEN
          CALL PWRITE(CPARAM,TITLE,LAMBDA,STOKES_I,STOKES_Q,STOKES_QV,
     &                STOKES_U,STOKES_UV,NPTS,IO_LU,OUT_LU)
C
         ELSE IF (CMD.EQ.'put'.OR.CMD.EQ.'push') THEN
          CALL PUT(TITLE,LAMBDA,STOKES_I,STOKES_Q,STOKES_QV,STOKES_U,
     &             STOKES_UV,NPTS,
     &             STK_TITLE,STK_LAMBDA,STK_STOKES_I,STK_STOKES_Q,
     &             STK_STOKES_QV,STK_STOKES_U,STK_STOKES_UV,
     &             STK_NPTS,TOP_STK,OUT_LU)
C
         ELSE IF (CMD.EQ.'get'.OR.CMD.EQ.'pop') THEN
          CALL GET(TITLE,LAMBDA,STOKES_I,STOKES_Q,STOKES_QV,STOKES_U,
     &             STOKES_UV,NPTS,NPARAMS,PARAMS,
     &             STK_TITLE,STK_LAMBDA,STK_STOKES_I,STK_STOKES_Q,
     &             STK_STOKES_QV,STK_STOKES_U,STK_STOKES_UV,
     &             STK_NPTS,TOP_STK,OUT_LU,OUT_LU)
C
         ELSE IF (CMD.EQ.'comms') THEN
           WRITE(OUT_LU,'(1X,A)') 'Command history:'
           DO I=1,MIN(NHIST,MAXHIST)
            IF (NHIST.LE.MAXHIST) THEN
              NCOMM=I
                ELSE
              NCOMM=NHIST-MAXHIST+I
            ENDIF
            WRITE(OUT_LU,'(1X,I4,A2,A70)') NCOMM,' ',COMM_HIST(I)
           ENDDO
C
         ELSE IF (CMD.EQ.'merge') THEN
         CALL MERGE(NPARAMS,PARAMS,
     &             STK_TITLE,STK_LAMBDA,STK_STOKES_I,STK_STOKES_Q,
     &             STK_STOKES_QV,STK_STOKES_U,STK_STOKES_UV,
     &             STK_NPTS,TOP_STK,NPTS,LAMBDA,STOKES_I,STOKES_Q,
     &             STOKES_QV,STOKES_U,STOKES_UV,TITLE,OUT_LU)
C
         ELSE IF (CMD.EQ.'wmerge') THEN
         CALL WMERGE(NPARAMS,PARAMS,
     &             STK_TITLE,STK_LAMBDA,STK_STOKES_I,STK_STOKES_Q,
     &             STK_STOKES_QV,STK_STOKES_U,STK_STOKES_UV,
     &             STK_NPTS,TOP_STK,NPTS,LAMBDA,STOKES_I,STOKES_Q,
     &             STOKES_QV,STOKES_U,STOKES_UV,TITLE,OUT_LU)
C
         ELSE IF (CMD.EQ.'madd') THEN
          CALL MADD(NPARAMS,PARAMS,
     &             STK_TITLE,STK_LAMBDA,STK_STOKES_I,STK_STOKES_Q,
     &             STK_STOKES_QV,STK_STOKES_U,STK_STOKES_UV,
     &             STK_NPTS,TOP_STK,NPTS,LAMBDA,STOKES_I,STOKES_Q,
     &             STOKES_QV,STOKES_U,STOKES_UV,TITLE,OUT_LU)

         ELSE IF (CMD.EQ.'list') THEN
          CALL LIST(TITLE,LAMBDA,STOKES_I,STOKES_Q,STOKES_QV,STOKES_U,
     &             STOKES_UV,NPTS,
     &             STK_TITLE,STK_LAMBDA,STK_STOKES_I,STK_STOKES_Q,
     &             STK_STOKES_QV,STK_STOKES_U,STK_STOKES_UV,
     &             STK_NPTS,TOP_STK,NPARAMS,PARAMS,OUT_LU)
C
         ELSE IF (CMD.EQ.'ls') THEN
          CALL LS(STK_TITLE,TOP_STK,OUT_LU)
C
         ELSE IF (CMD.EQ.'del') THEN
          CALL SDELETE(NPARAMS,PARAMS,
     &             STK_TITLE,STK_LAMBDA,STK_STOKES_I,STK_STOKES_Q,
     &             STK_STOKES_QV,STK_STOKES_U,STK_STOKES_UV,
     &             STK_NPTS,TOP_STK,OUT_LU)
C
         ELSE IF (CMD.EQ.'title') THEN
          CALL RTITLE(CPARAM,TITLE,OUT_LU)
C
         ELSE IF (CMD.EQ.'xlabel') THEN
          XLAB=CPARAM
C
         ELSE IF (CMD.EQ.'ilabel') THEN
          ILAB=CPARAM
C
         ELSE IF (CMD.EQ.'pflabel') THEN
          PFLAB=CPARAM
C
         ELSE IF (CMD.EQ.'plabel') THEN
          PLAB=CPARAM
C
         ELSE IF (CMD.EQ.'tlabel') THEN
          TLAB=CPARAM
C
         ELSE IF (CMD.EQ.'dev') THEN
          CALL BEGIN_PLOT(NPARAMS,PARAMS,CPARAM,OUT_LU)
C
         ELSE IF (CMD.EQ.'cursor') THEN
          CALL CURSOR(OUT_LU)
C
         ELSE IF (CMD.EQ.'drawline') THEN
          CALL DRAWLINE(NPARAMS,PARAMS,OUT_LU)
C
         ELSE IF (CMD.EQ.'fitpa') THEN
          CALL  FIT_PA(STOKES_I,STOKES_Q,STOKES_U,STOKES_QV,
     &                     STOKES_UV,LAMBDA,NPTS,
     &                     NPARAMS,PARAMS,ACOEFF,NCOEFF,OUT_LU)
C
         ELSE IF (CMD.EQ.'pacalib') THEN
          CALL PA_CALIB(STOKES_I,STOKES_Q,STOKES_U,
     &                  STOKES_QV,STOKES_UV,
     &                    LAMBDA,NPTS,ACOEFF,NCOEFF,OUT_LU)

C
         ELSE IF (CMD.EQ.'iplot') THEN
          CALL IPLOT(TITLE,LAMBDA,STOKES_I,
     &                  NPTS,BOX,
     &                  IAUTOLIM,IMAX,IMIN,WAUTOLIM,WMAX,WMIN,OUT_LU)
C
         ELSE IF (CMD.EQ.'bin') THEN
         CALL CONST_BIN(TOP_STK,NPARAMS,PARAMS,STK_LAMBDA,STK_STOKES_I,
     &   STK_STOKES_Q,STK_STOKES_QV,
     &   STK_STOKES_U,STK_STOKES_UV,STK_NPTS,STOKES_I,STOKES_Q,
     &   STOKES_QV,STOKES_U,STOKES_UV,LAMBDA,NPTS,OUT_LU)
C
         ELSE IF (CMD.EQ.'box') THEN
          BOX = .TRUE.
C
         ELSE IF (CMD.EQ.'nobox') THEN
          BOX = .FALSE.
C
         ELSE IF (CMD.EQ.'lrot') THEN
          LROT = .TRUE.
C
         ELSE IF (CMD.EQ.'nolrot') THEN
          LSTYLE = 1
          LROT = .FALSE.
C
         ELSE IF (CMD.EQ.'crot') THEN
          CROT = .TRUE.
C
         ELSE IF (CMD.EQ.'nocrot') THEN
          CROT = .FALSE.
          LCOL = 1
C
         ELSE IF (CMD.EQ.'wrange') THEN
          CALL SRANGE(NPARAMS,PARAMS,WMIN,WMAX,WAUTOLIM,OUT_LU)
C
         ELSE IF (CMD.EQ.'irange') THEN
          CALL SRANGE(NPARAMS,PARAMS,IMIN,IMAX,IAUTOLIM,OUT_LU)
C
         ELSE IF (CMD.EQ.'cirange') THEN
          CALL CIRANGE(WMIN,WMAX,IMIN,IMAX,IAUTOLIM,PFLUX,OUT_LU)
C
         ELSE IF (CMD.EQ.'prange') THEN
          CALL SRANGE(NPARAMS,PARAMS,PMIN,PMAX,PAUTOLIM,OUT_LU)
C
         ELSE IF (CMD.EQ.'cprange') THEN
          CALL CPRANGE(WMIN,WMAX,PMIN,PMAX,PAUTOLIM,PFLUX,
     &                   QU_TRIPLOT,OUT_LU)
C
         ELSE IF (CMD.EQ.'trange') THEN
          CALL SRANGE(NPARAMS,PARAMS,TMIN,TMAX,TAUTOLIM,OUT_LU)
C
         ELSE IF (CMD.EQ.'ctrange') THEN
          CALL CTRANGE(WMIN,WMAX,TMIN,TMAX,TAUTOLIM,PFLUX,
     &                 QU_TRIPLOT,OUT_LU)
C
         ELSE IF (CMD.EQ.'qrange') THEN
          CALL SRANGE(NPARAMS,PARAMS,QMIN,QMAX,QAUTOLIM,OUT_LU)
C
         ELSE IF (CMD.EQ.'urange') THEN
          CALL SRANGE(NPARAMS,PARAMS,UMIN,UMAX,UAUTOLIM,OUT_LU)
C
         ELSE IF (CMD.EQ.'pfrange') THEN
          CALL SRANGE(NPARAMS,PARAMS,PFMIN,PFMAX,PFAUTOLIM,OUT_LU)
C
         ELSE IF (CMD.EQ.'cpfrange') THEN
          CALL CPFRANGE(WMIN,WMAX,PFMIN,PFMAX,PFAUTOLIM,PFLUX,
     &                   QU_TRIPLOT,OUT_LU)
C
         ELSE IF (CMD.EQ.'cwrange') THEN
          CALL CWRANGE(WMIN,WMAX,WAUTOLIM,OUT_LU)
C
         ELSE IF (CMD.EQ.'wauto') THEN
          WAUTOLIM = .TRUE.
C
         ELSE IF (CMD.EQ.'iauto') THEN
          IAUTOLIM = .TRUE.
C
         ELSE IF (CMD.EQ.'pauto') THEN
          PAUTOLIM = .TRUE.
C
         ELSE IF (CMD.EQ.'tauto') THEN
          TAUTOLIM = .TRUE.
C
         ELSE IF (CMD.EQ.'qauto') THEN
          QAUTOLIM = .TRUE.
C
         ELSE IF (CMD.EQ.'uauto') THEN
          UAUTOLIM = .TRUE.
C
         ELSE IF (CMD.EQ.'pfauto') THEN
          PFAUTOLIM = .TRUE.
C
         ELSE IF (CMD.EQ.'qupanel') THEN
          QU_TRIPLOT=.TRUE.
          PFLUX=.FALSE.
C
         ELSE IF (CMD.EQ.'noqupanel') THEN
          QU_TRIPLOT=.FALSE.
         ELSE IF (CMD.EQ.'pflux') THEN
          PFLUX=.TRUE.
          QU_TRIPLOT=.FALSE.
C
         ELSE IF (CMD.EQ.'nopflux') THEN
          PFLUX=.FALSE.
C
         ELSE IF (CMD.EQ.'triplot'.OR.CMD.EQ.'tri') THEN
         CALL TRIPLOT_IFACE(TOP_STK,STK_TITLE,STK_NPTS,
     &    STK_LAMBDA,
     &    STK_STOKES_I,STK_STOKES_Q,STK_STOKES_QV,
     &    STK_STOKES_U,STK_STOKES_UV,
     &    TITLE,NPTS,LAMBDA,STOKES_I,STOKES_Q,STOKES_QV,
     &    STOKES_U,STOKES_UV,
     &    NPARAMS,PARAMS,TMAX,TMIN,
     &    PMAX,PMIN,IMAX,IMIN,WMIN,WMAX,BOX,WAUTOLIM,
     &    IAUTOLIM,PAUTOLIM,TAUTOLIM,LROT,CROT,T_HI,T_LOW,
     &    POLY,XLAB,ILAB,PLAB,PFLAB,TLAB,PFLUX,PFMAX,PFMIN,PFAUTOLIM,
     &    LCOL,LSTYLE,HIST,MARK,OUT_LU,
     &    QU_TRIPLOT,QAUTOLIM,QMAX,QMIN,UAUTOLIM,UMAX,UMIN)

C
         ELSE IF (CMD.EQ.'contdef') THEN
          CALL CONTDEF(NPARAMS,PARAMS,CONT_ST,CONT_EN,NZONES,OUT_LU)
C
         ELSE IF (CMD.EQ.'contlist') THEN
          CALL CONTLIST(CONT_ST,CONT_EN,NZONES,OUT_LU)
C
         ELSE IF (CMD.EQ.'contfit') THEN
         CALL CONTFIT(NPARAMS,PARAMS,TOP_STK,
     &   STK_STOKES_I,STK_STOKES_Q,STK_STOKES_QV,
     &   STK_STOKES_U,STK_STOKES_UV,STK_LAMBDA,
     &   STK_NPTS,STOKES_I,STOKES_Q,
     &   STOKES_QV,STOKES_U,STOKES_UV,LAMBDA,NPTS,NZONES,
     &   CONT_ST,CONT_EN,OUT_LU)
C
         ELSE IF (CMD.EQ.'subtract') THEN 
         CALL PMATH(NPARAMS,PARAMS,TOP_STK,
     &   STK_STOKES_I,STK_STOKES_Q,STK_STOKES_QV,
     &   STK_STOKES_U,STK_STOKES_UV,STK_LAMBDA,
     &   STK_NPTS,STOKES_I,STOKES_Q,
     &   STOKES_QV,STOKES_U,STOKES_UV,LAMBDA,NPTS,'-',OUT_LU)
C
         ELSE IF (CMD.EQ.'add') THEN 
         CALL PMATH(NPARAMS,PARAMS,TOP_STK,
     &   STK_STOKES_I,STK_STOKES_Q,STK_STOKES_QV,
     &   STK_STOKES_U,STK_STOKES_UV,STK_LAMBDA,
     &   STK_NPTS,STOKES_I,STOKES_Q,
     &   STOKES_QV,STOKES_U,STOKES_UV,LAMBDA,NPTS,'+',OUT_LU)
C
         ELSE IF (CMD.EQ.'idiv') THEN 
         CALL PMATH(NPARAMS,PARAMS,TOP_STK,
     &   STK_STOKES_I,STK_STOKES_Q,STK_STOKES_QV,
     &   STK_STOKES_U,STK_STOKES_UV,STK_LAMBDA,
     &   STK_NPTS,STOKES_I,STOKES_Q,
     &   STOKES_QV,STOKES_U,STOKES_UV,LAMBDA,NPTS,'I',OUT_LU)
C
         ELSE IF (CMD.EQ.'contsub') THEN 
         CALL PMATH(NPARAMS,PARAMS,TOP_STK,
     &   STK_STOKES_I,STK_STOKES_Q,STK_STOKES_QV,
     &   STK_STOKES_U,STK_STOKES_UV,STK_LAMBDA,
     &   STK_NPTS,STOKES_I,STOKES_Q,
     &   STOKES_QV,STOKES_U,STOKES_UV,LAMBDA,NPTS,'C',OUT_LU)
C
         ELSE IF (CMD.EQ.'contadd') THEN 
         CALL PMATH(NPARAMS,PARAMS,TOP_STK,
     &   STK_STOKES_I,STK_STOKES_Q,STK_STOKES_QV,
     &   STK_STOKES_U,STK_STOKES_UV,STK_LAMBDA,
     &   STK_NPTS,STOKES_I,STOKES_Q,
     &   STOKES_QV,STOKES_U,STOKES_UV,LAMBDA,NPTS,'A',OUT_LU)
C
         ELSE IF (CMD.EQ.'cadd') THEN
          CALL CMATH(NPARAMS,PARAMS,STOKES_I,STOKES_Q,STOKES_QV,
     &                 STOKES_U,STOKES_UV,LAMBDA,NPTS,'+',OUT_LU)
C
         ELSE IF (CMD.EQ.'csub') THEN
          CALL CMATH(NPARAMS,PARAMS,STOKES_I,STOKES_Q,STOKES_QV,
     &                 STOKES_U,STOKES_UV,LAMBDA,NPTS,'-',OUT_LU)
C
         ELSE IF (CMD.EQ.'statadd') THEN
          NPARAMS=2
          PARAMS(1)=STAT_Q_MEAN*100.
          PARAMS(2)=STAT_U_MEAN*100.
          CALL CMATH(NPARAMS,PARAMS,STOKES_I,STOKES_Q,STOKES_QV,
     &                 STOKES_U,STOKES_UV,LAMBDA,NPTS,'+',OUT_LU)
C
         ELSE IF (CMD.EQ.'statsub') THEN
          NPARAMS=2
          PARAMS(1)=STAT_Q_MEAN*100.
          PARAMS(2)=STAT_U_MEAN*100.
          CALL CMATH(NPARAMS,PARAMS,STOKES_I,STOKES_Q,STOKES_QV,
     &                 STOKES_U,STOKES_UV,LAMBDA,NPTS,'-',OUT_LU)
C
         ELSE IF (CMD.EQ.'tappend') THEN
          CALL TAPPEND(CPARAM,TITLE,OUT_LU)
C
         ELSE IF (CMD.EQ.'rotpa') THEN
         CALL ROTPA(NPARAMS,PARAMS,NPTS,STOKES_Q,STOKES_U,
     &              STOKES_QV,STOKES_UV,OUT_LU)
C
         ELSE IF (CMD.EQ.'rdtsp') THEN
         CALL RDTSP(CPARAM,NPTS,STOKES_I,STOKES_Q,STOKES_QV,STOKES_U,
     &                 STOKES_UV,LAMBDA,TITLE,OUT_LU)
C
         ELSE IF (CMD.EQ.'wrtsp') THEN
         CALL WRTSP(CPARAM,NPTS,STOKES_I,STOKES_Q,STOKES_QV,STOKES_U,
     &                 STOKES_UV,LAMBDA,TITLE,OUT_LU)
C
         ELSE IF (CMD.EQ.'wrstk') THEN
         CALL WRSTK(CPARAM,STK_NPTS,STK_STOKES_I,STK_STOKES_Q,
     &                STK_STOKES_QV,STK_STOKES_U,
     &                STK_STOKES_UV,STK_LAMBDA,STK_TITLE,TOP_STK,OUT_LU)
C
         ELSE IF (CMD.EQ.'rdstk') THEN
         CALL RDSTK(CPARAM,STK_NPTS,STK_STOKES_I,STK_STOKES_Q,
     &                STK_STOKES_QV,STK_STOKES_U,
     &                STK_STOKES_UV,STK_LAMBDA,STK_TITLE,TOP_STK,OUT_LU)

C
         ELSE IF (CMD.EQ.'ptheta') THEN
         CALL PTHETA(NZONES,CONT_ST,CONT_EN,NPTS,STOKES_I,STOKES_Q,
     &                  STOKES_QV,STOKES_U,STOKES_UV,LAMBDA,
     &                  STAT_Q_MEAN,STAT_U_MEAN,OUT_LU)
C
         ELSE IF (CMD.EQ.'xgrid') THEN
         CALL XGRID(NPARAMS,PARAMS,LAMBDA,STOKES_I,STOKES_Q,
     &                STOKES_QV,STOKES_U,STOKES_UV,NPTS,
     &                TITLE,OUT_LU)
C
         ELSE IF (CMD.EQ.'smooth') THEN
         CALL  BOXSMOOTH(NPARAMS,PARAMS,STOKES_I,STOKES_Q,STOKES_QV,
     &                  STOKES_U,STOKES_UV,LAMBDA,NPTS,OUT_LU)
C
         ELSE IF (CMD.EQ.'regrid') THEN
         CALL  REGRID(NPARAMS,PARAMS,STOKES_I,STOKES_Q,STOKES_QV,
     &                  STOKES_U,STOKES_UV,LAMBDA,NPTS,OUT_LU)
C
         ELSE IF (CMD.EQ.'xadd') THEN
         CALL XADD(NPARAMS,PARAMS,LAMBDA,NPTS,OUT_LU)
C
         ELSE IF (CMD.EQ.'chopw') THEN
         CALL CHOPW(NPARAMS,PARAMS,STOKES_I,STOKES_Q,STOKES_QV,
     &                 STOKES_U,STOKES_UV,LAMBDA,NPTS,OUT_LU)
C
         ELSE IF (CMD.EQ.'tow') THEN
         CALL TOW(NPARAMS,PARAMS,NPTS,LAMBDA,XLAB,OUT_LU)
C
         ELSE IF (CMD.EQ.'tov') THEN
         CALL TOV(NPARAMS,PARAMS,NPTS,LAMBDA,XLAB,OUT_LU)
C
         ELSE IF (CMD.EQ.'help') THEN
         CALL HELP(CPARAM,IO_LU,TRANS,OUT_LU,IN_LU)
C
         ELSE IF (CMD.EQ.'quit') THEN
          WRITE(OUT_LU,*) ' Quitting polmap...'
         ELSE
C
C If we got to here then we didn't recognise the command verb...
C
          CALL WR_ERROR('Unknown command verb: '//CMD(1:10)
     &                  ,OUT_LU)
        ENDIF
       ENDIF
       GOTO 999
C
C The following are the crash-out points...
C
66     CONTINUE 
       CALL WR_ERROR('Cannot open comfile',OUT_LU)
       COMFILE=.FALSE.
       DO I = COMFILE_LU,COMFILE_LU_ST,-1
        CLOSE(I)
       ENDDO
       COMFILE_LU = 0
       GOTO 999
67     CONTINUE
       CALL WR_ERROR('Cannot read comfile',OUT_LU)
       COMFILE = .FALSE.
       DO I = COMFILE_LU,COMFILE_LU_ST,-1
        CLOSE(I)
       ENDDO
       COMFILE_LU = 0
       GOTO 999
68     CONTINUE
C
C The comfile is complete so shut it and switch to the next comfile up
C (if there is one).
C
       CLOSE(COMFILE_LU)
       IF (COMFILE_LU.EQ.COMFILE_LU_ST) THEN
        WRITE(OUT_LU,*) 'Returning to terminal control'
        COMFILE = .FALSE.
        COMFILE_LU = 0
        ELSE
        COMFILE_LU = COMFILE_LU-1
       ENDIF
       GOTO 999

 998   CONTINUE
       CALL WR_ERROR('Cannot read command number',OUT_LU)

999    CONTINUE
       ENDDO
C
C Close down the pgplot system...
C
       CALL PGEND
C
C Program finish
C
      END
