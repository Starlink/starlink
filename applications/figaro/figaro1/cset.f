C+
      SUBROUTINE CSET
C
C     C S E T
C
C     Figaro function to set large interactively selected regions
C     of a spectrum to a constant value.  This is intended mainly
C     for use in generating mask spectra, or modifying calibration
C     spectra such as those used by BSMULT.  CSET assumes that a
C     spectrum has already been displayed by SPLOT, and will generate
C     a new data structure based on the spectrum displayed, with
C     only the data changed.
C
C     Command parameters -
C
C     VALUE       (Numeric) The value to use for the selected regions
C
C     OUTPUT      (Character) The name of the output file to
C                 be created.  If this is the same as the displayed
C                 spectrum, the data in the displayed spectrum will
C                 be modified.
C
C     Command keywords -
C
C     QUIT        Used to confirm quitting area selection.
C
C     User variables used -  (">" input, "<" output)
C
C     (>) TVFILE  The name of the displayed spectrum
C     (>) TVXST   The first displayed x-value
C     (>) TVXEN   The last displayed x-value
C     (>) TVHIGH  The maximum displayed y-value
C     (>) TVLOW   The minimum displayed y-value
C     (>) TVCOLOR The GRPLOT code for the plot colour
C     (>) SOFT    The device/type string defining the display device
C
C                                              KS / CIT 11th April 1984
C     Modified:
C
C     10th Aug 1987  DJA/ AAO. Revised DSA_ routines - some specs
C                    changed.
C                    Now uses DYN_ routines for dynamic memory handling
C     21st Mar 1988  KS/AAO.  Modified for GKS version of PGPLOT.
C     26th Mar 1991  KS / AAO.  Use of 'UPDATE' and 'WRITE' corrected in
C                    mapping calls.
C     28th Aug 1992  HME / UoE, Starlink. Change INCLUDE, remove TABs.
C     26th Jul 1993  HME / UoE, Starlink.  Disuse GKD_* and PAR_Q*, use
C                    PAR_ABORT.
C     23rd Jan 1995  HME / UoE, Starlink. Increase TVFILE to *132.
C      1st May 1997  JJL / Soton, Starlink. Maps variances and sets
C                    their values to zero when a pixel value is set.
C     2005 June 7    MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions
C
      INTEGER DSA_TYPESIZE
C
C     Limits for VALUE - close to the VAX number limits
C
      REAL FMAX,FMIN
      PARAMETER (FMAX=1.7E38,FMIN=-1.7E38)
C
C     Local variables
C
      INTEGER      BYTES        ! Number of bytes of workspace required
      INTEGER      CKEY         ! GRPCKG code of plot colour
      INTEGER      DIMS(10)     ! Sizes of dimensions of data
      REAL         HIGH         ! The brightest value displayed
      INTEGER      IGNORE       ! Used to pass an ignorable status
      REAL         LOW          ! The faintest value displayed
      INTEGER      NDIM         ! Number of dimensions in data
      INTEGER      NELM         ! Total number of elements in data
      INTEGER      NX           ! Size of 1st dimension
      INTEGER      OPTR         ! Dynamic-memory pointer to output data array
      INTEGER      OSLOT        ! Map slot number output data array
      CHARACTER    PGDEV*32     ! The name of the plotting device
      CHARACTER    SPECT*132    ! The file name of the displayed spectrum
      INTEGER      STATUS       ! Running status for DSA_ routines
      REAL         VALUE        ! Temporary real number
      LOGICAL      VEXIST       ! True is there is a variance structure
      INTEGER      VOPTR        ! Dynamic-memory pointer to output data array
      INTEGER      VOSLOT       ! Map slot number output data array
      INTEGER      VWPTR        ! Dynamic-memory pointer to workspace
      INTEGER      VWSLOT       ! Map slot number of workspace
      INTEGER      VXPTR        ! Dynamic memory pointer to x-axis data
      INTEGER      VXSLOT       ! Map slot number for x-axis data array
      INTEGER      WPTR         ! Dynamic-memory pointer to workspace
      INTEGER      WSLOT        ! Map slot number of workspace
      REAL         XEN          ! Value of leftmost pixel
      INTEGER      XPTR         ! Dynamic memory pointer to x-axis data
      INTEGER      XSLOT        ! Map slot number for x-axis data array
      REAL         XST          ! Value of rightmost pixel
C
C     Initialisation of DSA_ routines
C
      VEXIST=.FALSE.
      STATUS=0
      CALL DSA_OPEN(STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get value of 'SOFT'
C
      CALL VAR_GETCHR('SOFT',0,0,PGDEV,STATUS)
      IF (STATUS.NE.0) THEN
         CALL PAR_WRUSER('Unable to get soft device name.',IGNORE)
         CALL PAR_WRUSER('Probably no plot has been made.',IGNORE)
         GO TO 500
      END IF
C
C     Get the user variables describing the plot.
C
      CALL VAR_GETNUM('TVXST',0,0,XST,STATUS)
      IF (STATUS.EQ.0) THEN
         CALL VAR_GETNUM('TVXEN',0,0,XEN,STATUS)
         IF (STATUS.EQ.0) THEN
            CALL VAR_GETNUM('TVLOW',0,0,LOW,STATUS)
            IF (STATUS.EQ.0) THEN
               CALL VAR_GETNUM('TVHIGH',0,0,HIGH,STATUS)
               IF (STATUS.EQ.0) THEN
                  CALL VAR_GETCHR('TVFILE',0,0,SPECT,STATUS)
                  IF (STATUS.EQ.0) THEN
                     CALL VAR_GETNUM('TVCOLOR',0,0,VALUE,STATUS)
                     CKEY=VALUE
                  END IF
               END IF
            END IF
         END IF
      END IF
      IF (STATUS.NE.0) THEN
         CALL PAR_WRUSER('Unable to obtain plot variables',IGNORE)
         CALL PAR_WRUSER('Probably no plot has been made.',IGNORE)
         GO TO 500
      END IF
C
C     Open spectrum file
C
      CALL DSA_NAMED_INPUT('SPECT',SPECT,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get size of data
C
      CALL DSA_DATA_SIZE('SPECT',10,NDIM,DIMS,NELM,STATUS)
      NX=DIMS(1)
C
C     Get value for VALUE
C
      CALL PAR_RDVAL('VALUE',FMIN,FMAX,1.,' ',VALUE)
C
C     Get name for output file
C
      CALL DSA_OUTPUT('OUTPUT','OUTPUT','SPECT',0,0,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Map data array to receive fitted spectrum
C
      CALL DSA_MAP_DATA('OUTPUT','UPDATE','FLOAT',OPTR,OSLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Map the X-array
C
      CALL DSA_MAP_AXIS_DATA('SPECT',1,'READ','FLOAT',XPTR,XSLOT,
     :                       STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Test to see if the variance array exists.
C
      CALL DSA_SEEK_VARIANCE('SPECT',VEXIST,STATUS)
      IF (VEXIST) THEN
      CALL DSA_MAP_VARIANCE('SPECT','READ','FLOAT',VXPTR,VXSLOT,STATUS)
      CALL DSA_MAP_VARIANCE('OUTPUT','UPDATE','FLOAT',VOPTR,VOSLOT,
     :                       STATUS)
      END IF
      IF(STATUS.NE.0) GO TO 500

C
C     Get a copy of the original data in a work area
C
      BYTES=NX*DSA_TYPESIZE('FLOAT',STATUS)
      CALL DSA_GET_WORKSPACE(BYTES,WPTR,WSLOT,STATUS)
      CALL GEN_MOVE(BYTES,%VAL(CNF_PVAL(OPTR)),%VAL(CNF_PVAL(WPTR)))
C
C     If needed, set aside variance work space
C
      IF (VEXIST) THEN
        CALL DSA_GET_WORKSPACE(BYTES,VWPTR,VWSLOT,STATUS)
        CALL GEN_MOVE(BYTES,%VAL(CNF_PVAL(VOPTR)),%VAL(CNF_PVAL(VWPTR)))
      END IF
      IF (STATUS.NE.0) GO TO 500
C
C     Now interactively modify the spectrum.
C
      CALL FIG_CSET(VALUE,PGDEV,XST,XEN,HIGH,LOW,CKEY,NX,
     :              %VAL(CNF_PVAL(XPTR)),%VAL(CNF_PVAL(WPTR)),
     :              %VAL(CNF_PVAL(OPTR)),%VAL(CNF_PVAL(VXPTR)),
     :              %VAL(CNF_PVAL(VWPTR)),%VAL(CNF_PVAL(VOPTR)),
     :              VEXIST,STATUS)
C
C     Tidy up
C
  500 CONTINUE
C
C     Closedown everything
C
      CALL DSA_CLOSE(STATUS)
C
      END
C+
      SUBROUTINE FIG_CSET(VALUE,PGDEV,XST,XEN,HIGH,LOW,CKEY,NX,XDATA,
     :                    SDATA,DATA,XVAR,SVAR,VAR,VEXIST,STATUS)
C
C     F I G _ C S F I T
C
C     Allows the user to interactively set regions of a spectrum to
C     a constant value.   Assumes the spectrum has already been
C     displayed.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) VALUE     (Real) The default value to use.
C     (>) PGDEV     (Character) The device/type for the plots, as
C                   required by PGPLOT.
C     (>) XST       (Real) The initial displayed X value
C     (>) XEN       (Real) The final displayed X value
C     (>) HIGH      (Real) The maximum displayed Y value
C     (>) LOW       (Real) The minimum displayed Y value
C     (>) CKEY      (Integer) The GRPCKG code for the plot colour
C     (>) NX        (Integer) The number of points in the spectrum
C     (>) XDATA     (Real array XDATA(NX)) The X-values for each of
C                   the points in the spectrum.
C     (>) SDATA     (Real array SDATA(NX)) The original spectrum data
C                   - used if the spectrum has to be redrawn.
C     (<) DATA      (Real array DATA(NX)) Receives the generated spectrum
C     (>) XVAR      (Real array XVAR(NX)) The X-values for each of
C                   the variances in the spectrum.
C     (>) SVAR      (Real array SVAR(NX)) The original spectrum variances
C                   - used if the spectrum has to be redrawn.
C     (<) VAR       (Real array VAR(NX)) Receives the generated variances
C     (>) VEXIST    (Logical) True if the variance array exists
C     (<) STATUS    (Integer) Status return.  If unable to open the
C                   plotting device, this will be non-zero.
C
C     Subroutines / functions used -
C
C     PAR_WRUSER  (PAR_ package)  Output message to user.
C     GEN_BSEARCH (GEN_   "    )  Find nearest value in an array
C     GEN_MOVE    ( "     "    )  Fast copy between arrays
C     ICH_FOLD    (ICH_   "    )  Convert string to upper case
C     ICH_LEN     ( "     "    )  Last non-blank char in string.
C     PGBEGIN     (PGPLOT) Open plotting device.
C     PGEND       (   "  ) Terminate plot.
C     PGBIN       (   "  ) Histogram plot of a set of data points
C     PGCURSE     (   "  ) Get cursor position
C     PGVSTAND    (   "  ) Set standard viewport
C     PGWINDOW    (   "  ) Set plotting window
C     PGPOINT     (   "  ) Plot symbol(s)
C     PGSCI       (   "  ) Set plotting colour
C     PGSLW       (   "  ) Set line intensity
C     FIG_CSHELP  (Figaro routine) Output help info for CSET
C
C                                          KS / CIT 26th July 1984
C     Modified:
C
C     21st Mar 1988.  KS/AAO.  Modified for GKS version of PGPLOT.
C                     Use of GKD_ routines introduced.
C     26th Jul 1993.  HME/UoE, Starlink.  Disuse GKD_* and PAR_Q*, use
C                     PAR_ABORT.
C     24th JUl 1996.  MJCL/Starlink, UCL.  Changed string catenation
C                     for Linux port.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER CKEY,NX,STATUS
      REAL VALUE,XST,XEN,HIGH,LOW,SDATA(NX),XDATA(NX),DATA(NX)
      REAL VAR(NX),XVAR(NX),SVAR(NX)
      CHARACTER*(*) PGDEV
      LOGICAL VEXIST
C
C     Limits for VALUE - close to the VAX number limits
C
      REAL FMAX,FMIN
      PARAMETER (FMAX=1.7E38,FMIN=-1.7E38)
C
C     Functions
C
      LOGICAL PAR_ABORT
      INTEGER GEN_BSEARCH,ICH_LEN,PGBEGIN
C
C     White colour code
C
      INTEGER WHITE
      PARAMETER (WHITE=1)
C
C     Maximum number of regions to remember (for Undo)
C
      INTEGER MAXREM
      PARAMETER (MAXREM=20)
C
C     Local variables
C
      LOGICAL LSET, REPEAT, REVX, RSET
      INTEGER CMPTAB(8), ILWAS(MAXREM), ILX, IRWAS(MAXREM), IRX
      INTEGER IX, IX1, IX2, LPTR, NSTAT
      REAL    VAL, VALWAS(MAXREM), VWAS, X, Y
      CHARACTER PGDEVL*64
      CHARACTER CHR*1
C
C     Complementary colour codes for the various PGPLOT colours
C
      DATA CMPTAB/1,1,5,6,7,2,3,4/
C
C     Initiate PGPLOT - note use of /APPEND to prevent screen erase.
C
      PGDEVL=PGDEV(:ICH_LEN(PGDEV))//'/APPEND'
      STATUS=PGBEGIN(1,PGDEVL,1,1)
      IF (STATUS.NE.1) THEN
         CALL PAR_WRUSER('Unable to open graphics device',NSTAT)
         GO TO 600
      END IF
C
C     Define window and viewport
C
      CALL PGWINDOW(XST,XEN,LOW,HIGH)
      CALL PGVSTAND
C
C     Start by copying SDATA into DATA and SVAR into VAR if needed
C
      CALL GEN_MOVE(NX*4,SDATA,DATA)
      IF (VEXIST) CALL GEN_MOVE(NX*4,SVAR,VAR)
C
C     Quick bit of help
C
      CALL PAR_WRUSER(
     : '"L" & "R" to select ends of regions, "Q" to quit, "?" for help'
     :                                                         ,STATUS)
C
C     Initial values
C
      DO LPTR=1,MAXREM
         ILWAS(LPTR)=0
         IRWAS(LPTR)=0
      END DO
      REVX=XDATA(1).GT.XDATA(NX)
      LPTR=0
      VAL=VALUE
      LSET=.FALSE.
      RSET=.FALSE.
      X=(XST+XEN)*.5
      Y=VAL
C
C     Continue until user quits
C
      REPEAT=.TRUE.
      DO WHILE (REPEAT)
         CALL PGCURSE(X,Y,CHR)
         CALL ICH_FOLD(CHR)
C
C        If data is reversed in X, 'L' and 'R' reverse meanings
C
         IF (REVX) THEN
            IF (CHR.EQ.'L') THEN
               CHR='R'
            ELSE IF (CHR.EQ.'R') THEN
               CHR='L'
            END IF
         END IF
         IF (CHR.EQ.'L') THEN
C
C           'L' is for 'Set Left end of region'
C
            IF (LSET) THEN
               CALL PAR_WRUSER('Previous Left setting ignored',STATUS)
            END IF
            LSET=.TRUE.
            ILX=MAX(1,GEN_BSEARCH(XDATA,NX,X))
            IF (RSET) THEN
               IF (IRX.LT.ILX) THEN
                  CALL PAR_WRUSER(
     :              'Left should be to the of right',STATUS)
                  LSET=.FALSE.
               END IF
            END IF
            IF (LSET) CALL PGPOINT(1,XDATA(ILX),VAL,ICHAR('X'))
C
         ELSE IF (CHR.EQ.'R') THEN
C
C           'R' is for 'set Right end of region'
C
            IF (RSET) THEN
               CALL PAR_WRUSER('Previous setting ignored',STATUS)
            END IF
            RSET=.TRUE.
            IRX=GEN_BSEARCH(XDATA,NX,X)
            IF (IRX.EQ.0) IRX=NX
            IF (LSET) THEN
               IF (IRX.LT.ILX) THEN
                  CALL PAR_WRUSER(
     :               'Right should be to the right of left',STATUS)
                  RSET=.FALSE.
               END IF
            END IF
            IF (RSET) CALL PGPOINT(1,XDATA(IRX),VAL,ICHAR('X'))
C
         ELSE IF (CHR.EQ.'Q') THEN
C
C           'Q' is for 'QUIT'
C
            CALL PAR_CNPAR('QUIT')
            CALL PAR_RDKEY('QUIT',.FALSE.,REPEAT)
            IF (PAR_ABORT()) GO TO 600
            REPEAT=.NOT.REPEAT
C
         ELSE IF (CHR.EQ.'V') THEN
C
C           'V' is for 'set new Value'
C
            CALL PAR_CNPAR('VALUE')
            CALL PAR_RDVAL('VALUE',FMIN,FMAX,VAL,' ',VAL)
            IF (PAR_ABORT()) GO TO 600
            Y=VAL
C
         ELSE IF (CHR.EQ.'?'.OR.CHR.EQ.'H') THEN
C
C           '?' or 'H' are both for 'Help'
C
            CALL FIG_CSHELP
C
         ELSE IF (CHR.EQ.'U') THEN
C
C           'U' is for 'Undo last change'
C
            IF (LPTR.EQ.0) THEN
               CALL PAR_WRUSER('No region set',STATUS)
            ELSE
               RSET=.FALSE.
               LSET=.FALSE.
               ILX=ILWAS(LPTR)
               IRX=IRWAS(LPTR)
               ILWAS(LPTR)=0
               IRWAS(LPTR)=0
               VWAS=VALWAS(LPTR)
               LPTR=LPTR-1
               IF (LPTR.EQ.0) THEN
                  IF (ILWAS(MAXREM).NE.0) THEN
                     LPTR=MAXREM
                  ELSE
                     LPTR=0
                  END IF
               END IF
               DO IX=ILX,IRX
                  DATA(IX)=VWAS
               END DO
               IX1=MAX(1,ILX-1)
               IX2=MIN(NX,IRX+1)
               CALL PGSCI(0)
               CALL PGBIN(IX2-IX1+1,XDATA(IX1),DATA(IX1),.TRUE.)
               CALL PGSCI(CKEY)
               DO IX=ILX,IRX
                  DATA(IX)=SDATA(IX)
               END DO
               IF (VEXIST) THEN
                  DO IX = ILX, IRX
                     VAR(IX) = SVAR(IX)
                  END DO
               END IF
               CALL PGBIN(IX2-IX1+1,XDATA(IX1),DATA(IX1),.TRUE.)
            END IF
         END IF
C
C        See if we have a region defined.
C
         IF (LSET.AND.RSET) THEN
            LSET=.FALSE.
            RSET=.FALSE.
C
C           Set the region to the constant value, remember the
C           region in case it has to be undone.
C
            DO IX=ILX,IRX
               DATA(IX)=VAL
            END DO
            IF (VEXIST) THEN
               DO IX=ILX,IRX
                 VAR(IX)=0
               END DO
            END IF
            LPTR=LPTR+1
            IF (LPTR.GT.MAXREM) THEN
               LPTR=1
               CALL PAR_WRUSER('Too many changes to remember them all',
     :                                                           STATUS)
               CALL PAR_WRUSER(
     :             '"Undo" will not work on the early changes',STATUS)
            END IF
            ILWAS(LPTR)=ILX
            IRWAS(LPTR)=IRX
            VALWAS(LPTR)=VAL
C
C           Draw the new data for the region
C
            IX1=MAX(1,ILX-1)
            IX2=MIN(NX,IRX+1)
            CALL PGSCI(CMPTAB(CKEY))
            CALL PGBIN(IX2-IX1+1,XDATA(IX1),DATA(IX1),.TRUE.)
            CALL PGSCI(WHITE)
         END IF
      END DO
C
C     Indicate OK
C
      STATUS=0
C
C     Exit
C
  600 CONTINUE
C
C     Close down plotting device
C
      CALL PGEND
C
      END
C+
      SUBROUTINE FIG_CSHELP
C
C     F I G _ C S H E L P
C
C     Outputs help information for CSET.
C
C     Parameters -  None
C
C     Common variables - None
C
C     Subroutines / functions used -
C
C
C                                              KS / CIT 12th April 1984
C     Modified:
C
C     21st Mar 1988  KS / AAO.  Modified for GKS version of PGPLOT.
C                    GKD_ routines used instead of GRPCKG and PAR_
C     26th Jul 1993  HME/UoE, Starlink.  Disuse GKD_*.
C+
      IMPLICIT NONE
C
C     Local variables
C
      INTEGER STATUS
C
      CALL PAR_WRUSER('L - Set left end of region to cursor x position',
     :                                                           STATUS)
      CALL PAR_WRUSER(
     :        'R - Set right end of region to cursor x position',STATUS)
      CALL PAR_WRUSER('V - change Value of constant regions are set to',
     :                                                           STATUS)
      CALL PAR_WRUSER('Q - Quit',STATUS)
      CALL PAR_WRUSER('U - Undo last change',STATUS)
      CALL PAR_WRUSER('? or H - output this help information',STATUS)
      CALL PAR_WRUSER(' ',STATUS)
C
      END
