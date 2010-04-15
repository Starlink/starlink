C+
      SUBROUTINE TIPPEX
C
C     T I P P E X
C
C     Figaro function to modify a displayed spectrum using the
C     cursor.
C
C     Command parameters -
C
C     OUTPUT      (Character) The name of the output file to
C                 be created.  If this is the same as the displayed
C                 spectrum, the data in the displayed spectrum will
C                 be modified.
C
C     Command keywords -
C
C     CONFIRM     Used to confirm change to data.
C     QUIT        Used to confirm quitting application.
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
C                                              KS / CIT 25th May 1983
C     Modified:
C
C      4th Aug 1987  Revised DSA_ routines - some specs changed. Now
C                    uses DYN_ package for dynamic memory handling.
C     18th Mar 1988  KS / AAO. Modified for use with GKS version
C                    of PGPLOT.
C     26th Mar 1991  KS / AAO.  Use of 'UPDATE' and 'WRITE' corrected in
C                    mapping calls.
C     13th Dec 1991  HME / UoE, Starlink. Replace GRETEXT by PGETXT.
C     31st Aug 1992  HME / UoE, Starlink. INCLUDE changed, TABs removed.
C     27th Jul 1993  HME / UoE, Starlink.  Disuse GKD_* except
C                    GKD_WRITE_LINE. Disuse PAR_Q*, use PAR_ABORT.
C                    Added parameters CONFIRM, QUIT.
C     23rd Jan 1995  HME / UoE, Starlink. Increase TVFILE to *132.
C     2005 June 10   MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Local variables
C
      INTEGER      CKEY          ! The GRPCKG colour code for the plot
      CHARACTER    DEVICE*32     ! The PG package device name
      INTEGER      DIMS(10)      ! Sizes of dimensions of data
      REAL         HIGH          ! The 'brightest' value in the image
      INTEGER      IGNORE        ! Used to pass ignorable status
      REAL         LOW           ! The 'lowest' value in the image
      INTEGER      NDIM          ! Number of dimensions in data
      INTEGER      NELM          ! Total number of elements in data
      INTEGER      NX            ! Size of first dimension
      INTEGER      OPTR          ! Dynamic-memory pointer to output data
                                 ! array
      INTEGER      OSLOT         ! Map slot number outputdata array
      CHARACTER    SPECT*132     ! The actual file name of the image
      INTEGER      STATUS        ! Running status for DSA_ routines
      REAL         VALUE         ! Temporary real number
      INTEGER      XEN           ! Pixel number of the display's right
                                 ! edge
      INTEGER      XPTR          ! Dynamic-memory pointer to x-axis data array
      INTEGER      XSLOT         ! Map slot number for x-axis data array
      INTEGER      XST           ! Pixel number of the display's left
                                 ! edge
C
C     Initialisation of DSA_ routines
C
      STATUS=0
      CALL DSA_OPEN(STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get value of 'SOFT'
C
      CALL VAR_GETCHR('SOFT',0,0,DEVICE,STATUS)
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
C     Map the X-array (if there is one).  If there isn't, create
C     one, using the element numbers.
C
      CALL DSA_MAP_AXIS_DATA('SPECT',1,'READ','FLOAT',XPTR,XSLOT,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Now interactively modify the spectrum.
C
      CALL FIG_FUDGE(DEVICE,XST,XEN,HIGH,LOW,CKEY,NX,
     :               %VAL(CNF_PVAL(XPTR)),%VAL(CNF_PVAL(OPTR)),STATUS)
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
      SUBROUTINE FIG_FUDGE(PGDEV,XST,XEN,HIGH,LOW,CKEY,NX,XDATA,
     :                                              DATA,STATUS)
C
C     F I G _ C S F I T
C
C     If a spectrum is already displayed, this routine allows the
C     user to modify the value of the pixels interactively, using
C     the cursor.
C
C     Parameters -  (">" input, "<" output)
C
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
C     (<) DATA      (Real array DATA(NX)) Receives the generated spectrum
C     (<) STATUS    (Integer) Status return.  If unable to open the
C                   plotting device, this will be non-zero.
C
C     Subroutines / functions used -
C
C     PAR_WRUSER  (PAR_ package) Output message to user.
C     ICH_FOLD    (ICH_   "    ) Convert character to upper case.
C     ICH_LEN     ( "     "    ) Last non-blank char in string.
C     GEN_BSEARCH (GEN_   "    ) Search for nearest value in a table.
C     PGBEGIN    (PGPLOT)  Open plotting device.
C     PGADVANCE  (   "  ) Start new plot - ie erase screen.
C     PGBOX      (   "  ) Draw axes for plot.
C     PGEND      (   "  ) Terminate plot.
C     PGCURSE    (   "  ) Select a point using cursor.
C     PGWINDOW   (   "  ) Set plotting window.
C     PGVSTAND   (   "  ) Set standard viewport for plots.
C     PGLINE     (   "  ) Join-dots plot of a set of data points.
C     PGSCI      (   "  ) Set plotting colour.
C     GKD_INIT        (GKD_ package) Initialise dialogue routines
C     GKD_CLEAR_ALPHA ( "      "   ) Write line to screen
C     GKD_QUEST       ( "      "   ) Get YES/NO response
C
C                                          KS / CIT 25th May 1983
C     Modified:
C
C     18th March 1988  KS / AAO. Modified for use with GKS version of
C                      PGPLOT.  GKD_ now used for dialogue calls.
C     13th Dec 1991    HME / UoE, Starlink. Replace GRETEXT by PGETXT.
C     24th Jul 1996    MJCL / Starlink, UCL.  String catenation for
C                      Linux port.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER CKEY,NX,STATUS
      REAL XST,XEN,HIGH,LOW,XDATA(NX),DATA(NX)
      CHARACTER*(*) PGDEV
C
C     Functions
C
      LOGICAL PAR_ABORT
      INTEGER ICH_LEN,ICH_FOLD,ICH_ENCODE,PGBEGIN,GEN_BSEARCH
C
C     PGPLOT code for white
C
      INTEGER WHITE
      PARAMETER (WHITE=1)
C
C     Local variables
C
      LOGICAL DONE,PQUEST
      INTEGER INVOKE,IX,NEXT,NSTAT
      REAL    X,Y
      CHARACTER CH,STRING*64
C
C     Initiate PGPLOT - note use of /APPEND to prevent erase
C
      STRING=PGDEV(:ICH_LEN(PGDEV))//'/APPEND'
      STATUS=PGBEGIN(1,STRING,1,1)
      IF (STATUS.NE.1) THEN
         CALL PAR_WRUSER('Unable to open graphics device',NSTAT)
         GO TO 600
      END IF
C
C     Define window and viewport
C
      CALL PGWINDOW(XST,XEN,LOW,HIGH)
      CALL PGVSTAND
      CALL GKD_WRITE_LINE('Space to set point, "Q" to quit')
C
C     This loop continues until the user is satisfied.
C
      DONE=.FALSE.
      DO WHILE (.NOT.DONE)
C
C        Get cursor position and character input
C
         CALL PGCURSE(X,Y,CH)
         INVOKE=ICH_FOLD(CH)
         IF (CH.EQ.'Q') THEN
C
C           'Q' for "Quit"
C
            CALL PGETXT
            CALL PAR_CNPAR('QUIT')
            CALL PAR_RDKEY('QUIT',.TRUE.,DONE)
            IF (PAR_ABORT()) GO TO 600
C
         ELSE IF (CH.EQ.' ') THEN
C
C           Space indicates a point to be modified
C
            IX=GEN_BSEARCH(XDATA,NX,X)
            STRING='Pixel # '
            NSTAT=ICH_ENCODE(STRING,FLOAT(IX),9,0,NEXT)
            STRING(NEXT:)=', X value = '
            NSTAT=ICH_ENCODE(STRING,X,NEXT+12,3,NEXT)
            CALL GKD_WRITE_LINE(STRING)
            STRING='Current data ='
            NSTAT=ICH_ENCODE(STRING,DATA(IX),16,3,NEXT)
            CALL GKD_WRITE_LINE(STRING)
            STRING='Change to indicated value of'
            NSTAT=ICH_ENCODE(STRING,Y,30,3,NEXT)
            CALL GKD_WRITE_LINE(STRING)
            CALL PAR_CNPAR('CONFIRM')
            CALL PAR_RDKEY('CONFIRM',.TRUE.,PQUEST)
            IF (PAR_ABORT()) GO TO 600
            IF (PQUEST) DATA(IX)=Y
C
         END IF
C
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
      CALL PGSCI(WHITE)
      CALL PGEND
C
      END
