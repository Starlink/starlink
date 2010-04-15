C+
      SUBROUTINE CCUR
C
C     C C U R
C
C     Uses the cursor to get information about data as displayed on
C     the soft graphics device.
C
C     Command variables - None.
C
C     Command keywords - None.
C
C     User variables -     (">" input, "<" output)
C
C     (>) SOFT     (Character) Device / type for soft plots.
C                  See documentation on 'SOFT' command for
C                  details.
C     (>) TVXST    (Numeric) X-start value for current plot.
C     (>) TVXEN    (Numeric) X-end value for current plot.
C     (>) TVLOW    (Numeric) Lowest value of current plot.
C     (>) TVHIGH   (Numeric) Highest value of current plot.
C     (>) TVFILE   (Character) Name of currently displayed data.
C
C                                  KS / CIT 23rd July 1984
C     Modified:
C
C     14th Aug 1985  KS / AAO.  Now uses VMS V4 SMG$ routines instead
C                    of the old RTL terminal-independent I/O routines.
C                    SCREEN keyword added.
C     20th Jul 1987  DJA / AAO.  New DSA_ routines - specifications for
C                    some have changed.
C     22nd Jul 1987  DJA / AAO.  Modified dynamic memory routines - now
C                    uses DYN_ package
C     15th Mar 1988  KS / AAO.  Modified for use with the GKS version
C                    of PGPLOT.  SCREEN keyword no longer used, since
C                    GKD_ calls are used for terminal output.
C     28th Sep 1992  HME / UoE, Starlink.  INCLUDE changed. TABs
C                    removed. Unused declarations of SMG$... removed.
C     23rd Jan 1995  HME / UoE, Starlink. Increase TVFILE to *132.
C     2005 June 7    MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions used
C
      INTEGER GEN_BSEARCH,ICH_LEN,PGBEGIN
      INTEGER ICH_ENCODE
      REAL GEN_ELEMF
C
C     Local variables
C
      CHARACTER CH               ! User's response to prompting
      CHARACTER DEVICE*32        ! PGPLOT plotting device
      INTEGER   DIMS(10)         ! Dimensions of data
      CHARACTER DLABEL*32        ! Structure data axis label
      REAL      DUMMY            ! REAL dummy arguement
      CHARACTER DUNITS*32        ! Structure data axis units
      INTEGER   DPTR             ! Dynamic-memory pointer to data array
      INTEGER   DSLOT            ! Dynamic-memory pointer to data array
      CHARACTER FILE*132         ! Name of the last file plotted
      LOGICAL   FIRST            ! TRUE if first time through loop
      REAL      HIGH             ! Maximum Y-value for a plot
      INTEGER   IGNORE           ! Used to send dummy status arguement
      INTEGER   INVOKE           ! Dummy function value
      INTEGER   IXPIX            ! Number of pixel nearest to cursor
      REAL      LOW              ! Minimum Y-value for a plot
      INTEGER   NDIM             ! Dimensionality of input data structure
      INTEGER   NX               ! Total number of elements per cross-section
      INTEGER   NEXT             ! Next character in string
      LOGICAL   REPEAT           ! Loop control variable, TRUE to go on
      INTEGER   STATUS           ! Status return from DSA_xxx routines
      CHARACTER STRING*80        ! Output string
      CHARACTER STRINGS(2)*64    ! Receives data and axis information
      REAL      VALUE            ! Temporary REAL
      REAL      X                ! Current cursor x-position
      REAL      XEN              ! Rightmost x-value on screen
      CHARACTER XLABEL*32        ! Structure x-axis label
      INTEGER   XPTR             ! Dynamic-memory pointer to x-axis data
      INTEGER   XSLOT            ! Map slot number used for x-axis info
      REAL      XST              ! Leftmost x-value on the screen
      CHARACTER XUNITS*32        ! Structure x-axis units
      REAL      Y                ! Current cursor y-position
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
C     Get the user variables describing the plot limits
C
      CALL VAR_GETNUM('TVXST',0,0,XST,STATUS)
      IF (STATUS.EQ.0) THEN
         CALL VAR_GETNUM('TVXEN',0,0,XEN,STATUS)
         IF (STATUS.EQ.0) THEN
            CALL VAR_GETNUM('TVLOW',0,0,LOW,STATUS)
            IF (STATUS.EQ.0) THEN
               CALL VAR_GETNUM('TVHIGH',0,0,HIGH,STATUS)
               IF (STATUS.EQ.0) THEN
                  CALL VAR_GETCHR('TVFILE',0,0,FILE,STATUS)
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
C     Open the displayed data file
C
      CALL DSA_NAMED_INPUT ('SPECT',FILE(:ICH_LEN(FILE)),STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Map the X-axis array - note that we do not require
C     that there be an X array.
C
      CALL DSA_MAP_AXIS_DATA('SPECT',1,'READ','FLOAT',XPTR,XSLOT,
     :                       STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Get main data array
C
      CALL DSA_DATA_SIZE('SPECT',1,NDIM,DIMS,NX,STATUS)
      CALL DSA_MAP_DATA('SPECT','READ','FLOAT',DPTR,DSLOT,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Get the labels and units for the two axes
C
      CALL DSA_GET_AXIS_INFO ('SPECT',1,2,STRINGS,0,DUMMY,STATUS)
      XUNITS=STRINGS(1)
      XLABEL=STRINGS(2)
      IF (STATUS.NE.0) XLABEL='X'
      CALL DSA_GET_DATA_INFO ('SPECT',2,STRINGS,0,DUMMY,STATUS)
      DUNITS=STRINGS(1)
      DLABEL=STRINGS(2)
C
C     At this point, we have all the information we need..
C
C     Initiate PGPLOT.  Use of PGWINDOW and PGVSTAND instead of
C     PGENV predates /APPEND qualifier, but works.
C
      STATUS=PGBEGIN(1,DEVICE(:ICH_LEN(DEVICE))//'/APPEND',1,1)
      IF (STATUS.NE.1) THEN
         CALL PAR_WRUSER('Unable to open graphics device',IGNORE)
         GO TO 500
      END IF
C
C     Define window and viewport
C
      CALL PGWINDOW(XST,XEN,LOW,HIGH)
      CALL PGVSTAND
C
C     Clear text screen, put out helpful information
C
      CALL GKD_INIT(DEVICE)
      CALL GKD_CLEAR_ALPHA
      CALL GKD_WRITE_LINE(
     :          'Hit "Q" to exit, any key to indicate position.')
C
C     Initial cursor position
C
      X=.5*(XST+XEN)
      Y=.5*(LOW+HIGH)
C
C     Loop, displaying position whenever key is hit, until
C     'Quit' is indicated.
C
      FIRST=.TRUE.
      REPEAT=.TRUE.
      DO WHILE (REPEAT)
C
C        Get cursor position
C
         CALL PGCURSE(X,Y,CH)
C
C        Find nearest pixel value
C
         VALUE=GEN_BSEARCH(%VAL(CNF_PVAL(XPTR)),NX,X)
         IXPIX=MAX(1,MIN(NX,GEN_BSEARCH(%VAL(CNF_PVAL(XPTR)),NX,X)))
         VALUE=GEN_ELEMF(%VAL(CNF_PVAL(DPTR)),IXPIX)
C
C        Encode and output information
C
         STRING='X = '
         INVOKE=ICH_ENCODE(STRING,X,5,7,NEXT)
         STRING(NEXT:)=', Y = '
         INVOKE=ICH_ENCODE(STRING,Y,NEXT+7,7,NEXT)
         STRING(NEXT:)=', Pixel = '
         INVOKE=ICH_ENCODE(STRING,FLOAT(IXPIX),NEXT+11,0,NEXT)
         STRING(NEXT:)=', Value = '
         INVOKE=ICH_ENCODE(STRING,VALUE,NEXT+11,7,NEXT)
         CALL GKD_CLEAR_ALPHA
         CALL GKD_WRITE_LINE(STRING(:NEXT-1))
         REPEAT=(CH.NE.'Q').AND.(CH.NE.'q')
      END DO
C
C     Close down PGPLOT
C
      CALL GKD_CLOSE
      CALL PGEND
C
C     Close down
C
  500 CONTINUE
C
      CALL DSA_CLOSE(STATUS)
C
      END
