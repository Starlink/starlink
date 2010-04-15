C+
      SUBROUTINE HOPT
C
C     H O P T
C
C     Performs a histogram equalisation on an image in an attempt
C     to enhance the contrast.  See, for example, Gonzalez and
C     Wintz, 'Digital Image Processing', Addison-Wesley).
C
C     Command parameters -
C
C     IMAGE  The name of the structure containing the image.
C
C     MINVAL The minimum value to be used in generating the
C            histogram.
C
C     MAXVAL The maximum value to be used in generating the
C            histogram.
C
C     OUTPUT The name of the result of the operation.  This can
C            be the same as for IMAGE.  If not, a new structure
C            is created, with everything but the data a direct
C            copy of the input.
C
C     Command keywords -
C
C     EQUALISE  Actually perform an equalisation.  If this is
C               false (most likely if NOEQUALISE is specified)
C               then all that happens is that the distribution
C               histogram is generated and displayed.
C
C     User variables -
C
C     SOFT    (Character) The device/type to be used for terminal
C             graphics.
C
C                                      KS / CIT 27th Jan 1983
C
C     Modified:
C
C     30th Jul 1987  DJA / AAO. New DSA_ routines - some specs changed.
C                    Modified dynamic memory handling, now uses DYN_
C                    routines
C     28th Sep 1992  HME / UoE, Starlink.  INCLUDE changed, TABs
C                    removed.
C     11th Jan 1995  HME / UoE, Starlink. Passive AGI compliance,
C                    use FIG_PGBEG/END.
C     2005 June 7    MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions
C
      INTEGER FIG_PGBEG
C
C     Size of histogram array
C
      INTEGER NH
      PARAMETER (NH=1024)
C
C     Plotting logical unit
C
      INTEGER LU
      PARAMETER (LU=1)
C
C     Local variables
C
      CHARACTER    DEVICE*64    ! Actual name of plotting device
      INTEGER      DIMS(10)     ! Sizes of dimensions of data
      REAL         DMAX         !
      REAL         DMIN         !
      INTEGER      DPTR         ! Dynamic-memory pointer to data array
      INTEGER      DSLOT        ! Map slot number of input data array
      LOGICAL      EQUAL        ! See above
      LOGICAL      GRAPHOK      ! PG package status
      REAL         HIST(NH)     !
      INTEGER      I            !
      INTEGER      IGNORE       ! Used to pass an ignorable status
      INTEGER      NDIM         ! Number of dimensions in data
      INTEGER      NELM         ! Total number of elements in data
      INTEGER      NG           !
      INTEGER      NX           ! Size of 1st dimension
      INTEGER      NY           ! Size of 2nd dimension (if present)
      LOGICAL      OK           !
      INTEGER      OPTR         ! Dynamic-memory pointer to output data
                                ! array
      INTEGER      OSLOT        ! Map slot number outputdata array
      INTEGER      STATUS       ! Running status for DSA_ routines
      REAL         VMAX         !
      REAL         VMIN         !
      REAL         XVALS(NH)    !
C
C     Initialisation of DSA_ routines
C
      STATUS=0
      CALL DSA_OPEN(STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get input name
C
      CALL DSA_INPUT('IMAGE','IMAGE',STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get dimensions of input data
C
      CALL DSA_DATA_SIZE('IMAGE',10,NDIM,DIMS,NELM,STATUS)
      IF (NDIM.LT.2) THEN
         CALL PAR_WRUSER('This data is not an image',IGNORE)
         GO TO 500
      END IF
      NY=DIMS(2)
      NX=DIMS(1)
C
C     Map input data
C
      CALL DSA_MAP_DATA('IMAGE','READ','FLOAT',DPTR,DSLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get range of data in image.
C
      CALL GEN_RANGEF(%VAL(CNF_PVAL(DPTR)),1,NELM,DMAX,DMIN)
C
C     Get range to use for optimisation
C
      OK=.FALSE.
      DO WHILE (.NOT.OK)
         CALL PAR_RDVAL('MINVAL',DMIN,DMAX,DMIN,' ',VMIN)
         CALL PAR_RDVAL('MAXVAL',VMIN,1E38,DMAX,' ',VMAX)
         OK=VMAX.GT.VMIN
         IF (.NOT.OK) CALL PAR_WRUSER('Max and min cannot be the same',
     :                                                      STATUS)
      END DO
C
C     Do we just want to look at the histogram, or are we
C     actually going to equalise it?
C
      CALL PAR_RDKEY('EQUALISE',.FALSE.,EQUAL)
C
C     Try to open the graphics soft copy device
C
      CALL VAR_GETCHR('SOFT',0,0,DEVICE,STATUS)
      GRAPHOK=STATUS.EQ.0
      IF (GRAPHOK) THEN
         IF (EQUAL) THEN
            NG=2
         ELSE
            NG=1
         END IF
         STATUS=FIG_PGBEG(LU,DEVICE,1,NG)
         GRAPHOK=STATUS.EQ.1
      END IF
      STATUS=0
C
C     If a new image is to be created, see about opening it.
C
      IF (EQUAL) THEN
C
C        Get output structure name
C
         CALL DSA_OUTPUT('OUTPUT','OUTPUT','IMAGE',0,0,STATUS)
C
C        Map output data
C
         CALL DSA_MAP_DATA('OUTPUT','WRITE','FLOAT',OPTR,OSLOT,STATUS)
      END IF
      IF (STATUS.NE.0) GOTO 500
C
C     Work out the initial histogram
C
      CALL IHOPT1(%VAL(CNF_PVAL(DPTR)),NX,NY,VMIN,VMAX,NH,HIST)
C
C     Display the histogram
C
      IF (GRAPHOK) THEN
         DO I=1,NH
            XVALS(I)=VMIN+FLOAT(I)*(VMAX-VMIN)/FLOAT(NH)
         END DO
         CALL GEN_RANGEF(HIST,1,NH,DMAX,DMIN)
         CALL PGENV(XVALS(1),XVALS(NH),DMIN,DMAX,.FALSE.,1)
         CALL PGLABEL('Data values','Number','Data histogram')
         CALL PGBIN(NH,XVALS,HIST,.TRUE.)
      END IF
C
C     If we are to generate an equalised image, do it
C
      IF (EQUAL) THEN
         CALL IHOPT2(%VAL(CNF_PVAL(DPTR)),NX,NY,VMIN,VMAX,NH,HIST,
     :               %VAL(CNF_PVAL(OPTR)))
C
C        Then generate the new histogram and display it.
C
         IF (GRAPHOK) THEN
            CALL IHOPT1(%VAL(CNF_PVAL(OPTR)),NX,NY,VMIN,VMAX,NH,HIST)
            CALL GEN_RANGEF(HIST,1,NH,DMAX,DMIN)
            CALL PGENV(XVALS(1),XVALS(NH),DMIN,DMAX,.FALSE.,1)
            CALL PGLABEL('Data values','Number',
     :                                     'Equalised histogram')
            CALL PGBIN(NH,XVALS,HIST,.TRUE.)
         END IF
      END IF
C
C     Close down plotting
C
      IF (GRAPHOK) CALL FIG_PGEND
C
C     Closedown everything
C
500   CONTINUE
      CALL DSA_CLOSE(STATUS)
C
      END
C+
      SUBROUTINE IHOPT1 (IN,IX,IY,VMIN,VMAX,NH,HIST)
C
C     I H O P T 1
C
C     First stage histogram optimization of an image.
C     This routine generates a histogram of the count
C     distribution in an image, within a given range.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) IN     (Real array IN(IX,IY)) The image data.
C     (>) IX     (Integer) The first dimension of IN.
C     (>) IY     (Integer) The second dimension of IN.
C     (>) VMIN   (Real) The lowest limit of the range
C                to be considered.
C     (>) VMAX   (Real) The highest limit of the range
C                to be considered.
C     (>) NH     (Integer) The dimension of HIST.
C     (<) HIST   (Real array HIST(NH)) Returns containing
C                the distribution of counts between
C                VMIN (corresponds to HIST(1)) and VMAX
C                corresponds to HIST(NH))
C
C                                    KS / CIT  25th Jan 1983
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER IX,IY,NH
      REAL VMIN,VMAX,HIST(NH),IN(IX*IY)
C                             (1D arrays generate faster code)
C     Local variables
C
      INTEGER I,KBIN,NELM
      REAL RBWID,VAL
C
      DO I=1,NH
         HIST(I)=0.
      END DO
      RBWID=FLOAT(NH)/(VMAX-VMIN)
      NELM=IX*IY
      DO I=1,NELM
         VAL=IN(I)
         IF ((VAL.LE.VMAX).AND.(VAL.GE.VMIN)) THEN
            KBIN=(VAL-VMIN)*RBWID+1
            IF ((KBIN.GE.1).AND.(KBIN.LE.NH)) THEN
               HIST(KBIN)=HIST(KBIN)+1.
            END IF
         END IF
      END DO
C
      END
C+
      SUBROUTINE IHOPT2 (IN,IX,IY,VMIN,VMAX,NH,HIST,OUT)
C
C     Generates a histogram equalised image, given the
C     original image and the histogram of count distributions
C     as generated by subroutine IHOPT1.
C
C     Parameters -  (">" input, "!" modified, "<" output)
C
C     (>) IN     (Real array IN(IX,IY)) The image data.
C     (>) IX     (Integer) The first dimension of IN.
C     (>) IY     (Integer) The second dimension of IN.
C     (>) VMIN   (Real) The lowest limit of the range
C                being considered.
C     (>) VMAX   (Real) The highest limit of the range
C                being considered.
C     (>) NH     (Integer) The dimension of HIST.
C     (!) HIST   (Real array HIST(NH)) Passed containing
C                the distribution of counts between
C                VMIN (corresponds to HIST(1)) and VMAX
C                corresponds to HIST(NH)). Used as workspace
C                - does not return containing much of use.
C     (<) OUT    (Real array OUT(IX,IY)) The equalised image.
C                Data within the range VMAX..VMIN are re-
C                distributed to give an equalised histogram
C                and data outside the range are set to
C                VMIN or VMAX.
C
C                                     KS / CIT 27th Jan 1983
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER IX,IY,NH
      REAL HIST(NH),VMIN,VMAX,IN(IX*IY),OUT(IX*IY)
C                             (1D arrays generate better code)
C     Local variables
C
      INTEGER I,KBIN,NELMS
      REAL HMAX,RBWID,VAL
C
      DO I=2,NH
         HIST(I)=HIST(I)+HIST(I-1)
      END DO
      HMAX=HIST(NH)
      DO I=1,NH
         HIST(I)=HIST(I)/HMAX*(VMAX-VMIN)+VMIN
      END DO
      NELMS=IX*IY
      RBWID=FLOAT(NH)/(VMAX-VMIN)
      DO I=1,NELMS
         VAL=IN(I)
         IF (VAL.GT.VMAX) THEN
            OUT(I)=VAL
         ELSE IF (VAL.LT.VMIN) THEN
            OUT(I)=VAL
         ELSE
            KBIN=MIN(NH,MAX(1,INT((VAL-VMIN)*RBWID+1)))
            OUT(I)=HIST(KBIN)
         END IF
      END DO
C
      END
