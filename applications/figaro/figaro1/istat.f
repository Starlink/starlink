C+
      SUBROUTINE ISTAT
C
C     I S T A T
C
C     Examines an image (or a subset of an image) an determines
C     a number of useful statistics about the data in it, such as
C     the mean, max and minimum values.  For a full list, see below
C     under 'user variables'.
C
C     Command parameters -
C
C     IMAGE  (Char) The name of the structure containing the image.
C     YSTART (Numeric) The Y value for the start of the subset.
C     YEND   (Numeric) The Y value for the end of the subset.
C     XSTART (Numeric) The X value for the start of the subset.
C     XEND   (Numeric) The X value for the end of the subset.
C
C     Command keywords
C
C     PASS2   If specified, the sigma value is calculated using two
C             passes throught the data.  The one-pass algorithm normally
C             used is prone to rounding error when large numbers of
C             pixels are involved, but is rather faster.  If fewer than
C             10,000 pixels are involved, ISTAT always uses two passes,
C             since the other overheads dominate the time taken.
C     MEDIAN  If specified, the median value of the image data is
C             calculated (a possibly time-consuming business).
C
C     User variables -   ("<" output)
C
C     (<) STAT_TOTAL   The sum of the data.
C     (<) STAT_MIN     The minimum data value.
C     (<) STAT_MAX     The maximum data value.
C     (<) STAT_MEAN    The mean data value.
C     (<) STAT_MEDIAN  The median data value (set if MEDIAN specified)
C     (<) STAT_XMAX    The x-value of the pixel where the max was found.
C     (<) STAT_YMAX    The y-  "   "   "    "     "     "   "   "    "
C     (<) STAT_XMIN    The x-  "   "   "    "     "     " min   "    "
C     (<) STAT_YMIN    The y-  "   "   "    "     "     "   "   "    "
C     (<) STAT_SIGMA   The standard distribution of the data
C     (<) STAT_SIZE    The number of pixels examined
C     (<) STAT_XSTART  The first pixel number examined in X
C     (<) STAT_XEND    The last    "     "        "     " X
C     (<) STAT_YSTART  The first pixel number examined in Y
C     (<) STAT_YEND    The last    "     "        "     " Y
C
C                                      KS / CIT 7th June 1984
C     Modified:
C
C     07 May 1985  KS / AAO.  X and Y limits (in pixels) now used to
C                  set user variables.
C     30 Jun 1986  KS / AAO.  Error handling revised to make sure
C                  files closed under error conditions.
C     19 Mar 1987  KS / AAO.  PASS2 keyword added and call to GEN_ASTAT2
C                  added.  (Note, GEN_ASTAT also modified, so default
C                  single pass calculation is slightly better anyway.)
C     20 Jul 1987  DJA / AAO. Revised DSA_ routines - some
C                  specifications changed. Updated all WRUSER calls to
C                  PAR_WRUSERs.
C     22 Jul 1987  DJA / AAO. Modifed dynamic memory routines - now use
C                  DYN_ package.
C     31 Jul 1987  KS / AAO.  Added call to DSA_SET_RANGE.
C     05 Apr 1989  KS / AAO.  MEDIAN added.
C     30 Mar 1991  KS / AAO.  Fix integer division bug if !! given in
C                  response to IMAGE prompt.  Y range no longer listed
C                  for 1D data. Range info includes axis values if
C                  known.
C     07 Oct 1992  HME / UoE, Starlink.  INCLUDE changed, TABs
C                  removed.
C     21 Feb 1996  HME / UoE, Starlink. Convert to FDA:
C                  No concurrent mapping. Had to swap mapping axis data
C                  and getting axis range.
C     21 May 1997  MJCL / Starlink, UCL.
C                  Moved Y-range string output into the test for
C                  Y-range.
C     2005 June 8  MJC / Starlink  Use CNF_PVAL for pointers to
C                  mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions
C
      LOGICAL PAR_ABORT
      INTEGER ICH_ENCODE
      REAL    GEN_ELEMF
C
C     Local variables
C
      REAL      AMAX             ! The maximum data value
      REAL      AMIN             ! The minimum data value
      INTEGER   DDIMS(10)        ! Sizes of the dimensions of the data
      INTEGER   DPTR             ! Dynamic-memory pointer to data array
      INTEGER   DSLOT            ! Map slot number used for data
      INTEGER   IGNORE           ! Used to ignore status codes
      INTEGER   IXEN             ! Last pixel to be counted along x-axis
      INTEGER   IXST             ! First "    "   "     "     "      "
      INTEGER   IYEN             ! Last  "    "   "     "     "   y-axis
      INTEGER   IYST             ! First "    "   "     "     "      "
      REAL      MEAN             ! Average value of a pixel in the image
      LOGICAL   MEDIAN           ! Value of the MEDIAN keyword
      REAL      MEDVAL           ! Value of median of data
      INTEGER   NDELM            ! Total number of elements in the data
      INTEGER   NDIM             ! Dimensionality of data array
      INTEGER   NEXT             ! ICH_KEY arguement - ignored
      INTEGER   NSUB             ! Number of elements in subset
      INTEGER   NX               ! Size of 1st dimension of data
      INTEGER   NY               ! Size of 2nd dimension of data
      LOGICAL   PASS2            ! TRUE if a second pass is to be done
      REAL      SIGMA            ! Standard distribution of data
      REAL      SIZE             ! Number of pixels examined
      INTEGER   STATUS           ! Status return from DSA_xxx routines
      CHARACTER STRING*80        ! Output string
      REAL      TOTAL            ! Total of all values in pixels
      REAL      VALUE            ! Axis data value
      INTEGER   WPTR             ! Dynamic mem element for work array
      INTEGER   WSLOT            ! Slot number for workspace - ignored
      LOGICAL   XEXIST           ! True if X-axis info exists
      REAL      XMAX             ! Pixel x-co-ord of 'brightest' pixel
      REAL      XMIN             !   "        "   "    'lowest'    "
      LOGICAL   YEXIST           ! True if Y-axis info exists
      REAL      YMAX             ! Pixel y-co-ord of 'brightest' pixel
      REAL      YMIN             !   "        "   "    'lowest'    "
      INTEGER   XSLOT            ! Map slot number used for x-axis info
      INTEGER   XPTR             ! Dynamic-memory pointer to x-axis data
      INTEGER   YPTR             ! Dynamic-memory pointer to y-axis data
      INTEGER   YSLOT            ! Map slot number used for y-axis info
C
C     Initialisation of DSA_ routines
C
      STATUS=0
      CALL DSA_OPEN(STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get input name
C
      CALL DSA_INPUT ('IMAGE','IMAGE',STATUS)
C
C     Get dimensions of input data
C
      CALL DSA_DATA_SIZE ('IMAGE',2,NDIM,DDIMS,NDELM,STATUS)
C
C     Map input data
C
      CALL DSA_MAP_DATA ('IMAGE','READ','FLOAT',DPTR,DSLOT,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     See if we have a data component giving Y-values
C
      NX=DDIMS(1)
      NY=NDELM/NX
      IF (NY.GT.1) THEN
C
C        Get limits in Y.
C
         CALL DSA_AXIS_RANGE ('IMAGE',2,' ',.FALSE.,YMIN,YMAX,
     :                        IYST,IYEN,STATUS)
         CALL DSA_SEEK_AXIS ('IMAGE',2,YEXIST,STATUS)
         CALL DSA_MAP_AXIS_DATA ('IMAGE',2,'READ','FLOAT',YPTR,
     :                           YSLOT,STATUS)
      ELSE
         IYST=1
         IYEN=1
      END IF
      IF (STATUS.NE.0) GOTO 500
C
C     And ditto for x-axis
C
      CALL DSA_AXIS_RANGE ('IMAGE',1,' ',.FALSE.,XMIN,XMAX,
     :                     IXST,IXEN,STATUS)
      CALL DSA_SEEK_AXIS ('IMAGE',1,XEXIST,STATUS)
      CALL DSA_MAP_AXIS_DATA ('IMAGE',1,'READ','FLOAT',XPTR,
     :                        XSLOT,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     See if 2-pass option is required.
C
      IF (((IXEN-IXST+1)*(IYEN-IYST+1)).LE.10000) THEN
         PASS2=.TRUE.
      ELSE
         CALL PAR_RDKEY('PASS2',.FALSE.,PASS2)
      END IF
C
C     See if median required.  If it is, get the necessary workspace
C     for a copy of the data.
C
      CALL PAR_RDKEY ('MEDIAN',.FALSE.,MEDIAN)
      IF (PAR_ABORT()) GO TO 500       ! User requested abort
      IF (MEDIAN) THEN
         NSUB=(IYEN-IYST+1)*(IXEN-IXST+1)
         CALL DSA_GET_WORK_ARRAY (NSUB,'FLOAT',WPTR,WSLOT,STATUS)
         IF (STATUS.NE.0) GO TO 500    ! Error exit
      END IF
C
C     Get the statistics
C
      IF (PASS2) THEN
         CALL GEN_ASTAT2(%VAL(CNF_PVAL(DPTR)),NX,NY,IXST,IXEN,IYST,
     :                   IYEN,TOTAL,AMAX,AMIN,MEAN,XMAX,XMIN,YMAX,
     :                   YMIN,SIGMA,SIZE)
      ELSE
         CALL GEN_ASTAT(%VAL(CNF_PVAL(DPTR)),NX,NY,IXST,IXEN,IYST,
     :                  IYEN,TOTAL,AMAX,AMIN,MEAN,XMAX,XMIN,YMAX,
     :                  YMIN,SIGMA,SIZE)
      END IF
C
C     If the median is required, the data has to be copied into
C     the work array so it can be sorted.
C
      IF (MEDIAN) THEN
         CALL FIG_MED2D(%VAL(CNF_PVAL(DPTR)),NX,NY,IXST,IXEN,IYST,IYEN,
     :                  %VAL(CNF_PVAL(WPTR)),MEDVAL)
      END IF
C
C     Set the range values in the structure, if all the data was used.
C
      IF ((IXST.EQ.1)  .AND. (IYST.EQ.1).AND.
     :    (IXEN.EQ.NX) .AND. (IYEN.EQ.NY)) THEN
         CALL DSA_SET_RANGE ('IMAGE',AMIN,AMAX,STATUS)
      END IF
C
C     Format and output the information
C
      CALL PAR_WRUSER(' ',IGNORE)
      IF (NY.GT.1) THEN
         STRING='Y-range '
         IGNORE=ICH_ENCODE(STRING,FLOAT(IYST),9,0,NEXT)
         IF (YEXIST.AND.(NDIM.EQ.2)) THEN
            STRING(NEXT:)=' ('
            VALUE=GEN_ELEMF(%VAL(CNF_PVAL(YPTR)),IYST)
            IGNORE=ICH_ENCODE(STRING,VALUE,NEXT+2,3,NEXT)
            STRING(NEXT:)=')'
            NEXT=NEXT+1
         END IF
         STRING(NEXT:)=' to '
         IGNORE=ICH_ENCODE(STRING,FLOAT(IYEN),NEXT+4,0,NEXT)
         IF (YEXIST.AND.(NDIM.EQ.2)) THEN
            STRING(NEXT:)=' ('
            VALUE=GEN_ELEMF(%VAL(CNF_PVAL(YPTR)),IYEN)
            IGNORE=ICH_ENCODE(STRING,VALUE,NEXT+2,3,NEXT)
            STRING(NEXT:)=')'
            NEXT=NEXT+1
         END IF
         CALL PAR_WRUSER(STRING,IGNORE)
      END IF
      STRING='X-range '
      IGNORE=ICH_ENCODE(STRING,FLOAT(IXST),9,0,NEXT)
      IF (XEXIST) THEN
         STRING(NEXT:)=' ('
         VALUE=GEN_ELEMF(%VAL(CNF_PVAL(XPTR)),IXST)
         IGNORE=ICH_ENCODE(STRING,VALUE,NEXT+2,3,NEXT)
         STRING(NEXT:)=')'
         NEXT=NEXT+1
      END IF
      STRING(NEXT:)=' to '
      IGNORE=ICH_ENCODE(STRING,FLOAT(IXEN),NEXT+4,0,NEXT)
      IF (XEXIST) THEN
         STRING(NEXT:)=' ('
         VALUE=GEN_ELEMF(%VAL(CNF_PVAL(XPTR)),IXEN)
         IGNORE=ICH_ENCODE(STRING,VALUE,NEXT+2,3,NEXT)
         STRING(NEXT:)=')'
         NEXT=NEXT+1
      END IF
      CALL PAR_WRUSER(STRING,IGNORE)
      STRING='Total (over '
      IGNORE=ICH_ENCODE(STRING,SIZE,13,4,NEXT)
      STRING(NEXT:)=' pixels) = '
      STATUS=ICH_ENCODE(STRING,TOTAL,NEXT+11,4,NEXT)
      CALL PAR_WRUSER(STRING,IGNORE)
      STRING='Max   = '
      IGNORE=ICH_ENCODE(STRING,AMAX,9,4,NEXT)
      NEXT=MAX(NEXT,17)
      STRING(NEXT:)=' in pixel ('
      IGNORE=ICH_ENCODE(STRING,XMAX,NEXT+11,0,NEXT)
      STRING(NEXT:NEXT)=','
      IGNORE=ICH_ENCODE(STRING,YMAX,NEXT+1,0,NEXT)
      STRING(NEXT:)=')'
      CALL PAR_WRUSER(STRING,IGNORE)
      STRING='Min   = '
      IGNORE=ICH_ENCODE(STRING,AMIN,9,4,NEXT)
      NEXT=MAX(NEXT,17)
      STRING(NEXT:)=' in pixel ('
      IGNORE=ICH_ENCODE(STRING,XMIN,NEXT+11,0,NEXT)
      STRING(NEXT:NEXT)=','
      IGNORE=ICH_ENCODE(STRING,YMIN,NEXT+1,0,NEXT)
      STRING(NEXT:)=')'
      CALL PAR_WRUSER(STRING,IGNORE)
      STRING='Mean  = '
      IGNORE=ICH_ENCODE(STRING,MEAN,9,4,NEXT)
      CALL PAR_WRUSER(STRING,IGNORE)
      STRING='Sigma = '
      IGNORE=ICH_ENCODE(STRING,SIGMA,9,5,NEXT)
      CALL PAR_WRUSER(STRING,IGNORE)
      IF (MEDIAN) THEN
         STRING='Median  = '
         IGNORE=ICH_ENCODE(STRING,MEDVAL,9,4,NEXT)
         CALL PAR_WRUSER(STRING,IGNORE)
      END IF
      CALL PAR_WRUSER(' ',IGNORE)
C
C     Now set the user variables
C
      CALL VAR_SETNUM('STAT_TOTAL',0,0,TOTAL,STATUS)
      CALL VAR_SETNUM('STAT_MAX',0,0,AMAX,STATUS)
      CALL VAR_SETNUM('STAT_MIN',0,0,AMIN,STATUS)
      CALL VAR_SETNUM('STAT_MEAN',0,0,MEAN,STATUS)
      CALL VAR_SETNUM('STAT_SIGMA',0,0,SIGMA,STATUS)
      CALL VAR_SETNUM('STAT_SIZE',0,0,SIZE,STATUS)
      CALL VAR_SETNUM('STAT_XMAX',0,0,XMAX,STATUS)
      CALL VAR_SETNUM('STAT_XMIN',0,0,XMIN,STATUS)
      CALL VAR_SETNUM('STAT_YMIN',0,0,YMIN,STATUS)
      CALL VAR_SETNUM('STAT_YMAX',0,0,YMAX,STATUS)
      CALL VAR_SETNUM('STAT_XSTART',0,0,FLOAT(IXST),STATUS)
      CALL VAR_SETNUM('STAT_XEND',0,0,FLOAT(IXEN),STATUS)
      CALL VAR_SETNUM('STAT_YSTART',0,0,FLOAT(IYST),STATUS)
      CALL VAR_SETNUM('STAT_YEND',0,0,FLOAT(IYEN),STATUS)
      IF (MEDIAN) CALL VAR_SETNUM('STAT_MEDIAN',0,0,MEDVAL,STATUS)
C
C     Close everything down.
C
  500 CONTINUE
C
      CALL DSA_CLOSE(STATUS)
C
      END
C+
      SUBROUTINE FIG_MED2D(DATA,NX,NY,IXST,IXEN,IYST,IYEN,
     :                                           WORK,MEDVAL)
C
C     F I G _ M E D 2 D
C
C     Calculates the median value in a given subset of an image.
C
C     Parameters -  (">" input, "W" workspace, "<" output)
C
C     (>) DATA     (Real array DATA(NX,NY)) The image in question.
C     (>) NX       (Integer) The first dimension of the image.
C     (>) NY       (Integer) The second dimension of the image.
C     (>) IXST     (Integer) The first element in the x-direction to
C                  be included in the subset.
C     (>) IXEN     (Integer) The last element in the x-direction to
C                  be included in the subset.
C     (>) IYST     (Integer) The first element in the y-direction to
C                  be included in the subset.
C     (>) IYEN     (Integer) The last element in the y-direction to
C                  be included in the subset.
C     (W) WORK     (Real array) Workspace.  Needs to have as many
C                  elements as are included in the subset.
C     (<) MEDVAL   (Real) The calculated median value.
C
C                                                  KS / AAO 5th May 1989.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX,NY,IXST,IXEN,IYST,IYEN
      REAL DATA(NX,NY),WORK(*),MEDVAL
C
C     Functions
C
      REAL GEN_QFMED
C
C     Local variables
C
      INTEGER IX,IY,NELM
C
C     Copy subset elements into array.
C
      NELM=0
      DO IY=IYST,IYEN
         DO IX=IXST,IXEN
            NELM=NELM+1
            WORK(NELM)=DATA(IX,IY)
         END DO
      END DO
C
C     Calculate median
C
      MEDVAL=GEN_QFMED(WORK,NELM)
C
      END
