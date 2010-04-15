C+
C                           D V D P L O T
C
C  Routine name:
C     DVDPLOT
C
C  Function:
C     Plots the data in one data array against the data in another.
C
C  Description:
C     DVDPLOT (Data Versus Data PLOT) plots the data in the main data
C     array in one Figaro structure against the corresponding data
C     elements of the main data array in another structure.  This was
C     originally written to help with linearity tests (where data in an
C     image taken at a low data rate could be plotted against one taken
C     at a higher data rate), but may have other applications.
C
C  Usage:
C     DVDPLOT IMage IMAGE2 XLow XHigh LOw HIgh AUtoscale
C
C  Parameters:
C     IMAGE     (Character) The name of the first structure.  It is this
C               structure whose data is plotted against the data in
C               IMAGE2, so its data values form the Y values of the
C               plotted points.
C     IMAGE2    (Character) The name of the second structure.  Its data
C               values form the X values of the plotted points.
C     XLOW      (Numeric) The low end of the data range plotted in X
C               (i.e. the lower limit for the data in IMAGE2).
C     XHIGH     (Numeric) The high end of the data range plotted in X
C               (i.e. the upper limit for the data in IMAGE2).
C     LOW       (Numeric) The low end of the data range plotted in Y
C               (i.e. the lower limit for the data in IMAGE).
C     HIGH      (Numeric) The high end of the data range plotted in Y
C               (i.e. the upper limit for the data in IMAGE).
C
C  Keywords:
C     WHOLE     If specified, XLOW and XHIGH will be set to the limits
C               of the data in IMAGE2.
C     AUTOSCALE If specified, LOW and HIGH will be set to the limits
C               of the data in IMAGE.
C     HARDCOPY  If specified, a hard copy plot will be produced.
C
C  User variables used:
C     HARD      (Character) PGPLOT specification for hardcopy plot device
C     SOFT      (Character) PGPLOT specification for softcopy plot device
C
C  Error information:  Ignored.
C
C  Quality information:  Handled using flagged values.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 28th June 1990.
C-
C  History:
C     28th Jun 1990  Original version.  KS / AAO.
C     5th  Oct 1992  HME / UoE, Starlink.  INCLUDE changed.
C     11th Jan 1995  HME / UoE, Starlink. Passive AGI compliance,
C                    use FIG_PGBEG/END. Actually do call FIG_PGEND.
C                    Before, PGEND was not called.
C     6th  Aug 1997  MJCL / Starlink, UCL.  Explicit typecast of
C                    the fourth argument to PGPOINT due to Solaris
C                    problem.
C     2005 June 7    MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C+
      SUBROUTINE DVDPLOT
C
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Floating point limits
C
      REAL FMAX, FMIN
      PARAMETER (FMAX=1.7E38, FMIN=-1.7E38)
C
C     Local variables
C
      LOGICAL   AUTO              ! Value of AUTOSCALE keyword
      CHARACTER DEVICE*32         ! Plot device name (PGPLOT spec)
      INTEGER   DIMS(10)          ! Data dimensions (ignored)
      LOGICAL   FAULT             ! Indicates a non-DSA error
      REAL      FLAG              ! Flag value used for data
      LOGICAL   HARD              ! Valuer of HARDCOPY keyword
      REAL      HIGH              ! Higher data value to be plotted in Y
      INTEGER   IGNORE            ! Status value we don't care about
      INTEGER   IPTR              ! Dynamic memory address of IMAGE data
      INTEGER   IPTR2             ! Dynamic memory address of IMAGE2 data
      REAL      LOW               ! Lower data value to be plotted in Y
      INTEGER   NDIM              ! Number of data dimensions - ignored
      INTEGER   NELM              ! Number of data elements
      INTEGER   SLOT              ! DSA mapping slot - ignored
      CHARACTER SOFT*32           ! Soft plot device name (PGPLOT spec)
      INTEGER   STATUS            ! Inherited status used by DSA routines
      INTEGER   VSTATUS           ! Status returned by VAR routines
      LOGICAL   WHOLE             ! Value of WHOLE keyword
      REAL      XHIGH             ! Higher X-value to be plotted
      REAL      XLOW              ! Lower X-value to be plotted
C
C     Initial values
C
      FAULT=.FALSE.
C
C     Initialise DSA routines
C
      STATUS=0
      CALL DSA_OPEN (STATUS)
C
C     See if a SOFT device has been specified.  (We prefer to do this
C     test now, because otherwise we may go through quite a lot of
C     effort only to find we can't do the plot.  Even if HARD is
C     going to be used, we might as well test for a soft device.)
C
      CALL VAR_GETCHR ('SOFT',0,0,SOFT,VSTATUS)
      IF (VSTATUS.NE.0) THEN
         CALL PAR_WRUSER ('No soft plot device specified.',IGNORE)
         CALL PAR_WRUSER ('Use the SOFT command to set one.',IGNORE)
         FAULT=.TRUE.
         GO TO 500      ! Error exit
      END IF
C
C     Open the two images
C
      CALL DSA_INPUT ('IMAGE','IMAGE',STATUS)
      CALL DSA_INPUT ('IMAGE2','IMAGE2',STATUS)
C
C     Check that the dimensions of the images match, and get the number
C     of elements in each.
C
      CALL DSA_MATCH_SIZES ('IMAGE','IMAGE2',STATUS)
      CALL DSA_DATA_SIZE ('IMAGE',10,NDIM,DIMS,NELM,STATUS)
C
C     We really can't risk plotting a bad point in one array against
C     a good point in the other, so we can't just ignore quality
C     data.  We handle it through flagged values, which is probably
C     easiest.
C
      CALL DSA_USE_FLAGGED_VALUES ('IMAGE',STATUS)
      CALL DSA_USE_FLAGGED_VALUES ('IMAGE2',STATUS)
      CALL DSA_GET_FLAG_VALUE ('FLOAT',FLAG,STATUS)
C
C     Map the two main data arrays
C
      CALL DSA_MAP_DATA ('IMAGE','READ','FLOAT',IPTR,SLOT,STATUS)
      CALL DSA_MAP_DATA ('IMAGE2','READ','FLOAT',IPTR2,SLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500    ! Error exit
C
C     See if 'WHOLE' was specified.  If so, get range of IMAGE2 data.
C     Otherwise get the range from XLOW and XHIGH.
C
      CALL PAR_RDKEY ('WHOLE',.FALSE.,WHOLE)
      IF (WHOLE) THEN
         CALL DVDPLOT_RANGEF (%VAL(CNF_PVAL(IPTR2)),NELM,FLAG,XHIGH,
     :                        XLOW)
         CALL PAR_SDVAL ('XLOW',XLOW,IGNORE)
         CALL PAR_SDVAL ('XHIGH',XHIGH,IGNORE)
      ELSE
         CALL PAR_RDVAL ('XLOW',FMIN,FMAX,0.0,' ',XLOW)
         CALL PAR_RDVAL ('XHIGH',XLOW,FMAX,1000.0,' ',XHIGH)
      END IF
      IF (XHIGH.EQ.XLOW) THEN
         IF (XLOW.EQ.0.0) THEN
            XHIGH=0.01
         ELSE
            XHIGH=ABS(XLOW)*1.1
         END IF
      END IF
C
C     Similarly, if 'AUTOSCALE' was specified, then calculate HIGH and
C     LOW from the data in IMAGE (noting that we only consider the
C     elements corresponding to elements of IMAGE2 between XHIGH
C     and XLOW).
C
      CALL PAR_RDKEY ('AUTOSCALE',.FALSE.,AUTO)
      IF (AUTO) THEN
         CALL DVDPLOT_RANGE (NELM,%VAL(CNF_PVAL(IPTR)),
     :                       %VAL(CNF_PVAL(IPTR2)),
     :                       FLAG,XHIGH,XLOW,HIGH,LOW)
         CALL PAR_SDVAL ('LOW',LOW,IGNORE)
         CALL PAR_SDVAL ('HIGH',HIGH,IGNORE)
      ELSE
         CALL PAR_RDVAL ('LOW',FMIN,FMAX,0.0,' ',LOW)
         CALL PAR_RDVAL ('HIGH',LOW,FMAX,1000.0,' ',HIGH)
      END IF
      IF (HIGH.EQ.LOW) THEN
         IF (LOW.EQ.0.0) THEN
            HIGH=0.01
         ELSE
            HIGH=ABS(LOW)*1.1
         END IF
      END IF
C
C     See if HARDCOPY has been specified.
C
      CALL PAR_RDKEY ('HARDCOPY',.FALSE.,HARD)
      IF (HARD) THEN
         CALL VAR_GETCHR ('HARD',0,0,DEVICE,VSTATUS)
         IF (VSTATUS.NE.0) THEN
            CALL PAR_WRUSER ('No hardcopy device specified.',IGNORE)
            CALL PAR_WRUSER ('Use the HARD command to set one',IGNORE)
            CALL PAR_WRUSER ('Will produce a soft copy plot instead',
     :                                                         IGNORE)
            DEVICE=SOFT
         END IF
      ELSE
         DEVICE=SOFT
      END IF
C
C     Now do the real work.
C
      CALL DVDPLOT_PLOT (NELM,%VAL(CNF_PVAL(IPTR)),
     :                   %VAL(CNF_PVAL(IPTR2)),
     :                   FLAG,XLOW,XHIGH,LOW,HIGH,DEVICE)
C
C     Close everything down.
C
  500 CONTINUE
      CALL DSA_CLOSE(STATUS)
      IF (FAULT) CALL FIG_SETERR
C
      END
C+
      SUBROUTINE DVDPLOT_RANGEF (ARRAY,NELM,FLAG,VMAX,VMIN)
C
C     D V D P L O T _ R A N G E F
C
C     Utility routine used by DVDPLOT.  Gets the range of data values
C     in one array (ARRAY) between two specified elements, ignoring
C     any flagged values.  GEN_RANGEF can't be used because of the
C     requirement to handle flags, but there ought to be some general
C     routine to do this.
C
C     Parameters:   (">" input, "<" output)
C
C     (>) ARRAY     (Real array ARRAY(NELM)).  The array whose range is
C                   to be determined.
C     (>) NELM      (Integer) Number of array elements.
C     (>) FLAG      (Real) The value used to flag 'bad' pixels.
C     (<) VMAX      (Real) The higher limit of the values in ARRAY.
C     (<) VMIN      (Real) The lower limit of the values in ARRAY.
C
C                                              KS / AAO 28th June 1990.
C+
      IMPLICIT NONE
C
C     Parameters.
C
      INTEGER NELM
      REAL ARRAY(NELM), FLAG, VMAX, VMIN
C
C     Local variables
C
      INTEGER I           ! Loop index through array
      INTEGER I1          ! First non-flagged element
C
C     Get first non-flagged element
C
      I1=0
      DO I=1,NELM
         IF (ARRAY(I).NE.FLAG) THEN
            I1=I
            GO TO 320    ! Break out of I loop
         END IF
      END DO
  320 CONTINUE
C
C     Get range of rest of array
C
      IF (I1.EQ.0) THEN
         VMAX=0.0
         VMIN=0.0
      ELSE
         VMAX=ARRAY(I1)
         VMIN=ARRAY(I1)
         DO I=I1+1,NELM
            IF (ARRAY(I).NE.FLAG) THEN
               IF (ARRAY(I).LT.VMIN) THEN
                  VMIN=ARRAY(I)
               ELSE
                  IF (ARRAY(I).GT.VMAX) VMAX=ARRAY(I)
               END IF
            END IF
         END DO
      END IF
C
      END
C+
      SUBROUTINE DVDPLOT_RANGE (NELM,ARRAY,ARRAY2,FLAG,XHIGH,XLOW,
     :                                                      HIGH,LOW)
C
C     D V D P L O T _ R A N G E
C
C     Utility routine used by DVDPLOT.  Gets the range of data values
C     in one array (ARRAY) for which the corresponding values in a
C     second array (ARRAY2) are in a given range.  Flagged values in
C     either array are ignored.
C
C     Parameters:   (">" input, "<" output)
C
C     (>) NELM      (Integer) Number of array elements.
C     (>) ARRAY     (Real array ARRAY(NELM)).  The array whose range is
C                   to be determined.
C     (>) ARRAY2    (Real array ARRAY2(NELM)).  The second array.
C     (>) FLAG      (Real) The value used to flag 'bad' pixels.
C     (>) XHIGH     (Real) The higher limit of the values in ARRAY2 to use.
C     (>) XLOW      (Real) The lower limit of the values in ARRAY2 to use.
C     (<) HIGH      (Real) The higher limit of the values in ARRAY.
C     (<) LOW       (Real) The lower limit of the values in ARRAY.
C
C                                              KS / AAO 28th June 1990.
C+
      IMPLICIT NONE
C
C     Parameters.
C
      INTEGER NELM
      REAL ARRAY(NELM), ARRAY2(NELM), FLAG, XHIGH, XLOW, HIGH, LOW
C
C     Local variables
C
      INTEGER I                     ! General loop index
      INTEGER I1                    ! First element with ARRAY2 value in range
C
C     Get range of values.  Start by finding first value that qualifies.
C
      I1=0
      DO I=1,NELM
         IF ((ARRAY2(I).NE.FLAG).AND.(ARRAY(I).NE.FLAG).AND.
     :             (ARRAY2(I).GE.XLOW).AND.(ARRAY2(I).LE.XHIGH)) THEN
            I1=I
            GO TO 320     ! Break out of I loop
         END IF
      END DO
  320 CONTINUE
C
C     Now get range of remaining qualifying values
C
      IF (I1.EQ.0) THEN
         LOW=0.0
         HIGH=0.0
      ELSE
         LOW=ARRAY(I1)
         HIGH=ARRAY(I1)
         DO I=I1+1,NELM
            IF ((ARRAY2(I).NE.FLAG).AND.(ARRAY(I).NE.FLAG).AND.
     :             (ARRAY2(I).GE.XLOW).AND.(ARRAY2(I).LE.XHIGH)) THEN
               IF (ARRAY(I).GT.HIGH) THEN
                  HIGH=ARRAY(I)
               ELSE
                  IF (ARRAY(I).LT.LOW) LOW=ARRAY(I)
               END IF
            END IF
         END DO
      END IF
C
      END
C+
      SUBROUTINE DVDPLOT_PLOT (NELM,ARRAY,ARRAY2,FLAG,
     :                                 XLOW,XHIGH,LOW,HIGH,DEVICE)
C
C     D V D P L O T _ P L O T
C
C     DVDPLOT utility.  Plots the values in one array against those
C     in another, within the limits specified and on the device specified.
C     Points flagged in either array are ignored.
C
C     Parameters:   (">" input, "<" output)
C
C     (>) NELM      (Integer) Number of array elements.
C     (>) ARRAY     (Real array ARRAY(NELM)).  The array whose values
C                   give the Y value of the plotted points.
C     (>) ARRAY2    (Real array ARRAY2(NELM)).  The second array, whose
C                   values give the X values of the plotted points.
C     (>) FLAG      (Real) The value used to flag 'bad' pixels.
C     (>) XHIGH     (Real) The higher limit of the values in ARRAY2 to use.
C     (>) XLOW      (Real) The lower limit of the values in ARRAY2 to use.
C     (>) HIGH      (Real) The higher limit of the values in ARRAY.
C     (>) LOW       (Real) The lower limit of the values in ARRAY.
C     (>) DEVICE    (Character string) The PGPLOT specification for
C                   the device to be used.
C
C                                              KS / AAO 28th June 1990.
C+
      IMPLICIT NONE
C
C     Parameters.
C
      INTEGER NELM
      REAL ARRAY(NELM), ARRAY2(NELM), FLAG, XHIGH, XLOW, HIGH, LOW
      CHARACTER*(*) DEVICE
C
C     Functions
C
      INTEGER FIG_PGBEG
C
C     Local variables
C
      INTEGER I                ! Loop index through elements
      INTEGER STATUS           ! Status from opening of plotting device
C
C     Open the plotting device
C
      STATUS=FIG_PGBEG(0,DEVICE,1,1)
      IF (STATUS.EQ.1) THEN
         CALL PGENV (XLOW,XHIGH,LOW,HIGH,0,0)
         DO I=1,NELM
            IF ((ARRAY2(I).GE.XLOW).AND.(ARRAY2(I).LE.XHIGH)) THEN
               IF ((ARRAY(I).GE.LOW).AND.(ARRAY(I).LE.HIGH)) THEN
                  IF ((ARRAY(I).NE.FLAG).AND.(ARRAY2(I).NE.FLAG)) THEN
                     CALL PGPOINT (1,ARRAY2(I),ARRAY(I),ICHAR( '+' ) )
                  END IF
               END IF
            END IF
         END DO
         CALL FIG_PGEND
      END IF
C
      END
