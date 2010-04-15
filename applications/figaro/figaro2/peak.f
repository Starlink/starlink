C+
      SUBROUTINE PEAK
C
C     P E A K
C
C     Determines the position and width of the largest peak in a data
C     array.  The data is treated as circular - ie the first element is
C     regarded as adjacent to the last element.  This routine is
C     intended for use analysing the results of cross-correlations,
C     and returns the position in terms of a shift relative to the
C     first element.  The algorithm used is a two step one: first a
C     parabolic fit to the five points closest to the peak gives a
C     value for the shift and width; the shift is then refined by
C     convolution with the derivative of a gaussian.
C
C     Command parameters -
C
C     SPECTRUM (Character) The name of the data to to be analysed.
C
C     User variables -
C
C     (<) SHIFT   (Numeric) The shift of the peak relative to the
C                 center of the first element, in pixels.
C     (<) WIDTH   (Numeric) The width of the peak, again in pixels.
C
C                                               KS / AAO 29th Sept 1986
C
C     Modified:
C
C     20th July 1987  DJA / AAO . Revised DSA_ routines - some
C                     specifications changed.
C     22nd July 1987  DJA / AAO . Modified dynamic memory handling -
C                     now uses DYN_ routines.
C     23rd Sep  1992  HME / UoE, Starlink.  TABs removed, INCLUDE
C                     changed.  Changed WXYFIT to FIG_WXYFIT.
C     2005 June 14    MJC / Starlink  Use CNF_PVAL for pointers to
C                     mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions
C
      INTEGER ICH_ENCODE
C
C     Local variables
C
      INTEGER   DIMS(10)         ! Sizes of the dimensions of the data
      INTEGER   DPTR             ! Dynamic-memory pointer to data array
      INTEGER   DSLOT            ! Map slot number used for data
      INTEGER   IGNORE           ! Used to ignore status codes
      INTEGER   INVOKE           ! Used to invoke functions
      INTEGER   NDIM             ! Dimensionality of input data
      INTEGER   NEXT             ! Dummy arguement for ICH_ routines
      INTEGER   NX               ! Number of elements in 1D array
      INTEGER   NELM             ! Total number of elements in x-axis
      REAL      PARSH            ! Starting shift for parabola fit
      REAL      SHIFT            ! Distance of highest peak from 1st
                                 ! pixel
      INTEGER   STATUS           ! Status return from DSA_xxx routines
      CHARACTER STRING*80        ! Output message string
      REAL      WIDTH            ! Width of highest peak
C
C     Initialisation of DSA_ routines
C
      STATUS=0
      CALL DSA_OPEN(STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get input name
C
      CALL DSA_INPUT ('SPECT','SPECTRUM',STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get dimensions of input data
C
      CALL DSA_DATA_SIZE ('SPECT',1,NDIM,DIMS,NELM,STATUS)
      NX=DIMS(1)
C
C     Map data
C
      CALL DSA_MAP_DATA ('SPECT','READ','FLOAT',DPTR,DSLOT,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Determine position of peak.
C
      CALL FIG_FITPEAK(NX,%VAL(CNF_PVAL(DPTR)),SHIFT,WIDTH,STATUS)
      IF (STATUS.NE.0) THEN
         CALL PAR_WRUSER('Unable to fit peak',STATUS)
         GO TO 500
      END IF
C
C     Use parabolic analysis as starting point for gaussian convolution
C
      PARSH=SHIFT
      CALL FIG_GCPEAK(NX,%VAL(CNF_PVAL(DPTR)),SHIFT,WIDTH,STATUS)
      IF (STATUS.NE.0) THEN
         CALL PAR_WRUSER(
     :     'Failed to apply Gaussian convolution algorithm to data',
     :     STATUS)
         CALL PAR_WRUSER('Will use result from parabola fit.',STATUS)
         SHIFT=PARSH
      END IF
C
C     Output result
C
      CALL PAR_WRUSER(' ',STATUS)
      STRING='Peak is shifted '
      INVOKE=ICH_ENCODE(STRING,SHIFT,17,4,NEXT)
      STRING(NEXT:)=' pixels relative to center of 1st pixel,'
      CALL PAR_WRUSER(STRING(:NEXT+39),IGNORE)
      STRING='and has a width of '
      INVOKE=ICH_ENCODE(STRING,WIDTH,20,3,NEXT)
      STRING(NEXT:)=' pixels.'
      CALL PAR_WRUSER(STRING(:NEXT+7),IGNORE)
      CALL PAR_WRUSER(' ',IGNORE)
C
C     And set user variables
C
      CALL VAR_SETNUM('SHIFT',0,0,SHIFT,STATUS)
      CALL VAR_SETNUM('WIDTH',0,0,WIDTH,STATUS)
C
  500 CONTINUE
C
C     Close everything down
C
      CALL DSA_CLOSE (STATUS)
C
      END
C+
      SUBROUTINE FIG_FITPEAK(NX,DATA,SHIFT,WIDTH,STATUS)
C
C     F I G _ F I T P E A K
C
C     Returns the center and width of the highest peak in a data
C     array that it treats as circular.  This is a utility routine for
C     the Figaro PEAK command.  The procedure uses a parabolic fit to
C     the five points around the peak.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) NX     (Integer) The size of the data array
C     (>) DATA   (Real array DATA(NX)) The data array
C     (<) SHIFT  (Real) The position of the center of the highest peak,
C                given as a shift relative to the center of the first
C                data element.
C     (<) WIDTH  (Real) The width of the peak, in pixels.
C     (<) STATUS (Integer) A status code. 0=> OK, non-zero => unable
C                to find/fit peak.
C
C     Common variables used -  None
C
C     Subroutines / functions used -
C
C     FIG_WXYFIT    (Figaro routine) Least squares polynomiual fit.
C
C                                             KS / AAO 29th Sept 1986
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX, STATUS
      REAL DATA(NX), SHIFT, WIDTH
C
C     Local variables
C
      INTEGER I, IMAX, IX
      REAL CENTER, HEIGHT, X(5), Y(5), W(5), VMAX
      DOUBLE PRECISION A(3)
C
C     First, find position of strongest peak.
C
      VMAX=DATA(1)
      IMAX=1
      DO I=2,NX
         IF (DATA(I).GT.VMAX) THEN
            VMAX=DATA(I)
            IMAX=I
         END IF
      END DO
C
C     Fill work array with data around peak
C
      DO I=1,5
         X(I)=I
         W(I)=1.0D0
         IX=I+IMAX-3
         IF (IX.LT.1) IX=NX+IX
         IF (IX.GT.NX) IX=IX-NX
         Y(I)=DATA(IX)
      END DO
C
C     Perform parabolic fit
C
      CALL FIG_WXYFIT(X,Y,W,5,A,2)
C
C     Determine center and width from coefficients of fitted parabola
C
      CENTER=-A(2)/(2*A(1))
      HEIGHT=A(3)-A(2)*A(2)/(4.0*A(1))
      WIDTH=-2.0*HEIGHT/A(1)
      IF (WIDTH.GT.0.0) THEN
         WIDTH=SQRT(WIDTH)
      ELSE
         WIDTH=-SQRT(-WIDTH)
      END IF
C
C     Convert center into shift
C
      SHIFT=CENTER+FLOAT(IMAX-4)
      IF (SHIFT.GT.FLOAT(NX/2)) SHIFT=SHIFT-FLOAT(NX)
C
C     Status is always good, for this version
C
      STATUS=0
C
      END
C+
      SUBROUTINE FIG_GCPEAK(NX,DATA,SHIFT,WIDTH,STATUS)
C
C     F I G _ G C P E A K
C
C     Returns the center and width of the highest peak in a data
C     array that it treats as circular.  This is a utility routine for
C     the Figaro PEAK command.  The procedure uses convolution with
C     the derivative of a gaussian, using initial results passed to
C     it as the starting point of the analysis.
C
C     Parameters -  (">" input, "!" output, "<" output)
C
C     (>) NX     (Integer) The size of the data array
C     (>) DATA   (Real array DATA(NX)) The data array
C     (!) SHIFT  (Real) The position of the center of the highest peak,
C                given as a shift relative to the center of the first
C                data element.
C     (!) WIDTH  (Real) The width of the peak, in pixels.
C     (<) STATUS (Integer) A status code. 0=> OK, non-zero => unable
C                to find/fit peak.
C
C     Common variables used -  None
C
C     Subroutines / functions used -
C
C     GEN_CENTROID (GEN_ package) Find line peak by convolution with
C                                 the derivative of a Gaussian.
C
C                                             KS / AAO 29th Sept 1986
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX, STATUS
      REAL DATA(NX), SHIFT, WIDTH
C
C     Local variables
C
      INTEGER I, ICENT, IX
      REAL CENT, SIG, STRENGTH, WORK(120)
C
C     Get the central pixel, and build up the data array around it
C     (this allows for the possibility that it is at the very end of
C     the data).
C
      IF (SHIFT.LT.0.0) THEN
         CENT=FLOAT(NX)+SHIFT+1.0
      ELSE
         CENT=SHIFT+1.0
      END IF
      ICENT=CENT
      SIG=WIDTH*0.5
      DO I=1,120
         IX=I+ICENT-60
         IF (IX.LT.1) IX=NX+IX
         IF (IX.GT.NX) IX=IX-NX
         WORK(I)=DATA(IX)
      END DO
C
C     Apply the Gaussian convolution algorithm
C
      CENT=60.0+CENT-ICENT
      CALL GEN_CENTROID(WORK,120,SIG,CENT,STRENGTH,STATUS)
      CENT=CENT-60.0+ICENT
C
C     Convert center value into shift
C
      SHIFT=CENT-1.0
      IF (SHIFT.GT.FLOAT(NX/2)) SHIFT=SHIFT-FLOAT(NX)
C
      END
