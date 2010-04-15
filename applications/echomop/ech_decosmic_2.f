      SUBROUTINE ECH_DECOSMIC_2(
     :           IMAGE,
     :           QUALITY,
     :           NX,
     :           NY,
     :           N_ORDERS,
     :           MAXIMUM_POLY,
     :           TRACE_POLYNOMIAL,
     :           MAX_SKY_PIXELS,
     :           OBJ_MASK,
     :           SKY_MASK,
     :           DEK_BELOW,
     :           DEK_ABOVE,
     :           FIT_ORDER,
     :           FIT_POLY,
     :           SIGMA_THRESH,
     :           PROB_THRESH,
     :           SKY_SPECTRUM,
     :           X_TRACE_COORD,
     :           Y_TRACE_COORD,
     :           DATA,
     :           DATA_SRT,
     :           XAXIS,
     :           YAXIS,
     :           GAUSSIAN,
     :           INDEX_X,
     :           INDEX_Y,
     :           ENERGY,
     :           PERCENTAGE,
     :           FRACTION,
     :           W_TO_FIT,
     :           COUNT_PER_X,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_DECOSMIC_2

*  Purpose:
*     Automatic cosmic ray identifier.

*  Description:
*     Determines the spatial profile, and the degree to which each
*     pixel exceeds its expected intensity consistent with the overall
*     flux at that X-increment.  The worst n-sigma pixels are rejected,
*     and the whole process repeated iteratively until the CDF converges
*     with the probability predicted using a gaussian distribution.

*  Invocation:
*     CALL ECH_DECOSMIC_2(
*     :    IMAGE,
*     :    QUALITY,
*     :    NX,
*     :    NY,
*     :    N_ORDERS,
*     :    MAXIMUM_POLY,
*     :    TRACE_POLYNOMIAL,
*     :    MAX_SKY_PIXELS,
*     :    OBJ_MASK,
*     :    SKY_MASK,
*     :    DEK_BELOW,
*     :    DEK_ABOVE,
*     :    FIT_ORDER,
*     :    FIT_POLY,
*     :    SIGMA_THRESH,
*     :    PROB_THRESH,
*     :    SKY_SPECTRUM,
*     :    X_TRACE_COORD,
*     :    Y_TRACE_COORD,
*     :    DATA,
*     :    DATA_SRT,
*     :    XAXIS,
*     :    YAXIS,
*     :    GAUSSIAN,
*     :    INDEX_X,
*     :    INDEX_Y,
*     :    ENERGY,
*     :    PERCENTAGE,
*     :    FRACTION,
*     :    W_TO_FIT,
*     :    COUNT_PER_X,
*     :    STATUS
*     :   )

*  Arguments:
*     IMAGE = REAL (Given)
*        Input frame image of dimensions nx columns and ny rows.
*     NX = INTEGER (Given)
*        Number of columns in frame.
*     NY = INTEGER (Given)
*        Number of rows in frame.
*     N_ORDERS = INTEGER (Given)
*        Number of orders in echellogram.
*     DEK_BELOW = INTEGER (Given)
*        Dekker distance below order traces.
*     DEK_ABOVE = INTEGER (Given)
*        Dekker distance above order traces.
*     MAX_SKY_PIXELS = INTEGER (Given)
*        Maximum number of pixels perpendicular to order.
*     TRACE_POLYNOMIAL = DOUBLE (Given)
*        Polynomial coefficients of order traces.
*     OBJ_MASK = INTEGER (Given)
*        Object status of increments below/above order.
*     SKY_MASK = INTEGER (Given)
*        Object status of increments below/above order.
*     FIT_ORDER = INTEGER (Given)
*        Degree of polynomial to fit to fractions.
*     SIGMA_THRESH = REAL (Given)
*        Clipping point threshold in sigma.
*     PROB_THRESH = REAL (Given)
*        Probability threshold at which to stop clipping.
*     SKY_SPECTRUM = REAL (Returned)
*        Sky spectrum estimates.
*     DATA = REAL ( TEMPORARY WORKSPACE )
*        Ratios of observed/predicted energy.
*     DATA_SRT = REAL ( TEMPORARY WORKSPACE )
*        Ratios of observed/predicted energy.
*     YAXIS = REAL ( TEMPORARY WORKSPACE )
*        Theoretical CDF values.
*     XAXIS = REAL ( TEMPORARY WORKSPACE )
*        Theoretical data points.
*     GAUSSIAN = REAL ( TEMPORARY WORKSPACE )
*        Theoretical gaussian values.
*     INDEX_X = SHORT ( TEMPORARY WORKSPACE )
*        X coords of pixels.
*     INDEX_Y = SHORT ( TEMPORARY WORKSPACE )
*        Y coords of pixels.
*     X_TRACE_COORD = DOUBLE ( TEMPORARY WORKSPACE )
*        X coordinates of trace.
*     Y_TRACE_COORD = DOUBLE ( TEMPORARY WORKSPACE )
*        Y coordinates of trace.
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.
*     QUALITY = LOGICAL (Given)
*        Input data frame quality flags array.
*     MAXIMUM_POLY = INTEGER (Given)
*        Maximum number of fit coefficients allowed.
*     FIT_POLY = DOUBLE (Given and Returned)
*        Polynomial fits to object profile (workspace).
*     ENERGY = REAL (Given and Returned)
*        Total counts in an increment.
*     PERCENTAGE = REAL (Given and Returned)
*        Percentage of energy present.
*     FRACTION = REAL (Given and Returned)
*        Fraction of energy used.
*     W_TO_FIT = REAL (Given and Returned)
*        Fit workspace.
*     COUNT_PER_X = INTEGER (Given and Returned)
*        Pixels used per increment.

*  Method:
*     Remove any previous flagged cosmic rays
*     Initialising phase-of-process flags
*     Loop through object/sky processing
*        Loop through all orders
*           If order has a fitted trace then
*              Calculate path of order
*              Loop Setting up flag array for pixels to be used this offset
*                 If pixel should be processed then
*                    Setup flag and count usable pixels in image
*                    If any good pixels at all then
*                       Increment used counter
*                    Else set ignore-this-increment flag
*                    Endif
*                 Endif
*              End loop
*              Set default expectation values to all increments identical
*              Loop until no more pixels to clip from this order
*                 If enough values in the data set then
*                 Else
*                    Flag this increment as non-processable, stop clipping
*                 Endif
*              End loop
*              Display results, fit polynomials
*              If required, estimate sky
*           Endif
*        End loop
*        If sky clean done, switch to object clean
*        Else set sky cleaned flag
*        Endif
*     End loop
*     Restore any suspected sky line pixels

*  Authors:
*     DMILLS: Dave Mills (UCL, Starlink)
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     01-SEP-1992 (DMILLS):
*       Initial release.
*     18-JUL-1996 (MJC):
*       New prologue, tidy.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_REPORT.INC'
      INCLUDE 'ECH_DATA_CONSTRAINTS.INC'
      INCLUDE 'ECH_ENVIR_CONSTANTS.INC'
      INCLUDE 'ECH_QUALITIES.INC'

*  Arguments Given:
      INTEGER NX
      INTEGER NY
      INTEGER N_ORDERS
      REAL IMAGE( NX, NY )
      BYTE QUALITY( NX, NY )
      INTEGER DEK_BELOW( N_ORDERS )
      INTEGER DEK_ABOVE( N_ORDERS )
      INTEGER MAXIMUM_POLY
      INTEGER MAX_SKY_PIXELS
      DOUBLE PRECISION TRACE_POLYNOMIAL( MAXIMUM_POLY, N_ORDERS )
*          ! Polynomial coefficients of order traces.
      INTEGER FIT_ORDER
      REAL SIGMA_THRESH
      REAL PROB_THRESH

*  Arguments Returned:
      INTEGER OBJ_MASK( -MAX_SKY_PIXELS/2 : MAX_SKY_PIXELS/2, N_ORDERS )
*          ! Object status of increments below/above order.
      INTEGER SKY_MASK( -MAX_SKY_PIXELS/2 : MAX_SKY_PIXELS/2, N_ORDERS )
*          ! Sky status of increments below/above order.
      REAL SKY_SPECTRUM( NX, N_ORDERS )

*  Workspace variables used:
      REAL ENERGY( NX )
      REAL PERCENTAGE( NX )
      REAL FRACTION( NX )
      REAL W_TO_FIT( NX )
      INTEGER COUNT_PER_X( NX )

*  Workspace:
      REAL DATA( NX * MAX_SLICE_PIXELS )
*          ! Ratios of observed/predicted energy.
      REAL DATA_SRT( NX * MAX_SLICE_PIXELS )
*          ! Sorted Ratios of observed/predicted energy.
      REAL YAXIS( NX * MAX_SLICE_PIXELS )
*          ! Theoretical CDF values.
      REAL XAXIS( NX * MAX_SLICE_PIXELS )
*          ! Theoretical data points.
      REAL GAUSSIAN( NX * MAX_SLICE_PIXELS )
*          ! Theoretical gaussian values.
      INTEGER*2 INDEX_X( NX * MAX_SLICE_PIXELS )
*          ! X coords of pixels.
      INTEGER*2 INDEX_Y( NX * MAX_SLICE_PIXELS )
*          ! Y coords of pixels.
      DOUBLE PRECISION X_TRACE_COORD( NX )
      DOUBLE PRECISION Y_TRACE_COORD( NX )

*  Status:
      INTEGER STATUS

*  Local Variables:
      DOUBLE PRECISION FIT_POLY( MAXIMUM_POLY, -MAX_SLICE_PIXELS / 2:
     :                 MAX_SLICE_PIXELS / 2, N_ORDERS )
*          ! Fitted polynomials modeling increments.
      DOUBLE PRECISION PROB
      DOUBLE PRECISION LAST_PROB

      REAL EXPECTED_PERCENT( -MAX_SLICE_PIXELS/2 : MAX_SLICE_PIXELS/2 )
*          ! Predicted percentage per order increment.
      REAL ENERGY_PER_INC( -MAX_SLICE_PIXELS/2 : MAX_SLICE_PIXELS/2 )
*          ! Predicted energy per order increment.
      REAL MAX
      REAL MEAN
      REAL SIGMA
      REAL MEAN_ENERGY

      INTEGER COUNT( -MAX_SLICE_PIXELS / 2 : MAX_SLICE_PIXELS / 2 )
*          ! Count of good pixels per order increment.
      INTEGER I
      INTEGER IY
      INTEGER IX
      INTEGER IY_DELTA
      INTEGER MCOUNT
      INTEGER USED_PIXELS
      INTEGER IQUALITY
      INTEGER CLIPPED_COUNT
      INTEGER ISIZE
      INTEGER IORD
      INTEGER PCOUNT
      INTEGER PIXEL_COUNT

      LOGICAL IGNORED ( -MAX_SLICE_PIXELS/2 : MAX_SLICE_PIXELS/2 )
*          ! TRUE if order increment has no good pixels.
      LOGICAL PROCESS ( -MAX_SLICE_PIXELS/2 : MAX_SLICE_PIXELS/2 )
*          ! TRUE if increment is being processed.
      LOGICAL CLIP
      LOGICAL OBJ_CLEANED
      LOGICAL SKY_CLEANED

*  Functions called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

*  Remove any previous flagged cosmic rays.
      DO IY = 1, NY
         DO IX = 1, NX
            IQUALITY = QUALITY( IX,IY )
            IF ( IAND( IQUALITY, QTY_COSMIC_RAY ) .EQ.
     :           QTY_COSMIC_RAY ) THEN
               QUALITY( IX, IY ) = QUALITY( IX, IY ) - QTY_COSMIC_RAY
            END IF
         END DO
      END DO

*  Cleaning proceeds in two phases, first the 'object' regions and then
*  the 'sky'.

*  Initialising phase-of-process flags.
      OBJ_CLEANED = .FALSE.
      SKY_CLEANED = .FALSE.

*  Loop through object/sky processing.
      DO WHILE ( .NOT. OBJ_CLEANED )

*     Loop through all orders.
         DO IORD = 1, N_ORDERS

*        If this order has no trace then do nothing.
            IF ( TRACE_POLYNOMIAL( 1, IORD ) .EQ. ECH__BAD_DOUBLE ) THEN
               GO TO 100
            END IF

*        Evaluate order trace path.
            CALL ECH_CALC_TRACE( NX, MAXIMUM_POLY,
     :           TRACE_POLYNOMIAL( 1, IORD ),
     :           X_TRACE_COORD, Y_TRACE_COORD, STATUS )
            CLIP = .TRUE.
            CLIPPED_COUNT = 0
            LAST_PROB = 0.0
            USED_PIXELS = 0

*        Loop Setting up flag array for pixels to be used at this offset.
            DO IY_DELTA = DEK_BELOW( IORD ), DEK_ABOVE( IORD )
               IGNORED( IY_DELTA ) = .FALSE.
               PROCESS( IY_DELTA ) = .FALSE.

*           If pixel should be processed then.
               IF ( ( ( SKY_MASK( IY_DELTA, IORD ) .GT. 0 ) .AND.
     :              .NOT. SKY_CLEANED ) .OR.
     :              ( OBJ_MASK( IY_DELTA, IORD ) .GT. 0) .AND.
     :              SKY_CLEANED ) THEN

*              Setup flag and count usable pixels in image.
                  PROCESS( IY_DELTA )  = .TRUE.
                  PCOUNT = 0
                  DO I = 1, NX
                     IY = INT( Y_TRACE_COORD( I ) + 0.5 ) + IY_DELTA
                     IF ( IY .GT. 0 .AND. IY .LE. NY ) THEN
                       IF ( QUALITY( I, IY ) .EQ. 0 )
     :                    PCOUNT = PCOUNT + 1
                     ENDIF
                  END DO

*              If any good pixels at all then increment used counter.
                  IF ( PCOUNT .GT. 0 ) THEN
                     USED_PIXELS = USED_PIXELS + 1

*              Else set ignore-this-increment flag.
                  ELSE
                     IGNORED( IY_DELTA ) = .TRUE.
                  ENDIF
               ENDIF
            END DO

*        Set default expectation values to all increments identical.
            DO IY_DELTA = DEK_BELOW( IORD ), DEK_ABOVE( IORD )
               IF ( PROCESS( IY_DELTA ) .AND.
     :              .NOT. IGNORED( IY_DELTA ) ) THEN
                  EXPECTED_PERCENT( IY_DELTA ) = 1.0 /
     :                  FLOAT( USED_PIXELS )
               END IF
            END DO

*        Loop until no more pixels to clip from this order.
            DO WHILE ( CLIP )
               CALL ECH_DECOS_PREDICT(
     :              NX, NY, IMAGE, QUALITY, ENERGY, ENERGY_PER_INC,
     :              COUNT_PER_X, PERCENTAGE, DEK_BELOW( IORD ),
     :              DEK_ABOVE( IORD ), PROCESS, IGNORED, COUNT,
     :              EXPECTED_PERCENT, PIXEL_COUNT, MEAN, SIGMA,
     :              MCOUNT, MEAN_ENERGY, Y_TRACE_COORD, DATA,
     :              INDEX_X, INDEX_Y, STATUS )

*            If enough values in the data set.
               IF ( PIXEL_COUNT .GT. 20 ) THEN
                  CALL ECH_DECOS_THEORETICAL(
     :                 NX, IORD, PIXEL_COUNT, MCOUNT, MEAN, SIGMA,
     :                 MEAN_ENERGY, PROB, DATA, DATA_SRT, XAXIS,
     :                 YAXIS, GAUSSIAN, STATUS )
                  CALL ECH_DECOS_PROB(
     :                 PROB, PROB_THRESH, NX, NY, QUALITY, MEAN,
     :                 SIGMA, SIGMA_THRESH, MAX, IORD, PIXEL_COUNT,
     :                 LAST_PROB, DATA, DATA_SRT, XAXIS, YAXIS,
     :                 GAUSSIAN, INDEX_X,INDEX_Y, CLIPPED_COUNT,
     :                 CLIP, STATUS )

*           Flag this increment as non-processable, stop clipping.
               ELSE
                  CLIP = .FALSE.
               ENDIF
            END DO

*        Display results, fit polynomials.
            CALL ECH_DECOS_FIT_PROFINC(
     :           NX, NY, IMAGE, QUALITY, DEK_BELOW( IORD ),
     :           DEK_ABOVE( IORD ), PROCESS, COUNT, ENERGY,
     :           MAXIMUM_POLY,
     :           FIT_POLY( 1,-MAX_SLICE_PIXELS / 2,IORD ),
     :           FIT_ORDER, DATA_SRT, FRACTION, XAXIS, YAXIS,
     :           W_TO_FIT, Y_TRACE_COORD, STATUS )

*        If required, estimate sky.
            IF ( .NOT. SKY_CLEANED )
     :          CALL ECH_DECOS_ESTIMATE_SKY(
     :               NX, NY, IMAGE, QUALITY, DEK_BELOW( IORD ),
     :               DEK_ABOVE( IORD ), PROCESS, COUNT_PER_X,
     :               SKY_SPECTRUM( 1, IORD ), Y_TRACE_COORD,
     :               STATUS )
  100       CONTINUE
         END DO

*     If sky clean done, switch to object clean.
         IF ( SKY_CLEANED ) THEN
            OBJ_CLEANED = .TRUE.

*     Else set sky cleaned flag.
         ELSE
           SKY_CLEANED = .TRUE.
         ENDIF
      END DO

*  Restore any suspected sky line pixels.
      IORD  = N_ORDERS / 2
      IF ( IORD .EQ. 0 ) IORD = 1
      ISIZE = DEK_ABOVE( IORD ) - DEK_BELOW( IORD ) - 1
      CALL ECH_DECOS_SKYLINE( NX, NY, ISIZE, IMAGE, QUALITY, STATUS )

      END


      SUBROUTINE SORT1( IA, N )
*+
*-
      IMPLICIT NONE
      INTEGER N
      real ia( n )
      real it
      INTEGER INT, IFIN
      INTEGER I, II, J

      int=2
   10 int=2*int
      if(int.lt.n)goto 10
      int=min0(n,(3*int)/4-1)
   20 int=int/2
      ifin=n-int
      do 70 ii=1,ifin
      i=ii
      j=i+int
      if(ia(i).le.ia(j))goto 70
      it=ia(j)
   40 ia(j)=ia(i)
      j=i
      i=i-int
      if(i.le.0)goto 60
      if(ia(i).gt.it)goto 40
   60 ia(j)=it
   70 continue
      if(int.gt.1)goto 20
      return
      end


      REAL FUNCTION POISSON( MEAN, COUNT )
*+
*-
      IMPLICIT NONE
      REAL MEAN
      REAL COUNT

      INTEGER FACTORIAL
      EXTERNAL FACTORIAL

      POISSON = ( mean ** count  /
     :            FLOAT( FACTORIAL( INT( count ) ) )
     :           ) * EXP( -mean )

      END


      INTEGER FUNCTION FACTORIAL( COUNT )
*+
*-
      IMPLICIT NONE
      INTEGER COUNT
      INTEGER RESULT( 0 : 12 )

      DATA RESULT
     :      / 1, 1, 2, 6, 24, 120, 720, 5040, 40320, 36288,
     :        3628800, 39916800, 479001600 /

      IF ( COUNT .LE. 12 ) THEN
         FACTORIAL = RESULT( COUNT )
      ENDIF

      END

      real function probks( alam )
*+
*-
      parameter( eps1 = 0.001, eps2 = 1.e-8 )

      a2=-2.*alam**2
      fac=2.
      probks=0.
      termbf=0.
      do j=1,100
         term=fac*exp(a2*j**2)
         probks=probks+term
         if(abs(term).le.eps1*termbf.or.abs(term).le.eps2*probks)return
         fac=-fac
         termbf = ABS( term )
      END DO
      probks = 0.0

      end
