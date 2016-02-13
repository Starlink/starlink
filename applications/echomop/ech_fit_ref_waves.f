      SUBROUTINE ECH_FIT_REF_WAVES(
     :           NX,
     :           MAX_FEATURES,
     :           FTR_LIST,
     :           START_WAVELENGTH,
     :           END_WAVELENGTH,
     :           EXTRACTED_REF,
     :           INTERACTIVE,
     :           ORDER,
     :           ORDER_IDNUM,
     :           MIN_DISPERSION,
     :           MAX_DISPERSION,
     :           NO_OF_POSITIONS,
     :           WAVE_NPOLY,
     :           MAX_PERM_FTRS,
     :           FTR_POSITIONS,
     :           INPUT_FTR_POSITIONS,
     :           OBS_STRENGTH,
     :           STRENGTH_FACTOR,
     :           BEST_FTR,
     :           BEST_NEXT_FTR,
     :           BEST_NEXT_FTR2,
     :           BEST_PREV_FTR,
     :           BEST_PREV_FTR2,
     :           BEST_POSINDEX,
     :           META_GUESS,
     :           META_FTRS,
     :           META_COUNT,
     :           MAXIMUM_POLY,
     :           FITTER,
     :           FINAL_WAVE_COEFFS,
     :           IDENTIFIED_FTRS,
     :           IDEN_FTR_POSITION,
     :           IDEN_FTR_STATUS,
     :           IDEN_FTR_WAVELENGTH,
     :           FIT_WAVES,
     :           FIT_WAVES2,
     :           FIT_WAVES_WORK,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_FIT_REF_WAVES

*  Purpose:
*     Fit function to identified arc line wavelength/positions.

*  Invocation:
*     CALL ECH_FIT_REF_WAVES(
*     :    NX,
*     :    MAX_FEATURES,
*     :    FTR_LIST,
*     :    START_WAVELENGTH,
*     :    END_WAVELENGTH,
*     :    EXTRACTED_REF,
*     :    INTERACTIVE,
*     :    ORDER,
*     :    ORDER_IDNUM,
*     :    MIN_DISPERSION,
*     :    MAX_DISPERSION,
*     :    NO_OF_POSITIONS,
*     :    WAVE_NPOLY,
*     :    MAX_PERM_FTRS,
*     :    FTR_POSITIONS,
*     :    INPUT_FTR_POSITIONS,
*     :    OBS_STRENGTH,
*     :    STRENGTH_FACTOR,
*     :    BEST_FTR,
*     :    BEST_NEXT_FTR,
*     :    BEST_NEXT_FTR2,
*     :    BEST_PREV_FTR,
*     :    BEST_PREV_FTR2,
*     :    BEST_POSINDEX,
*     :    META_GUESS,
*     :    META_FTRS,
*     :    META_COUNT,
*     :    MAXIMUM_POLY,
*     :    FITTER,
*     :    FINAL_WAVE_COEFFS,
*     :    IDENTIFIED_FTRS,
*     :    IDEN_FTR_POSITION,
*     :    IDEN_FTR_STATUS,
*     :    IDEN_FTR_WAVELENGTH,
*     :    FIT_WAVES,
*     :    FIT_WAVES2,
*     :    FIT_WAVES_WORK,
*     :    STATUS
*     :   )

*  Arguments:
*    NX = INTEGER (Given)
*        Number of columns in frame.
*    START_WAVELENGTH = REAL (Given)
*        Start search window wavelength.
*    END_WAVELENGTH = REAL (Given)
*        End search window wavelength.
*    EXTRACTED_REF = REAL (Given)
*        Extracted reference spectrum.
*    INTERACTIVE = LOGICAL (Given)
*        TRUE if in interactive mode.
*    ORDER = INTEGER (Returned)
*        Order being processed.
*    MIN_DISPERSION = INTEGER (Returned)
*        Minimum dispersion.
*    MAX_DISPERSION = INTEGER (Returned)
*        Maximum dispersion.
*    BEST_FTR = INTEGER (Returned)
*        Feature identifications.
*    BEST_POSINDEX = BYTE (Returned)
*        Indicies of 4 neighbours composing the meta-feature.
*    BEST_PREV_FTR = INTEGER (Returned)
*        Prev neighbour prediction for candidates.
*    BEST_PREV_FTR2 = INTEGER (Returned)
*        2nd prev neighbour prediction for candidates.
*    BEST_NEXT_FTR = INTEGER (Returned)
*        Next neighbour prediction for candidates.
*    BEST_NEXT_FTR2 = INTEGER (Returned)
*        2nd neighbour prediction for candidates.
*    MAX_PERM_FTRS = INTEGER (Given)
*        Maximum number of features to use in a fit.
*    INPUT_FTR_POSITIONS = REAL (Given)
*        Observed central positions of features.
*    OBS_STRENGTH = REAL (Given)
*        Observed strengths (usually intensity) of features.
*    STRENGTH_FACTOR = REAL (Given)
*        Minimum strength of fitted features.
*    NO_OF_POSITIONS = INTEGER (Given)
*        Number of features observed.
*    FTR_POSITIONS = INTEGER (Given)
*        Active features.
*    META_GUESS = INTEGER (Given)
*        Candidate indices for solution features.
*    META_FTRS = INTEGER (Given)
*        Feature indices for solution features.
*    META_COUNT = INTEGER (Given)
*        Number of features in solution.
*    WAVE_NPOLY = INTEGER (Given)
*        Default order of wavelength polynomial fit.
*    MAXIMUM_POLY = INTEGER (Given)
*        Maximum order of wavelength polynomial fit.
*    FINAL_WAVE_COEFFS = DOUBLE (Returned)
*        Wavelength polynomial coefficients.
*    IDENTIFIED_FTRS = INTEGER (Returned)
*        Count of identified features.
*    IDEN_FTR_POSITION = REAL (Returned)
*        Positions identified features.
*    IDEN_FTR_STATUS = INTEGER (Returned)
*        Statuses of identified features.
*    IDEN_FTR_WAVELENGTH = REAL (Returned)
*        Wavelengths of identified features.
*    FIT_WAVES = REAL (Temporary Workspace)
*        Work array for polynoimal fitting.
*    FIT_WAVES2 = REAL (Temporary Workspace)
*        Work array for polynoimal fitting.
*    FIT_WAVES_WORK = REAL (Temporary Workspace)
*        Work array for polynoimal fitting.
*    MAX_FEATURES = INTEGER (Given)
*        Maximum number of observed features per order.
*    FTR_LIST = REAL (Given)
*        List of known arc line wavelengths.
*    ORDER_IDNUM = INTEGER (Given)
*        Echelle order number.
*    FITTER = CHAR (Given)
*        Type of wavelength fitting function in use (POLY/SPLINE).
*    STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

*  Bugs:
*     None known.

*  Authors:
*     Dave Mills STARLINK (ZUVAD::DMILLS)

*  History:
*     1992 Sept 1 : Initial release

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_REPORT.INC'
      INCLUDE 'ECH_DATA_CONSTRAINTS.INC'
      INCLUDE 'ECH_ENVIRONMENT.INC'
      INCLUDE 'ECH_FEATURE.INC'

*  Arguments Given:
      INTEGER MAX_FEATURES
      REAL FTR_LIST( MAX_FEATURES )
      INTEGER NX
      REAL START_WAVELENGTH
      REAL END_WAVELENGTH
      REAL EXTRACTED_REF( NX )
      LOGICAL INTERACTIVE
      INTEGER ORDER
      INTEGER ORDER_IDNUM
      INTEGER NO_OF_POSITIONS
      INTEGER MAX_PERM_FTRS
      REAL FTR_POSITIONS( MAX_ALLOWED_RF_FEAT )
*          ! Active features used in a fit.
      REAL INPUT_FTR_POSITIONS( MAX_PERM_FTRS )
*          ! Observed central positions of features.
      REAL OBS_STRENGTH( MAX_PERM_FTRS )
*          ! Observed strengths (usually intensity) of features.
      REAL STRENGTH_FACTOR
      INTEGER WAVE_NPOLY
      INTEGER MAXIMUM_POLY
      CHARACTER*( * ) FITTER
      INTEGER META_FTRS( MAX_ID_FTRS )
      INTEGER META_GUESS( MAX_ID_FTRS )
      INTEGER META_COUNT
      INTEGER IDENTIFIED_FTRS

*  Arguments Returned:
      REAL MIN_DISPERSION
      REAL MAX_DISPERSION
      DOUBLE PRECISION FINAL_WAVE_COEFFS( MAXIMUM_POLY )
*          ! Wavelength polynomial coefficients.
      REAL IDEN_FTR_POSITION( MAX_PERM_FTRS )
*          ! Positions identified features.
      REAL IDEN_FTR_WAVELENGTH( MAX_PERM_FTRS )
*          ! Positions identified features.
      INTEGER IDEN_FTR_STATUS( MAX_PERM_FTRS )
*          ! Positions identified features.
      BYTE BEST_POSINDEX( MAX_FTR_CAND, 4, MAX_ID_FTRS )
*          ! Count of times a candidate is seen.
      INTEGER BEST_PREV_FTR( MAX_FTR_CAND, MAX_ID_FTRS )
*          ! Prev neighbour prediction for candidates.
      INTEGER BEST_PREV_FTR2( MAX_FTR_CAND, MAX_ID_FTRS )
*          ! 2nd prev neighbour for candidates.
      INTEGER BEST_NEXT_FTR( MAX_FTR_CAND, MAX_ID_FTRS )
*          ! Next neigb prediction for candidates.
      INTEGER BEST_NEXT_FTR2( MAX_FTR_CAND, MAX_ID_FTRS )
*          ! 2nd neighb for candidates.
      INTEGER BEST_FTR( MAX_FTR_CAND, MAX_ID_FTRS )
*          ! DB index for candidates.

*  Workspace:
      REAL FIT_WAVES( NX*4 )
      REAL FIT_WAVES2( NX*4 )
      REAL FIT_WAVES_WORK( NX*4 )

*  Status:
      INTEGER STATUS

*  Local Variables:
      DOUBLE PRECISION TEMP_WAVE_COEFFS( MAX_FIT_COEFFS )

      REAL FTR_WAVELENGTHS( MAX_FIT_FTRS )
      REAL CHANS( MAX_FIT_FTRS )
      REAL WAVES( MAX_FIT_FTRS )
      REAL WWEIGHTS( MAX_FIT_FTRS )
      REAL RMSES( MAX_FIT_FTRS )
      REAL FITS( MAX_FIT_FTRS )
      REAL FTR_WAVELENGTH2( MAX_FIT_FTRS )
      REAL WDUMMY( 2 )
      REAL BEST_START
      REAL BEST_END
      REAL OVERALL_DISPERSION
      REAL WAVE_DELTA
      REAL MAX_STRENGTH
      REAL POSSIBLE_WAVELENGTH
      REAL WAVE_FIT_DELTA
      REAL RMS
      REAL RMS_LIMIT
      REAL NEAR1
      REAL ORIGIN
      REAL PER_PIXEL

      INTEGER STATI( MAX_FIT_FTRS )
      INTEGER I
      INTEGER II
      INTEGER ID_FEATURE
      INTEGER LOW_NCOEFF
      INTEGER SAVE_NPOLY
      INTEGER TEMP_NPOLY
      INTEGER USE_NPOLY
      INTEGER NLID
      INTEGER BLENDS
      INTEGER INEAR1
      INTEGER INEAR2
      INTEGER NCHAR1
      INTEGER NCHAR2
      INTEGER NCHAR3

      LOGICAL AUTOCLIP

      CHARACTER*16 REF_STR1
      CHARACTER*16 REF_STR2
      CHARACTER*16 REF_STR3
      CHARACTER*6 SFITTER

      COMMON / KEEP_POLY /  SAVE_NPOLY

*  Functions Called:
      REAL ARC_ARFIND
      EXTERNAL ARC_ARFIND
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

*  Set bad status return by default.
      STATUS = -1
      WDUMMY( 1 ) = 1.0
      SAVE_NPOLY = MAX( SAVE_NPOLY, WAVE_NPOLY )
      IF ( WAVE_NPOLY .LT. 2 ) WAVE_NPOLY = SAVE_NPOLY
      DO I = 1, MAXIMUM_POLY
         TEMP_WAVE_COEFFS( I ) = FINAL_WAVE_COEFFS( I )
      END DO
      DO I = MAXIMUM_POLY + 1, MAX_FIT_COEFFS
         TEMP_WAVE_COEFFS( I ) = 0.0
      END DO

*  Default weights for each feature set to 1.0.
      DO I = 1, MAX_FIT_FTRS
         WWEIGHTS( I ) = 1.0
      END DO

      IF ( NO_OF_POSITIONS .EQ. 0 ) THEN
         DO WHILE ( NO_OF_POSITIONS .LT. MAX_PERM_FTRS .AND.
     :              INPUT_FTR_POSITIONS( NO_OF_POSITIONS+1 ) .GT. 0.0 )
            NO_OF_POSITIONS = NO_OF_POSITIONS + 1
         END DO
      END IF

      IF ( META_COUNT .GT. 1 ) THEN

*     Calculate estimate of dispersion and wavelength range.
         OVERALL_DISPERSION = (
     :         FTR_POSITIONS( META_FTRS( META_COUNT ) ) -
     :         FTR_POSITIONS( META_FTRS( 1 ) ) ) /
     :         ( FTR_LIST( BEST_FTR( META_GUESS( META_COUNT ),
     :         META_FTRS( META_COUNT ) ) ) -
     :         FTR_LIST( BEST_FTR( META_GUESS( 1 ), META_FTRS( 1 ) ) ) )

         BEST_START = FTR_LIST( BEST_FTR( META_GUESS( 1 ),
     :         META_FTRS( 1 ) ) ) - FTR_POSITIONS( META_FTRS( 1 ) ) /
     :         OVERALL_DISPERSION

         BEST_END = BEST_START + FLOAT( NX ) / OVERALL_DISPERSION

*     Check dispersion and wavelength limits here and give up if no good.
         OVERALL_DISPERSION = ABS( OVERALL_DISPERSION )
         IF ( OVERALL_DISPERSION .LT. MIN_DISPERSION .OR.
     :        OVERALL_DISPERSION .GT. MAX_DISPERSION .OR.
     :        BEST_START .LT. START_WAVELENGTH .OR.
     :        BEST_END .GT. END_WAVELENGTH ) THEN
            CALL ECH_REPORT( 0,
     :           ' Solution fails range/scale criteria.' )
            STATUS = ECH__AUTO_IDENTIFY
            GO TO 999

         ELSE

*        Calculate reciprocal (used to estimate hoped-for accuracy of fit).
            RMS_LIMIT = 1.0 / OVERALL_DISPERSION

*        Report initial results of identification.
            CALL CHR_RTOC( FLOAT( INT( OVERALL_DISPERSION * 10000.0 )
     :           ) / 10000.0, REF_STR1, NCHAR1 )
            CALL CHR_RTOC( BEST_START, REF_STR2, NCHAR2 )
            CALL CHR_RTOC( BEST_END, REF_STR3, NCHAR3 )
            REPORT_STRING = ' Dispersion: ' // REF_STR1( :NCHAR1 ) //
     :            ' from W=' // REF_STR2( :NCHAR2 ) //
     :            ' to ' // REF_STR3( :NCHAR3 ) // '.'
            CALL ECH_REPORT( 0, REPORT_STRING )
            WRITE ( REPORT_STRING, 1001 ) RMS_LIMIT
            CALL ECH_REPORT( 0, REPORT_STRING )

*        Setup details of first two features (specified as left hand side
*        neighbours of the first identified meta-feature).
            CHANS( 1 ) = FTR_POSITIONS( BEST_POSINDEX(
     :            META_GUESS( 1 ), 1, META_FTRS( 1 ) ) )
            CHANS( 2 ) = FTR_POSITIONS ( BEST_POSINDEX(
     :            META_GUESS( 1 ), 2, META_FTRS( 1 ) ) )
            WAVES( 1 ) = FTR_LIST( BEST_PREV_FTR2( META_GUESS( 1 ),
     :            META_FTRS( 1 ) ) )
            WAVES( 2 ) = FTR_LIST( BEST_PREV_FTR( META_GUESS ( 1 ),
     :            META_FTRS( 1 ) ) )

*        Setup details (wavelength/position) for identified features.
            DO I = 1, META_COUNT
               CHANS( I + 2 ) = FTR_POSITIONS( META_FTRS( I ) )
               WAVES( I + 2 ) = FTR_LIST( BEST_FTR( META_GUESS( I ),
     :               META_FTRS( I ) ) )
            END DO

*        Setup details of last two features (specified as right hand side
*        neighbours of the last identified meta-feature).
            CHANS( META_COUNT + 3 ) = FTR_POSITIONS( BEST_POSINDEX(
     :            META_GUESS( META_COUNT ), 3,
     :            META_FTRS( META_COUNT ) ) )
            CHANS( META_COUNT + 4 ) = FTR_POSITIONS(  BEST_POSINDEX(
     :            META_GUESS( META_COUNT ), 4,
     :            META_FTRS( META_COUNT ) ) )
            WAVES( META_COUNT + 3 ) =
     :            FTR_LIST( BEST_NEXT_FTR( META_GUESS( META_COUNT ),
     :            META_FTRS( META_COUNT ) ) )
            WAVES( META_COUNT + 4 ) =
     :            FTR_LIST( BEST_NEXT_FTR2( META_GUESS( META_COUNT ),
     :            META_FTRS( META_COUNT ) ) )

*        Set number of polynomial coeffiecients to maximum for first fit.
            NLID = META_COUNT + 4
            LOW_NCOEFF = MAX( 2, MIN( SAVE_NPOLY, NLID / 2 ) )
            IF ( FITTER .EQ. 'SPLINE' ) LOW_NCOEFF = 16
            AUTOCLIP = .FALSE.

*        Fit a polynomial through the feature wavelength/position data.
            DO I = 1, MAX_FIT_COEFFS
               TEMP_WAVE_COEFFS( I ) = 0.0
            END DO
            RMS = 1.0E6
            IF ( LOW_NCOEFF .LT. 2 ) LOW_NCOEFF = 2
            IF ( NLID .GT. 0 )
     :         CALL ECH_WAVE_POLYFIT( CHANS, WAVES, STATI, WWEIGHTS,
     :              RMSES, FITS, AUTOCLIP, NLID, NX, MAX_FIT_COEFFS,
     :              .TRUE., LOW_NCOEFF, TEMP_WAVE_COEFFS, RMS )
            LOW_NCOEFF = MAX( 2, MIN( SAVE_NPOLY, NLID / 2 ) )
            IF ( FITTER .EQ. 'SPLINE' ) LOW_NCOEFF = 16

*        Report results.
            WRITE ( REPORT_STRING, 1002 ) NLID, RMS
            CALL ECH_REPORT( 0, REPORT_STRING )

*        Generate a wavelength scale using the polynomial.
            DO I = 1, NX
               FIT_WAVES_WORK( I ) = FLOAT( I )
            END DO
            CALL ECH_FEVAL( ' ', MAXIMUM_POLY, TEMP_WAVE_COEFFS,
     :           NX, FIT_WAVES_WORK, FIT_WAVES, STATUS )

            MAX_STRENGTH = 1E-20
            DO I = 1, NO_OF_POSITIONS
               IF ( OBS_STRENGTH( I ) .GT. MAX_STRENGTH )
     :            MAX_STRENGTH = OBS_STRENGTH( I )
            END DO

*        Loop through all observed features.
            IDENTIFIED_FTRS = 0
            WAVE_FIT_DELTA = RMS_LIMIT
            DO I = 1, NO_OF_POSITIONS

*        Predict wavelength for feature position.
            LOW_NCOEFF = MAX( 2, MIN( SAVE_NPOLY, NLID / 2 ) )
            IF ( FITTER .EQ. 'SPLINE' ) LOW_NCOEFF = 16
            CALL ECH_FEVAL( FITTER, LOW_NCOEFF, TEMP_WAVE_COEFFS, 1,
     :           INPUT_FTR_POSITIONS( I ), FTR_WAVELENGTHS( I ),
     :           STATUS )
            LOW_NCOEFF = MAX( 2, MIN( SAVE_NPOLY, NLID / 2 ) )
            IF ( FITTER .EQ. 'SPLINE' ) LOW_NCOEFF = 16

*        Loop through all identified features.
            DO II = 1, NLID

*           If feature wavelength known, use it.
               IF ( INT( INPUT_FTR_POSITIONS( I ) * 10000.0 )
     :              .EQ. INT( CHANS( II ) * 10000.0 ) ) THEN
                   FTR_WAVELENGTHS( I ) = WAVES( II )
               END IF
            END DO

            FTR_WAVELENGTH2( I ) = 0.0
            INEAR1 = 0
            INEAR2 = 0
            NEAR1 = 1.0E20
            IF ( NLID .GT. 1 ) THEN
               DO ID_FEATURE = 1, NLID
                  IF ( ABS( INPUT_FTR_POSITIONS( I ) -
     :                 WAVES( ID_FEATURE ) ) .LT. NEAR1 ) THEN
                     NEAR1 = ABS( INPUT_FTR_POSITIONS( I ) -
     :                       CHANS( ID_FEATURE ) )
                     INEAR1 = ID_FEATURE
                  END IF
               END DO
               IF ( INEAR1 .EQ. 1 ) THEN
                  INEAR2 = 2

               ELSE IF ( INEAR1 .EQ. NLID ) THEN
                  INEAR2 = NLID - 1

               ELSE IF ( CHANS( INEAR1 ) .GT.
     :                   INPUT_FTR_POSITIONS( I ) ) THEN
                  INEAR2 = INEAR1 - 1

               ELSE
                  INEAR2 = INEAR1 + 1
               END IF
               IF ( INEAR2 .LT. INEAR1 ) THEN
                  ID_FEATURE = INEAR1
                  INEAR1 = INEAR2
                  INEAR2 = ID_FEATURE
               END IF
               ORIGIN = WAVES( INEAR1 ) -
     :               ( WAVES( INEAR2 ) - WAVES( INEAR1 ) ) /
     :               ( CHANS( INEAR2 ) - CHANS( INEAR1 ) ) *
     :               CHANS( INEAR1 )
               PER_PIXEL = ( WAVES( INEAR2 ) - WAVES( INEAR1 ) ) /
     :               ( CHANS( INEAR2 ) - CHANS( INEAR1 ) )
               FTR_WAVELENGTH2( I ) = ORIGIN + PER_PIXEL *
     :               INPUT_FTR_POSITIONS( I )
               FTR_WAVELENGTH2( I ) = ( FTR_WAVELENGTH2( I ) +
     :               FTR_WAVELENGTHS( I ) ) / 2.0
            END IF

*        Search for nearest matching wavelength in database.
            POSSIBLE_WAVELENGTH = ARC_ARFIND( FTR_LIST, MAX_FEATURES,
     :            FTR_WAVELENGTHS( I ) )

*        If match is close enough then.
            IF ( ABS( POSSIBLE_WAVELENGTH - FTR_WAVELENGTHS( I ) ) .LE.
     :           WAVE_FIT_DELTA .AND.
     :           ( OBS_STRENGTH( I ) / MAX_STRENGTH .GE.
     :           1.0 / STRENGTH_FACTOR ) ) THEN

*           Flag feature as identified, use feature strength to as weight.
               IF ( IDENTIFIED_FTRS .LT. MAX_FIT_FTRS ) THEN
                  IDENTIFIED_FTRS = IDENTIFIED_FTRS + 1
                  IDEN_FTR_POSITION( IDENTIFIED_FTRS ) =
     :                  INPUT_FTR_POSITIONS( I )
                  IDEN_FTR_WAVELENGTH( IDENTIFIED_FTRS ) =
     :                  POSSIBLE_WAVELENGTH
                  IDEN_FTR_STATUS( IDENTIFIED_FTRS ) = 0
                  WWEIGHTS( IDENTIFIED_FTRS ) = MIN( 0.01,
     :                  OBS_STRENGTH( I ) )
               END IF

            ELSE IF ( FTR_WAVELENGTH2( I ) .GT. 0.0 ) THEN
               POSSIBLE_WAVELENGTH = ARC_ARFIND( FTR_LIST,
     :               MAX_FEATURES, FTR_WAVELENGTH2( I ) )
               WAVE_DELTA = ABS( POSSIBLE_WAVELENGTH -
     :               FTR_WAVELENGTH2( I ) )
               IF ( ( ABS( POSSIBLE_WAVELENGTH - FTR_WAVELENGTH2( I ) )
     :              .LE. WAVE_FIT_DELTA .AND.
     :              ( OBS_STRENGTH( I ) / MAX_STRENGTH .GE.
     :              1.0 / STRENGTH_FACTOR ) ) ) THEN
                  IF ( IDENTIFIED_FTRS .LT. MAX_FIT_FTRS ) THEN

*                 Flag feature as identified, use feature strength
*                 as weight.
                     IF ( IDENTIFIED_FTRS .EQ. 0 ) THEN
                        IDENTIFIED_FTRS = IDENTIFIED_FTRS + 1

                     ELSE IF ( POSSIBLE_WAVELENGTH .GT.
     :                         WAVES( IDENTIFIED_FTRS ) .OR.
     :                         WAVE_DELTA .GT.
     :                         RMSES( IDENTIFIED_FTRS ) ) THEN
                        IDENTIFIED_FTRS = IDENTIFIED_FTRS + 1
                     END IF
                     IDEN_FTR_POSITION( IDENTIFIED_FTRS ) =
     :                     INPUT_FTR_POSITIONS( I )
                     IDEN_FTR_WAVELENGTH( IDENTIFIED_FTRS ) =
     :                     POSSIBLE_WAVELENGTH
                     IDEN_FTR_STATUS( IDENTIFIED_FTRS ) = 0
                     WWEIGHTS( IDENTIFIED_FTRS ) =
     :                     MIN( 0.01, OBS_STRENGTH( I ) )
                     RMSES( IDENTIFIED_FTRS ) = WAVE_DELTA
                  END IF
               END IF
            END IF
         END DO

*     Exit now if we've rejected all the lines already.
         IF ( IDENTIFIED_FTRS .LT. 1 ) THEN
            STATUS = ECH__AUTO_IDENTIFY
            GO TO 999
         END IF

*     Re-fit wavelength polynomial using all provisionally
*     identified features.
         IF ( .NOT. INTERACTIVE ) THEN
            SFITTER = FITTER
            FITTER = '?'
            USE_NPOLY = MAXIMUM_POLY
            CALL ECH_FEVAL( FITTER, USE_NPOLY, FINAL_WAVE_COEFFS,
     :           1, WDUMMY, WDUMMY, STATUS )
            IF ( FITTER .NE. '?' ) THEN
               WAVE_NPOLY = USE_NPOLY

            ELSE
               FITTER = SFITTER
            END IF
         END IF
         AUTOCLIP = .TRUE.
         RMS = 1.0E6
         TEMP_NPOLY = SAVE_NPOLY
         IF ( WAVE_NPOLY .LT. 2 ) TEMP_NPOLY = 2
         IF ( IDENTIFIED_FTRS .GT. 2 )
     :      CALL ECH_WAVE_POLYFIT( IDEN_FTR_POSITION,
     :           IDEN_FTR_WAVELENGTH, IDEN_FTR_STATUS, WWEIGHTS, RMSES,
     :           FITS, AUTOCLIP, IDENTIFIED_FTRS, NX, MAX_FIT_COEFFS,
     :           .TRUE., TEMP_NPOLY, TEMP_WAVE_COEFFS, RMS )
            NLID = IDENTIFIED_FTRS
         END IF

*     Report results.
         WRITE ( REPORT_STRING, 1003 ) NLID, RMS
         CALL ECH_REPORT( 0, REPORT_STRING )

      ELSE
         FITTER = '?'
         LOW_NCOEFF = MAXIMUM_POLY
         CALL ECH_FEVAL( FITTER, LOW_NCOEFF, TEMP_WAVE_COEFFS,
     :        1, WDUMMY, WDUMMY, STATUS )
         IDENTIFIED_FTRS = 0
         DO I = 1, MAX_ID_FTRS
            IF ( IDEN_FTR_POSITION( I ) .GT. 0.0 )
     :         IDENTIFIED_FTRS = IDENTIFIED_FTRS + 1
         END DO
         NLID = IDENTIFIED_FTRS
         LOW_NCOEFF = MAX( 2, MIN( SAVE_NPOLY, NLID ) )
         IF ( FITTER .EQ. 'SPLINE' ) LOW_NCOEFF = 16
         WAVE_NPOLY = LOW_NCOEFF
         IF ( IDENTIFIED_FTRS .GT. 2 ) THEN
            IF ( WAVE_NPOLY .EQ. 0 ) THEN
               WAVE_NPOLY = SAVE_NPOLY
               DO I = 1, MAX_FIT_COEFFS
                  TEMP_WAVE_COEFFS( I ) = 0.0
               END DO
               AUTOCLIP = .FALSE.
               RMS = 1.0E6
               IF ( WAVE_NPOLY .LT. 2 ) WAVE_NPOLY = 2
               CALL ECH_WAVE_POLYFIT( IDEN_FTR_POSITION,
     :              IDEN_FTR_WAVELENGTH, IDEN_FTR_STATUS, WWEIGHTS,
     :              RMSES, FITS, AUTOCLIP, IDENTIFIED_FTRS, NX,
     :              MAX_FIT_COEFFS, .TRUE., WAVE_NPOLY,
     :              TEMP_WAVE_COEFFS, RMS )
               NLID = IDENTIFIED_FTRS

            ELSE
               WRITE ( REPORT_STRING, 1008 ) FITTER, WAVE_NPOLY
               CALL ECH_REPORT( 0, REPORT_STRING )
            END IF
            OVERALL_DISPERSION = ABS( ( IDEN_FTR_POSITION(
     :            IDENTIFIED_FTRS ) - IDEN_FTR_POSITION( 1 ) ) /
     :            ( IDEN_FTR_WAVELENGTH( IDENTIFIED_FTRS ) -
     :            IDEN_FTR_WAVELENGTH( 1 ) ) )
            RMS_LIMIT = 1.0 / OVERALL_DISPERSION

         ELSE
            RMS_LIMIT = 1.0 / MIN_DISPERSION
         END IF
         WRITE ( REPORT_STRING, 1001 ) RMS_LIMIT
         CALL ECH_REPORT( 0, REPORT_STRING )
         WAVE_FIT_DELTA = RMS_LIMIT
      END IF

*  If rms deviation acceptable, or interactive mode then.
      IF ( RMS .LT. 3.0 * RMS_LIMIT .OR. INTERACTIVE ) THEN
         IF ( IDENTIFIED_FTRS .GT. 0 ) THEN

*        Generate a wavelength scale using the polynomial.
            DO I = 1, NX
               FIT_WAVES_WORK( I ) = FLOAT( I )
            END DO
            IF ( WAVE_NPOLY .GT. 15 ) FITTER = 'SPLINE'
            IF ( WAVE_NPOLY .LT. 2 ) WAVE_NPOLY = 2
            CALL ECH_FEVAL( FITTER, WAVE_NPOLY, TEMP_WAVE_COEFFS,
     :           NX, FIT_WAVES_WORK, FIT_WAVES2, STATUS )

*        Identify potential blends.
            CALL ECH_CHECK_BLENDS( MAX_FEATURES, FTR_LIST,
     :          MAX_PERM_FTRS, IDENTIFIED_FTRS, IDEN_FTR_WAVELENGTH,
     :          IDEN_FTR_STATUS, WAVE_FIT_DELTA, BLENDS, STATUS )
            IF ( BLENDS .GT. 0 .AND. .NOT. INTERACTIVE ) THEN
               AUTOCLIP = .TRUE.
               RMS = 1.0E6
               IF ( WAVE_NPOLY .LT. 2 ) WAVE_NPOLY = 2
               IF ( IDENTIFIED_FTRS .GT. 2 )
     :            CALL ECH_WAVE_POLYFIT( IDEN_FTR_POSITION,
     :                 IDEN_FTR_WAVELENGTH, IDEN_FTR_STATUS, WWEIGHTS,
     :                 RMSES, FITS, AUTOCLIP, IDENTIFIED_FTRS, NX,
     :                 MAX_FIT_COEFFS,.TRUE.,WAVE_NPOLY,
     :                 TEMP_WAVE_COEFFS, RMS )
               NLID = IDENTIFIED_FTRS
            END IF

*        Compute final estimate of dispersion, start/end wavelength.
            IF ( WAVE_NPOLY .LT. 2 ) WAVE_NPOLY = 2
            WDUMMY( 1 ) = 1.0
            CALL ECH_FEVAL( FITTER, WAVE_NPOLY, TEMP_WAVE_COEFFS, 1,
     :           WDUMMY, BEST_START, STATUS )
            IF ( WAVE_NPOLY .LT. 2 ) WAVE_NPOLY = 2
            WDUMMY( 1 ) = FLOAT( NX )
            CALL ECH_FEVAL( FITTER, WAVE_NPOLY, TEMP_WAVE_COEFFS, 1,
     :          WDUMMY, BEST_END, STATUS )
            IF ( BEST_END .NE. BEST_START ) THEN
               OVERALL_DISPERSION = ABS( FLOAT( NX ) /
     :               ( BEST_END - BEST_START ) )
               CALL CHR_RTOC( FLOAT( INT( 10000.0 / OVERALL_DISPERSION
     :              ) ) / 10000.0, REF_STR1, NCHAR1 )
               CALL CHR_RTOC( BEST_START, REF_STR2, NCHAR2 )
               CALL CHR_RTOC( BEST_END, REF_STR3, NCHAR3 )
               REPORT_STRING = ' Dispersion: ' // REF_STR1( :NCHAR1 ) //
     :               ' from W=' // REF_STR2( :NCHAR2 ) //
     :               ' to ' // REF_STR3( :NCHAR3 ) // '.'

            ELSE
               REPORT_STRING =
     :               ' Dispersion and wavelength range not known.'
            END IF
            CALL ECH_REPORT( 0, REPORT_STRING )
         END IF

*     Plot / Allow interactive editing of fit.
         CALL ECH_EDIT_ID_WAVES( NX, MAX_FEATURES, FTR_LIST,
     :        EXTRACTED_REF, ORDER, ORDER_IDNUM, NO_OF_POSITIONS,
     :        INTERACTIVE, WAVE_NPOLY, MAX_PERM_FTRS,
     :        INPUT_FTR_POSITIONS, STRENGTH_FACTOR, MAXIMUM_POLY,
     :        FITTER, RMS, RMS_LIMIT, TEMP_WAVE_COEFFS,
     :        IDENTIFIED_FTRS, IDEN_FTR_POSITION, IDEN_FTR_STATUS,
     :        IDEN_FTR_WAVELENGTH, WWEIGHTS, RMSES, FITS, STATUS )

*     If in interactive mode the.
         IF ( INTERACTIVE ) THEN

*        If final fit was acceptable then.
            IF ( status .EQ. ECH__SAVE_WFIT ) THEN

*           Set return status 'good'
*           Copy polynomial coefficients into permanent array
               STATUS = 0
               DO I = 1, WAVE_NPOLY
                  FINAL_WAVE_COEFFS( I ) = TEMP_WAVE_COEFFS( I )
               END DO
               DO I = WAVE_NPOLY + 1, MAXIMUM_POLY
                  FINAL_WAVE_COEFFS( I ) = 0.0
               END DO
               CALL ECH_REPORT( 0,
     : ' Stored Wavelength polynomial updated using latest fit.' )

*        Else if user requested a QUIT this order then.
            ELSE IF ( STATUS .EQ. ECH__QUIT_WFIT ) THEN
               STATUS = 0
            END IF

         ELSE

*        If both start/end wavelengths and dispersion are within
*        acceptable limits now that final fit has been done.
            IF ( BEST_START .GE. START_WAVELENGTH .AND.
     :           BEST_END .LE. END_WAVELENGTH .AND.
     :           OVERALL_DISPERSION .GE. MIN_DISPERSION .AND.
     :           OVERALL_DISPERSION .LE. MAX_DISPERSION ) THEN

*           Copy polynomial coefficients into permanent array.
               STATUS = 0
               DO I = 1, WAVE_NPOLY
                  FINAL_WAVE_COEFFS( I ) = TEMP_WAVE_COEFFS( I )
               END DO
               DO I = WAVE_NPOLY + 1, MAXIMUM_POLY
                  FINAL_WAVE_COEFFS( I ) = 0.0
               END DO
               WRITE ( REPORT_STRING, 1005 )
               CALL ECH_REPORT( 0, REPORT_STRING )

*        Report problem.
            ELSE
               CALL ECH_SET_CONTEXT( 'PROBLEM', 'Wave-fit start/end' )
               WRITE ( REPORT_STRING, 1006 )
               CALL ECH_REPORT( 0, REPORT_STRING )
            END IF
         END IF

      ELSE
         CALL ECH_SET_CONTEXT ( 'PROBLEM', 'Wave-fit deviations' )
         WRITE ( REPORT_STRING, 1007 )
         CALL ECH_REPORT( 0, REPORT_STRING )
      END IF

  999 CONTINUE

 1001 FORMAT ( 1X, 'RMS acceptable error level estimated at ',
     :         1PE10.4, '.' )
 1002 FORMAT ( 1X, 'Initial low degree fit > No. of lines= ',I3,
     :         ' RMS=',1PE10.4, '.' )
 1003 FORMAT ( 1X, 'Full nth degree fit > No. of lines= ',I3,
     :         ' RMS= ',1PE10.4, '.' )
 1005 FORMAT ( 1X, 'Wavelength fit accepted.' )
 1006 FORMAT ( 1X, 'Extent of fitted wavelength scale insufficient.' )
 1007 FORMAT ( 1X, 'RMS deviations from fit too large.' )
 1008 FORMAT ( 1X, 'Current wavelength fit is a ',A,' using ',
     :         I4,' coefficients.' )
      END
