      SUBROUTINE ECH_ID_REF_FEATURES(
     :           NX,
     :           START_WAVELENGTH,
     :           END_WAVELENGTH,
     :           EXTRACTED_REF,
     :           INTERACTIVE,
     :           CHECK_REVERSED,
     :           MAX_FEATURES,
     :           DB_SCOPE,
     :           FTR_DB_INDEX_SIZE,
     :           FTR_LIST,
     :           FTR_STRENGTH,
     :           FTR_DB,
     :           FTR_DB_INDEX_L,
     :           FTR_DB_INDEX_R,
     :           FTR_DB_INDEX_WAVE,
     :           FTR_DB_QUICK_INDEX,
     :           FTR_DB_QUICK_VALUE,
     :           QUICK_INDEX_SIZE,
     :           ORDER,
     :           ORDER_IDNUM,
     :           MIN_DISPERSION,
     :           MAX_DISPERSION,
     :           DIFFER_THRESH,
     :           MAX_POSITIONS_TO_USE,
     :           START_DELTA_POSN,
     :           MAX_DELTA_POSN,
     :           STRENGTH_FACTOR,
     :           MAX_PERM_FTRS,
     :           IPF_POS,
     :           OBS_STRENGTH,
     :           WAVE_NPOLY,
     :           MAXIMUM_POLY,
     :           FITTER,
     :           FINAL_WAVE_COEFFS,
     :           IDF_COUNT,
     :           IDF_POS,
     :           IDF_STAT,
     :           IDF_WAVE,
     :           FIT_WAVES,
     :           FIT_WAVES2,
     :           FIT_WAVES_WORK,
     :           LEFT_OFFSET,
     :           RIGHT_OFFSET,
     :           PREV_INDEX,
     :           NEXT_INDEX,
     :           RATIOS,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_ID_REF_FEATURES

*  Purpose:
*     Main wavelength calibration module.

*  Description:
*     This routine takes a set of potential feature candidates (usually arc
*     line positions) and attempts to match them to a subset of the feature
*     database.  Polynomial fits in wavelength are then made using the
*     provisionally identified features. Automatic/Interactive optimisation
*     is  then performed to obtain a best-fit of position vs. wavelength
*     for an order.

*  Invocation:
*     CALL ECH_ID_REF_FEATURES(
*     :    NX,
*     :    START_WAVELENGTH,
*     :    END_WAVELENGTH,
*     :    EXTRACTED_REF,
*     :    INTERACTIVE,
*     :    CHECK_REVERSED,
*     :    MAX_FEATURES,
*     :    DB_SCOPE,
*     :    FTR_DB_INDEX_SIZE,
*     :    FTR_LIST,
*     :    FTR_STRENGTH,
*     :    FTR_DB,
*     :    FTR_DB_INDEX_L,
*     :    FTR_DB_INDEX_R,
*     :    FTR_DB_INDEX_WAVE,
*     :    FTR_DB_QUICK_INDEX,
*     :    FTR_DB_QUICK_VALUE,
*     :    QUICK_INDEX_SIZE,
*     :    ORDER,
*     :    ORDER_IDNUM,
*     :    MIN_DISPERSION,
*     :    MAX_DISPERSION,
*     :    DIFFER_THRESH,
*     :    MAX_POSITIONS_TO_USE,
*     :    START_DELTA_POSN,
*     :    MAX_DELTA_POSN,
*     :    STRENGTH_FACTOR,
*     :    MAX_PERM_FTRS,
*     :    IPF_POS,
*     :    OBS_STRENGTH,
*     :    WAVE_NPOLY,
*     :    MAXIMUM_POLY,
*     :    FITTER,
*     :    FINAL_WAVE_COEFFS,
*     :    IDF_COUNT,
*     :    IDF_POS,
*     :    IDF_STAT,
*     :    IDF_WAVE,
*     :    FIT_WAVES,
*     :    FIT_WAVES2,
*     :    FIT_WAVES_WORK,
*     :    LEFT_OFFSET,
*     :    RIGHT_OFFSET,
*     :    PREV_INDEX,
*     :    NEXT_INDEX,
*     :    RATIOS,
*     :    STATUS
*     :   )

*  Arguments:
*     NX = INTEGER (Given)
*        Number of columns in frame.
*     START_WAVELENGTH = REAL (Given)
*        Start search window wavelength.
*     END_WAVELENGTH = REAL (Given)
*        End search window wavelength.
*     EXTRACTED_REF = REAL (Given)
*        Extracted reference spectrum.
*     INTERACTIVE = LOGICAL (Given)
*        TRUE if interactive mode is used.
*     CHECK_REVERSED = LOGICAL (Given)
*        TRUE if reversed arcs allowed.
*     ORDER = INTEGER (Given)
*        Number of order being processed.
*     MIN_DISPERSION = REAL (Given)
*        Minimum dispersion for search window.
*     MAX_DISPERSION = REAL (Given)
*        Maximum dispersion for search window.
*     DIFFER_THRESH = REAL (Given)
*        User specified difference threshold for ratios.
*     MAX_POSITIONS_TO_USE INTEGER (Given)
*        Max number of features to use for initial id.
*     START_DELTA_POSN = INTEGER (Given)
*        Starting number of neighbours to consider.
*     MAX_DELTA_POSN = INTEGER (Given)
*        Maximum number of neighbours to consider.
*     STRENGTH_FACTOR = REAL (Given)
*        Minimum ratio of features to use (in terms of max str.
*     MAX_PERM_FTRS = INTEGER (Given)
*        Maximum number of features to use in a fit.
*     IPF_POS = REAL (Given)
*        Observed central positions of features.
*     OBS_STRENGTH = REAL (Given)
*        Observed strengths (usually intensity) of features.
*     WAVE_NPOLY = INTEGER (Given)
*        Default order of wavelength polynomial fit.
*     MAXIMUM_POLY = INTEGER (Given)
*        Maximum order of wavelength polynomial fit.
*     FINAL_WAVE_COEFFS = DOUBLE (Returned)
*        Wavelength polynomial coefficients.
*     IDF_COUNT = INTEGER (Returned)
*        Count of identified features.
*     IDF_POS = REAL (Returned)
*        Positions identified features.
*     IDF_STAT = INTEGER (Returned)
*        Statuses of identified features.
*     IDF_WAVE = REAL (Returned)
*        Wavelengths of identified features.
*     FIT_WAVES = REAL (Temporary Workspace)
*        Work array for polynoimal fitting.
*     FIT_WAVES2 = REAL (Temporary Workspace)
*        Work array for polynoimal fitting.
*     FIT_WAVES_WORK = REAL (Temporary Workspace)
*        Work array for polynoimal fitting.
*     MAX_FEATURES = INTEGER (Given)
*        Maximum number of observed features per order.
*     DB_SCOPE = INTEGER (Temporary Workspace)
*        Maximum number of neighbours available in database.
*     FTR_DB_INDEX_SIZE = INTEGER (Given)
*        Feature database index size.
*     FTR_LIST = REAL (Given)
*        List of known arc line wavelengths.
*     FTR_STRENGTH = REAL (Given)
*        Array of arc line expected strengths.
*     FTR_DB = REAL (Given)
*        Feature ratio database main array.
*     FTR_DB_INDEX_L = BYTE (Given)
*        Feature  database left neighbour index.
*     FTR_DB_INDEX_R = BYTE (Given)
*        Feature database right neighbour index.
*     FTR_DB_INDEX_WAVE = SHORT (Given)
*        Feature database wavelength list index.
*     FTR_DB_QUICK_INDEX = INTEGER (Given)
*        Feature database index to main array.
*     FTR_DB_QUICK_VALUE = REAL (Given)
*        Feature database index index starting values in main array.
*     ORDER_IDNUM = INTEGER (Given)
*        Echelle order number.
*     FITTER = CHAR (Given)
*        Type of wavelength fitting function in use (POLY/SPLINE).
*     LEFT_OFFSET = INTEGER (Given and Returned)
*        Which left neighbour was used.
*     RIGHT_OFFSET = INTEGER (Given and Returned)
*        Which right neighbour was used.
*     PREV_INDEX = INTEGER (Given and Returned)
*        Pointer to entry with previous greatest ratio.
*     NEXT_INDEX = INTEGER (Given and Returned)
*        Pointer to entry with next greatest ratio.
*     RATIOS = REAL (Given and Returned)
*        Ratios of features inter-neigbour distances.
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

*  Authors:
*     DMILLS: Dave Mills (UCL, Starlink)
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     01-SEP-1992 (DMILLS):
*       Initial release.
*     05-JUN-1997 (MJC):
*       Various changes including: added prologue, fixed reversed
*       arc feature ordering bug, moved quick index sizing up two
*       calls to ech_wavelength_calib.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_REPORT.INC'
      INCLUDE 'ECH_FEATURE.INC'
      INCLUDE 'ECH_FEATURE_DB.INC'
      INCLUDE 'ECH_DATA_CONSTRAINTS.INC'

*  Arguments Given:
      INTEGER NX
      REAL START_WAVELENGTH
      REAL END_WAVELENGTH
      REAL EXTRACTED_REF( NX )
      LOGICAL INTERACTIVE
      LOGICAL CHECK_REVERSED
      INTEGER ORDER
      INTEGER ORDER_IDNUM
      INTEGER QUICK_INDEX_SIZE
      REAL DIFFER_THRESH
      INTEGER MAX_POSITIONS_TO_USE
      INTEGER START_DELTA_POSN
      INTEGER MAX_DELTA_POSN
      REAL STRENGTH_FACTOR
      INTEGER MAX_PERM_FTRS
      REAL IPF_POS( MAX_PERM_FTRS )
*          ! Observed central positions of features.
      REAL OBS_STRENGTH( MAX_PERM_FTRS )
*          ! Observed strengths (usually intensity) of features.
      INTEGER WAVE_NPOLY
      INTEGER MAXIMUM_POLY
      CHARACTER*( * ) FITTER
      INTEGER IDF_COUNT

*  Arguments Returned:
      REAL MIN_DISPERSION
      REAL MAX_DISPERSION
      DOUBLE PRECISION FINAL_WAVE_COEFFS( MAXIMUM_POLY )
*          ! Wavelength polynomial coefficients.
      REAL IDF_POS( MAX_PERM_FTRS )
*          ! Positions identified features.
      REAL IDF_WAVE( MAX_PERM_FTRS )
*          ! Positions identified features.
      INTEGER IDF_STAT( MAX_PERM_FTRS )
*          ! Positions identified features.

*  Workspace:
      BYTE LEFT_OFFSET( MAX_META_INDEX, 2, MAX_ID_FTRS )
*          ! Which left neighbour was used.
      BYTE RIGHT_OFFSET( MAX_META_INDEX, 2, MAX_ID_FTRS )
*          ! Which right neighbour was used.
      INTEGER NEXT_INDEX( MAX_META_INDEX, MAX_ID_FTRS )
*          ! Pointer to entry with next greatest ratio.
      INTEGER PREV_INDEX( MAX_META_INDEX, MAX_ID_FTRS )
*          ! Pointer to entry with previous greatest ratio.
      REAL RATIOS( MAX_META_INDEX, 4, MAX_ID_FTRS )
      REAL FIT_WAVES( NX * 4 )
      REAL FIT_WAVES2( NX * 4 )
      REAL FIT_WAVES_WORK( NX * 4 )

*  Status:
      INTEGER STATUS

*  Local Variables:
      REAL FTR_POSITIONS( MAX_ALLOWED_RF_FEAT )
*          ! Temporary storage for feature positions.
      REAL INV_FTR_POSITIONS( MAX_ALLOWED_RF_FEAT )
*          ! Temporary storage for inverted feature positions.
      REAL BEST_DISTANCES( MAX_FTR_CAND, MAX_ID_FTRS )
*          ! Distance measures for top candidates.
      REAL BEST_DISPERSION( MAX_FTR_CAND, MAX_ID_FTRS )
*          ! Dispersions for top candidates.
      REAL MAX_STRENGTH
      REAL POS_OF_MAX_STRENGTH

      INTEGER BEST_FTR( MAX_FTR_CAND, MAX_ID_FTRS )
*          ! Feature indices for top candidates.
      INTEGER BEST_NEXT_FTR( MAX_FTR_CAND, MAX_ID_FTRS )
*          ! Feature neighbour indices for top candidates.
      INTEGER BEST_NEXT_FTR2( MAX_FTR_CAND, MAX_ID_FTRS )
*          ! Feature neighbour indices for top candidates.
      INTEGER BEST_PREV_FTR( MAX_FTR_CAND, MAX_ID_FTRS )
*          ! Feature neighbour indices for top candidates.
      INTEGER BEST_PREV_FTR2( MAX_FTR_CAND, MAX_ID_FTRS )
*          ! Feature neighbour indices for top candidates.
      INTEGER META_GUESS( MAX_ID_FTRS )
      INTEGER META_FTRS( MAX_ID_FTRS )
      INTEGER INV_META_GUESS( MAX_ID_FTRS )
      INTEGER INV_META_FTRS( MAX_ID_FTRS )
      INTEGER START_WAVELENGTH_INDEX
      INTEGER END_WAVELENGTH_INDEX
      INTEGER META_COUNT
      INTEGER INV_META_COUNT
      INTEGER NO_OF_FEATURES
      INTEGER DELTA_POSITION_LIMIT
      INTEGER USED_FEATURES
      INTEGER MIN_SOLUTION_SIZE
      INTEGER ADD_FEATURES
      INTEGER ORIG_FEATURES
      INTEGER II_OF_MAX_STRENGTH
      INTEGER NO_OF_POSITIONS
      INTEGER META_SCOPE
      INTEGER NUM_RATIOS
      INTEGER I
      INTEGER II
      INTEGER NCHAR1
      INTEGER NCHAR2

      LOGICAL FTR_USED( MAX_ALLOWED_RF_FEAT )
*          ! Flags for features used this search.
      LOGICAL GOT_A_MATCH
      LOGICAL MANUAL

      BYTE BEST_POSINDEX( MAX_FTR_CAND, 4, MAX_ID_FTRS )
*          ! Database neighbour indices for top candidates.

      CHARACTER*16 REF_STR1
      CHARACTER*16 REF_STR2

*  Functions Called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

*  Check for modification of identified line lists.
      MANUAL = .FALSE.
      IF ( STATUS .EQ. ECH__ID_MANUAL ) THEN
         MANUAL = .TRUE.
      END IF

      CALL ECH_CHECK_ID_FTRS( NX, INTERACTIVE, MAX_PERM_FTRS,
     :     IPF_POS, OBS_STRENGTH, WAVE_NPOLY, MAXIMUM_POLY, FITTER,
     :     FINAL_WAVE_COEFFS, IDF_COUNT, IDF_POS, IDF_STAT, IDF_WAVE,
     :     STATUS )
      IF ( STATUS .EQ. ECH__ID_DONE ) THEN
         STATUS = 0
         IF ( .NOT. MANUAL ) THEN
            GO TO 999
         END IF
      END IF

*  Initialise 'identified feature' data arrays if automatic run.
      IF ( .NOT. MANUAL ) THEN
         IF ( IDF_COUNT .EQ. 0 ) THEN
            DO II = MAX_PERM_FTRS, 1, -1
               IDF_POS( II  ) = 0.0
               IDF_WAVE( II ) = 0.0
            END DO

         ELSE
            WRITE ( REPORT_STRING,1007 ) ORDER
            CALL ECH_REPORT( 0, REPORT_STRING )
            GO TO 999
         END IF
      END IF

*  Count number of features in database list.
      NO_OF_FEATURES = MAX_FEATURES
      DO I = MAX_ALLOWED_RF_FEAT, 1, -1
         FTR_USED( I ) = .FALSE.
      END DO

*  Count number of observed features for which positions could be obtained.
      NO_OF_POSITIONS = 0
      DO WHILE ( IPF_POS( NO_OF_POSITIONS + 1 ) .GT. 0.0
     :           .AND. NO_OF_POSITIONS + 1 .LT. MAX_PERM_FTRS )
         NO_OF_POSITIONS = NO_OF_POSITIONS + 1
      END DO

      IF ( NO_OF_POSITIONS .EQ. 0 ) THEN
         CALL ECH_REPORT( 0,
     :        ' No reference features located for this order.' )
         GO TO 999
      END IF

*  Determine wavelength search window.
      CALL ECH_GET_WAVE_WINDOW( NX, NO_OF_FEATURES, MIN_DISPERSION,
     :     MAX_DISPERSION, START_WAVELENGTH, END_WAVELENGTH,
     :     MAX_FEATURES, FTR_LIST, MAX_PERM_FTRS, IDF_POS,
     :     IDF_WAVE, START_WAVELENGTH_INDEX,
     :     END_WAVELENGTH_INDEX, STATUS )

*  Start with strongest features and progressively add in the fainter
*  ones until we find a good match.
*  Determine strongest feature in observed set.
      MAX_STRENGTH = 0.0
      DO II = NO_OF_POSITIONS, 1, -1
         IF ( OBS_STRENGTH( II ) .GT. MAX_STRENGTH ) THEN
            MAX_STRENGTH = OBS_STRENGTH( II )
            POS_OF_MAX_STRENGTH = IPF_POS( II )
            II_OF_MAX_STRENGTH = II
         END IF
      END DO

*  Count all features which are at least 'strength_factor' as strong,
*  i.e. if strength_factor=10 then any feature with a strength greater
*  than max feature strength / 10 will be considered
      ADD_FEATURES = 0
      DO II = NO_OF_POSITIONS, 1, -1
         IF ( OBS_STRENGTH( II ) .GT. MAX_STRENGTH / STRENGTH_FACTOR )
     :      ADD_FEATURES = ADD_FEATURES + 1
      END DO

*  Limit the number of features to start with to between 12 and 16.
      ADD_FEATURES = MIN( MAX( 12, ADD_FEATURES ), 16 )
      ORIG_FEATURES = ADD_FEATURES

*  Start with 'start_delta_posn' neighbours on each side of features.
      DELTA_POSITION_LIMIT = START_DELTA_POSN

*  Clear 'reversed' flag (assume wavelength increases with X).
      META_COUNT = 0
      INV_META_COUNT = 0

*  Inform user of initial state.
      USED_FEATURES = 0
      GOT_A_MATCH = .FALSE.

*  Loop until solution found, or all permutations have been tried
      DO WHILE ( USED_FEATURES .LT. NO_OF_POSITIONS .AND.
     :           USED_FEATURES .LT. MAX_POSITIONS_TO_USE .AND.
     :           .NOT. GOT_A_MATCH )
         STATUS = 0

*     If no strength information was available then use all observed
*     features.  This should not happen normally - this is just a
*     last-ditch attempt for when it does.
         IF ( MAX_STRENGTH .EQ. 0.0 ) THEN
            USED_FEATURES = MIN( NO_OF_POSITIONS, MAX_ALLOWED_RF_FEAT )
            DO II = 1, USED_FEATURES
               FTR_POSITIONS( II ) = IPF_POS( II )
               INV_FTR_POSITIONS( USED_FEATURES - II + 1 ) =
     :               FLOAT( NX ) - IPF_POS( II ) + 1.0
            END DO

         ELSE

*        Extract next most intense 'add_features' and add to
*        'ftr_positions' array.
*
*        Loop through number of features to be added to active list.
            DO I = 1, ADD_FEATURES

*           Search for strongest non-active feature in appropriate
*           fraction of spectrum.
               MAX_STRENGTH = 0.0
               DO II = 1, NO_OF_POSITIONS
                  IF ( .NOT. FTR_USED( II ) ) THEN
                     IF ( OBS_STRENGTH( II ) .GT. MAX_STRENGTH ) THEN
                        MAX_STRENGTH = OBS_STRENGTH( II )
                        POS_OF_MAX_STRENGTH = IPF_POS( II )
                        II_OF_MAX_STRENGTH = II
                     END IF
                  END IF
               END DO

*           Add it to list of active features.
               USED_FEATURES = USED_FEATURES + 1
               FTR_POSITIONS( USED_FEATURES  ) =  POS_OF_MAX_STRENGTH
               FTR_USED( II_OF_MAX_STRENGTH ) = .TRUE.
            END DO

*        Clear array of active feature positions
            DO I = MAX_ALLOWED_RF_FEAT, 1, -1
               FTR_POSITIONS( I ) = 0.0
               INV_FTR_POSITIONS( I ) = 0.0
            END DO

*        Build array of active feature positions with latest set.
            USED_FEATURES = 0
            ADD_FEATURES = MIN( MAX( 3, NO_OF_POSITIONS / 10 ), 10 )
            DO I = 1, NO_OF_POSITIONS
               IF ( FTR_USED( I ) ) THEN
                  USED_FEATURES = USED_FEATURES + 1
                  FTR_POSITIONS( USED_FEATURES ) = IPF_POS( I )
                  INV_FTR_POSITIONS( USED_FEATURES ) = FLOAT( NX ) -
     :                  IPF_POS( I ) + 1.0
               END IF
            END DO
            DO I = 1, USED_FEATURES
               INV_FTR_POSITIONS( I ) = FLOAT( NX ) -
     :               FTR_POSITIONS( USED_FEATURES - I + 1 ) + 1.0
            END DO
         END IF

         REPORT_STRING = ' -----------------------------------' //
     :         '-----------------------------------'
         CALL ECH_REPORT( 0, REPORT_STRING )

         CALL CHR_ITOC( ORDER, REF_STR1, NCHAR1 )
         REPORT_STRING = ' Constraints for order ' //
     :         REF_STR1( :NCHAR1 ) // '.'
         CALL ECH_REPORT( 0, REPORT_STRING )

         CALL CHR_ITOC( USED_FEATURES, REF_STR1, NCHAR1 )
         REPORT_STRING = ' Searches set to use the strongest ' //
     :         REF_STR1( :NCHAR1 ) // ' features.'
         CALL ECH_REPORT( 0, REPORT_STRING )

         CALL CHR_RTOC( START_WAVELENGTH, REF_STR1, NCHAR1 )
         CALL CHR_RTOC( END_WAVELENGTH, REF_STR2, NCHAR2 )
         REPORT_STRING = ' Wavelength range from ' //
     :         REF_STR1( :NCHAR1 ) // ' to ' //
     :         REF_STR2( :NCHAR2 ) // '.'
         CALL ECH_REPORT( 0, REPORT_STRING )

         CALL CHR_ITOC( DELTA_POSITION_LIMIT, REF_STR1, NCHAR1 )
         REPORT_STRING = ' Neighbours to be considered: +/-' //
     :         REF_STR1( :NCHAR1 ) // '.'
         CALL ECH_REPORT( 0, REPORT_STRING )

         REPORT_STRING = ' -----------------------------------' //
     :         '-----------------------------------'
         CALL ECH_REPORT( 0, REPORT_STRING )

*     Set requirement for at least 9 (6 if interactive) feature solutions.
         MIN_SOLUTION_SIZE = 6
         IF ( .NOT. INTERACTIVE ) MIN_SOLUTION_SIZE = 9

*     Search for inverted matches.
         IF ( .NOT. MANUAL .AND. CHECK_REVERSED ) THEN
            CALL ECH_REPORT( 0, ' Checking for reversed arc.' )

*        Calculate neighbour-neighbour distance ratios for active feature set.
            META_SCOPE = DELTA_POSITION_LIMIT
            CALL ECH_CALC_META_FTRS( MAX_PERM_FTRS, INV_FTR_POSITIONS,
     :           USED_FEATURES, META_SCOPE, LEFT_OFFSET, RIGHT_OFFSET,
     :           RATIOS, NUM_RATIOS, NEXT_INDEX, PREV_INDEX, STATUS )

*        Evaluate distance measures based upon meta-features
*        (4 ratios/5 features).
            CALL ECH_EVAL_META_DIST( MAX_FEATURES, DB_SCOPE,
     :           FTR_DB_INDEX_SIZE, FTR_LIST, FTR_STRENGTH, FTR_DB,
     :           FTR_DB_INDEX_L, FTR_DB_INDEX_R, FTR_DB_INDEX_WAVE,
     :           FTR_DB_QUICK_INDEX, FTR_DB_QUICK_VALUE,
     :           QUICK_INDEX_SIZE,
     :           INV_FTR_POSITIONS, USED_FEATURES, META_SCOPE,
     :           DIFFER_THRESH, MIN_DISPERSION, MAX_DISPERSION,
     :           START_WAVELENGTH_INDEX, END_WAVELENGTH_INDEX,
     :           LEFT_OFFSET, RIGHT_OFFSET, RATIOS, NEXT_INDEX,
     :           BEST_DISPERSION, BEST_FTR, BEST_DISTANCES,
     :           BEST_POSINDEX, BEST_PREV_FTR, BEST_PREV_FTR2,
     :           BEST_NEXT_FTR, BEST_NEXT_FTR2, STATUS )

*        Search top meta-feature candidates for consistent solutions.
            INV_META_COUNT = 0
            CALL ECH_TEST_META_FTRS( MAX_FEATURES, FTR_LIST,
     :           BEST_DISPERSION, BEST_FTR, BEST_DISTANCES,
     :           BEST_PREV_FTR, BEST_PREV_FTR2, BEST_NEXT_FTR,
     :           BEST_NEXT_FTR2, USED_FEATURES, INV_META_FTRS,
     :           INV_META_GUESS, INV_META_COUNT, STATUS )
         END IF

         IF ( .NOT. MANUAL .AND.
     :        INV_META_COUNT + 4 .LT. MIN_SOLUTION_SIZE ) THEN
            CALL ECH_REPORT( 0, ' Checking for normal arc.' )

*        Calculate neighbour-neighbour distance ratios for active feature set.
            META_SCOPE = DELTA_POSITION_LIMIT
            CALL ECH_CALC_META_FTRS( MAX_PERM_FTRS, FTR_POSITIONS,
     :           USED_FEATURES, META_SCOPE, LEFT_OFFSET, RIGHT_OFFSET,
     :           RATIOS, NUM_RATIOS, NEXT_INDEX, PREV_INDEX, STATUS )

*        Evaluate distance measures based upon meta-features
*        (4 ratios/5 features).
            CALL ECH_EVAL_META_DIST( MAX_FEATURES, DB_SCOPE,
     :           FTR_DB_INDEX_SIZE, FTR_LIST, FTR_STRENGTH, FTR_DB,
     :           FTR_DB_INDEX_L, FTR_DB_INDEX_R, FTR_DB_INDEX_WAVE,
     :           FTR_DB_QUICK_INDEX, FTR_DB_QUICK_VALUE,
     :           QUICK_INDEX_SIZE,
     :           FTR_POSITIONS, USED_FEATURES, META_SCOPE,
     :           DIFFER_THRESH, MIN_DISPERSION, MAX_DISPERSION,
     :           START_WAVELENGTH_INDEX, END_WAVELENGTH_INDEX,
     :           LEFT_OFFSET, RIGHT_OFFSET, RATIOS, NEXT_INDEX,
     :           BEST_DISPERSION, BEST_FTR, BEST_DISTANCES,
     :           BEST_POSINDEX, BEST_PREV_FTR, BEST_PREV_FTR2,
     :           BEST_NEXT_FTR, BEST_NEXT_FTR2, STATUS )

*        Search top meta-feature candidates for consistent solutions.
            META_COUNT = 0
            CALL ECH_TEST_META_FTRS( MAX_FEATURES, FTR_LIST,
     :           BEST_DISPERSION, BEST_FTR, BEST_DISTANCES,
     :           BEST_PREV_FTR, BEST_PREV_FTR2, BEST_NEXT_FTR,
     :           BEST_NEXT_FTR2, USED_FEATURES, META_FTRS, META_GUESS,
     :           META_COUNT, STATUS )
         END IF

*     If a solution is found involving at least 'min_solution_size'
*     features.
         IF ( MANUAL .OR.
     :        META_COUNT + 4 .GE. MIN_SOLUTION_SIZE .OR.
     :        INV_META_COUNT + 4 .GE. MIN_SOLUTION_SIZE ) THEN

*        Fit/refine polynomial in wavelength.
            IF ( MANUAL .OR. META_COUNT + 4 .GE. MIN_SOLUTION_SIZE )
     :           THEN
               IF ( WAVE_NPOLY .LT. 3 ) WAVE_NPOLY = 3
               CALL ECH_FIT_REF_WAVES( NX, MAX_FEATURES, FTR_LIST,
     :              START_WAVELENGTH, END_WAVELENGTH, EXTRACTED_REF,
     :              INTERACTIVE, ORDER, ORDER_IDNUM, MIN_DISPERSION,
     :              MAX_DISPERSION, NO_OF_POSITIONS, WAVE_NPOLY,
     :              MAX_PERM_FTRS, FTR_POSITIONS, IPF_POS,
     :              OBS_STRENGTH, STRENGTH_FACTOR, BEST_FTR,
     :              BEST_NEXT_FTR, BEST_NEXT_FTR2, BEST_PREV_FTR,
     :              BEST_PREV_FTR2, BEST_POSINDEX, META_GUESS,
     :              META_FTRS, META_COUNT, MAXIMUM_POLY,
     :              FITTER, FINAL_WAVE_COEFFS, IDF_COUNT, IDF_POS,
     :              IDF_STAT, IDF_WAVE, FIT_WAVES, FIT_WAVES2,
     :              FIT_WAVES_WORK, STATUS )

            ELSE IF ( INV_META_COUNT + 4 .GE. MIN_SOLUTION_SIZE ) THEN
               IF ( WAVE_NPOLY .LT. 3 ) WAVE_NPOLY = 3
               CALL ECH_FIT_REF_WAVES( NX, MAX_FEATURES, FTR_LIST,
     :              START_WAVELENGTH, END_WAVELENGTH, EXTRACTED_REF,
     :              INTERACTIVE, ORDER, ORDER_IDNUM, MIN_DISPERSION,
     :              MAX_DISPERSION, NO_OF_POSITIONS, WAVE_NPOLY,
     :              MAX_PERM_FTRS, INV_FTR_POSITIONS, IPF_POS,
     :              OBS_STRENGTH, STRENGTH_FACTOR, BEST_FTR,
     :              BEST_NEXT_FTR, BEST_NEXT_FTR2, BEST_PREV_FTR,
     :              BEST_PREV_FTR2, BEST_POSINDEX, INV_META_GUESS,
     :              INV_META_FTRS, INV_META_COUNT, MAXIMUM_POLY,
     :              FITTER, FINAL_WAVE_COEFFS, IDF_COUNT, IDF_POS,
     :              IDF_STAT, IDF_WAVE, FIT_WAVES, FIT_WAVES2,
     :              FIT_WAVES_WORK, STATUS )
            END IF

*        Set flag if successful.
            IF ( STATUS .EQ. 0 ) GOT_A_MATCH = .TRUE.
            IF ( STATUS .EQ. ECH__AUTO_IDENTIFY ) MANUAL = .FALSE.

*     Report failure.
         ELSE
            WRITE ( REPORT_STRING, 1002 )
            CALL ECH_REPORT( 0, REPORT_STRING )
            STATUS = -1
         END IF

*     If no solution yet, and full set of features has been used.
         IF ( STATUS .NE. 0 .AND. (
     :        USED_FEATURES + ADD_FEATURES .GT. NO_OF_POSITIONS .OR.
     :        USED_FEATURES + ADD_FEATURES .GT. MAX_POSITIONS_TO_USE ) )
     :        THEN

*        Clear active feature flag array.
            DO I = NO_OF_POSITIONS, 1, -1
               FTR_USED( I ) = .FALSE.
            END DO
            USED_FEATURES = 0

*        Reset number of active features to original set
            ADD_FEATURES = ORIG_FEATURES

*        If still scope for increasing number of neighbours considered then
            IF ( DELTA_POSITION_LIMIT .LT. MAX_DELTA_POSN ) THEN

*           Increase by 1, and inform user.
               IF ( DELTA_POSITION_LIMIT * 2 .GE. NO_OF_POSITIONS ) THEN
                  CALL ECH_REPORT( 0, ' Not enough candidate features '
     :                 // 'to increase search scope.' )
                  WRITE ( report_string, 1004 )
                  CALL ECH_REPORT( 0, report_string )
                  GOT_A_MATCH = .TRUE.
                  IDF_COUNT = 0

               ELSE
                  DELTA_POSITION_LIMIT = DELTA_POSITION_LIMIT + 1
                  WRITE ( REPORT_STRING, 1003 )
                  CALL ECH_REPORT( 0, REPORT_STRING )
                  IF ( ADD_FEATURES .LE. DELTA_POSITION_LIMIT * 2 + 2 )
     :               ADD_FEATURES = DELTA_POSITION_LIMIT * 3
               END IF

*        Report failure.
            ELSE
               WRITE ( REPORT_STRING, 1004 )
               CALL ECH_REPORT( 0, REPORT_STRING )
               GOT_A_MATCH = .TRUE.
               IDF_COUNT = 0
            END IF
         END IF
      END DO

  999 CONTINUE

 1002 FORMAT ( 1X, 'No solutions found, increasing active features.' )
 1003 FORMAT ( 1X, 'No solutions found, increasing neighbour limit.' )
 1004 FORMAT ( 1X, 'No solutions found, abandoning order.' )
 1007 FORMAT ( 1X, 'Order ', I3, ' already has identifications.' )

      END
