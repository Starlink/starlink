      SUBROUTINE ECH_WAVELENGTH_CALIB(
     :           NX,
     :           LOW_WAVE_LIMIT,
     :           HI_WAVE_LIMIT,
     :           EXTRACTED_REF,
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
     :           WVCAL_AUTO,
     :           CHECK_REVERSED,
     :           N_ORDERS,
     :           RMIN_DISPERSION,
     :           RMAX_DISPERSION,
     :           DIFFER_THRESH,
     :           MAX_POSITIONS_TO_USE,
     :           START_DELTA_POSN,
     :           MAX_DELTA_POSN,
     :           STRENGTH_FACTOR,
     :           CENTRAL_WAVELENGTH,
     :           CENTRAL_ORDERNUM,
     :           MAX_PERM_FTRS,
     :           OBS_POSITION,
     :           OBS_STRENGTH,
     :           WAVE_NPOLY,
     :           WAVE_COEFFS,
     :           MAXIMUM_POLY,
     :           FITTER,
     :           ID_COUNT,
     :           ID_POSITION,
     :           ID_STATUS,
     :           ID_WAVELENGTH,
     :           START_SEARCH_WAVE,
     :           END_SEARCH_WAVE,
     :           ORDER_ID_NUMBER,
     :           FITTED_WAVES,
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
*     ECHOMOP - ECH_WAVELENGTH_CALIB

*  Purpose:
*     Main loop through orders for wavelength calibration.

*  Description:
*     This routine performs wavelength calibration using a reference feature
*     list, usually provided by an ARC-lamp exposure. The routine expects the
*     candidate features to be identified and used as 'knowns' for the
*     calibration (as position/intensity pairs). It then attempts to match
*     using a reference database; and finally generates polynomials to
*     fit the observed positions of features.
*
*     The routine allows selection of  the major optional operations during
*     wavelength calibration. Options are presented in a menu form and
*     selected by typing a one or two character string, followed by carriage
*     return. The following options are supported:
*
*     AI (automatic identification), initiates a search and match of the
*     feature database. Any preset limitations on the wavelength and
*     dispersion range will be taken into account. When a solution is
*     found, a report to the user includes a probable status and the user
*     may then opt to investigate the solution further (I), or search
*     for further solutions by re-selecting the AI option.
*
*     AF ( automatic polynomial fitting), performs a cycle of fitting
*     polynomials to identified lines, then clipping away any
*     badly deviant lines, and re-fitting, etc. Fitting ceases when a
*     stable set of identified features remains.
*
*     E (exit), leaves the line identification menu and updates the
*     reduction database copy of the wavelength polynomial to reflect the
*     lastest calculated fit.
*
*     F (fit), does a single shot polynomial fit to the currently
*     identified lines. A report of deviations is generated but no
*     automatic  clipping of deviant lines is performed.
*
*     H (help), provides interactive browsing of the relevant sections
*     of the help library for line identification.
*
*     I  (interactive identification), enters the interactive line
*     specification, examination section. This section provides features
*     for addition/deletion/re-fitting/listing etc of identified lines.
*
*     IM (import ECHARC data), is used to provide an interface to the
*     ECHARC arc line identification program. In general the XP option
*     would first be used to export data for ECHARC processing. ECHARC
*     would then be run,and finally the data IMported back into this
*     package.
*
*     O (order selection), is used to change the currently selected order
*     when operating manually. The order number which is to be selected
*     will be prompted for.
*
*     P (polynomial degree), is used to alter the degree of polynomial
*     to be used for the wavelength fitting. This may vary up to the
*     maximum specified by the tunable parameter TUNE_MAXPOLY.
*
*     XP (export data for ECHARC), is used to provide an interface to the
*     ECHARC arc line identification program. In general the XP option
*     would first be used to export data for ECHARC processing. ECHARC
*     would then be run,and finally the data IMported back into this
*     package.

*  Invocation:
*     CALL ECH_WAVELENGTH_CALIB(
*     :    NX,
*     :    LOW_WAVE_LIMIT,
*     :    HI_WAVE_LIMIT,
*     :    EXTRACTED_REF,
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
*     :    WVCAL_AUTO,
*     :    CHECK_REVERSED,
*     :    N_ORDERS,
*     :    RMIN_DISPERSION,
*     :    RMAX_DISPERSION,
*     :    DIFFER_THRESH,
*     :    MAX_POSITIONS_TO_USE,
*     :    START_DELTA_POSN,
*     :    MAX_DELTA_POSN,
*     :    STRENGTH_FACTOR,
*     :    CENTRAL_WAVELENGTH,
*     :    CENTRAL_ORDERNUM,
*     :    MAX_PERM_FTRS,
*     :    OBS_POSITION,
*     :    OBS_STRENGTH,
*     :    WAVE_NPOLY,
*     :    WAVE_COEFFS,
*     :    MAXIMUM_POLY,
*     :    FITTER,
*     :    ID_COUNT,
*     :    ID_POSITION,
*     :    ID_STATUS,
*     :    ID_WAVELENGTH,
*     :    START_SEARCH_WAVE,
*     :    END_SEARCH_WAVE,
*     :    ORDER_ID_NUMBER,
*     :    FITTED_WAVES,
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
*     N_ORDERS = INTEGER (Given)
*        Number of orders in echellogram.
*     LOW_WAVE_LIMIT = REAL (Given)
*        Lower wavelnegth limit for searches.
*     HI_WAVE_LIMIT = REAL (Given)
*        Upper wavelnegth limit for searches.
*     EXTRACTED_REF = REAL (Given)
*        Extracted ref spectrum from plotting.
*     WVCAL_AUTO = LOGICAL (Given)
*        TRUE if automatic calibration required.
*     CHECK_REVERSED = LOGICAL (Given)
*        TRUE if reversed arcs allowed.
*     RMIN_DISPERSION = REAL (Given)
*        Minimum dispersion in units/pixels for searches.
*     RMAX_DISPERSION = REAL (Given)
*        Maximum dispersion in untis/pixels for searches.
*     DIFFER_THRESH = REAL (Given)
*        Max difference from database for a matching feature.
*     MAX_POSITIONS_TO_USE IN(Given)
*        Maximum features to use in initial identificatio.
*     START_DELTA_POSN = INTEGER (Given)
*        Number of feature neighbours to start with.
*     MAX_DELTA_POSN = INTEGER (Given)
*        Maximum number of feature neighbours to use.
*     STRENGTH_FACTOR = REAL (Given)
*        Minimum fraction of max intensity for features.
*     CENTRAL_WAVELENGTH = REAL (Given)
*        Approximate central wavelength in frame.
*     CENTRAL_ORDERNUM = INTEGER (Given)
*        Central order number in frame.
*     MAX_PERM_FTRS = REAL (Given)
*        Maximum number of features per  order.
*     OBS_POSITION = REAL (Given)
*        Position of all located features.
*     OBS_STRENGTH = REAL (Given)
*        Strength (usually intensity/FWHM) of features.
*        used for weighting polynomial fit, 'stronger'
*        features getting more weight
*     MAXIMUM_POLY = INTEGER (Given)
*        Maximum number of wavelength polynomial coefficients.
*     WAVE_COEFFS = DOUBLE (Returned)
*        Wavelength polynomial coefficients.
*     ID_COUNT = INTEGER (Returned)
*        Count of identified features per order.
*     ID_POSITION = REAL (Returned)
*        X-coordinates of identified features.
*     ID_STATUS = INTEGER (Returned)
*        Status flags for identified features.
*     ID_WAVELENGTH = REAL (Returned)
*        Wavelengths of identified features.
*     START_SEARCH_WAVE = REAL (Given and Returned)
*        Start wavelength for searches per order.
*     END_SEARCH_WAVE = REAL (Given and Returned)
*        End wavelength for searches per order.
*     ORDER_ID_NUMBER = INTEGER (Given and Returned)
*        Order wave-numbers for each order.
*     FITTED_WAVES = DOUBLE (Given and Returned)
*        Fitted wavelength scale.
*     FIT_WAVES = REAL (Temporary Workspace)
*        Fitting work array.
*     FIT_WAVES2 = REAL (Temporary Workspace)
*        Fitting work array.
*     FIT_WAVES_WORK = REAL (Temporary Workspace)
*        Fitting work array.
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
*     WAVE_NPOLY = INTEGER (Given)
*        Number of coefficients in wavelength fit.
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

*  Method:
*     Zero any unused coefficients
*     Check for possible order id number generation (also provides search limits)
*     If failed, then try a guesstimate based on central order number and wavelength
*     Endif
*     If mode is automatic identification/fitting then
*        Loop processing orders in order of increasing distance from the
*             middle of the frame
*           If middle order, or nth order above middle is present then
*              If good status, generate wavelength scale values
*           Endif
*           If NOT middle order AND nth order below middle is present then
*              If good status, generate wavelength scale values
*           Endif
*           If we have processed 3+ orders then
*              Re-Check for possible order number determinations
*           Endif
*        End loop
*     Else
*        Loop until user selects the EXIT/QUIT options
*              If good status, generate wavelength scale values
*              Re-Check for possible order number determinations
*        End loop
*     Endif

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
      INCLUDE 'ECH_MAPPING.INC'
      INCLUDE 'ECH_DATA_CONSTRAINTS.INC'
      INCLUDE 'ECH_REPORT.INC'
      INCLUDE 'ECH_FEATURE.INC'
      INCLUDE 'ECH_CONTEXT.INC'
      INCLUDE 'ECH_FEATURE_DB.INC'

*  Arguments Given:
      INTEGER NX
      INTEGER N_ORDERS
      REAL LOW_WAVE_LIMIT
      REAL HI_WAVE_LIMIT
      LOGICAL WVCAL_AUTO
      LOGICAL CHECK_REVERSED
      REAL RMIN_DISPERSION
      REAL RMAX_DISPERSION
      REAL DIFFER_THRESH
      INTEGER MAX_POSITIONS_TO_USE
      INTEGER START_DELTA_POSN
      CHARACTER*( * ) FITTER
      INTEGER MAX_DELTA_POSN
      REAL STRENGTH_FACTOR
      REAL CENTRAL_WAVELENGTH
      INTEGER CENTRAL_ORDERNUM
      INTEGER MAX_PERM_FTRS
      REAL OBS_POSITION( MAX_PERM_FTRS, N_ORDERS )
*          ! X coordinates of located features.
      REAL OBS_STRENGTH( MAX_PERM_FTRS, N_ORDERS )
*          ! 'Strength' of located features.
      INTEGER MAXIMUM_POLY
      INTEGER WAVE_NPOLY
      DOUBLE PRECISION WAVE_COEFFS( MAXIMUM_POLY, N_ORDERS )
*          ! Wavelength fit coefficients.
      INTEGER ID_COUNT( N_ORDERS )
*          ! Count of identified features.
      REAL ID_POSITION ( MAX_PERM_FTRS, N_ORDERS )
*          ! X-coordinates of identified features.
      REAL ID_WAVELENGTH( MAX_PERM_FTRS, N_ORDERS )
*          ! Wavelengths of identified features.
      REAL EXTRACTED_REF( NX, N_ORDERS )
*          ! Extracted reference spectrum.
      DOUBLE PRECISION FITTED_WAVES( NX, N_ORDERS )
*          ! Fitted wavelength scales.

*  Arguments Returned:
      INTEGER ID_STATUS( MAX_PERM_FTRS, N_ORDERS )
*          ! Status flags for identified features.
      REAL START_SEARCH_WAVE( N_ORDERS )
*          ! Start wavelength for searches.
      REAL END_SEARCH_WAVE( N_ORDERS )
*          ! End wavelength for searches.
      INTEGER ORDER_ID_NUMBER( N_ORDERS )
*          ! Order identifying wave-numbers.
      BYTE LEFT_OFFSET( MAX_META_INDEX, 2, MAX_ID_FTRS )
*          ! Which left neighbour was used.
      BYTE RIGHT_OFFSET( MAX_META_INDEX, 2, MAX_ID_FTRS )
*          ! Which right neighbour was used.
      INTEGER NEXT_INDEX( MAX_META_INDEX,MAX_ID_FTRS )
*          ! Pointer to entry with next greatest ratio.
      INTEGER PREV_INDEX( MAX_META_INDEX,MAX_ID_FTRS )
*          ! Pointer to entry with previous greatest ratio.
      REAL RATIOS( MAX_META_INDEX, 4,MAX_ID_FTRS )

*  Workspace:
      REAL FIT_WAVES( NX * 4 )
      REAL FIT_WAVES2( NX * 4 )
      REAL FIT_WAVES_WORK( NX * 4 )

*  Status:
      INTEGER STATUS

*  Local Variables:
      REAL WAVE_FIT_DELTA
      REAL MIN_DISPERSION
      REAL MAX_DISPERSION
      REAL SWOP

      INTEGER ORDER_PROC( MAX_ALLOWED_ORDERS )
      INTEGER LUN_ECHARC
      INTEGER I
      INTEGER II
      INTEGER ISTEP
      INTEGER ORDER
      INTEGER QUICK_INDEX_SIZE

      LOGICAL WVCAL_INTERACT

      CHARACTER*32 OPTION

*  Functions Called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

*  Zero-fill any unused coefficients.
      DO I = 1, N_ORDERS
         IF ( ID_COUNT( I ) .EQ. 0 ) THEN
            DO II = 1, MAXIMUM_POLY
               WAVE_COEFFS( II, I ) = 0.0
            END DO
         END IF
      END DO

*  Determine size of database quick-access index.
      QUICK_INDEX_SIZE = 0
      DO WHILE( FTR_DB_QUICK_INDEX( QUICK_INDEX_SIZE + 1 ) .GT. 0 )
         QUICK_INDEX_SIZE = QUICK_INDEX_SIZE + 1
      END DO

*  Check initial range of dispersions.
      IF ( RMIN_DISPERSION .GT. RMAX_DISPERSION ) THEN
         SWOP = RMIN_DISPERSION
         RMIN_DISPERSION = RMAX_DISPERSION
         RMAX_DISPERSION = SWOP
         CALL ECH_REPORT( 0,
     :   ' Search Dispersion limits (min/max) have been exchanged.' )
      END IF
      IF ( RMIN_DISPERSION .EQ. RMAX_DISPERSION ) THEN
         RMIN_DISPERSION = RMIN_DISPERSION / 2.0
         RMAX_DISPERSION = RMAX_DISPERSION * 2.0
         CALL ECH_REPORT( 0,
     :        ' Search Dispersion range has been extended.' )
      END IF

*  Check for dispersion values of zero, use very wide limits if so.
      IF ( RMIN_DISPERSION .EQ. 0.0 ) THEN
         RMIN_DISPERSION = 0.000001
         CALL ECH_REPORT( 0,
     :        ' Minimum dispersion set to very small value.' )
      END IF
      IF ( RMAX_DISPERSION .EQ. 0.0 ) THEN
         RMAX_DISPERSION = 100000.0
         CALL ECH_REPORT( 0,
     :        ' Maximum dispersion set to very large value.' )
      END IF

*  Internally we use the reciprocal of the actual dispersion.
      MIN_DISPERSION = 1.0 / RMAX_DISPERSION
      MAX_DISPERSION = 1.0 / RMIN_DISPERSION

*  Check for possible order ID number generation (also provides
*  search limits).
      WVCAL_INTERACT = .NOT. WVCAL_AUTO
      CALL ECH_CALC_ORDER_IDNUMS( NX, N_ORDERS, WVCAL_INTERACT,
     :     MAXIMUM_POLY, WAVE_COEFFS, START_SEARCH_WAVE,
     :     END_SEARCH_WAVE, ORDER_ID_NUMBER, STATUS )

*  If failed, then try a guesstimate based on central order number and
*  wavelength.
      IF ( STATUS .NE. 0 ) THEN
         STATUS = 0
         CALL ECH_ESTIMATE_WAVES( LOW_WAVE_LIMIT, HI_WAVE_LIMIT,
     :        N_ORDERS, CENTRAL_WAVELENGTH, CENTRAL_ORDERNUM,
     :        START_SEARCH_WAVE, END_SEARCH_WAVE, STATUS )
      END IF

*  If mode is automatic identification/fitting is true.
      IF ( .NOT. WVCAL_INTERACT ) THEN

*     Build list of orders numbers to process, start in the middle of the
*     frame.
         ORDER_PROC( 1 ) = MIN( MAX( 1, ( N_ORDERS + 1 ) / 2 ),
     :         N_ORDERS )
         DO I = 2, N_ORDERS, 2
            IF ( I .LT. N_ORDERS ) THEN
               ORDER_PROC( I ) = MIN( MAX( 1, ORDER_PROC( 1 ) - I / 2 ),
     :               N_ORDERS )
            ELSE
               ORDER_PROC( I ) = MIN( MAX( 1, ORDER_PROC( 1 ) +
     :               I / 2 ), N_ORDERS )
            END IF
            IF ( I + 1 .LE. N_ORDERS ) THEN
               ORDER_PROC( I + 1 ) = MIN( MAX( 1, ORDER_PROC( 1 ) +
     :               I / 2 ), N_ORDERS )
            END IF
         END DO

         DO ISTEP = 1, N_ORDERS
            ORDER = ORDER_PROC( ISTEP )
            CALL ECH_ID_REF_FEATURES( NX, START_SEARCH_WAVE( ORDER ),
     :           END_SEARCH_WAVE( ORDER ), EXTRACTED_REF( 1, ORDER ),
     :           WVCAL_INTERACT, CHECK_REVERSED, MAX_FEATURES,
     :           DB_SCOPE, FTR_DB_INDEX_SIZE, FTR_LIST, FTR_STRENGTH,
     :           FTR_DB, FTR_DB_INDEX_L, FTR_DB_INDEX_R,
     :           FTR_DB_INDEX_WAVE, FTR_DB_QUICK_INDEX,
     :           FTR_DB_QUICK_VALUE, QUICK_INDEX_SIZE,
     :           ORDER, ORDER_ID_NUMBER( ORDER ),
     :           MIN_DISPERSION, MAX_DISPERSION, DIFFER_THRESH,
     :           MAX_POSITIONS_TO_USE, START_DELTA_POSN, MAX_DELTA_POSN,
     :           STRENGTH_FACTOR, MAX_PERM_FTRS,
     :           OBS_POSITION( 1, ORDER ), OBS_STRENGTH( 1, ORDER ),
     :           WAVE_NPOLY, MAXIMUM_POLY, FITTER,
     :           WAVE_COEFFS( 1, ORDER ), ID_COUNT( ORDER ),
     :           ID_POSITION( 1, ORDER ), ID_STATUS( 1, ORDER ),
     :           ID_WAVELENGTH( 1, ORDER ), FIT_WAVES, FIT_WAVES2,
     :           FIT_WAVES_WORK, LEFT_OFFSET, RIGHT_OFFSET, PREV_INDEX,
     :           NEXT_INDEX, RATIOS, STATUS )

*        If good status, generate wavelength scale values.
            IF ( STATUS .EQ. 0 ) THEN
               DO I = 1, NX
                  FIT_WAVES( I ) = FLOAT( I )
               END DO
               CALL ECH_FEVAL( ' ', MAXIMUM_POLY,
     :              WAVE_COEFFS( 1, ORDER ), NX,
     :              FIT_WAVES, FIT_WAVES2, STATUS )
               DO I = 1, NX
                  FITTED_WAVES( I, ORDER ) = DBLE( FIT_WAVES2( I ) )
               END DO
            END IF

*        If we have processed 3 or more orders then re-check
*        for possible order number determinations.
            IF ( ISTEP .GE. 3 ) THEN
               CALL ECH_CALC_ORDER_IDNUMS( NX, N_ORDERS,
     :              WVCAL_INTERACT, MAXIMUM_POLY, WAVE_COEFFS,
     :              START_SEARCH_WAVE, END_SEARCH_WAVE,
     :              ORDER_ID_NUMBER, STATUS )
            END IF
         END DO

       ELSE

*      Loop until user selects the EXIT/QUIT options.
          STATUS = 0
          ORDER = 1
          OPTION = ' '
          DO WHILE ( OPTION .NE. 'EXIT' .AND. OPTION .NE. 'QUIT' )
             CALL ECH_ID_INTERACT( N_ORDERS, MAXIMUM_POLY,
     :            OPTION, ORDER, WAVE_NPOLY, STATUS )
             IF ( OPTION .EQ. 'AUTOIDENTIFY ORDER' .OR.
     :            OPTION .EQ. 'IDENTIFY' ) THEN
                IF ( OPTION .EQ. 'IDENTIFY' ) STATUS = ECH__ID_MANUAL
                CALL ECH_ID_REF_FEATURES( NX,
     :               START_SEARCH_WAVE( ORDER ),
     :               END_SEARCH_WAVE( ORDER ),
     :               EXTRACTED_REF( 1, ORDER ), WVCAL_INTERACT,
     :               CHECK_REVERSED, MAX_FEATURES,
     :               DB_SCOPE, FTR_DB_INDEX_SIZE, FTR_LIST,
     :               FTR_STRENGTH, FTR_DB, FTR_DB_INDEX_L,
     :               FTR_DB_INDEX_R, FTR_DB_INDEX_WAVE,
     :               FTR_DB_QUICK_INDEX, FTR_DB_QUICK_VALUE,
     :               QUICK_INDEX_SIZE, ORDER, ORDER_ID_NUMBER( ORDER ),
     :               MIN_DISPERSION, MAX_DISPERSION,
     :               DIFFER_THRESH, MAX_POSITIONS_TO_USE,
     :               START_DELTA_POSN, MAX_DELTA_POSN,
     :               STRENGTH_FACTOR, MAX_PERM_FTRS,
     :               OBS_POSITION( 1, ORDER ), OBS_STRENGTH( 1, ORDER ),
     :               WAVE_NPOLY, MAXIMUM_POLY, FITTER,
     :               WAVE_COEFFS( 1, ORDER ), ID_COUNT( ORDER ),
     :               ID_POSITION( 1, ORDER ), ID_STATUS( 1, ORDER ),
     :               ID_WAVELENGTH( 1, ORDER ), FIT_WAVES, FIT_WAVES2,
     :               FIT_WAVES_WORK, LEFT_OFFSET, RIGHT_OFFSET,
     :               PREV_INDEX, NEXT_INDEX, RATIOS, STATUS )

                WVCAL_INTERACT = .TRUE.

*            If good status, generate wavelength scale values.
                IF ( STATUS .EQ. 0 ) THEN
                   DO I = 1, NX
                      FIT_WAVES( I ) = FLOAT( I )
                   END DO
                   CALL ECH_FEVAL( ' ', MAXIMUM_POLY,
     :                   WAVE_COEFFS( 1, ORDER ), NX,
     :                   FIT_WAVES, FIT_WAVES2, STATUS )
                   DO I = 1, NX
                      FITTED_WAVES( I, ORDER ) = DBLE( FIT_WAVES2( I ) )
                   END DO
                   ORDER = MIN( ORDER + 1, N_ORDERS )
                END IF

*            Re-Check for possible order number determinations.
                CALL ECH_CALC_ORDER_IDNUMS( NX, N_ORDERS,
     :               WVCAL_INTERACT, MAXIMUM_POLY, WAVE_COEFFS,
     :               START_SEARCH_WAVE, END_SEARCH_WAVE,
     :               ORDER_ID_NUMBER, STATUS )

             ELSE IF ( OPTION .EQ. 'SAVE FIGARO=ECHARC' .OR.
     :                 OPTION .EQ. 'LOAD FIGARO=ECHARC' ) THEN
                CALL ECH_MODULE_INIT( 'ECH_XCHNG_ECHARC', STATUS )
                CALL ECH_XCHNG_ECHARC( OPTION( :4 ), NX, LUN_ECHARC,
     :               N_ORDERS, MAX_PERM_FTRS, ID_COUNT, ID_POSITION,
     :               ID_WAVELENGTH, ID_STATUS, WAVE_NPOLY,
     :               WAVE_FIT_DELTA, EXTRACTED_REF,
     :               CSTR_ECHARC_LABELX, CSTR_ECHARC_LABELY,
     :               CSTR_ECHARC_LABELZ, %VAL( IADDR_ECHARC_DATAX ),
     :               %VAL( IADDR_ECHARC_DATAY ),
     :               %VAL( IADDR_ECHARC_DATAZ ), STATUS )
                CALL ECH_MODULE_TIDYUP( 'ECH_XCHNG_ECHARC', STATUS )
             END IF
          END DO
      END IF

      END
