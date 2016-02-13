      SUBROUTINE SPD_WZKA( INFO,
     :   DB_SCOPE, MAX_ARC_LINES, FTR_DB_INDEX_SIZE,
     :   FTR_LIST, FTR_DB,
     :   FTR_DB_INDEX_L, FTR_DB_INDEX_R, FTR_DB_INDEX_WAVE,
     :   FTR_DB_QUICK_INDEX, FTR_DB_QUICK_VALUE,
     :   xstart, xend, start_wavelength, end_wavelength, check_reversed,
     :   min_dispersion, max_dispersion,
     :   differ_thresh, min_solution_size, max_positions_to_use,
     :   start_delta_posn, max_delta_posn, strength_factor,
     :   max_perm_ftrs, input_ftr_positions, obs_strength,
     :   identified_ftrs, iden_ftr_position,
     :   iden_ftr_wavelength, status )
*+
*  Name:
*     SPD_WZKA

*  Purpose:
*     FDB: main wavelength calibration module.

*  Language:
*     Fortran

*  Invocation:
*     CALL SPD_WZKA( INFO,
*        DB_SCOPE, MAX_ARC_LINES, FTR_DB_INDEX_SIZE,
*        FTR_LIST, FTR_DB,
*        FTR_DB_INDEX_L, FTR_DB_INDEX_R, FTR_DB_INDEX_WAVE,
*        FTR_DB_QUICK_INDEX, FTR_DB_QUICK_VALUE,
*        xstart, xend, start_wavelength, end_wavelength, check_reversed,
*        min_dispersion, max_dispersion,
*        differ_thresh, min_solution_size, max_positions_to_use,
*        start_delta_posn, max_delta_posn, strength_factor,
*        max_perm_ftrs, input_ftr_positions, obs_strength,
*        identified_ftrs, iden_ftr_position,
*        iden_ftr_wavelength, status )

*  Description:
*     This is a modification of Dave Mills' routine ID_REF_FEATURES (cf.
*     Mills 1992). "FDB" stands for "feature data base", which is a data
*     base of known features in arc spectra.
*
*
*     This algorithm performs INITIAL line identification. It is
*     important to VERIFY the returned values by fitting a wavelength
*     scale (eg POLYMOMIAL or SPLINE fit), and rejecting any outliers.
*
*     The algorithm should be given the positions of all conceivable
*     arc lines in the spectrum. It does not use the fainter lines
*     unless it unable to identify using only the brightest, BUT you
*     will get more robust behaviour if you always provide ALL POSSIBLE
*     CANDIDATE LINES for potential identification.
*
*     The algorithm should not be fed severly blended line positions as
*     chances of incorrect identifications will be significantly worse
*     (this is the exception to the rule above).
*
*     The speed of the algorithm varies approximately linearly with
*     -  wavelength range
*     -  dispersion range
*     so the better constraints you provide the faster it will run.
*
*     CAUTION: the algorithm takes your constraints as HARD limits and
*        it usually more robust to accept a slightly longer runtime by
*        relaxing the ranges a little.
*
*     If the algorithm runs and keeps looping increasing its set of
*     neighbours, then the most likely causes are as follows:
*     -  wavelength scale does not icrease with increasing x (set the
*        'check_reverse' arguement and try again).
*     -  wavelength or dispersion range too small (increase them both by
*        a factor of 2 and try again).
*
*     If the program crashes whilst trying to calculate ratios, then the
*     most likely cause is that the set of supplied line positions is
*     not monotonically increasing.
*
*
*     This routine is the top level in a set of seven routines. And ther
*     is another single top level routine GENERATE_FDB. Their names in
*     Specdre's set of subroutines and in Dave's distributed set of
*     routines are:
*        SPD_WZLB       GEREATE_FDB
*        SPD_WZKA       ID_REF_FEATURES
*           SPD_WZKB       GET_WAVE_WINDOW
*           SPD_WZKC       CALC_META_FTRS
*           SPD_WZKD       EVAL_META_DIST
*              SPD_WZKE       META_CANDIDATE
*              SPD_WZKF       TOP_META_CANDS
*           SPD_WZKG       TEST_META_FTRS
*
*     This routine takes a set of potential feature candidates (usually
*     arc line positions) and attempts to match them to a subset of the
*     feature database. Polynomial fits in wavelength are then made
*     using the provisionally identified features. Automatic/Interactive
*     optimisation is then performed to obtain a best-fit of position
*     vs. wavelength for an order.

*  Arguments:
*     INFO = LOGICAL (Given)
*        If true, some messages are issued.
*     DB_SCOPE = INTEGER (Given)
*        An FDB array dimension.
*     MAX_ARC_LINES = INTEGER (Given)
*        An FDB array dimension.
*     FTR_DB_INDEX_SIZE = INTEGER (Given)
*        An FDB array dimension.
*     FTR_LIST( MAX_ARC_LINES ) = REAL (Given)
*        The FDB wavelengths (FTR_WAVE).
*     FTR_DB( DB_SCOPE, DB_SCOPE, MAX_ARC_LINES ) = REAL (Given)
*        The FDB DB array (FTR_DB).
*     FTR_DB_INDEX_L( DB_SCOPE*DB_SCOPE*MAX_ARC_LINES ) = BYTE (Given)
*        The FDB left index array (FTR_LEFT).
*     FTR_DB_INDEX_R( DB_SCOPE*DB_SCOPE*MAX_ARC_LINES ) = BYTE (Given)
*        The FDB right index array (FTR_LEFT).
*     FTR_DB_INDEX_WAVE( DB_SCOPE*DB_SCOPE*MAX_ARC_LINES )
*           = INTEGER * 2 (Given)
*        The FDB wavelength index array (WAVE_INDEX).
*     FTR_DB_QUICK_INDEX( FTR_DB_INDEX_SIZE ) = INTEGER (Given)
*        The FDB quick index array (QUICK_INDEX).
*     FTR_DB_QUICK_VALUE( FTR_DB_INDEX_SIZE ) = REAL (Given)
*        The FDB quick wavelength array (QENTRIES).
*     xstart = REAL (Given)
*        The location of the first pixel in the original arc spectrum.
*        Used for calculating and checking dispersions etc.
*     xend = REAL (Given)
*        The location of the last pixel in the original arc spectrum.
*        Used for calculating and checking dispersions etc.
*     start_wavelength = REAL (Given)
*        Start search window wavelength. The start wavelength for the
*        range to be searched. This value should be in the same units as
*        the values used in the .ARC file used to construct the database
*        (usually Angstroms). If in doubt a larger range than strictly
*        necessary should be provided as this will be more robust with
*        problem data.
*     end_wavelength = REAL (Given)
*        End search window wavelength. The end wavelength for the range
*        to be searched. This value should be in the same units as the
*        values used in the .ARC file used to construct the database
*        (usually Angstroms). If in doubt a larger range than strictly
*        necessary should be provided as this will be more robust with
*        problem data.
*     check_reversed = LOGICAL (Given)
*        TRUE if reversed arcs allowed. This logical arguement controls
*        whether the search is to be made in the `reversed' direction as
*        well as the  normal (wavelength increasing with x) direction.
*        It will not normally be used and is switched off by default to
*        save CPU time.
*     min_dispersion = REAL (Given)
*        Minimum dispersion for search window. The minimum dispersion
*        for the range to be searched. This value should be units of
*        PIXELS per WAVELENGTH UNIT (usually Angstroms). If in doubt a
*        larger range than strictly necessary should be provided as this
*        will be more robust with problem data.
*     max_dispersion = REAL (Given)
*        Maximum dispersion for search window. The maximum dispersion
*        for the range to be searched. This value should be units of
*        PIXELS per WAVELENGTH UNIT (usually Angstroms). If in doubt a
*        larger range than strictly necessary should be provided as this
*        will be more robust with problem data.
*     differ_thresh = REAL (Given)
*        User specified difference threshold for ratios. Recommended
*        value is 0.03. Specifies the maximum difference between the
*        ratios derived from observed features, and those in the
*        database with which a match is attempted. The difference is
*        evaluated by calculating
*           ABS ( 1 - ABS ( observed / reference ) )
*        giving a fractional measure of the difference. EG. difference =
*        0.01 indicates that the two quantities differ by approximately
*        1% of their average magnitude. The default is 0.03, and values
*        much larger than 0.1 are likely to generate a lot of
*        coincidence matches; values less than 0.01 may well miss 'good'
*        matches in less-than-ideal data. You may need to relax this
*        parameter if your arc spectra are VERY distorted (non-linear
*        scale).
*     min_solution_size = INTEGER (Given)
*        How many features must be identfied in order to accept a
*        solution.
*     max_positions_to_use = INTEGER (Given)
*        Max number of features to use for initial id. Recommended value
*        is 30. Specifies the maximum number of features to be used when
*        generating ratios for initial identification. In general, a
*        good solution can be found using only the strongest 8-16
*        features. The program slowly increases the number of features
*        it uses until an adequate solution if found. However, there may
*        be large numbers of weak features present which are not in the
*        reference database. This parameter allows the setting of an
*        absolute maximum on the number of features (per order) which
*        are to be considered. If you don't have 30 observed features,
*        better use the number of observed features here.
*     start_delta_posn = INTEGER (Given)
*        Starting number of neighbours to consider. Recommended value is
*        3. Specifies the starting number of neighbouring features (on
*        EACH side) to examine when generating ratios for matching.
*        Increasing this will lead to exponential increases in CPU time,
*        so it should be used with caution when all else fails. The
*        default value is 3. Higher values are tried  automatically by
*        the program if no solution can be found. The number of
*        neighbours considered is increased montonically until it
*        reaches the maximum of max_delta_posn, when the program gives
*        up.
*     max_delta_posn = INTEGER (Given)
*        Maximum number of neighbours to consider. Recommended value is
*        6. Specifies the maximum number of neighbouring features (on
*        EACH side) to examine when generating ratios for matching.
*        Increasing this will lead to exponential increases in CPU time,
*        so it should be used with caution when all else fails.
*     strength_factor = REAL (Given)
*        Minimum ratio of features to use (in terms of max strength
*        feature). Recommended value is 50.0. Specifies the minimum
*        strength of features to be used for initial identification. It
*        is specified by relating the strength of a feature to that of
*        the strongest feature present in that order. Any features of
*        strength max/strength_facator or greater, will be included in
*        the initial set. The default value is 50.0, thus all features
*        which are at least one fiftieth as strong of the strongest
*        feature will be eligible for identification.
*     max_perm_ftrs = INTEGER (Given)
*        Maximum number of features to use in a fit. Recommended value
*        is 100. Specifies the dimensions of you input and output data
*        arrays (containing the input line positions and intensities,
*        and the out wavelengths,positions etc). Actually you give the
*        dimension of input_ftr_positions etc. here, which is the number
*        of located features. If more than 100 features were located you
*        may consider to pick and choose from these before calling this
*        routine. You need at least 9 located features, since that many
*        must be identified for an acceptable solution.
*     input_ftr_positions( max_perm_ftrs ) = REAL (Given)
*        Observed central positions of features. Set of input arc line
*        positions are measured from your reference arc line spectrum.
*        These should normally be the result of fitting gaussian (or
*        other) profiles to the lines to get good centres. The positions
*        array must be provided sorted into increasing order or the
*        program may crash/loop endlessly. The positions used must be
*        pixel numbers, which means for NDF's pixel coordinates + 0.5.
*     obs_strength( max_perm_ftrs ) = REAL (Given)
*        Observed strengths (usually intensity) of features. Set of
*        input arc line intensities as measured from your arc line
*        spectrum. These values do not have to be very accurate as they
*        are only used to select the `brightest n lines' for an
*        identification pass. All strengths must be positive.
*     identified_ftrs = INTEGER (Given and Returned)
*        Count of identified features. The number of identified lines.
*        The given value must be zero when no lines have been identified
*        so far.
*     iden_ftr_position( max_perm_ftrs ) = REAL (Returned)
*        Positions identified features. The positions of the identified
*        lines.
*     iden_ftr_wavelength( max_perm_ftrs ) = REAL (Returned)
*        Wavelengths of identified features. The wavelengths of the
*        identified lines.
*     status = INTEGER (Given and Returned)
*        Input/Output status conditions. Zero for OK.
*        This is a status for use in the FDB routines, not a Starlink
*        inherited status.

*  References:
*     Mills, D., 1992, Automatic ARC wavelength calibration, in P.J.
*     Grosbol, R.C.E. de Ruijsscher (eds), 4th ESO/ST-ECF Data Analysis
*     Workshop, Garching, 13 - 14 May 1992, ESO Conference and Workshop
*     Proceedings No. 41, Garching bei Muenchen, 1992

*  Authors:
*     djm: Dave Mills (UCL)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     26 Feb 1990 (djm):
*        Original version (ID_REF_FEATURES).
*     07 Jun 1993 (hme):
*        Add the permanent data base arrays to the argument list and
*        avoid the common block.
*        Replace REPORT with MSG_OUT.
*        Fix the bug whereby the features in the data base were counted
*        up to where a wavelength is 0.0 even if that is beyond the
*        declared end of the vector.
*        All these array size count loops stopped one short of the
*        declared end of the array.
*        The initial add_features could be greater than the number of
*        positions, since it was at least 12.
*        If max_strength was 0.0, the routine would try to use as many
*        features as are in the data base, which usually is more than
*        were observed. A MAX should be a MIN.
*        Change minimum solution size (from 9). If few positions are
*        given, it is allowed to drop to number of given positions minus
*        2, though it must be at least 6.
*        Where this routine did assume positions to be pixel indices,
*        it now assumes pixel coordinates (=index-0.5). That is, the
*        additional constants for mirroring postition are 0.0 when they
*        used to be 1.0.
*        Change the neighbour limit: It used to be
*        delta_position_limit*2 .GE. no_of_positions. Now it is
*        delta_position_limit .GE. db_scope.
*        In messages use the word "row" instead of "order".
*     10 Jun 1993 (hme):
*        Replaced nx argument with xstart and xend.
*        Added min_solution_size as argument.
*     25 Jan 1995 (hme):
*        Renamed from SPADE.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

      IMPLICIT NONE

*     Global Constants:
      INTEGER         max_ftr_cand
      PARAMETER       ( max_ftr_cand = 100 )
      INTEGER         max_id_ftrs
      PARAMETER       ( max_id_ftrs = 50 )
      INTEGER         max_meta_index
      PARAMETER       ( max_meta_index = 500 )
      INTEGER         max_pos_cand
      PARAMETER       ( max_pos_cand = 1000 )
      INTEGER         max_allowed_rf_feat
      PARAMETER       ( max_allowed_rf_feat = 1000 )

*     Input/Output variables used
      LOGICAL INFO
      INTEGER DB_SCOPE
      INTEGER MAX_ARC_LINES
      INTEGER FTR_DB_INDEX_SIZE
      REAL FTR_LIST( MAX_ARC_LINES )
      REAL FTR_DB( DB_SCOPE, DB_SCOPE, MAX_ARC_LINES )
      BYTE FTR_DB_INDEX_L( DB_SCOPE * DB_SCOPE * MAX_ARC_LINES )
      BYTE FTR_DB_INDEX_R( DB_SCOPE * DB_SCOPE * MAX_ARC_LINES )
      INTEGER * 2
     :   FTR_DB_INDEX_WAVE( DB_SCOPE * DB_SCOPE * MAX_ARC_LINES )
      INTEGER FTR_DB_QUICK_INDEX( FTR_DB_INDEX_SIZE )
      REAL FTR_DB_QUICK_VALUE( FTR_DB_INDEX_SIZE )

      REAL xstart, xend          !Extent of frame
      REAL start_wavelength      !Start search window wavelength
      REAL end_wavelength        !End search window wavelength
      LOGICAL check_reversed     !TRUE if reversed arcs allowed
      REAL min_dispersion        !Minimum dispersion for search window
      REAL max_dispersion        !Maximum dispersion for search window
      REAL differ_thresh         !User specified difference threshold for ratios
      INTEGER min_solution_size  !Minimum number of features in solution
      INTEGER max_positions_to_use !Max number of features to use for initial id
      INTEGER start_delta_posn   !Starting number of neighbours to consider
      INTEGER max_delta_posn     !Maximum number of neighbours to consider
      REAL strength_factor       !Minimum ratio of features to use (in terms of max strength feature)
      INTEGER max_perm_ftrs      !Maximum number of features to use in a fit
      REAL input_ftr_positions ( max_perm_ftrs ) !Observed central positions of features
      REAL obs_strength ( max_perm_ftrs ) !Observed strengths (usually intensity) of features
      INTEGER identified_ftrs    !Count of identified features
      REAL iden_ftr_position ( max_perm_ftrs ) !Positions identified features
      REAL iden_ftr_wavelength ( max_perm_ftrs ) !Positions identified features
      INTEGER status             !Input/Output status condition

*     Workspace variables used
      BYTE left_offset ( max_meta_index , 2,max_id_ftrs) !Which left neighbour was used
      BYTE right_offset ( max_meta_index , 2,max_id_ftrs) !Which right neighbour was used
      INTEGER next_index ( max_meta_index,max_id_ftrs ) !Pointer to entry with next greatest ratio
      INTEGER prev_index ( max_meta_index,max_id_ftrs ) !Pointer to entry with previous greatest ratio
      REAL ratios ( max_meta_index , 4,max_id_ftrs ) !Ratios of features inter-neigbour distances

*     Local variables
      INTEGER i                  !General loop counter
      INTEGER meta_scope         !Scope of meta features (ie out to n'th neighbours)
      INTEGER asplit_ftrs ( 3 )
      INTEGER xsplit_ftrs ( 4 )
      INTEGER num_ratios         !Number of ratios to be searched for
      INTEGER ii                 !Loop counter
      REAL ftr_positions  ( max_allowed_rf_feat ) !Temporary storage for feature positions
      REAL inv_ftr_positions  ( max_allowed_rf_feat ) !Temporary storage for inverted feature positions
      LOGICAL ftr_used ( max_allowed_rf_feat ) !Flags for features used this  search
      INTEGER used_features      !Count of features used this search
      REAL max_strength          !Maximum feature strength
      INTEGER add_features       !Number of features to add for next search
      INTEGER orig_features      !Original number of features to add for next search
      REAL pos_of_max_strength   !Position of max strength feature
      INTEGER ii_of_max_strength !Index of max strength feature
      INTEGER no_of_positions    !Number of observed feature positions
      REAL best_distances ( max_ftr_cand , max_id_ftrs ) !Distance measures for top candidates
      REAL best_dispersion ( max_ftr_cand , max_id_ftrs ) !Dispersions for top candidates
      INTEGER best_ftr ( max_ftr_cand , max_id_ftrs ) !Feature indices for top candidates
      INTEGER best_next_ftr ( max_ftr_cand , max_id_ftrs ) !Feature neighbour indices for top candidates
      INTEGER best_next_ftr2 ( max_ftr_cand , max_id_ftrs ) !Feature neighbour indices for top candidates
      INTEGER best_prev_ftr ( max_ftr_cand , max_id_ftrs ) !Feature neighbour indices for top candidates
      INTEGER best_prev_ftr2 ( max_ftr_cand , max_id_ftrs ) !Feature neighbour indices for top candidates
      BYTE best_posindex ( max_ftr_cand , 4 , max_id_ftrs ) !Database neighbour indices for top candidates
      INTEGER start_wavelength_index !Database index for search start
      INTEGER end_wavelength_index !Database index for search end
      INTEGER meta_guess ( max_id_ftrs ) !Indicies of solution features
      INTEGER meta_ftrs ( max_id_ftrs ) !Indicies of solution features
      INTEGER inv_meta_guess ( max_id_ftrs ) !Indicies of solution features
      INTEGER inv_meta_ftrs ( max_id_ftrs ) !Indicies of solution features
      LOGICAL got_a_match        !TRUE if solution matches
      INTEGER meta_count         !Count of features in solution
      INTEGER inv_meta_count     !Count of features in solution
      INTEGER no_of_features     !Count of features
      LOGICAL reversed           !TRUE if spectrum is reversed
      LOGICAL manual             !TRUE if manual identification of lines going on
      LOGICAL switched           !TRUE if reverse has been checked
      INTEGER delta_position_limit !Limiting neighbour for this search
      INTEGER quick_index_size   !Size of quick index into database
      REAL best_start
      REAL best_end
      REAL overall_dispersion
      CHARACTER * ( 80 ) REPORT_STRING
      INTEGER IGNORE             ! MSG status
      DATA asplit_ftrs/1,2,3/
      DATA xsplit_ftrs/1,2,2,3/

      IGNORE = 0

*%      Initialise 'identified feature' data arrays if automatic run
        manual = .FALSE.
        IF ( .NOT. manual ) THEN
         IF ( identified_ftrs .EQ. 0 ) THEN
           DO ii = 1 , max_perm_ftrs
              iden_ftr_position ( ii  ) = 0.0
              iden_ftr_wavelength ( ii ) = 0.0
           END DO
         ELSE
           IF ( INFO ) THEN
              WRITE ( report_string,1007 )
              CALL MSG_OUT( 'FDB_REPORT', REPORT_STRING, IGNORE )
           END IF
           RETURN
         ENDIF
        ENDIF

*%      Count number of features in database list
        no_of_features = 0

       DO WHILE ( ftr_list ( no_of_features+1 )  .GT. 0.0 .AND.
     :            no_of_features+1 .LE. max_arc_lines )
          no_of_features = no_of_features + 1
       END DO

        DO i = 1 , max_allowed_rf_feat
           ftr_used ( i ) = .FALSE.
        END DO

*%      Count number of observed features for which positions could be obtained
        no_of_positions = 0
        DO WHILE ( input_ftr_positions ( no_of_positions+1 )
     :                          .GT. 0.0 .AND.
     :            (  no_of_positions+1 .LE. max_perm_ftrs  )  )
          no_of_positions = no_of_positions + 1
        END DO

        IF ( no_of_positions .LT. 1 ) THEN
           IF ( INFO ) CALL MSG_OUT( 'FDB_REPORT',
     :        ' No reference features located for this row', IGNORE )
           RETURN
        ENDIF

*%      Determine size of quick-access index into feature database
        quick_index_size = 0
        DO WHILE ( ftr_db_quick_index ( quick_index_size+1 )  .GT. 0
     :            .AND. (quick_index_size+1 .LE. ftr_db_index_size ) )
          quick_index_size = quick_index_size + 1
        END DO

*%      Determine wavelength search window
        CALL SPD_WZKB( INFO, MAX_ARC_LINES, FTR_LIST,
     :     INT(xend-xstart+0.5), no_of_features, min_dispersion,
     :     max_dispersion, start_wavelength, end_wavelength,
     :     max_perm_ftrs, iden_ftr_position, iden_ftr_wavelength,
     :     start_wavelength_index, end_wavelength_index, status )

*       Start with strongest features and progressively add in the fainter
*       ones till we find a good match
*%      Determine strongest feature in observed set
        max_strength = 0.0
        DO ii = 1 , no_of_positions
               IF ( obs_strength ( ii ) .GT.
     :                                  max_strength ) THEN
                 max_strength = obs_strength ( ii )
                 pos_of_max_strength =
     :                  input_ftr_positions ( ii )
                 ii_of_max_strength = ii
              ENDIF
        END DO

*%      Count all features which are at least 'strength_factor' as strong
*       IE if strength_factor=10 then any feature with a strength greater than
*          max feature strength / 10 will be considered
        add_features = 0
        DO ii = 1 , no_of_positions
           IF ( obs_strength ( ii ) .GT.
     :                      max_strength/strength_factor ) THEN
              add_features = add_features + 1
           ENDIF
        END DO

*%      MinMAX the number of features to start with between 12 and 16
        add_features =  MIN ( MAX ( 12 , add_features ) , 16 )
        add_features = MIN( add_features, no_of_positions )
        orig_features = add_features

*%      Start with 'start_delta_posn' neighbours on each side of features
        delta_position_limit = start_delta_posn

*%      Clear 'reversed' flag (assume wavelength increases with x)
        reversed = .FALSE.
        switched = .FALSE.
        meta_count = 0
        inv_meta_count = 0

*%      Inform user of initial state
        used_features = 0
        got_a_match = .FALSE.

*%      Loop until solution found , or all permutations have been tried
        DO WHILE ( ( used_features .LT. no_of_positions ) .AND.
     :             ( used_features .LT. max_positions_to_use ) .AND.
     :                .NOT. got_a_match )
        status = 0

*%       If no strength information was available then
*%       Use all observed features !!!
*        NB. This should not happen normally , this is just a last-ditch
*            attempt for when it does
         IF ( max_strength .EQ. 0.0 ) THEN
           DO ii = 1 , MIN(no_of_positions,max_id_ftrs)
              ftr_positions ( ii ) =
     :              input_ftr_positions ( ii )
              inv_ftr_positions ( no_of_positions-ii+1 ) =
     :              xend - input_ftr_positions ( ii ) + xstart
           END DO
           used_features = MIN(no_of_positions,max_id_ftrs)

*%       Else
         ELSE

*          Extract next most intense 'add_features' and add to
*          'ftr_positions' array
*%         Loop thru number of features to be added to active list
           DO i = 1 , add_features

*%            Search for strongest non-active feature in appropriate fraction of spectrum
              max_strength = 0.0
              DO ii = 1 , no_of_positions
                 IF ( .NOT. ftr_used ( ii ) ) THEN
                    IF ( obs_strength ( ii ) .GT.
     :                                  max_strength ) THEN
                       max_strength = obs_strength ( ii )
                       pos_of_max_strength = input_ftr_positions ( ii )
                       ii_of_max_strength = ii
                    ENDIF
                 ENDIF
              END DO

*%            Add it to list of active features
              used_features = used_features + 1
              ftr_positions ( used_features  ) =  pos_of_max_strength
              ftr_used ( ii_of_max_strength ) = .TRUE.

*%         End loop
           END DO

*%         Clear array of active feature positions
           DO i = 1 , no_of_positions
              ftr_positions ( i ) = 0.0
              inv_ftr_positions ( i ) = 0.0
           END DO

*%         Build array of active feature positions with latest set
           used_features = 0
           add_features = MIN ( MAX ( 3 , no_of_positions/10 ) , 10 )
           DO i = 1 , no_of_positions
              IF ( ftr_used ( i ) ) THEN
                 used_features = used_features + 1
                 ftr_positions ( used_features ) =
     :                                      input_ftr_positions ( i )
              ENDIF
           END DO
           used_features = 0
           DO i = no_of_positions , 1 , -1
              IF ( ftr_used ( i ) ) THEN
                 used_features = used_features + 1
                 inv_ftr_positions ( used_features ) =
     :              xend - input_ftr_positions ( i ) + xstart
              ENDIF
           END DO

*%       Endif
         ENDIF

         IF ( INFO ) THEN
          CALL MSG_OUT( 'FDB_REPORT',
     :                    '---------------------------------------'//
     :                    '----------------------------------------',
     :                    IGNORE )
          WRITE ( report_string , 1005 )
          CALL MSG_OUT( 'FDB_REPORT', report_string, IGNORE )
          WRITE ( report_string , 1000 ) used_features
          CALL MSG_OUT( 'FDB_REPORT', report_string, IGNORE )
          WRITE ( report_string , 1006 )
     :                   start_wavelength,end_wavelength
          CALL MSG_OUT( 'FDB_REPORT', report_string, IGNORE )
          WRITE ( report_string , 1001 ) delta_position_limit
          CALL MSG_OUT( 'FDB_REPORT', report_string, IGNORE )
          CALL MSG_OUT( 'FDB_REPORT',
     :                    '---------------------------------------'//
     :                    '----------------------------------------',
     :                    IGNORE )
         END IF

*%       Search for inverted matches if running automatically
         inv_meta_count = 0
         IF ( check_reversed ) THEN
          IF ( INFO ) CALL MSG_OUT( 'FDB_REPORT',
     :      ' Checking for reversed arc', IGNORE )

*%          Calculate neighbour-neighbour distance ratios for active feature set
            meta_scope = delta_position_limit
            CALL SPD_WZKC( INFO,
     :         max_perm_ftrs, inv_ftr_positions, used_features,
     :         meta_scope, left_offset, right_offset, ratios,
     :         num_ratios, next_index, prev_index, status )

*%          Evaluate distance measures based upon meta-features (4 ratios/5 features)
            CALL SPD_WZKD( INFO,
     :         DB_SCOPE, MAX_ARC_LINES, FTR_DB_INDEX_SIZE,
     :         FTR_LIST, FTR_DB,
     :         FTR_DB_INDEX_L, FTR_DB_INDEX_R, FTR_DB_INDEX_WAVE,
     :         FTR_DB_QUICK_INDEX, FTR_DB_QUICK_VALUE,
     :         inv_ftr_positions, used_features,
     :         meta_scope, differ_thresh, min_dispersion,
     :         max_dispersion, start_wavelength_index,
     :         end_wavelength_index, left_offset, right_offset,
     :         ratios, next_index, best_dispersion, best_ftr,
     :         best_distances, best_posindex, best_prev_ftr,
     :         best_prev_ftr2, best_next_ftr, best_next_ftr2,
     :         status )

*%          Search top meta-feature candidates for consistent solutions
            inv_meta_count = 0
            CALL SPD_WZKG( INFO, MAX_ARC_LINES, FTR_LIST,
     :         best_dispersion, best_ftr, best_distances,
     :         best_prev_ftr, best_prev_ftr2, best_next_ftr,
     :         best_next_ftr2, used_features, inv_meta_ftrs,
     :         inv_meta_guess, inv_meta_count, status )

*%       Endif
         ENDIF

         IF ( .NOT. manual .AND.
     :        inv_meta_count+4 .LT. min_solution_size ) THEN
            IF ( INFO ) CALL MSG_OUT( 'FDB_REPORT',
     :         ' Checking for normal arc', IGNORE )

*%       Calculate neighbour-neighbour distance ratios for active feature set
         meta_scope = delta_position_limit
         CALL SPD_WZKC( INFO,
     :      max_perm_ftrs, ftr_positions, used_features, meta_scope,
     :      left_offset, right_offset, ratios, num_ratios,
     :      next_index, prev_index, status )

*%       Evaluate distance measures based upon meta-features (4 ratios/5 features)
         CALL SPD_WZKD( INFO,
     :      DB_SCOPE, MAX_ARC_LINES, FTR_DB_INDEX_SIZE,
     :      FTR_LIST, FTR_DB,
     :      FTR_DB_INDEX_L, FTR_DB_INDEX_R, FTR_DB_INDEX_WAVE,
     :      FTR_DB_QUICK_INDEX, FTR_DB_QUICK_VALUE,
     :      ftr_positions, used_features, meta_scope, differ_thresh,
     :      min_dispersion, max_dispersion, start_wavelength_index,
     :      end_wavelength_index, left_offset, right_offset,
     :      ratios, next_index, best_dispersion, best_ftr,
     :      best_distances, best_posindex, best_prev_ftr,
     :      best_prev_ftr2, best_next_ftr, best_next_ftr2, status )

*%       Search top meta-feature candidates for consistent solutions
         meta_count = 0
         CALL SPD_WZKG( INFO, MAX_ARC_LINES, FTR_LIST,
     :      best_dispersion, best_ftr, best_distances, best_prev_ftr,
     :      best_prev_ftr2, best_next_ftr, best_next_ftr2,
     :      used_features, meta_ftrs, meta_guess, meta_count, status )

         ENDIF

*%       If a solution is found involving at least
*%                           'min_solution_size' features then
         IF ( manual .OR.
     :        meta_count+4 .GE. min_solution_size .OR.
     :        inv_meta_count+4 .GE. min_solution_size ) THEN

*%      Calculate estimate of dispersion and wavelength range
           overall_dispersion = ( ftr_positions ( meta_ftrs (
     :                                             meta_count ) )   -
     :                         ftr_positions ( meta_ftrs ( 1 ) ) )
     :                                                              /
     :                       ( ftr_list ( best_ftr (
     :                                meta_guess ( meta_count ) ,
     :                                meta_ftrs ( meta_count )))    -
     :                         ftr_list ( best_ftr (
     :                                meta_guess ( 1 ) ,
     :                                meta_ftrs ( 1 ) ) ) )

           best_start = ftr_list ( best_ftr ( meta_guess ( 1 ) ,
     :                                  meta_ftrs ( 1 ) ) ) -
     :                 ftr_positions ( meta_ftrs ( 1 ) ) /
     :                     overall_dispersion

           best_end = best_start
     :              + ( xend - xstart ) / overall_dispersion

           overall_dispersion = ABS ( overall_dispersion )
           IF ( overall_dispersion .LT. min_dispersion .OR.
     :         overall_dispersion .GT. max_dispersion .OR.
     :         best_start .LT. start_wavelength .OR.
     :         best_end .GT. end_wavelength ) THEN

              IF ( INFO ) CALL MSG_OUT( 'FDB_REPORT',
     :           ' Solution fails range/scale criteria', IGNORE )

           ELSE

*%      Report initial results of identification
            IF ( INFO ) THEN
               WRITE ( report_string , 1008 )
     :                       overall_dispersion ,
     :                       best_start , best_end
               CALL MSG_OUT( 'FDB_REPORT', report_string, IGNORE )
            END IF

            identified_ftrs = meta_count + 4

*%      Setup details (wavelength/position) for identified features
            iden_ftr_position ( 1 ) = ftr_positions (  best_posindex (
     :                                   meta_guess ( 1 ) ,1,
     :                                   meta_ftrs ( 1 )  ) )
            iden_ftr_position ( 2 ) = ftr_positions (  best_posindex (
     :                                   meta_guess ( 1 ) ,2,
     :                                   meta_ftrs ( 1 )  ) )
            iden_ftr_wavelength ( 1 ) =
     :               ftr_list ( best_prev_ftr2 ( meta_guess ( 1 ) ,
     :                                         meta_ftrs ( 1 ) ) )
            iden_ftr_wavelength ( 2 ) =
     :               ftr_list ( best_prev_ftr ( meta_guess ( 1 ) ,
     :                                        meta_ftrs ( 1 ) ) )

            DO i = 1 , meta_count
             iden_ftr_position ( i+2 ) =
     :                      ftr_positions ( meta_ftrs ( i ) )
             iden_ftr_wavelength ( i+2 ) =
     :                      ftr_list ( best_ftr ( meta_guess ( i ) ,
     :                                        meta_ftrs ( i ) ) )
            END DO

            iden_ftr_wavelength ( meta_count + 3 ) =
     :          ftr_list ( best_next_ftr ( meta_guess ( meta_count ) ,
     :                                  meta_ftrs ( meta_count ) ) )
            iden_ftr_wavelength ( meta_count + 4 ) =
     :          ftr_list ( best_next_ftr2 ( meta_guess ( meta_count ) ,
     :                                   meta_ftrs ( meta_count ) ) )

            iden_ftr_position ( meta_count + 3 ) =
     :                 ftr_positions (  best_posindex (
     :                                  meta_guess ( meta_count ) ,3,
     :                                  meta_ftrs ( meta_count )  ) )
            iden_ftr_position( meta_count + 4 ) =
     :                 ftr_positions (  best_posindex (
     :                                  meta_guess ( meta_count ) ,4,
     :                                  meta_ftrs ( meta_count )  ) )

            got_a_match = .TRUE.

           ENDIF

*%       Else
*%          Report failure
         ELSE

           IF ( INFO ) THEN
              WRITE ( report_string , 1002 )
              CALL MSG_OUT( 'FDB_REPORT', report_string, IGNORE )
           END IF
           status = -1

*%       Endif
         ENDIF

*%       If no solution yet , and full set of features has been used then
         IF ( status .NE. 0   .AND. (
     :       ( used_features + add_features .GT.
     :                              no_of_positions ) .OR.
     :       ( used_features + add_features .GT.
     :                         max_positions_to_use ) )  ) THEN

*%        Clear active feature flag array
          DO i = 1 , no_of_positions
             ftr_used ( i ) = .FALSE.
          END DO
          used_features = 0

*%        Reset number of active features to original set
          add_features =  orig_features

*%        If still scope for increasing number of neighbours considered then
          IF ( delta_position_limit .LT. max_delta_posn )THEN

*%           Increase by 1 , and inform user
             IF ( delta_position_limit .GE. db_scope ) THEN
                IF ( INFO ) THEN
                   CALL MSG_OUT( 'FDB_REPORT',
     :     ' Not enough candidate features to increase search scope',
     :                IGNORE )
                   WRITE ( report_string , 1004 )
                   CALL MSG_OUT( 'FDB_REPORT', report_string, IGNORE )
                END IF
                got_a_match = .TRUE.
                identified_ftrs = 0
             ELSE
                delta_position_limit = delta_position_limit + 1
                IF ( INFO ) THEN
                   WRITE ( report_string , 1003 )
                   CALL MSG_OUT( 'FDB_REPORT', report_string, IGNORE )
                END IF
                IF ( add_features .LE. delta_position_limit*2+2 )
     :                     add_features = delta_position_limit * 3
             ENDIF

*%        Else
*%           Report failure
          ELSE
             IF ( INFO ) THEN
                WRITE ( report_string , 1004 )
                CALL MSG_OUT( 'FDB_REPORT', report_string, IGNORE )
             END IF
             got_a_match = .TRUE.
             identified_ftrs = 0

*         Endif
          ENDIF

*%       Endif
         ENDIF

*%     End loop
       END DO

*

 1000  FORMAT ( 1X , 'Searches set to use the strongest ',
     :               I3,' features' )
 1001  FORMAT ( 1X , 'Neighbours to be considered :   +/- ',I2 )
 1002  FORMAT ( 1X , 'No adequate solutions found, ',
     :               'increasing active features' )
 1003  FORMAT ( 1X , 'No solutions found, increasing neighbour limit' )
 1004  FORMAT ( 1X , 'No solutions found, ABANDONING row' )
 1005  FORMAT ( 1X , 'Constraints for spectrum ' )
 1006  FORMAT ( 1X , 'Wavelength range from ',F15.1, ' to ',F15.1)
 1007  FORMAT ( 1X , 'Spectrum already has identifications' )

 1008  FORMAT ( 1X , 11HDispersion= ,f16.5,
     :                8H from W=,f10.2,4H to ,f10.2 )

      END
