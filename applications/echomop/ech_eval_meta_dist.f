      SUBROUTINE ECH_EVAL_META_DIST(
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
     :           POSITIONS,
     :           NUM_ACTV_FTRS,
     :           META_SCOPE,
     :           DIFFER_THRESH,
     :           MIN_DISPERSION,
     :           MAX_DISPERSION,
     :           START_WAVE_INDEX,
     :           END_WAVE_INDEX,
     :           LEFT_OFFSET,
     :           RIGHT_OFFSET,
     :           RATIOS,
     :           NEXT_INDEX,
     :           BEST_DISPERSION,
     :           BEST_FTR,
     :           BEST_DISTANCES,
     :           BEST_POSINDEX,
     :           BEST_PREV_FTR,
     :           BEST_PREV_FTR2,
     :           BEST_NEXT_FTR,
     :           BEST_NEXT_FTR2,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_EVAL_META_DIST

*  Purpose:
*     Calculate potential multi-arc-line features.

*  Description:
*     This routine calculates the possible 'meta-features' present in the
*     set of primary feature positions passed to it. Meta-features are
*     characterised by a set of ratios of left/right neighbour distances
*     which all (4) fall within 'differ_thresh' of the ideal situation of
*     perfect ratios (as compared with the reference feature database).
*     Further constraints are applied to filter the set of possible
*     candidate solutions; windows in both wavelength and dispersion are
*     applied according to input arguments.
*     The candidates which pass thru all the tests are then sorted into
*     decreasing order of merit and the top 'max_ftr_cand' ones are recorded
*     alond with details of their predicted dispersion, neighbours etc.

*  Invocation:
*     CALL ECH_EVAL_META_DIST(
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
*     :    POSITIONS,
*     :    NUM_ACTV_FTRS,
*     :    META_SCOPE,
*     :    DIFFER_THRESH,
*     :    MIN_DISPERSION,
*     :    MAX_DISPERSION,
*     :    START_WAVE_INDEX,
*     :    END_WAVE_INDEX,
*     :    LEFT_OFFSET,
*     :    RIGHT_OFFSET,
*     :    RATIOS,
*     :    NEXT_INDEX,
*     :    BEST_DISPERSION,
*     :    BEST_FTR,
*     :    BEST_DISTANCES,
*     :    BEST_POSINDEX,
*     :    BEST_PREV_FTR,
*     :    BEST_PREV_FTR2,
*     :    BEST_NEXT_FTR,
*     :    BEST_NEXT_FTR2,
*     :    STATUS
*     :   )

*  Arguments:
*     MAX_FEATURES = INTEGER (Given)
*        Maximum number of observed features per order.
*     POSITIONS = REAL (Given)
*        Observed x-coords of active features.
*     MIN_DISPERSION = REAL (Given)
*        Minimum allowed dispersion (pixels per A).
*     MAX_DISPERSION = REAL (Given)
*        Maximum allowed dispersion (pixels per A).
*     START_WAVE_INDEX = INTEGER (Given)
*        Index corresponding to lowest wavelength.
*     END_WAVE_INDEX = INTEGER (Given)
*        Index corresponding to highest wavelength.
*     DIFFER_THRESH = REAL (Given)
*        Maximum difference in ratios (percentage).
*     NUM_ACTV_FTRS = INTEGER (Given)
*        Number of active features to consider.
*     META_SCOPE = INTEGER (Given)
*        Scope of meta features (ie out to n'th neighbours).
*     LEFT_OFFSET = BYTE (Returned)
*        Which left neighbour was used.
*     RIGHT_OFFSET = BYTE (Returned)
*        Which right neighbour was used.
*     NEXT_INDEX = INTEGER (Returned)
*        Pointer to entry with next greatest ratio.
*     RATIOS = REAL (Returned)
*        Ratios of features inter-neigbour distances.
*     BEST_DISPERSION = REAL (Returned)
*        Dispersion for candidates.
*     BEST_FTR = INTEGER (Returned)
*        Feature identifications.
*     BEST_DISTANCES = REAL (Returned)
*        Distances for candidates.
*     BEST_POSINDEX = BYTE (Returned)
*        Indicies of 4 neighbours composing the meta-feature.
*     BEST_PREV_FTR = INTEGER (Returned)
*        Prev neighbour prediction for candidates.
*     BEST_PREV_FTR2 = INTEGER (Returned)
*        2nd prev neighbour prediction for candidates.
*     BEST_NEXT_FTR = INTEGER (Returned)
*        Next neighbour prediction for candidates.
*     BEST_NEXT_FTR2 = INTEGER (Returned)
*        2nd neighbour prediction for candidates.
*     DB_SCOPE = INTEGER (Returned)
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
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

*  Method:
*     Clear counters / best candidate storage
*     Count size of db feature quick access index
*     Loop through active features
*      Loop through all calculated ratios to be matched with
*        Calculate index for next (increasing value order) ratio
*        Get order for checking of the four ratios of a meta-feature
*        Calculate limiting slots of the feature database we need to search
*        Loop through quick index entries (into feature db)
*           If slot contains values within range required then
*            Loop through all values in this slot (of feature db)
*             If feature wavelength for this db entry is within range then
*                Get 1st left/right hand neighbouras from databsae
*                Calculate difference between database ratio and observed ratio
*                If ratios are close to each other then
*                 Loop thru all 2nd rhs neighbours out to db_scope
*                   Calculate difference between database ratio and observed ratio
*                   If ratios are close to each other then
*                    Loop thru 2nd left hand neighbours out to db_scope
*                     Calculate difference between database ratio and observed ratio
*                     If ratios are close to each other then
*                         Calculate difference between database ratio and observed ratio
*                         If ratios are close to each other then
*                            Test for reasonable candidate
*                         Endif
*                     Endif
*                    End loop
*                   Endif
*                 End loop
*                Endif
*             Endif
*            End loop
*           Endif
*        End loop
*      End loop
*      Select top 'max_candidates' candidates for eventual consistency search
*     End loop

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
      INCLUDE 'ECH_FEATURE.INC'
      INCLUDE 'ECH_FEATURE_DB.INC'

*  Arguments Given:
      REAL POSITIONS( MAX_FEATURES ) ! Observed x-coords of active features.
      REAL DIFFER_THRESH
      INTEGER NUM_ACTV_FTRS
      INTEGER META_SCOPE
      INTEGER START_WAVE_INDEX
      INTEGER END_WAVE_INDEX
      INTEGER QUICK_INDEX_SIZE

*  Arguments Returned:
      REAL MIN_DISPERSION
      REAL MAX_DISPERSION
      BYTE LEFT_OFFSET( MAX_META_INDEX, 2, MAX_ID_FTRS)
*          ! Which left neighbour was used.
      BYTE RIGHT_OFFSET( MAX_META_INDEX, 2, MAX_ID_FTRS)
*          ! Which right neighbour was used.
      INTEGER NEXT_INDEX( MAX_META_INDEX, MAX_ID_FTRS )
*          ! Pointer to entry with next greatest ratio.
      REAL RATIOS( MAX_META_INDEX, 4, MAX_ID_FTRS)
      REAL BEST_DISPERSION( MAX_FTR_CAND, MAX_ID_FTRS )
*          ! Dispersion for candidates.
      REAL BEST_DISTANCES( MAX_FTR_CAND, MAX_ID_FTRS )
*          ! Wegihts for candidates.
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

*  Status:
      INTEGER STATUS

*  Local Variables:
      REAL BPOS_DISPERSION( MAX_POS_CAND ) ! Dispersion for candidates.
      REAL BPOS_DISTANCES( MAX_POS_CAND )  ! Weights for candidates.
      REAL DIFFERENCE1
      REAL DIFFERENCE2
      REAL DIFFERENCE3
      REAL DIFFERENCE4
      REAL LOW_END
      REAL HIGH_END

      INTEGER BPOS_PREV_FTR( MAX_POS_CAND ) ! Prev. neighbour for candidates.
      INTEGER BPOS_PREV_FTR2( MAX_POS_CAND  ) ! 2nd prev neighbour for candidates.
      INTEGER BPOS_NEXT_FTR( MAX_POS_CAND  ) ! Next neigb for candidates.
      INTEGER BPOS_NEXT_FTR2( MAX_POS_CAND ) ! 2nd neighb for candidates.
      INTEGER BPOS_FTR( MAX_POS_CAND )       ! DB index for candidates.
      INTEGER CCNT( 10 )
      INTEGER I
      INTEGER II
      INTEGER IFTR
      INTEGER POS_L1
      INTEGER POS_L2
      INTEGER POS_R1
      INTEGER POS_R2
      INTEGER CAND_COUNT
      INTEGER IINDEX
      INTEGER IQINDEX
      INTEGER RINDEX
      INTEGER DB_INDEX
      INTEGER NCHAR1
      INTEGER NCHAR2

      LOGICAL FOUND_CANDS

      BYTE BPOS_POSINDEX( MAX_POS_CAND, 4 ) ! Count of times candidate is seen.

      CHARACTER*48 REP_STR
      CHARACTER*4 REF_STR1
      CHARACTER*4 REF_STR2

*  Functions Called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

*  Clear counters / best candidate storage.
      DO I = 1, 10
         CCNT( I )  = 0
      END DO
      DO I = 1, MAX_FTR_CAND
         DO II = 1, MAX_ID_FTRS
            BEST_FTR( I, II ) = 0
            BEST_PREV_FTR( I, II ) = 0
            BEST_PREV_FTR2( I, II ) = 0
            BEST_NEXT_FTR( I, II ) = 0
            BEST_NEXT_FTR2( I, II ) = 0
            BEST_DISTANCES( I, II ) = 0.0
            BEST_POSINDEX( I, 1, II ) = 0
            BEST_POSINDEX( I, 2, II ) = 0
            BEST_POSINDEX( I, 3, II ) = 0
            BEST_POSINDEX( I, 4, II ) = 0
         END DO
      END DO

      STATUS = 0

*  Loop through active features.
      FOUND_CANDS = .FALSE.
      DO IFTR = 3, NUM_ACTV_FTRS - 2
         CAND_COUNT = 0

*     Loop through all calculated ratios to be matched with.
         RINDEX = 1
         DO WHILE ( RINDEX .NE. NEXT_INDEX( RINDEX, IFTR ) )

*        Calculate index for next (increasing value order) ratio
*        Get order for checking of the four ratios of a meta-feature.
            RINDEX = NEXT_INDEX( RINDEX, IFTR )

*        Calculate limiting slots of the feature database we need to search.
            LOW_END = RATIOS( RINDEX,1,IFTR ) / ( 1.0 + DIFFER_THRESH )
            HIGH_END = RATIOS( RINDEX,1,IFTR ) * ( 1.0 + DIFFER_THRESH )

*        Loop through quick index entries (into feature db).
            DO IQINDEX = 1, QUICK_INDEX_SIZE

*           If slot contains values within range required then.
               IF ( ftr_db_quick_value( iqindex ) .GE. low_end .AND.
     :              ftr_db_quick_value( iqindex-1) .LE. high_end ) THEN

*              Loop through all values in this slot (of feature db).
                  DO iindex = ftr_db_quick_index( iqindex ),
     :                        ftr_db_quick_index( iqindex + 1 ) - 1
                     db_index = ftr_db_index_wave( iindex )

*                 If feature wavelength for this db entry is within range then.
                     IF ( db_index .GE. start_wave_index .AND.
     :                    db_index .LE. end_wave_index )  THEN

*                    Get 1st left/right hand neighbours from databsae.
                        pos_l1 = ftr_db_index_l( iindex )
                        pos_r1 = ftr_db_index_r( iindex )
                        ccnt( 1 ) = ccnt( 1 ) + 1

*                    Check that the ratio is available.
                        IF ( ftr_db( pos_l1, pos_r1, db_index ) .EQ.
     :                       0.0 ) THEN
                           GO TO 300
                        END IF

*                    Calculate difference between database ratio and
*                    observed ratio.
                        difference1 = ABS( 1.0 -
     :                                ABS( ratios( rindex, 1,iftr ) /
     :                                ftr_db( pos_l1, pos_r1,
     :                                        db_index ) ) )

*                    If ratios are not close to each other then.
                        IF ( difference1 .GE. differ_thresh ) THEN
                           GO TO 300
                        END IF

*                    Loop thru all 2nd rhs neighbours out to db_scope.
                        DO pos_r2 = pos_r1 + 1,
     :                          MIN( db_scope, MAX_FEATURES - DB_INDEX )

*                       Calculate difference between database ratio and
*                       observed ratio.
                           ccnt( 2 ) = ccnt( 2 ) + 1
                           difference2 = ABS( 1.0 -
     :                                ABS( ratios( rindex, 2,iftr ) /
     :                                ftr_db( pos_l1,
     :                                        pos_r2, db_index ) ) )

*                       If ratios are not close to each other then.
                           IF ( difference2 .GE. differ_thresh ) THEN
                              GO TO 200
                           END IF

*                       Loop thru 2nd left hand neighbours out to db_scope.
                           DO pos_l2 = pos_l1 + 1,
     :                                 MIN( db_scope, DB_INDEX - 1 )

*                          Calculate difference between database ratio and
*                          observed ratio.
                              ccnt( 3 ) = ccnt( 3 ) + 1
                              difference3 = ABS( 1.0 -
     :                                ABS( ratios( rindex, 3,iftr ) /
     :                                ftr_db( pos_l2,
     :                                        pos_r1, db_index ) ) )

*                          If ratios are not close to each other then.
                              IF ( difference3 .GE. differ_thresh ) THEN
                                 GO TO 100
                              END IF

*                          Calculate difference between database ratio and
*                          observed ratio.
                              ccnt( 4 ) = ccnt( 4 ) + 1
                              difference4 = ABS( 1.0 -
     :                                ABS( ratios( rindex, 4,iftr ) /
     :                                ftr_db( pos_l2,
     :                                        pos_r2, db_index ) ) )

*                          If ratios are close to each other then
*                          test for reasonable candidate.
                              IF ( difference4 .LT. differ_thresh ) THEN
                                 ccnt( 5 ) = ccnt( 5 ) + 1
                                 CALL ECH_META_CANDIDATE(
     :                              max_features,
     :                              db_scope,
     :                              ftr_db_index_size,
     :                              ftr_list,
     :                              ftr_strength,
     :                              ftr_db,
     :                              ftr_db_index_l,
     :                              ftr_db_index_r,
     :                              ftr_db_index_wave,
     :                              ftr_db_quick_index,
     :                              ftr_db_quick_value,
     :                              rindex,
     :                              db_index,
     :                              num_actv_ftrs,
     :                              differ_thresh,
     :                              min_dispersion,
     :                              max_dispersion,
     :                              iftr,
     :                              positions,
     :                              pos_l1,
     :                              pos_l2,
     :                              pos_r1,
     :                              pos_r2,
     :                              ratios(1,1,iftr),
     :                              left_offset(1,1,iftr),
     :                              right_offset(1,1,iftr),
     :                              bpos_dispersion,
     :                              bpos_ftr,
     :                              bpos_distances,
     :                              bpos_posindex,
     :                              bpos_prev_ftr,
     :                              bpos_prev_ftr2,
     :                              bpos_next_ftr,
     :                              bpos_next_ftr2,
     :                              cand_count,
     :                              ccnt,
     :                              status )
                              ENDIF
  100                         CONTINUE
                           END DO
  200                      CONTINUE
                        END DO
  300                   CONTINUE
                     END IF
                  END DO
               ENDIF
            END DO
         END DO

*     Select top 'max_candidates' candidates for eventual consistency search.
         CALL ECH_TOP_META_CANDS( BPOS_DISPERSION, BPOS_FTR,
     :        BPOS_DISTANCES, BPOS_POSINDEX, BPOS_PREV_FTR,
     :        BPOS_PREV_FTR2, BPOS_NEXT_FTR, BPOS_NEXT_FTR2,
     :        BEST_DISPERSION( 1, IFTR ), BEST_FTR( 1, IFTR ),
     :        BEST_DISTANCES( 1, IFTR ), BEST_POSINDEX( 1, 1, IFTR ),
     :        BEST_PREV_FTR( 1, IFTR ), BEST_PREV_FTR2( 1, IFTR ),
     :        BEST_NEXT_FTR( 1, IFTR ), BEST_NEXT_FTR2( 1, IFTR ),
     :        CAND_COUNT, STATUS )

         IF ( CAND_COUNT .NE. 0 ) THEN
            FOUND_CANDS = .TRUE.
            CALL CHR_ITOC( IFTR, REF_STR1, NCHAR1 )
            CALL CHR_ITOC( CAND_COUNT, REF_STR2, NCHAR2 )
            REP_STR = ' Feature ' // REF_STR1( :NCHAR1 ) //
     :            ' cand. count: ' // REF_STR2( :NCHAR2 ) // '.'
            CALL ECH_REPORT( 0, REP_STR )
         END IF
      END DO

      IF ( .NOT. FOUND_CANDS ) THEN
         CALL ECH_REPORT( 0, ' No candidates found.' )
      END IF

      END
