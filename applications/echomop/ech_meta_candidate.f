      SUBROUTINE ECH_META_CANDIDATE(
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
     :           RINDEX,
     :           DB_INDEX,
     :           NUM_ACTV_FTRS,
     :           DIFFER_THRESH,
     :           MIN_DISPERSION,
     :           MAX_DISPERSION,
     :           FTR_NUM,
     :           POSITIONS,
     :           POS_L1,
     :           POS_L2,
     :           POS_R1,
     :           POS_R2,
     :           RATIOS,
     :           LEFT_OFFSET,
     :           RIGHT_OFFSET,
     :           BPOS_DISPERSION,
     :           BPOS_FTR,
     :           BPOS_DISTANCES,
     :           BPOS_POSINDEX,
     :           BPOS_PREV_FTR,
     :           BPOS_PREV_FTR2,
     :           BPOS_NEXT_FTR,
     :           BPOS_NEXT_FTR2,
     :           CAND_COUNT,
     :           CCNT,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_META_CANDIDATE

*  Purpose:
*     Select top meta-feature candidates

*  Description:
*     This routine takes input meta features distances from the 'perfect'
*     solution, and saves the top 'meta_ftr_cand' candidates for each
*     feature. Candidates are first tested for consistency by demanding
*     that they accurately predict the four ratios when expanded to
*     include 6 features (they include 5 when they enter this routine).
*     The sixth feature will be the next-left for features more than
*     half way through the set of all features, and next-right for
*     features in the first half.
*
*       EG.  If feature 5 has a candidate which incorporates features
*            3, 4, 6, and 7 ; and there are a total of 20 features
*            under active analysis, then this routine will add in feature
*            8 and check that the predictions for it are consistent with
*            the set of 5 already obtained.
*
*    Candidates which pass this last test (i.e. generate neighbour-
*    neighbour ratios close to those observed in thre reference database)
*    will be added to the list of candidates for the current feature.
*    This list is not in any special order, the newest candidate is just
*    added to the end of the list.

*  Invocation:
*     CALL ECH_META_CANDIDATE(
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
*     :    RINDEX,
*     :    DB_INDEX,
*     :    NUM_ACTV_FTRS,
*     :    DIFFER_THRESH,
*     :    MIN_DISPERSION,
*     :    MAX_DISPERSION,
*     :    FTR_NUM,
*     :    POSITIONS,
*     :    POS_L1,
*     :    POS_L2,
*     :    POS_R1,
*     :    POS_R2,
*     :    RATIOS,
*     :    LEFT_OFFSET,
*     :    RIGHT_OFFSET,
*     :    BPOS_DISPERSION,
*     :    BPOS_FTR,
*     :    BPOS_DISTANCES,
*     :    BPOS_POSINDEX,
*     :    BPOS_PREV_FTR,
*     :    BPOS_PREV_FTR2,
*     :    BPOS_NEXT_FTR,
*     :    BPOS_NEXT_FTR2,
*     :    CAND_COUNT,
*     :    CCNT,
*     :    STATUS
*     :   )

*  Arguments:
*     RINDEX = INTEGER (Given)
*        Index into ratio property arrays.
*     DB_INDEX = INTEGER (Given)
*        Index into reference database.
*     FTR_NUM = INTEGER (Given)
*        Feature number (active).
*     POS_L1 = INTEGER (Given)
*        Offset of first left hand neighbour identified.
*     POS_R1 = INTEGER (Given)
*        Offset of first right hand neighbour identified.
*     POS_L2 = INTEGER (Given)
*        Offset of second left hand neighbour identified.
*     POS_R2 = INTEGER (Given)
*        Offset of second right hand neighbour identified.
*     POSITIONS = REAL (Given)
*        Observed x-coords of active features.
*     FTR_NUM = INTEGER (Given)
*        Index into feature arrays.
*     LEFT_OFFSET = BYTE (Given)
*        Which left neighbour was used.
*     RIGHT_OFFSET = BYTE (Given)
*        Which right neighbour was used.
*     RATIOS = REAL (Returned)
*        Ratios of features inter-neigbour distances
*        the list is not sorted into order as this
*        takes too long. Instead it is treated as
*        a linked list of entries, with next_index()
*        showing the order of entries
*     BPOS_DISPERSION = REAL (Returned)
*        Dispersion for candidates.
*     BPOS_DISTANCES = REAL (Returned)
*        Distances for candidates.
*     BPOS_POSINDEX = BYTE (Returned)
*        Indicies of 4 features composing meta-features.
*     BPOS_PREV_FTR = INTEGER (Returned)
*        Prev neighbour prediction for candidates.
*     BPOS_PREV_FTR2 = INTEGER (Returned)
*        2nd prev neighbour prediction for candidates.
*     BPOS_NEXT_FTR = INTEGER (Returned)
*        Next neighbour prediction for candidates.
*     BPOS_NEXT_FTR2 = INTEGER (Returned)
*        2nd neighbour prediction for candidates.
*     BPOS_FTR = INTEGER (Returned)
*        Feature identifications.
*     CAND_COUNT = INTEGER (Given and Returned)
*        NUmber of possible candidates so far.
*     CCNT = INTEGER (Given and Returned)
*        Counters of success/rejection at each stage.
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
*     NUM_ACTV_FTRS = INTEGER (Given)
*        Number of active features to consider.
*     DIFFER_THRESH = REAL (Given)
*        Maximum difference in ratios (percentage).
*     MIN_DISPERSION = INTEGER (Given and Returned)
*        Minimum dispersion.
*     MAX_DISPERSION = INTEGER (Given and Returned)
*        Maximum dispersion.
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
*       Tidy-up.
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

*  Arguments Given:
      INTEGER RINDEX
      INTEGER DB_INDEX
      INTEGER FTR_NUM
      INTEGER POS_L1
      INTEGER POS_L2
      INTEGER POS_R1
      INTEGER POS_R2
      REAL POSITIONS( MAX_FEATURES ) ! Observed X-coords of active features.
      REAL DIFFER_THRESH
      INTEGER NUM_ACTV_FTRS

*  Arguments Returned:
      INTEGER CAND_COUNT
      BYTE LEFT_OFFSET( MAX_META_INDEX, 2 )  ! Which left neighbour was used.
      BYTE RIGHT_OFFSET( MAX_META_INDEX, 2 ) ! Which right neighbour was used.
      REAL RATIOS( MAX_META_INDEX, 4 )
      REAL BPOS_DISPERSION( MAX_POS_CAND )   ! Dispersion for candidates.
      REAL BPOS_DISTANCES( MAX_POS_CAND )    ! Weights for candidates.
      BYTE BPOS_POSINDEX( MAX_POS_CAND, 4 )  ! Indicies of meta-feature
*                                            ! components.
      INTEGER BPOS_PREV_FTR( MAX_POS_CAND )  ! Prev neighbour prediction for
*                                            ! candidates.
      INTEGER BPOS_PREV_FTR2( MAX_POS_CAND ) ! 2nd prev neighbour for
*                                            ! candidates.
      INTEGER BPOS_NEXT_FTR( MAX_POS_CAND  ) ! Next neigb prediction for
*                                            ! candidates.
      INTEGER BPOS_NEXT_FTR2( MAX_POS_CAND ) ! 2nd neighb for candidates.
      INTEGER BPOS_FTR( MAX_POS_CAND  )      ! DB index for candidates.
      REAL MIN_DISPERSION
      REAL MAX_DISPERSION
      INTEGER CCNT( 10 )

*  Status:
      INTEGER STATUS

*  Local Variables:
      REAL DISTANCE    ! N-d origin distance which is a measure of
*                      ! the difference between the observed set of
*                      ! n-n ratios, and the 'perfect' set calculated
*                      ! using the reference list of wavelengths.
      REAL DISPERSION
      REAL DISPERSION2
      REAL RATIO_1
      REAL RATIO_2
      REAL RATIO_3
      REAL RATIO_4

      INTEGER POS2_L1
      INTEGER POS2_L2
      INTEGER POS2_R1
      INTEGER POS2_R2
      INTEGER EXTRA_L1
      INTEGER EXTRA_L2
      INTEGER EXTRA_R1
      INTEGER EXTRA_R2
      INTEGER START_L1
      INTEGER START_L2
      INTEGER START_R1
      INTEGER START_R2
      INTEGER END_L1
      INTEGER END_L2
      INTEGER END_R1
      INTEGER END_R2
      INTEGER IFTR2
      INTEGER DB_INDEX2
*.

*  If feature under study (primary feature) is more than halfway
*  through set of all active features.
      IF ( FTR_NUM .GT. NUM_ACTV_FTRS / 2 ) THEN

*     Number of extra feature to check is primary features' 1st left
*     neighbour.
         IFTR2 = FTR_NUM - LEFT_OFFSET( RINDEX, 1 )
         DB_INDEX2 = DB_INDEX - POS_L1

*     Calculate corresponding identities for the 3 known components of the
*     extra 'meta-feature', and the range of search for the unknown one.
         START_R1 = POS_L1
         END_R1 = POS_L1
         START_L1 = POS_L2 - POS_L1
         END_L1 = POS_L2 - POS_L1
         START_R2 =  POS_L1 + POS_R1
         END_R2 = MIN( POS_L1 + POS_R1, DB_SCOPE )
         START_L2 = START_L1 + 1
         END_L2 = DB_SCOPE
         EXTRA_R1 = LEFT_OFFSET( RINDEX,1 )
         EXTRA_R2 = LEFT_OFFSET( RINDEX,1) + RIGHT_OFFSET( RINDEX, 1 )
         EXTRA_L1 = LEFT_OFFSET( RINDEX,2 ) - LEFT_OFFSET( RINDEX, 1 )
         EXTRA_L2 = EXTRA_L1 + 1

      ELSE

*     Number of extra feature to check is primary features' 1st right
*     neighbour.
         IFTR2 = FTR_NUM + RIGHT_OFFSET( RINDEX, 1 )
         DB_INDEX2 = DB_INDEX + POS_R1

*     Calculate corresponding identities for the 3 known components of the
*     extra 'meta-feature', and the range of search for the unknown one.
         START_R1 = POS_R2 - POS_R1
         END_R1 = POS_R2 - POS_R1
         START_L1 = POS_R1
         END_L1 = POS_R1
         START_L2 = POS_L1 + POS_R1
         END_L2 = MIN( POS_L1 + POS_R1, DB_SCOPE )
         START_R2 = START_R1 + 1
         END_R2 = DB_SCOPE
         EXTRA_L1 = RIGHT_OFFSET( RINDEX, 1  )
         EXTRA_L2 = RIGHT_OFFSET( RINDEX, 1 ) + LEFT_OFFSET( RINDEX, 1 )
         EXTRA_R1 = RIGHT_OFFSET( RINDEX, 2 ) -
     :         RIGHT_OFFSET( RINDEX, 1 )
         EXTRA_R2 = EXTRA_R1 + 1
      END IF

*  If possible to check for extra meta-feature then
*  calculate ratios for extra meta-feature.
      IF ( IFTR2 - EXTRA_L1 .GT. 0 .AND.
     :     IFTR2 - EXTRA_L2 .GT. 0 .AND.
     :     IFTR2 + EXTRA_R1 .LE. NUM_ACTV_FTRS .AND.
     :     IFTR2 + EXTRA_R2 .LE. NUM_ACTV_FTRS  ) THEN
         RATIO_1 = ( POSITIONS( IFTR2 ) -
     :          POSITIONS( IFTR2 - EXTRA_L1 ) ) /
     :          ( POSITIONS( IFTR2 + EXTRA_R1 ) - POSITIONS( IFTR2 ) )
         RATIO_2 = ( POSITIONS( IFTR2 ) -
     :          POSITIONS( IFTR2 - EXTRA_L1 ) ) /
     :          ( POSITIONS( IFTR2 + EXTRA_R2 ) - POSITIONS( IFTR2 ) )
         RATIO_3 = ( POSITIONS( IFTR2 ) -
     :          POSITIONS( IFTR2 - EXTRA_L2 ) ) /
     :          ( POSITIONS( IFTR2 + EXTRA_R1 ) - POSITIONS( IFTR2 ) )
         RATIO_4 = ( POSITIONS( IFTR2 ) -
     :          POSITIONS( IFTR2 - EXTRA_L2 ) ) /
     :          ( POSITIONS( IFTR2 + EXTRA_R2 ) - POSITIONS( IFTR2 ) )

*     Loop through first left hand neighbours allowed.
         DO POS2_L1 = START_L1, MIN( END_L1, DB_INDEX2 - 1 )

*        Loop through first right hand neighbours allowed.
            DO POS2_R1 = START_R1,
     :            MIN( END_R1, MAX_FEATURES - DB_INDEX2 )

*           If ratio (L1/R1) is within threshold.
               IF ( ABS( 1.0 - ABS( RATIO_1 /
     :              FTR_DB( POS2_L1, POS2_R1, DB_INDEX2 ) ) ) .LT.
     :              DIFFER_THRESH ) THEN
                  CCNT( 6 ) = CCNT( 6 ) + 1

*              Loop through second right hand neighbours allowed.
                  DO POS2_R2 = START_R2,
     :                  MIN( END_R2, MAX_FEATURES - DB_INDEX2 )

*                 If ratio (L1/R2) is within threshold.
                     IF ( ABS( 1.0 - ABS( RATIO_2 /
     :                    FTR_DB( POS2_L1, POS2_R2, DB_INDEX2 ) ) )
     :                    .LT. DIFFER_THRESH ) THEN
                        CCNT( 7 ) = CCNT( 7 ) + 1

*                    Loop through second left hand neighbours allowed.
                        DO POS2_L2 = START_L2,
     :                        MIN( END_L2, DB_INDEX2 - 1 )

*                       If ratios (L2/R1 and L2/R2) are within threshold.
                           IF ( ABS( 1.0 - ABS( RATIO_3 /
     :                          FTR_DB( POS2_L2, POS2_R1, DB_INDEX2 ) ))
     :                           .LT. DIFFER_THRESH  .AND.
     :                          ( ABS( 1.0 - ABS( RATIO_4 /
     :                          FTR_DB( POS2_L2, POS2_R2, DB_INDEX2 ) ))
     :                          .LT. DIFFER_THRESH ) ) THEN
                              CCNT( 8 ) = CCNT( 8 ) + 1

*                          Calculate dispersion corresponding to predicted
*                          wavelengths.
                              DISPERSION = ( POSITIONS(
     :                              FTR_NUM + RIGHT_OFFSET( RINDEX, 2 )
     :                              ) - POSITIONS( FTR_NUM -
     :                              LEFT_OFFSET( RINDEX, 1 ) ) ) /
     :                              ( FTR_LIST( DB_INDEX + POS_R2 )  -
     :                              FTR_LIST( DB_INDEX - POS_L1 ) )
                              DISPERSION2 = ( POSITIONS(
     :                              FTR_NUM + RIGHT_OFFSET( RINDEX, 1 )
     :                              ) - POSITIONS( FTR_NUM -
     :                              LEFT_OFFSET( RINDEX, 2 ) ) ) /
     :                              ( FTR_LIST( DB_INDEX + POS_R1 )  -
     :                              FTR_LIST( DB_INDEX - POS_L2 ) )

*                          If dispersion consistent and within dispersion
*                          window.
                              IF ( ABS( 1.0 - DISPERSION / DISPERSION2 )
     :                             .LT. 0.1  .AND.
     :                             DISPERSION .GE. MIN_DISPERSION .AND.
     :                             DISPERSION .LE. MAX_DISPERSION ) THEN
                                 CCNT( 9 ) = CCNT( 9 ) + 1

*                             Calculate distance measure based on the
*                             difference between the four ratios and
*                             their 'perfect' values.
                                 DISTANCE = 0.0
                                 IF ( RATIOS( RINDEX, 1 ) .GT.
     :                                FTR_DB( POS_L1, POS_R1, DB_INDEX )
     :                                ) THEN
                                    DISTANCE = DISTANCE +
     :                                    ( RATIOS( RINDEX, 1 ) /
     :                                    FTR_DB( POS_L1, POS_R1,
     :                                    DB_INDEX ) ) ** 2.0

                                 ELSE
                                    DISTANCE = DISTANCE +
     :                                    ( FTR_DB( POS_L1, POS_R1,
     :                                    DB_INDEX ) / RATIOS( RINDEX,
     :                                    1 ) ) **2.0
                                 END IF
                                 IF ( RATIOS( RINDEX, 2 ) .GT.
     :                                FTR_DB( POS_L1, POS_R2, DB_INDEX )
     :                                ) THEN
                                    DISTANCE = DISTANCE +
     :                                    ( RATIOS( RINDEX, 2 ) /
     :                                    FTR_DB( POS_L1, POS_R2,
     :                                    DB_INDEX ) ) ** 2.0

                                 ELSE
                                    DISTANCE = DISTANCE +
     :                                    ( FTR_DB( POS_L1, POS_R2,
     :                                    DB_INDEX ) / RATIOS( RINDEX,
     :                                    2 ) ) ** 2.0
                                 END IF
                                 IF ( RATIOS( RINDEX, 3 ) .GT.
     :                                FTR_DB( POS_L2, POS_R1, DB_INDEX )
     :                                ) THEN
                                    DISTANCE = DISTANCE +
     :                                    ( RATIOS( RINDEX, 3 ) /
     :                                    FTR_DB( POS_L2, POS_R1,
     :                                    DB_INDEX ) ) ** 2.0

                                 ELSE
                                    DISTANCE = DISTANCE +
     :                                    ( FTR_DB( POS_L2, POS_R1,
     :                                    DB_INDEX ) / RATIOS( RINDEX,
     :                                    3 ) ) ** 2.0
                                 END IF
                                 IF ( RATIOS( RINDEX, 4 ) .GT.
     :                              FTR_DB( POS_L2, POS_R2, DB_INDEX )
     :                              ) THEN
                                    DISTANCE = DISTANCE +
     :                                    ( RATIOS( RINDEX, 4 ) /
     :                                    FTR_DB( POS_L2, POS_R2,
     :                                    DB_INDEX ) ) ** 2.0

                                 ELSE
                                    DISTANCE = DISTANCE +
     :                                    ( FTR_DB( POS_L2, POS_R2,
     :                                    DB_INDEX ) / RATIOS ( RINDEX,
     :                                    4 ) ) ** 2.0
                                 END IF

*                             Normalise distance measure
*                             (so that perfect = 0.0).
                                 DISTANCE = DISTANCE - 4.0

*                             Step to next candidate entry.
                                 CAND_COUNT = CAND_COUNT + 1
                                 STATUS = 0

*                             If max candidates exceeded then
*                             set return status accordingly.
                                 IF ( CAND_COUNT .GT. MAX_POS_CAND )
     :                                THEN
                                    STATUS = ECH__META_TOOMANY
                                    CAND_COUNT = CAND_COUNT - 1

*                             Add new candidate details.
                                 ELSE
                                    BPOS_PREV_FTR( CAND_COUNT ) =
     :                                    DB_INDEX - POS_L1
                                    BPOS_PREV_FTR2( CAND_COUNT ) =
     :                                    DB_INDEX - POS_L2
                                    BPOS_NEXT_FTR( CAND_COUNT ) =
     :                                    DB_INDEX + POS_R1
                                    BPOS_NEXT_FTR2( CAND_COUNT ) =
     :                                    DB_INDEX + POS_R2
                                    BPOS_DISTANCES( CAND_COUNT ) =
     :                                    DISTANCE
                                    BPOS_DISPERSION( CAND_COUNT ) =
     :                                    DISPERSION
                                    BPOS_FTR( CAND_COUNT ) = DB_INDEX
                                    BPOS_POSINDEX( CAND_COUNT, 1 ) =
     :                                    FTR_NUM -
     :                                    LEFT_OFFSET( RINDEX, 2 )
                                    BPOS_POSINDEX( CAND_COUNT, 2 ) =
     :                                    FTR_NUM -
     :                                    LEFT_OFFSET( RINDEX, 1 )
                                    BPOS_POSINDEX( CAND_COUNT, 3 ) =
     :                                    FTR_NUM +
     :                                    RIGHT_OFFSET( RINDEX, 1 )
                                    BPOS_POSINDEX( CAND_COUNT, 4 ) =
     :                              FTR_NUM +
     :                              RIGHT_OFFSET( RINDEX, 2 )
                                 END IF
                              END IF
                           END IF
                        END DO
                     END IF
                  END DO
               END IF
            END DO
         END DO
      END IF

      END
