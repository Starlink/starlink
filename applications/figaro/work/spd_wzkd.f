      SUBROUTINE SPD_WZKD( INFO,
     :   DB_SCOPE, MAX_ARC_LINES, FTR_DB_INDEX_SIZE,
     :   FTR_LIST, FTR_DB,
     :   FTR_DB_INDEX_L, FTR_DB_INDEX_R, FTR_DB_INDEX_WAVE,
     :   FTR_DB_QUICK_INDEX, FTR_DB_QUICK_VALUE,
     :   positions, num_actv_ftrs, meta_scope, differ_thresh,
     :   min_dispersion, max_dispersion, start_wave_index,
     :   end_wave_index, left_offset, right_offset, ratios,
     :   next_index, best_dispersion, best_ftr, best_distances,
     :   best_posindex, best_prev_ftr, best_prev_ftr2,
     :   best_next_ftr, best_next_ftr2, status )
*+
*  Name:
*     SPD_WZKD

*  Purpose:
*     FDB: calculate potential multi-arc-line features.

*  Language:
*     Fortran

*  Invocation:
*     CALL SPD_WZKD( INFO,
*        DB_SCOPE, MAX_ARC_LINES, FTR_DB_INDEX_SIZE,
*        FTR_LIST, FTR_DB,
*        FTR_DB_INDEX_L, FTR_DB_INDEX_R, FTR_DB_INDEX_WAVE,
*        FTR_DB_QUICK_INDEX, FTR_DB_QUICK_VALUE,
*        positions, num_actv_ftrs, meta_scope, differ_thresh,
*        min_dispersion, max_dispersion, start_wave_index,
*        end_wave_index, left_offset, right_offset, ratios,
*        next_index, best_dispersion, best_ftr, best_distances,
*        best_posindex, best_prev_ftr, best_prev_ftr2,
*        best_next_ftr, best_next_ftr2, status )

*  Description:
*     This is a modification of Dave Mills' routine EVAL_META_DIST (cf.
*     Mills 1992). "FDB" stands for "feature data base", which is a data
*     base of known features in arc spectra.
*
*     This routine calculates the possible 'meta-features' present in
*     the set of primary feature positions passed to it. Meta-features
*     are characterised by a set of ratios of left/right neighbour
*     distances which all (4) fall within 'differ_thresh' of the ideal
*     situation of perfect ratios (as compared with the reference
*     feature database). Further constraints are applied to further
*     filter the set of possible candidate solutions; windows in both
*     wavelength and dispersion are applied according to input
*     arguments. The candidates which pass thru all the tests are then
*     sorted into decreasing order of merit and the top 'max_ftr_cand'
*     ones are recorded alond with details of their predicted
*     dispersion, neighbours etc.

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
*     positions() = REAL (Given)
*        Observed x-coords of active features
*     min_dispersion = REAL (Given)
*        Minimum allowed dispersion (pixels per A)
*     max_dispersion = REAL (Given)
*        Maximum allowed dispersion (pixels per A)
*     start_wave_index = INTEGER (Given)
*        Index corresponding to lowest wavelength
*     end_wave_index = INTEGER (Given)
*        Index corresponding to highest wavelength
*     differ_thresh = REAL (Given)
*        Maximum difference in ratios (percentage)
*     num_actv_ftrs = INTEGER (Given)
*        Number of active features to consider
*     meta_scope = INTEGER (Given)
*        Scope of meta features (ie out to n'th neighbours)
*     left_offset() = BYTE (Returned)
*        Which left neighbour was used
*     right_offset() = BYTE (Returned)
*        Which right neighbour was used
*     next_index() = INTEGER (Returned)
*        Pointer to entry with next greatest ratio
*     ratios() = REAL (Returned)
*        Ratios of features inter-neigbour distances the list is not
*        sorted into order as this takes too long. Instead it is
*        treated as a linked list of entries , with next_index()
*        showing the order of entries
*     best_dispersion() = REAL (Returned)
*        Dispersion for candidates
*     best_ftr() = INTEGER (Returned)
*        Feature identifications
*     best_distances() = REAL (Returned)
*        Distances for candidates
*     best_posindex() = BYTE (Returned)
*        Indicies of 4 neighbours composing the meta-feature
*     best_prev_ftr() = INTEGER (Returned)
*        Prev neighbour prediction for candidates
*     best_prev_ftr2() = INTEGER (Returned)
*        2nd prev neighbour prediction for candidates
*     best_next_ftr() = INTEGER (Returned)
*        Next neighbour prediction for candidates
*     best_next_ftr2() = INTEGER (Returned)
*        2nd neighbour prediction for candidates
*     status = INTEGER (Given and Returned)
*        Input/Output status conditions
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
*     31 Jan 1990 (djm):
*        Original version (EVAL_META_DIST).
*     25 May 1990 (hme):
*        Add the permanent data base arrays to the argument list and
*        avoid the common block.
*     25 Jan 1995 (hme):
*        Renamed from SPADH.
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

      INTEGER num_actv_ftrs
      REAL positions ( num_actv_ftrs ) !Observed x-coords of active features
      REAL differ_thresh         !maximum percentage difference in ratios
      INTEGER meta_scope         !Scope of meta features (ie out to n'th neighbours)
      REAL min_dispersion        !minimum allowed dispersion
      REAL max_dispersion        !maximum allowed dispersion
      INTEGER start_wave_index   !index corresponding to lowest wavelength
      INTEGER end_wave_index     !index corresponding to highest wavelength
      BYTE left_offset ( max_meta_index , 2 , max_id_ftrs) !Which left neighbour was used
      BYTE right_offset ( max_meta_index , 2, max_id_ftrs) !Which right neighbour was used
      INTEGER next_index ( max_meta_index, max_id_ftrs ) !Pointer to entry with next greatest ratio
      REAL ratios ( max_meta_index , 4, max_id_ftrs) !Ratios of features inter-neigbour distances
      REAL best_dispersion ( max_ftr_cand , max_id_ftrs ) !Dispersion for candidates
      REAL best_distances ( max_ftr_cand , max_id_ftrs ) !Wegihts for candidates
      BYTE best_posindex ( max_ftr_cand , 4 , max_id_ftrs ) !Count of times a candidate is seen
      INTEGER best_prev_ftr ( max_ftr_cand , max_id_ftrs ) !Prev neighbour prediction for candidates
      INTEGER best_prev_ftr2 ( max_ftr_cand , max_id_ftrs ) !2nd prev neighbour for candidates
      INTEGER best_next_ftr ( max_ftr_cand , max_id_ftrs ) !Next neigb prediction for candidates
      INTEGER best_next_ftr2 ( max_ftr_cand , max_id_ftrs ) !2nd neighb for candidates
      INTEGER best_ftr ( max_ftr_cand , max_id_ftrs ) !DB index for candidates
      INTEGER status             !Input/Output status condition

*     Local variables
      INTEGER i,ii               !General loop counter
      INTEGER iftr               !Index pointing into positions (ie which feature)
      INTEGER pos_l1             !Possible First left hand neighbour
      INTEGER pos_l2             !Possible Second left hand neighbour
      INTEGER pos_r1             !Possible First right hand neighbour
      INTEGER pos_r2             !Possible Second right hand neighbour
      REAL difference1           !Difference between observed/db ratios
      REAL difference2           !Difference between observed/db ratios
      REAL difference3           !Difference between observed/db ratios
      REAL difference4           !Difference between observed/db ratios
      INTEGER ccnt ( 10 )        !Counters for each stage of candidate rejection
      REAL bpos_dispersion ( max_pos_cand ) !Dispersion for candidates
      REAL bpos_distances ( max_pos_cand ) !Wegihts for candidates
      BYTE bpos_posindex ( max_pos_cand , 4 ) !Count of times a candidate is seen
      INTEGER bpos_prev_ftr ( max_pos_cand ) !Prev neighbour prediction for candidates
      INTEGER bpos_prev_ftr2 ( max_pos_cand  ) !2nd prev neighbour for candidates
      INTEGER bpos_next_ftr ( max_pos_cand  ) !Next neigb prediction for candidates
      INTEGER bpos_next_ftr2 ( max_pos_cand ) !2nd neighb for candidates
      INTEGER bpos_ftr ( max_pos_cand ) !DB index for candidates
      INTEGER cand_count         !Count of candiates for this gfeature
      BYTE ratidx ( 4 , 4 )
      REAL low_end
      REAL high_end
      INTEGER iindex
      INTEGER quick_index_size
      INTEGER iqindex
      INTEGER rindex
      INTEGER db_index
      CHARACTER * ( 80 ) REPORT_STRING
      INTEGER IGNORE             ! MSG status
      DATA ratidx   / 1 , 2 , 3 , 4 ,
     :                2 , 1 , 3 , 4 ,
     :                3 , 1 , 2 , 4 ,
     :                4 , 1 , 2 , 3   /

      IGNORE = 0

*%      Clear counters / best candidate storage
        DO i = 1 , 10
         ccnt ( i )  = 0
        END DO
        DO i = 1 , max_ftr_cand
         DO ii = 1 , max_id_ftrs
           best_ftr ( i,ii ) = 0
           best_prev_ftr ( i,ii ) = 0
           best_prev_ftr2 ( i,ii ) = 0
           best_next_ftr ( i,ii ) = 0
           best_next_ftr2 ( i,ii ) = 0
           best_distances ( i,ii ) = 0.0
           best_posindex ( i,1,ii ) = 0
           best_posindex ( i,2,ii ) = 0
           best_posindex ( i,3,ii ) = 0
           best_posindex ( i,4,ii ) = 0
         END DO
        END DO

*%      Count size of db feature quick access index
        status = 0
        quick_index_size = 0
        DO WHILE ( ftr_db_quick_index ( quick_index_size+1 )  .GT. 0 )
          quick_index_size = quick_index_size + 1
        END DO

*%      Loop through active features
* 2 >>>>
        DO iftr = 3 , num_actv_ftrs - 2

         cand_count = 0

*%       Loop through all calculated ratios to be matched with
         rindex = 1
* 3 >>>>>
         DO WHILE ( rindex .NE. next_index ( rindex , iftr ) )

*%         Calculate index for next (increasing value order) ratio
*%         Get order for checking of the four ratios of a meta-feature
           rindex = next_index ( rindex , iftr )

*%         Calculate limiting slots of the feature database we need to search
           low_end = ratios ( rindex,1,iftr )  /
     :                              ( 1.0 + differ_thresh )
           high_end = ratios ( rindex,1,iftr ) *
     :                              ( 1.0 + differ_thresh )

*%         Loop through quick index entries (into feature db)
* 3 >>>>>>>
           DO iqindex = 1 , quick_index_size

*%            If slot contains values within range required then
              IF ( ftr_db_quick_value ( iqindex ) .GE. low_end .AND.
     :            ftr_db_quick_value ( iqindex-1) .LE. high_end ) THEN

*%             Loop through all values in this slot (of feature db)
* 4 >>>>>>>>>>>
               DO iindex =  ftr_db_quick_index ( iqindex ) ,
     :                    ftr_db_quick_index ( iqindex+1 ) - 1

*%              If feature wavelength for this db entry is within range then
                db_index = ftr_db_index_wave ( iindex )
                IF ( db_index .GE. start_wave_index .AND.
     :               db_index .LE. end_wave_index )  THEN

*%                 Get 1st left/right hand neighbouras from databsae
                   pos_l1 = ftr_db_index_l ( iindex )
                   pos_r1 = ftr_db_index_r ( iindex )
                   ccnt(1) = ccnt(1) + 1

*%                 Calculate difference between database ratio and observed ratio
                   difference1 = ABS ( 1.0 -
     :                         ABS ( ratios ( rindex , 1,iftr ) /
     :                                     ftr_db ( pos_l1 , pos_r1 ,
     :                                               db_index )
     :                                    )
     :                                )

*%                 If ratios are close to each other then
                   IF ( difference1 .LT. differ_thresh ) THEN

*%                  Loop thru all 2nd rhs neighbours out to db_scope
* 5 >>>>>>>>>>>>>>>>
                    DO pos_r2 = pos_r1 + 1 , db_scope

*%                    Calculate difference between database ratio and observed ratio
                      ccnt(2) = ccnt(2) + 1
                      difference2 = ABS ( 1.0 -
     :                         ABS ( ratios ( rindex , 2,iftr ) /
     :                                   ftr_db ( pos_l1 ,
     :                                            pos_r2 , db_index )
     :                                    )
     :                                  )

*%                    If ratios are close to each other then
                      IF ( difference2 .LT. differ_thresh ) THEN

*%                     Loop thru 2nd left hand neighbours out to db_scope
* 6 >>>>>>>>>>>>>>>>>>>
                       DO pos_l2 = pos_l1 + 1 , db_scope

*%                      Calculate difference between database ratio and observed ratio
                        ccnt(3) = ccnt(3) + 1
                        difference3 = ABS ( 1.0 -
     :                         ABS ( ratios ( rindex , 3,iftr ) /
     :                                   ftr_db ( pos_l2 ,
     :                                            pos_r1 , db_index )
     :                                    )
     :                                   )

*%                      If ratios are close to each other then
                        IF ( difference3 .LT. differ_thresh ) THEN

*%                          Calculate difference between database ratio and observed ratio
                            ccnt(4) = ccnt(4) + 1
                            difference4 = ABS ( 1.0 -
     :                       ABS ( ratios ( rindex , 4,iftr ) /
     :                                   ftr_db ( pos_l2 ,
     :                                            pos_r2 , db_index )
     :                                      )
     :                                    )

*%                          If ratios are close to each other then
                            IF ( difference4 .LT. differ_thresh ) THEN

*%                             Test for reasonable candidate
                               ccnt(5) = ccnt(5) + 1
                               CALL SPD_WZKE( DB_SCOPE, MAX_ARC_LINES,
     :                           FTR_LIST, FTR_DB,
     :                           rindex, db_index, num_actv_ftrs,
     :                           differ_thresh, min_dispersion,
     :                           max_dispersion, iftr, positions,
     :                           pos_l1, pos_l2, pos_r1, pos_r2,
     :                           ratios(1,1,iftr),
     :                           left_offset(1,1,iftr),
     :                           right_offset(1,1,iftr),
     :                           bpos_dispersion, bpos_ftr,
     :                           bpos_distances, bpos_posindex,
     :                           bpos_prev_ftr, bpos_prev_ftr2,
     :                           bpos_next_ftr, bpos_next_ftr2,
     :                           cand_count, ccnt, status )

*%                          Endif
                            ENDIF

*%                      Endif
                        ENDIF

*%                     End loop
* 6 <<<<<<<<<<<<<<<<<<<
                       END DO

*%                    Endif
                      ENDIF

*%                  End loop
* 5 <<<<<<<<<<<<<<<<
                    END DO

*%                 Endif
                   ENDIF

*%              Endif
                ENDIF

 900            CONTINUE

*%             End loop
* 4 <<<<<<<<<<<
               END DO

*%            Endif
              ENDIF

*%         End loop
* 3 <<<<<<<
           END DO

*%       End loop
* 2 <<<<<
         END DO

*%       Select top 'max_candidates' candidates for eventual consistency search
         CALL SPD_WZKF(
     :      bpos_dispersion, bpos_ftr, bpos_distances, bpos_posindex,
     :      bpos_prev_ftr, bpos_prev_ftr2, bpos_next_ftr,
     :      bpos_next_ftr2, best_dispersion(1,iftr), best_ftr(1,iftr),
     :      best_distances(1,iftr), best_posindex(1,1,iftr),
     :      best_prev_ftr(1,iftr), best_prev_ftr2(1,iftr),
     :      best_next_ftr(1,iftr), best_next_ftr2(1,iftr),
     :      cand_count, status )

         IF ( INFO ) THEN
            WRITE ( report_string , 1000 ) iftr , cand_count
            CALL MSG_OUT( 'FDB_REPORT', report_string, IGNORE )
         END IF

*%      End loop
* 1 <<<<
        END DO

*
 1000 FORMAT ( 1X , 'Candidate count for feature ' , I3 , ' is ', I5 )

      END
