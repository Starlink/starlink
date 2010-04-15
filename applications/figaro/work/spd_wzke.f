      SUBROUTINE SPD_WZKE( DB_SCOPE, MAX_ARC_LINES, FTR_LIST, FTR_DB,
     :   rindex, db_index, num_actv_ftrs, differ_thresh,
     :   min_dispersion, max_dispersion, ftr_num, positions,
     :   pos_l1, pos_l2, pos_r1, pos_r2, ratios,
     :   left_offset, right_offset, bpos_dispersion, bpos_ftr,
     :   bpos_distances, bpos_posindex, bpos_prev_ftr,
     :   bpos_prev_ftr2, bpos_next_ftr, bpos_next_ftr2,
     :   cand_count, ccnt, status )
*+
*  Name:
*     SPD_WZKE

*  Purpose:
*     FDB: select top meta-feature candidates.

*  Language:
*     Fortran

*  Invocation:
*     CALL SPD_WZKE( DB_SCOPE, MAX_ARC_LINES, FTR_LIST, FTR_DB,
*        rindex, db_index, num_actv_ftrs, differ_thresh,
*        min_dispersion, max_dispersion, ftr_num, positions,
*        pos_l1, pos_l2, pos_r1, pos_r2, ratios,
*        left_offset, right_offset, bpos_dispersion, bpos_ftr,
*        bpos_distances, bpos_posindex, bpos_prev_ftr,
*        bpos_prev_ftr2, bpos_next_ftr, bpos_next_ftr2,
*        cand_count, ccnt, status )

*  Description:
*     This is a modification of Dave Mills' routine META_CANDIDATE (cf.
*     Mills 1992). "FDB" stands for "feature data base", which is a data
*     base of known features in arc spectra.
*
*     This routine takes input meta features distances from the
*     'perfect' solution , and saves the top 'meta_ftr_cand' candidates
*     for each feature. Candidates are first tested for consistency by
*     demanding that they accurately predict the four ratios when
*     expanded to include 6 features (they include 5 when they enter
*     this routine). The sixth feature will be the next-left for
*     features more than  half way thru the set of all features , and
*     next-right for features in the first half.
*
*       EG.  If feature 5 has a candidate which incorporates features
*            3 , 4 , 6 , and 7 ; and there are a total of 20 features
*            under active analysis, then this routine will add in
*            feature 8 and check that the predictions for it are
*            consistent with the set of 5 already obtained.
*
*    Candidates which pass this last test (ie generate
*    neighbour-neiggbour ratios close to those observed in thr reference
*    database) will be added to the list of candidates for the current
*    feature. This list is not in any special order, the newest
*    candidate is just added to the end of the list.

*  Arguments:
*     DB_SCOPE = INTEGER (Given)
*        An FDB array dimension.
*     MAX_ARC_LINES = INTEGER (Given)
*        An FDB array dimension.
*     FTR_LIST( MAX_ARC_LINES ) = REAL (Given)
*        The FDB wavelengths (FTR_WAVE).
*     FTR_DB( DB_SCOPE, DB_SCOPE, MAX_ARC_LINES ) = REAL (Given)
*        The FDB DB array (FTR_DB).
*     rindex = INTEGER (Given)
*        Index into ratio property arrays
*     db_index = INTEGER (Given)
*        Index into reference database
*     ftr_num = INTEGER (Given)
*        Feature number (active)
*     pos_l1 = INTEGER (Given)
*        Offset of first left hand neighbour identified
*     pos_r1 = INTEGER (Given)
*        Offset of first right hand neighbour identified
*     pos_l2 = INTEGER (Given)
*        Offset of second left hand neighbour identified
*     pos_r2 = INTEGER (Given)
*        Offset of second right hand neighbour identified
*     positions() = REAL (Given)
*        Observed x-coords of active features
*     ftr_num = INTEGER (Given)
*        Index into feature arrays
*     left_offset() = BYTE (Given)
*        Which left neighbour was used
*     right_offset() = BYTE (Given)
*        Which right neighbour was used
*     ratios() = REAL (Returned)
*        Ratios of features inter-neigbour distances the list is not
*        sorted into order as this takes too long. Instead it is
*        treated as a linked list of entries , with next_index()
*        showing the order of entries
*     bpos_dispersion() = REAL (Returned)
*        Dispersion for candidates
*     bpos_distances() = REAL (Returned)
*        Distances for candidates
*     bpos_posindex() = BYTE (Returned)
*        Indicies of 4 features composing meta-features
*     bpos_prev_ftr() = INTEGER (Returned)
*        Prev neighbour prediction for candidates
*     bpos_prev_ftr2() = INTEGER (Returned)
*        2nd prev neighbour prediction for candidates
*     bpos_next_ftr() = INTEGER (Returned)
*        Next neighbour prediction for candidates
*     bpos_next_ftr2() = INTEGER (Returned)
*        2nd neighbour prediction for candidates
*     bpos_ftr() = INTEGER (Returned)
*        Feature identifications
*     cand_count = INTEGER (Given and Returned)
*        NUmber of possible candidates so far
*     ccnt( 10 ) = INTEGER (Given and Returned)
*        Counters of success/rejection at each stage
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
*        Original version (META_CANDIDATE).
*     25 May 1990 (hme):
*        Add the permanent data base arrays to the argument list and
*        avoid the common block.
*        Disuse status constant parameters.
*     25 Jan 1995 (hme):
*        Renamed from SPADJ.
*     {enter_changes_here}

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
      INTEGER DB_SCOPE
      INTEGER MAX_ARC_LINES
      REAL FTR_LIST( MAX_ARC_LINES )
      REAL FTR_DB( DB_SCOPE, DB_SCOPE, MAX_ARC_LINES )

      INTEGER cand_count         !Candidate index in meta feature arrays
      INTEGER rindex             !Index into ratios arrays
      INTEGER db_index           !Index into active feature arrays
      INTEGER ftr_num            !Feature Index into active feature array
      INTEGER pos_l1             !Offset of 1st left neighbour
      INTEGER pos_l2             !Offset of 2nd left neighbour
      INTEGER pos_r1             !Offset of 1st right neighbour
      INTEGER pos_r2             !Offset of 2nd right neighbour
      REAL positions ( max_allowed_rf_feat ) !Observed x-coords of active features
      BYTE left_offset ( max_meta_index , 2 ) !Which left neighbour was used
      BYTE right_offset ( max_meta_index , 2 ) !Which right neighbour was used
      REAL ratios ( max_meta_index , 4 ) !Ratios of features inter-neigbour distances
      REAL bpos_dispersion ( max_pos_cand  ) !Dispersion for candidates
      REAL bpos_distances ( max_pos_cand ) !Wegihts for candidates
      BYTE bpos_posindex ( max_pos_cand , 4 ) !Indicies of meta-feature components
      INTEGER bpos_prev_ftr ( max_pos_cand ) !Prev neighbour prediction for candidates
      INTEGER bpos_prev_ftr2 ( max_pos_cand ) !2nd prev neighbour for candidates
      INTEGER bpos_next_ftr ( max_pos_cand  ) !Next neigb prediction for candidates
      INTEGER bpos_next_ftr2 ( max_pos_cand ) !2nd neighb for candidates
      INTEGER bpos_ftr ( max_pos_cand  ) !DB index for candidates
      REAL min_dispersion
      REAL max_dispersion
      REAL differ_thresh
      INTEGER num_actv_ftrs
      INTEGER ccnt ( 10 )        !Counters of success/rejection at each stage
      INTEGER status             !Input/Output status condition

*     Local variables
      REAL distance
*          N-d origin distance which is
*          a measure of the difference between the
*          observed set of n-n ratios,  and the
*          'perfect' set calculated using the reference
*          list of wavelengths.
      REAL dispersion            !Predicted dispersion over feature
      REAL dispersion2           !Predicted dispersion over feature
      REAL ratio_1               !L1/R1 ratio for adjacent feature
      REAL ratio_2               !L1/R2 ratio for adjacent feature
      REAL ratio_3               !L2/R1 ratio for adjacent feature
      REAL ratio_4               !L2/R2 ratio for adjacent feature
      INTEGER pos2_l1            !Possible First left hand neighbour
      INTEGER pos2_l2            !Possible Second left hand neighbour
      INTEGER pos2_r1            !Possible First right hand neighbour
      INTEGER pos2_r2            !Possible Second right hand neighbour
      INTEGER extra_l1           !Extra check 1st left hand neighbour offset
      INTEGER extra_l2           !Extra check 2nd left hand neighbour offse
      INTEGER extra_r1           !Extra check 1st right hand neighbour offse
      INTEGER extra_r2           !Extra check 2nd right hand neighbour offse
      INTEGER start_l1           !Start db offset to 1st lhn
      INTEGER start_l2           !Start db offset to 2nd lhn
      INTEGER start_r1           !Start db offset to 1st rhn
      INTEGER start_r2           !Start db offset to 2nd rhn
      INTEGER end_l1             !End db offset to 1st lhn
      INTEGER end_l2             !End db offset to 2nd lhn
      INTEGER end_r1             !End db offset to 1st rhn
      INTEGER end_r2             !End db offset to 2nd rhn
      INTEGER iftr2              !Extra feature number to calculate ratios for
      INTEGER db_index2          !Extra db index predicted for feature

*%    If feature under study (primary feature) is more than halfway
*%    thru set of all active features then
      IF ( ftr_num .GT. num_actv_ftrs/2 ) THEN

*%       Number of extra feature to check is primary features' 1st left neighbour
         iftr2 = ftr_num - left_offset ( rindex ,1 )
         db_index2 = db_index - pos_l1

*%       Calculate corresponding identities for the 3 known components of the
*%       extra 'meta-feature' , and the range of search for the unknown one
         start_r1 = pos_l1
         end_r1 = pos_l1
         start_l1 = pos_l2 - pos_l1
         end_l1 = pos_l2 - pos_l1
         start_r2 =  pos_l1 + pos_r1
         end_r2 = MIN ( pos_l1 + pos_r1 , db_scope )
         start_l2 = start_l1 + 1
         end_l2 = db_scope
         extra_r1 = left_offset ( rindex ,1 )
         extra_r2 = left_offset ( rindex ,1) +
     :                     right_offset ( rindex , 1 )
         extra_l1 = left_offset ( rindex ,2 ) -
     :                     left_offset ( rindex , 1 )
         extra_l2 = extra_l1 + 1

*%    Else
      ELSE

*%       Number of extra feature to check is primary features' 1st right neighbour
         iftr2 = ftr_num + right_offset ( rindex , 1 )
         db_index2 = db_index + pos_r1

*%       Calculate corresponding identities for the 3 known components of the
*%       extra 'meta-feature' , and the range of search for the unknown one
         start_r1 = pos_r2-pos_r1
         end_r1 = pos_r2-pos_r1
         start_l1 = pos_r1
         end_l1 = pos_r1
         start_l2 = pos_l1+pos_r1
         end_l2 = MIN ( pos_l1+pos_r1 , db_scope )
         start_r2 = start_r1 + 1
         end_r2 = db_scope
         extra_l1 = right_offset ( rindex , 1  )
         extra_l2 = right_offset ( rindex , 1 ) +
     :                        left_offset ( rindex , 1  )
         extra_r1 = right_offset ( rindex , 2 ) -
     :                        right_offset ( rindex , 1 )
         extra_r2 = extra_r1 + 1

*%    Endif
      ENDIF

*%    If possible to check for extra meta-feature then
*%     Calculate ratios for extra meta-feature
      IF ( iftr2-extra_l1 .GT. 0 .AND.
     :     iftr2-extra_l2 .GT. 0 .AND.
     :     iftr2+extra_r1 .LE. num_actv_ftrs .AND.
     :     iftr2+extra_r2 .LE. num_actv_ftrs  ) THEN

       ratio_1 = ( positions ( iftr2 ) -
     :                   positions ( iftr2-extra_l1 ) ) /
     :                     ( positions ( iftr2+extra_r1  ) -
     :                       positions ( iftr2 ) )
       ratio_2 = ( positions ( iftr2 ) -
     :                   positions ( iftr2-extra_l1 ) ) /
     :                     ( positions ( iftr2+extra_r2 ) -
     :                       positions ( iftr2 ) )
       ratio_3 = ( positions ( iftr2 ) -
     :                   positions ( iftr2-extra_l2 ) ) /
     :                     ( positions ( iftr2+extra_r1 ) -
     :                       positions ( iftr2 ) )
       ratio_4 = ( positions ( iftr2 ) -
     :                   positions ( iftr2-extra_l2 ) ) /
     :                     ( positions ( iftr2+extra_r2 ) -
     :                       positions ( iftr2 ) )

*%     Loop thru first left hand neighbours allowed
* 1 >>>
       DO pos2_l1 = start_l1 , end_l1

*%      Loop thru first right hand neighbours allowed
* 2 >>>>
        DO pos2_r1 = start_r1 , end_r1

*%       If ratio (L1/R1) is within threshold then
         IF ( ABS ( 1.0 - ABS ( ratio_1 /
     :           ftr_db ( pos2_l1 , pos2_r1 ,db_index2 ) ) )
     :                        .LT. differ_thresh ) THEN
           ccnt(6) = ccnt(6) + 1

*%         Loop thru second right hand neighbours allowed
* 3 >>>>>>>
           DO pos2_r2 = start_r2 , end_r2

*%           If ratio (L1/R2) is within threshold then
             IF ( ABS ( 1.0 - ABS ( ratio_2 /
     :             ftr_db ( pos2_l1 , pos2_r2 , db_index2 ) ) )
     :                        .LT. differ_thresh ) THEN
               ccnt(7) = ccnt(7)+ 1

*%             Loop thru second left hand neighbours allowed
* 4 >>>>>>>>>>>
               DO pos2_l2 = start_l2 , end_l2

*%               If ratios (L2/R1 and L2/R2) are within threshold then
                 IF ( ABS ( 1.0 - ABS ( ratio_3 /
     :                ftr_db ( pos2_l2 , pos2_r1 , db_index2 ) ) )
     :                           .LT. differ_thresh  .AND.
     :                         ( ABS ( 1.0 - ABS ( ratio_4 /
     :                ftr_db ( pos2_l2 , pos2_r2 , db_index2 ) ) )
     :                                  .LT. differ_thresh ) ) THEN
                   ccnt(8) = ccnt(8) + 1

*%                 Calculate dispersion corresponding to predicted wavelengths
                   dispersion = (  positions
     :                    ( ftr_num + right_offset ( rindex,2 ) ) -
     :                                       positions
     :                    ( ftr_num - left_offset ( rindex,1 ) ) ) /
     :                            ( ftr_list ( db_index + pos_r2 )  -
     :                              ftr_list ( db_index - pos_l1 )  )
                   dispersion2 = (  positions
     :                    ( ftr_num + right_offset ( rindex,1 ) ) -
     :                                       positions
     :                    ( ftr_num - left_offset ( rindex,2 ) ) ) /
     :                            ( ftr_list ( db_index + pos_r1 )  -
     :                              ftr_list ( db_index - pos_l2 )  )

*%                 If dispersion consistent and within dispersion window then
                   IF ( ABS ( 1.0 - dispersion / dispersion2 )
     :                                              .LT. 0.1  .AND.
     :                  ( ( dispersion .GE. min_dispersion) .AND.
     :                    ( dispersion .LE. max_dispersion) )
     :                                                   )  THEN
                      ccnt(9) = ccnt(9) + 1

*%                    Calculate distance measure based on the difference between
*%                    the four ratios and their 'perfect' values
                      distance = 0.0
                      IF ( ratios(rindex,1) .GT.
     :                     ftr_db ( pos_l1,pos_r1,db_index) ) THEN

                         distance = distance +
     :                              ( ratios(rindex,1) /
     :                        ftr_db ( pos_l1,pos_r1,db_index) ) **2.0

                      ELSE
                         distance = distance +
     :                         ( ftr_db ( pos_l1,pos_r1,db_index)  /
     :                           ratios ( rindex,1 ) ) **2.0

                      ENDIF


                      IF ( ratios(rindex,2) .GT.
     :                     ftr_db ( pos_l1,pos_r2,db_index) ) THEN

                         distance = distance +
     :                              ( ratios(rindex,2) /
     :                        ftr_db ( pos_l1,pos_r2,db_index) ) **2.0

                      ELSE

                         distance = distance +
     :                         ( ftr_db ( pos_l1,pos_r2,db_index)  /
     :                           ratios ( rindex,2 ) ) **2.0

                      ENDIF


                      IF ( ratios(rindex,3) .GT.
     :                     ftr_db ( pos_l2,pos_r1,db_index) ) THEN

                         distance = distance +
     :                              ( ratios(rindex,3) /
     :                        ftr_db ( pos_l2,pos_r1,db_index) ) **2.0

                      ELSE

                         distance = distance +
     :                         ( ftr_db ( pos_l2,pos_r1,db_index)  /
     :                           ratios ( rindex,3) ) **2.0

                      ENDIF


                      IF ( ratios(rindex,4) .GT.
     :                     ftr_db ( pos_l2,pos_r2,db_index) ) THEN

                         distance = distance +
     :                              ( ratios(rindex,4) /
     :                        ftr_db ( pos_l2,pos_r2,db_index) ) **2.0

                      ELSE

                         distance = distance +
     :                         ( ftr_db ( pos_l2,pos_r2,db_index)  /
     :                           ratios ( rindex,4 ) ) **2.0

                      ENDIF

*%                    Normalise distance measure (so that perfect = 0.0)
                      distance = distance - 4.0

*%                    Step to next candidate entry
                      status = 0
                      cand_count = cand_count + 1

*%                    If max candidates exceeded then
*%                       Set return status accordingly
                      IF ( cand_count .GT. max_pos_cand ) THEN

                         status = -2
                         cand_count = cand_count - 1

*%                    Else
                      ELSE

*%                       Add new candidate details
                         bpos_prev_ftr ( cand_count ) =
     :                                            db_index - pos_l1
                         bpos_prev_ftr2 ( cand_count ) =
     :                                            db_index - pos_l2
                         bpos_next_ftr ( cand_count ) =
     :                                            db_index + pos_r1
                         bpos_next_ftr2 ( cand_count ) =
     :                                            db_index + pos_r2
                         bpos_distances ( cand_count ) = distance
                         bpos_dispersion ( cand_count ) = dispersion
                         bpos_ftr ( cand_count ) = db_index
                         bpos_posindex ( cand_count , 1 ) = ftr_num -
     :                                  left_offset ( rindex , 2 )
                         bpos_posindex ( cand_count , 2 ) = ftr_num -
     :                                  left_offset ( rindex , 1 )
                         bpos_posindex ( cand_count , 3 ) = ftr_num +
     :                                  right_offset ( rindex , 1 )
                         bpos_posindex ( cand_count , 4 ) = ftr_num +
     :                                  right_offset ( rindex , 2 )

*%                    Endif
                      ENDIF

*%                 Endif
                   ENDIF

*%               Endif
                 ENDIF

*%             End loop
* 4 <<<<<<<<<<<
               END DO

*%           Endif
             ENDIF

*%         End loop
* 3 <<<<<<<
           END DO

*%       Endif
         ENDIF

*%      End loop
* 2 <<<<
        END DO

*%     End loop
* 1 <<<
       END DO

*%    Endif
      ENDIF

      END
