      SUBROUTINE SPD_WZKF(
     :   bpos_dispersion, bpos_ftr, bpos_distances, bpos_posindex,
     :   bpos_prev_ftr, bpos_prev_ftr2, bpos_next_ftr,
     :   bpos_next_ftr2, best_dispersion, best_ftr,
     :   best_distances, best_posindex, best_prev_ftr,
     :   best_prev_ftr2, best_next_ftr, best_next_ftr2,
     :   cand_count, status )
*+
*  Name:
*     SPD_WZKF

*  Purpose:
*     FDB: select best meta-features

*  Language:
*     Fortran

*  Invocation:
*     CALL SPD_WZKF(
*        bpos_dispersion, bpos_ftr, bpos_distances, bpos_posindex,
*        bpos_prev_ftr, bpos_prev_ftr2, bpos_next_ftr,
*        bpos_next_ftr2, best_dispersion, best_ftr,
*        best_distances, best_posindex, best_prev_ftr,
*        best_prev_ftr2, best_next_ftr, best_next_ftr2,
*        cand_count, status )

*  Description:
*     This is a modification of Dave Mills' routine TOP_META_CANDS (cf.
*     Mills 1992). "FDB" stands for "feature data base", which is a data
*     base of known features in arc spectra.
*
*     This routine takes input meta features distances from the
*     'perfect' solution , and saves the top 'meta_ftr_cand' candidates
*     for each feature.

*  Arguments:
*     bpos_dispersion() = REAL (Returned)
*        Dispersion for candidates
*     bpos_distances() = REAL (Returned)
*        Distances for candidates
*     bpos_posindex() = BYTE (Returned)
*        Indicies of 5 features composing meta-features
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
*     best_dispersion() = REAL (Returned)
*        Dispersion for candidates
*     best_distances() = REAL (Returned)
*        Distances for candidates
*     best_posindex() = BYTE (Returned)
*        Indicies of 5 features composing meta-features
*     best_prev_ftr() = INTEGER (Returned)
*        Prev neighbour prediction for candidates
*     best_prev_ftr2() = INTEGER (Returned)
*        2nd prev neighbour prediction for candidates
*     best_next_ftr() = INTEGER (Returned)
*        Next neighbour prediction for candidates
*     best_next_ftr2() = INTEGER (Returned)
*        2nd neighbour prediction for candidates
*     best_ftr() = INTEGER (Returned)
*        Feature identifications
*     cand_count = INTEGER (Given and Returned)
*        NUmber of possible candidates so far
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
*     01 Feb 1990 (djm):
*        Original version (TOP_META_CANDS).
*     26 May 1993 (hme):
*        Just some tidying.
*     25 Jan 1995 (hme):
*        Renamed from SPADK.
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
      INTEGER cand_count         !Candidate index in meta feature arrays
      REAL bpos_dispersion ( max_pos_cand  ) !Dispersion for candidates
      REAL bpos_distances ( max_pos_cand ) !Wegihts for candidates
      BYTE bpos_posindex ( max_pos_cand , 4 ) !Indicies of meta-feature components
      INTEGER bpos_prev_ftr ( max_pos_cand ) !Prev neighbour prediction for candidates
      INTEGER bpos_prev_ftr2 ( max_pos_cand ) !2nd prev neighbour for candidates
      INTEGER bpos_next_ftr ( max_pos_cand  ) !Next neigb prediction for candidates
      INTEGER bpos_next_ftr2 ( max_pos_cand ) !2nd neighb for candidates
      INTEGER bpos_ftr ( max_pos_cand  ) !DB index for candidates
      REAL best_dispersion ( max_ftr_cand  ) !Dispersion for candidates
      REAL best_distances ( max_ftr_cand ) !Wegihts for candidates
      BYTE best_posindex ( max_ftr_cand , 4 ) !Indicies of meta-feature components
      INTEGER best_prev_ftr ( max_ftr_cand ) !Prev neighbour prediction for candidates
      INTEGER best_prev_ftr2 ( max_ftr_cand ) !2nd prev neighbour for candidates
      INTEGER best_next_ftr ( max_ftr_cand  ) !Next neigb prediction for candidates
      INTEGER best_next_ftr2 ( max_ftr_cand ) !2nd neighb for candidates
      INTEGER best_ftr ( max_ftr_cand  ) !DB index for candidates
      INTEGER status             !Input/Output status condition

*     Local variables
      INTEGER i                  !General loop counter
      LOGICAL found_all          !TRUE when already a candidate at same wavelength
      INTEGER best_count         !Number of best candidates found so far
      REAL best                  !Best distance found so far this loop
      INTEGER got_best           !Index for top candidate

*%    Check for special case of no candidates at all
      best_count = 0
      found_all = .FALSE.
      IF ( cand_count .EQ. 0 ) found_all = .TRUE.

*%    Loop until found all (or max_ftr_cand) the best candidates
      DO WHILE ( .NOT. found_all )

*%      If top 'max_ftr_cand' candidates found then
        IF ( best_count+1 .GT. max_ftr_cand ) THEN

           found_all = .TRUE.

*       Else
        ELSE

*%         Initialise best comparator
           found_all = .FALSE.
           best = 1.0e23
           got_best = 0

*%         Loop through all possible candidates recorded
           DO i = 1 , cand_count

*%            If best so far then
              IF ( bpos_distances ( i ) .GT. 0.0 .AND.
     :             bpos_distances ( i ) .LT. best ) THEN

*%               Make a note
                 got_best = i
                 best = bpos_distances ( i )

*%            Endif
              ENDIF

*%         End loop
           END DO

*%         If a best candidate was found then
           IF ( got_best .GT. 0 ) THEN

*%            Add it to list of the best max_ftr_cand candidates
              best_count = best_count + 1
              best_distances ( best_count ) =
     :                             bpos_distances ( got_best )
              best_dispersion ( best_count ) =
     :                             bpos_dispersion ( got_best )
              best_ftr ( best_count ) =
     :                             bpos_ftr ( got_best )
              best_next_ftr ( best_count ) =
     :                             bpos_next_ftr ( got_best )
              best_next_ftr2 ( best_count ) =
     :                             bpos_next_ftr2 ( got_best )
              best_prev_ftr ( best_count ) =
     :                             bpos_prev_ftr ( got_best )
              best_prev_ftr2 ( best_count ) =
     :                             bpos_prev_ftr2 ( got_best )
              best_posindex ( best_count , 1 ) =
     :                             bpos_posindex ( got_best , 1 )
              best_posindex ( best_count , 2 ) =
     :                             bpos_posindex ( got_best , 2 )
              best_posindex ( best_count , 3 ) =
     :                             bpos_posindex ( got_best , 3 )
              best_posindex ( best_count , 4 ) =
     :                             bpos_posindex ( got_best , 4 )

*%            Disable original copy of candidate
              bpos_distances ( got_best ) = 0.0

*%         Else we've found all the candidates now
           ELSE

              found_all = .TRUE.

*%         Endif
           ENDIF

*%      Endif
        ENDIF

*%    End loop
      END DO

*-----
*%    Clear any remaining unused candidate storage
      IF ( best_count+1 .LT. max_ftr_cand ) THEN
         DO i = best_count+1 , max_ftr_cand
             best_ftr ( i ) = 0
             best_next_ftr ( i ) = 0
             best_next_ftr2 ( i ) = 0
             best_prev_ftr ( i ) = 0
             best_prev_ftr2 ( i ) = 0
             best_dispersion ( i ) = 0.0
             best_distances ( i ) = 0.0
             best_posindex ( i,1 ) = 0
             best_posindex ( i,2 ) = 0
             best_posindex ( i,3 ) = 0
             best_posindex ( i,4 ) = 0
         END DO
      ENDIF

      cand_count = best_count

      END
