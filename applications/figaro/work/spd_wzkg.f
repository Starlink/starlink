      SUBROUTINE SPD_WZKG( INFO, MAX_ARC_LINES, FTR_LIST,
     :   best_dispersion, best_ftr, best_distances, best_prev_ftr,
     :   best_prev_ftr2, best_next_ftr, best_next_ftr2,
     :   num_active_ftrs, meta_ftrs, meta_ftr_guess, meta_count,
     :   status )
*+
*  Name:
*     SPD_WZKG

*  Purpose:
*     FDB: check candidates meta-features

*  Language:
*     Fortran

*  Invocation:
*     CALL SPD_WZKG( INFO, MAX_ARC_LINES, FTR_LIST,
*        best_dispersion, best_ftr, best_distances, best_prev_ftr,
*        best_prev_ftr2, best_next_ftr, best_next_ftr2,
*        num_active_ftrs, meta_ftrs, meta_ftr_guess, meta_count,
*        status )

*  Description:
*     This is a modification of Dave Mills' routine TEST_META_FTRS (cf.
*     Mills 1992). "FDB" stands for "feature data base", which is a data
*     base of known features in arc spectra.
*
*     This routine searches the table of the top 'max_idents'
*     identification candidates for observed features. It locates the
*     very best set of such identifications by applying the following
*     criteria :
*
*         At least one of the two neighbours predicted by the
*         identification must actually appear as an identification of a
*         feature in a subsequent entry.
*
*         The dispersions calculated by subsequent 'pairs' of consistent
*         feature indentifications must be in agreement to at least a
*         (user tunable) threshold (meta_disp_thresh).
*
*         A set of n+1 consistent features is a better candidate
*         solution than any set of only n features
*
*         For two sets both of n candidates , the better one is that
*         which has a lower origin distance in n-d space from the
*         'perfect solution'.

*  Arguments:
*     INFO = LOGICAL (Given)
*        If true, some messages are issued.
*     MAX_ARC_LINES = INTEGER (Given)
*        An FDB array dimension.
*     FTR_LIST( MAX_ARC_LINES ) = REAL (Given)
*        The FDB wavelengths (FTR_WAVE).
*     num_active_ftrs = INTEGER (Given)
*        Number of test feature positions.
*     best_dispersion() = REAL (Given)
*        Dispersion for candidates.
*     best_distances() = REAL (Given)
*        Distances for candidates.
*     best_prev_ftr() = INTEGER (Given)
*        Prev neighbour prediction for candidates.
*     best_prev_ftr2() = INTEGER (Given)
*        2nd prev neighbour prediction for candidates.
*     best_next_ftr() = INTEGER (Given)
*        Next neighbour prediction for candidates.
*     best_next_ftr2() = INTEGER (Given)
*        2nd neighbour prediction for candidates.
*     best_ftr() = INTEGER (Given)
*        Feature identifications.
*     meta_count = INTEGER (Returned)
*        Count of consistent features in solution.
*     meta_ftrs() = INTEGER (Returned)
*        Index numbers of features in the solution.
*     meta_ftr_guess() = INTEGER (Returned)
*        Index numbers of candidates in the solution.
*     status = INTEGER (Given and Returned)
*        Input/Output status conditions.
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
*     27 Jan 1990 (djm):
*        Original version (TEST_META_FTRS).
*     26 May 1993 (hme):
*        Disuse status constant parameters.
*        Add the permanent data base arrays to the argument list and
*        avoid the common block.
*     25 Jan 1995 (hme):
*        Renamed from SPADL.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

      IMPLICIT NONE

*  Global Constants:
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
      INTEGER MAX_ARC_LINES
      REAL FTR_LIST( MAX_ARC_LINES )

      INTEGER num_active_ftrs    !Number of test feature positions
      REAL best_dispersion ( max_ftr_cand , max_id_ftrs ) !Dispersion for candidates
      REAL best_distances ( max_ftr_cand , max_id_ftrs ) !Wegihts for candidates
      INTEGER best_prev_ftr ( max_ftr_cand , max_id_ftrs ) !Prev neighbour prediction for candidates
      INTEGER best_prev_ftr2 ( max_ftr_cand , max_id_ftrs ) !2nd prev neighbour for candidates
      INTEGER best_next_ftr ( max_ftr_cand , max_id_ftrs ) !Next neigb prediction for candidates
      INTEGER best_next_ftr2 ( max_ftr_cand , max_id_ftrs ) !2nd neighb for candidates
      INTEGER best_ftr ( max_ftr_cand , max_id_ftrs ) !Dispersion for candidates
      INTEGER meta_count         !Count of consistent features in solution
      INTEGER meta_ftrs ( max_id_ftrs ) !Numbers of features
      INTEGER meta_ftr_guess ( max_id_ftrs ) !Numbers of candidates
      REAL meta_disp_thresh      !Maximum percentage difference for consistency
      INTEGER status             !Input/Output status condition

*     Local variables
      INTEGER i,ii,iii,iv        !General loop counters
      INTEGER search_for         !1st expected neighbouring feature
      INTEGER search_for2        !2nd expected neighbouring feature
      LOGICAL found              !TRUE when an expected feature was identified
      INTEGER current_ftr        !number of current feature
      INTEGER current_guess      !number of current candidate identification
      INTEGER features_found     !count of consistent features in solution
      INTEGER best_so_far        !count of features in best solution
      REAL trial_disp            !dispersion of next (possible) feature
      REAL distance              !Distance of solution from ideal
      REAL current_dispersion    !dispersion of current feature
      REAL best_distance         !lowest n-d origin distance so far
      INTEGER temp_ftrs ( max_id_ftrs ) !feature numbers for current solution
      INTEGER temp_ftr_guess ( max_id_ftrs ) !candidate numbers for current solution
      INTEGER try_ftr            !next feature to check
      LOGICAL found_match        !TRUE if any solution was found
      CHARACTER * ( 80 ) REPORT_STRING
      INTEGER IGNORE             ! MSG status

      IGNORE = 0
      meta_disp_thresh = 0.1

*%    Initialise solution characteristics
      found_match = .FALSE.
      best_so_far = 2
      best_distance = 1.0e20

*%    Loop through each active feature with at least 4 neighbours
* 1>>>
      DO i = 3 , num_active_ftrs-2

*%      Initialise candidate counter to 1 (first candidate is usually the best)
        ii = 1

*%      Loop until all candidates have been checked
* 2 >>>>
        DO WHILE ( ii .LE. max_ftr_cand  .AND.
     :             best_ftr ( ii , i ) .GT. 0 )

*%          Initialise the potential solution at 1 feature / first candidate
            found = .TRUE.
            current_ftr = i
            current_guess = ii
            features_found = 1
            distance = best_distances ( ii , i )
            temp_ftrs ( features_found ) = current_ftr
            temp_ftr_guess ( features_found ) =
     :                                   current_guess

*%          Loop until we fail to find a consistent neighbour
* 3 >>>>>>>>
            DO WHILE ( found )

*%             Setup up which features we want to see as neighbours
               found = .FALSE.
               search_for = best_next_ftr
     :                        ( current_guess , current_ftr )
               search_for2 = best_next_ftr2
     :                        ( current_guess , current_ftr )

*%             Save dispersion consistent with candidate identification
               current_dispersion = best_dispersion
     :                          ( current_guess , current_ftr )

*%             If we have a neighbour to look for then
               IF ( search_for .GT. 0 ) THEN

*%               Loop through all subsequent features
* 4 >>>>>>>>>>>>>
                 DO try_ftr = 1 , num_active_ftrs-i

*%                  If required identification not found yet the
                    IF ( .NOT. found ) THEN

*%                     Loop through all candidate identifications for a feature
* 5 >>>>>>>>>>>>>>>>>>>
                       DO iii = 1 , max_ftr_cand

*%                        If feature/candidate matches required identification then
                         IF ( current_ftr + try_ftr .LE.
     :                                         num_active_ftrs ) THEN
                          IF ((best_ftr ( iii , current_ftr+try_ftr )
     :                            .EQ.     search_for )  .OR.
     :                         best_ftr ( iii , current_ftr+try_ftr )
     :                            .EQ.     search_for2 ) THEN

*%                           If predicted dispersion is consistent then
                             trial_disp = best_dispersion
     :                               ( iii , current_ftr + try_ftr )

                             IF ( ABS ( 1.0 -
     :                                  trial_disp /
     :                                  current_dispersion )
     :                                   .LT. meta_disp_thresh ) THEN

*%                              Make this feature/candidate the new base for the search
                                found = .TRUE.
                                features_found = features_found + 1
                                current_ftr = current_ftr + try_ftr
                                current_guess = iii
                                distance = distance + best_distances (
     :                                    current_guess,current_ftr )
                                temp_ftrs ( features_found ) =
     :                                                   current_ftr
                                temp_ftr_guess ( features_found ) =
     :                                                 current_guess

*%                              EXIT INNER LOOPS (resume at ***)
                                GOTO 100

**%                          Endif
                             ENDIF

*%                        Endif
                          ENDIF

                         ENDIF

*%                     End loop
                       END DO
* 5 <<<<<<<<<<<<<<<<<<<

*%                  Endif
                    ENDIF

*%               End loop
                 END DO
* 4 <<<<<<<<<<<<<

*%               *** Re-entry point for premature loop exits
 100             CONTINUE

*%             Endif
               ENDIF

*%          End loop
            END DO
* 3 <<<<<<<<

*%          If current solution has at least as many features as the best-so-far then
            IF ( features_found .GE. best_so_far ) THEN

*%            Calculate average distance of contributing features
              distance = distance / FLOAT ( features_found )

*%            If current solution has MORE features then previous best , or
*%                    has a lower average distance then
              IF ( features_found .GT. best_so_far .OR.
     :            (  distance .LT. best_distance ) ) THEN

*%               Adopt current solution as best-so-far
                 found_match = .TRUE.
                 best_so_far = features_found
                 best_distance = distance

*%               Report on it
                 IF ( INFO ) THEN
                    WRITE ( report_string , 1000 ) best_so_far
                    CALL MSG_OUT( 'FDB_REPORT', report_string, IGNORE )
                    WRITE ( report_string , 1001 ) distance
                    CALL MSG_OUT( 'FDB_REPORT', report_string, IGNORE )
                    WRITE ( report_string , 1002 )
     :                             ftr_list ( best_ftr ( ii , i ) )
                    CALL MSG_OUT( 'FDB_REPORT', report_string, IGNORE )
                 END IF

*%               Copy solution into best-yet arrays
                 DO iv = 1 , best_so_far
                    meta_ftrs ( iv ) = temp_ftrs ( iv )
                    meta_ftr_guess ( iv ) = temp_ftr_guess ( iv )
                 END DO

*%            Endif
              ENDIF

*%          Endif
            ENDIF

*%          Increment base feature counter
            ii = ii + 1

*%        End loop
          END DO
* 2 <<<<<<

*%     End loop
       END DO
* 1 <<<

*%     If a viable solution has been found then
       IF ( found_match ) THEN

*%        Setp up count of features in solution
          meta_count = best_so_far

*%     Else set status
       ELSE

          meta_count = 0
          status = -3

*%     Endif
       ENDIF

*
 1000 FORMAT ( 1X , 'Best solution so far has ',I3,' members' )
 1001 FORMAT ( 1X , 26X , 'Deviation from ideal = ',E8.3 )
 1002 FORMAT ( 1X , 26X , 'Wavelengths around ',F8.2 )

      END
