      SUBROUTINE ECH_TEST_META_FTRS(
     :           MAX_FEATURES,
     :           FTR_LIST,
     :           BEST_DISPERSION,
     :           BEST_FTR,
     :           BEST_DISTANCES,
     :           BEST_PREV_FTR,
     :           BEST_PREV_FTR2,
     :           BEST_NEXT_FTR,
     :           BEST_NEXT_FTR2,
     :           NUM_ACTIVE_FTRS,
     :           META_FTRS,
     :           META_FTR_GUESS,
     :           META_COUNT,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_TEST_META_FTRS

*  Purpose:
*     Check candidates meta-features.

*  Description:
*     This routine searches the table of the top 'max_idents'
*     identification candidates for observed features.  It
*     locates the very best set of such identifications by
*     applying the following criteria:
*
*      o At least one of the two neighbours predicted by the
*        identification must actually appear as an identification
*        of a feature in a subsequent entry.
*
*      o The dispersions calculated by subsequent 'pairs' of
*        consistent feature indentifications must be in agreement
*        to at least a (user-tunable) threshold (meta_disp_thresh).
*
*      o A set of n+1 consistent features is a better candidate
*        solution than any set of only n features.
*
*      o For two sets both of n candidates, the better one is that
*        which has a lower origin distance in n-d space from the
*        'perfect solution'.

*  Invocation:
*     CALL ECH_TEST_META_FTRS(
*     :    MAX_FEATURES,
*     :    FTR_LIST,
*     :    BEST_DISPERSION,
*     :    BEST_FTR,
*     :    BEST_DISTANCES,
*     :    BEST_PREV_FTR,
*     :    BEST_PREV_FTR2,
*     :    BEST_NEXT_FTR,
*     :    BEST_NEXT_FTR2,
*     :    NUM_ACTIVE_FTRS,
*     :    META_FTRS,
*     :    META_FTR_GUESS,
*     :    META_COUNT,
*     :    STATUS
*     :   )

*  Arguments:
*     NUM_ACTIVE_FTRS = INTEGER (Given)
*        Number of test feature positions.
*     BEST_DISPERSION = REAL (Given)
*        Dispersion for candidates.
*     BEST_DISTANCES = REAL (Given)
*        Distances for candidates.
*     BEST_PREV_FTR = INTEGER (Given)
*        Prev neighbour prediction for candidates.
*     BEST_PREV_FTR2 = INTEGER (Given)
*        2nd prev neighbour prediction for candidates.
*     BEST_NEXT_FTR = INTEGER (Given)
*        Next neighbour prediction for candidates.
*     BEST_NEXT_FTR2 = INTEGER (Given)
*        2nd neighbour prediction for candidates.
*     BEST_FTR = INTEGER (Given)
*        Feature identifications.
*     META_COUNT = INTEGER (Returned)
*        Count of consistent features in solution.
*     META_FTRS = INTEGER (Returned)
*        Index numbers of features in the solution.
*     META_FTR_GUESS = INTEGER (Returned)
*        Index numbers of candidates in the solution.
*     MAX_FEATURES = INTEGER (Given)
*        Maximum number of observed features per order.
*     FTR_LIST = REAL (Given)
*        List of known arc line wavelengths.
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

*  Authors:
*     DMILLS: Dave Mills (UCL, Starlink)
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     01-SEP-1992 (DMILLS):
*       Initial release.
*     04-JUN-1997 (MJC):
*       Added prologue, tidy-up.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_REPORT.INC'
      INCLUDE 'ECH_FEATURE.INC'

*  Arguments Given:
      INTEGER NUM_ACTIVE_FTRS
      INTEGER META_COUNT
      INTEGER META_FTRS( MAX_ID_FTRS )
      INTEGER MAX_FEATURES
      REAL FTR_LIST( MAX_FEATURES )

*  Arguments Returned:
      REAL BEST_DISPERSION( MAX_FTR_CAND, MAX_ID_FTRS )
*          ! Dispersion for candidates.
      REAL BEST_DISTANCES( MAX_FTR_CAND, MAX_ID_FTRS )
*          ! Wegihts for candidates.
      INTEGER BEST_PREV_FTR( MAX_FTR_CAND, MAX_ID_FTRS )
*          ! Prev neighbour prediction for candidates.
      INTEGER BEST_PREV_FTR2( MAX_FTR_CAND, MAX_ID_FTRS )
*          ! 2nd prev neighbour for candidates.
      INTEGER BEST_NEXT_FTR( MAX_FTR_CAND, MAX_ID_FTRS )
*          ! Next neigb prediction for candidates.
      INTEGER BEST_NEXT_FTR2( MAX_FTR_CAND, MAX_ID_FTRS )
*          ! 2nd neighb for candidates.
      INTEGER BEST_FTR( MAX_FTR_CAND, MAX_ID_FTRS )
*          ! Dispersion for candidates.
      INTEGER META_FTR_GUESS( MAX_ID_FTRS )
*          ! Numbers of candidates.

*  Status:
      INTEGER STATUS

*  Local Variables:
      REAL META_DISP_THRESH ! Maximum percentage difference for consistency.
      REAL TRIAL_DISP
      REAL DISTANCE
      REAL CURRENT_DISPERSION
      REAL BEST_DISTANCE

      INTEGER TEMP_FTRS( MAX_ID_FTRS ) ! Feature nos. for current solution.
      INTEGER TEMP_FTR_GUESS( MAX_ID_FTRS ) ! Candidate nos.
      INTEGER I
      INTEGER II
      INTEGER III
      INTEGER IV
      INTEGER SEARCH_FOR
      INTEGER SEARCH_FOR2
      INTEGER CURRENT_FTR
      INTEGER CURRENT_GUESS
      INTEGER FEATURES_FOUND
      INTEGER BEST_SO_FAR
      INTEGER TRY_FTR
      INTEGER NCHAR1

      LOGICAL FOUND
      LOGICAL FOUND_MATCH

      CHARACTER*16 REF_STR1

*  Functions Called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) )  RETURN

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

      META_DISP_THRESH = 0.1

*  Initialise solution characteristics.
      FOUND_MATCH = .FALSE.
      BEST_SO_FAR = 2
      BEST_DISTANCE = 1.0E20

*  Loop through each active feature with at least 4 neighbours.
      DO I = 3, NUM_ACTIVE_FTRS - 2

*     Initialise candidate counter to 1 (first candidate is usually the best).
         II = 1

*     Loop until all candidates have been checked.
         DO WHILE ( II .LE. MAX_FTR_CAND .AND. BEST_FTR( II,I ) .GT. 0 )

*        Initialise the potential solution at 1 feature / first candidate.
            FOUND = .TRUE.
            CURRENT_FTR = I
            CURRENT_GUESS = II
            FEATURES_FOUND = 1
            DISTANCE = BEST_DISTANCES( II, I )
            TEMP_FTRS( FEATURES_FOUND ) = CURRENT_FTR
            TEMP_FTR_GUESS( FEATURES_FOUND ) = CURRENT_GUESS

*        Loop until we fail to find a consistent neighbour.
            DO WHILE ( FOUND )

*           Setup up which features we want to see as neighbours.
               FOUND = .FALSE.
               SEARCH_FOR = BEST_NEXT_FTR( CURRENT_GUESS, CURRENT_FTR )
               SEARCH_FOR2 = BEST_NEXT_FTR2( CURRENT_GUESS,CURRENT_FTR )

*           Save dispersion consistent with candidate identification.
               CURRENT_DISPERSION = BEST_DISPERSION( CURRENT_GUESS,
     :               CURRENT_FTR )

*           If we have a neighbour to look for.
               IF ( SEARCH_FOR .GT. 0 ) THEN

*              Loop through all subsequent features
                  DO TRY_FTR = 1, NUM_ACTIVE_FTRS - I

*                 If required identification not found yet.
                     IF ( .NOT. FOUND ) THEN

*                    Loop through all candidate identifications for a feature.
                        DO III = 1, MAX_FTR_CAND

*                       If feature/candidate matches required
*                       identification.
                           IF ( CURRENT_FTR + TRY_FTR .LE.
     :                          NUM_ACTIVE_FTRS ) THEN
                              IF ( BEST_FTR( III, CURRENT_FTR+TRY_FTR )
     :                            .EQ. SEARCH_FOR .OR.
     :                             BEST_FTR( III, CURRENT_FTR+TRY_FTR )
     :                            .EQ. SEARCH_FOR2 ) THEN
                                 TRIAL_DISP = BEST_DISPERSION(
     :                                 III, CURRENT_FTR + TRY_FTR )

*                            If predicted dispersion is consistent.
                                 IF ( ABS( 1.0 - TRIAL_DISP /
     :                                CURRENT_DISPERSION ) .LT.
     :                                META_DISP_THRESH ) THEN

*                                Make this feature/candidate the new base
*                                for the search.
                                    FOUND = .TRUE.
                                    FEATURES_FOUND = FEATURES_FOUND + 1
                                    CURRENT_FTR = CURRENT_FTR + TRY_FTR
                                    CURRENT_GUESS = III
                                    DISTANCE = DISTANCE +
     :                                    BEST_DISTANCES(
     :                                    CURRENT_GUESS, CURRENT_FTR )
                                    TEMP_FTRS( FEATURES_FOUND ) =
     :                                    CURRENT_FTR
                                    TEMP_FTR_GUESS( FEATURES_FOUND ) =
     :                                    CURRENT_GUESS

*                                Exit inner loops.
                                    GO TO 100
                                 END IF
                              END IF
                           END IF
                        END DO
                     END IF
                  END DO

*              Re-entry point for early loop exits.
 100              CONTINUE
               END IF
            END DO

*        If current solution has at least as many features as the
*        best-so-far then.
            IF ( FEATURES_FOUND .GE. BEST_SO_FAR ) THEN

*           Calculate average distance of contributing features.
               DISTANCE = DISTANCE / FLOAT( FEATURES_FOUND )

*           If current solution has MORE features then previous best, or
*           has a lower average distance then.
               IF ( FEATURES_FOUND .GT. BEST_SO_FAR .OR.
     :              DISTANCE .LT. BEST_DISTANCE ) THEN

*              Adopt current solution as best-so-far.
                  FOUND_MATCH = .TRUE.
                  BEST_SO_FAR = FEATURES_FOUND
                  BEST_DISTANCE = DISTANCE

*              Report on it.
                  CALL CHR_ITOC( BEST_SO_FAR, REF_STR1, NCHAR1 )
                  REPORT_STRING = ' Best solution so far has ' //
     :                  REF_STR1( :NCHAR1 ) // ' members.'
                  CALL ECH_REPORT( 0, REPORT_STRING )

                  CALL CHR_RTOC( REAL( NINT( 100000.0 * DISTANCE ) ) /
     :                 100000.0, REF_STR1, NCHAR1 )
                  REPORT_STRING = ' Deviation from ideal solution: ' //
     :                  REF_STR1( :NCHAR1 ) // '.'
                  CALL ECH_REPORT( 0, REPORT_STRING )

                  CALL CHR_RTOC( FTR_LIST( BEST_FTR( II, I ) ),
     :                 REF_STR1, NCHAR1 )
                  REPORT_STRING = ' Wavelengths around: ' //
     :                  REF_STR1( :NCHAR1 ) // '.'
                  CALL ECH_REPORT( 0, REPORT_STRING )

*              Copy solution into best-yet arrays.
                  DO IV = 1, BEST_SO_FAR
                     META_FTRS( IV ) = TEMP_FTRS( IV )
                     META_FTR_GUESS( IV ) = TEMP_FTR_GUESS( IV )
                  END DO
               END IF
            END IF

*        Increment base-feature counter.
            II = II + 1
         END DO
      END DO

*  If a viable solution has been found setup count of features in solution.
      IF ( FOUND_MATCH ) THEN
         META_COUNT = BEST_SO_FAR

      ELSE
         META_COUNT = 0
         STATUS = ECH__META_NOSOLUTION
      END IF

      END
