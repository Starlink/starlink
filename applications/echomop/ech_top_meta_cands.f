      SUBROUTINE ECH_TOP_META_CANDS(
     :           BPOS_DISPERSION,
     :           BPOS_FTR,
     :           BPOS_DISTANCES,
     :           BPOS_POSINDEX,
     :           BPOS_PREV_FTR,
     :           BPOS_PREV_FTR2,
     :           BPOS_NEXT_FTR,
     :           BPOS_NEXT_FTR2,
     :           BEST_DISPERSION,
     :           BEST_FTR,
     :           BEST_DISTANCES,
     :           BEST_POSINDEX,
     :           BEST_PREV_FTR,
     :           BEST_PREV_FTR2,
     :           BEST_NEXT_FTR,
     :           BEST_NEXT_FTR2,
     :           CAND_COUNT,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_TOP_META_CANDS

*  Purpose:
*     Select best meta-features.

*  Description:
*     This routine takes input meta features distances from the 'perfect'
*     solution, and saves the top 'meta_ftr_cand' candidates for each
*     feature.

*  Invocation:
*     CALL ECH_TOP_META_CANDS(
*     :    BPOS_DISPERSION,
*     :    BPOS_FTR,
*     :    BPOS_DISTANCES,
*     :    BPOS_POSINDEX,
*     :    BPOS_PREV_FTR,
*     :    BPOS_PREV_FTR2,
*     :    BPOS_NEXT_FTR,
*     :    BPOS_NEXT_FTR2,
*     :    BEST_DISPERSION,
*     :    BEST_FTR,
*     :    BEST_DISTANCES,
*     :    BEST_POSINDEX,
*     :    BEST_PREV_FTR,
*     :    BEST_PREV_FTR2,
*     :    BEST_NEXT_FTR,
*     :    BEST_NEXT_FTR2,
*     :    CAND_COUNT,
*     :    STATUS
*     :   )

*  Arguments:
*     BPOS_DISPERSION = REAL (Returned)
*        Dispersion for candidates.
*     BPOS_DISTANCES = REAL (Returned)
*        Distances for candidates.
*     BPOS_POSINDEX = BYTE (Returned)
*        Indicies of 5 features composing meta-features.
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
*     BEST_DISPERSION = REAL (Returned)
*        Dispersion for candidates.
*     BEST_DISTANCES = REAL (Returned)
*        Distances for candidates.
*     BEST_POSINDEX = BYTE (Returned)
*        Indicies of 5 features composing meta-features.
*     BEST_PREV_FTR = INTEGER (Returned)
*        Prev neighbour prediction for candidates.
*     BEST_PREV_FTR2 = INTEGER (Returned)
*        2nd prev neighbour prediction for candidates.
*     BEST_NEXT_FTR = INTEGER (Returned)
*        Next neighbour prediction for candidates.
*     BEST_NEXT_FTR2 = INTEGER (Returned)
*        2nd neighbour prediction for candidates.
*     BEST_FTR = INTEGER (Returned)
*        Feature identifications.
*     CAND_COUNT = INTEGER (Given and Returned)
*        NUmber of possible candidates so far.
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

*  Method:
*     Check for special case of no candidates at all
*     Loop until found all (or max_ftr_cand) the best candidates
*       If top 'max_ftr_cand' candidates found then
*          Initialise best comparator
*          Loop through all possible candidates recorded
*             If best so far then
*                Make a note
*             Endif
*          End loop
*          If a best candidate was found then
*             Add it to list of the best max_ftr_cand candidates
*             Disable original copy of candidate
*          Else we've found all the candidates now
*          Endif
*       Endif
*     End loop
*     Clear any remaining unused candidate storage

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

*  Arguments Returned:
      INTEGER CAND_COUNT
      REAL BPOS_DISPERSION( MAX_POS_CAND  )
*          ! Dispersion for candidates.
      REAL BPOS_DISTANCES( MAX_POS_CAND )
*          ! Wegihts for candidates.
      BYTE BPOS_POSINDEX( MAX_POS_CAND, 4 )
*          ! Indicies of meta-feature components.
      INTEGER BPOS_PREV_FTR( MAX_POS_CAND )
*          ! Prev neighbour prediction for candidates.
      INTEGER BPOS_PREV_FTR2( MAX_POS_CAND )
*          ! 2nd prev neighbour for candidates.
      INTEGER BPOS_NEXT_FTR( MAX_POS_CAND  )
*          ! Next neigb prediction for candidates.
      INTEGER BPOS_NEXT_FTR2( MAX_POS_CAND )
*          ! 2nd neighb for candidates.
      INTEGER BPOS_FTR( MAX_POS_CAND  )
*          ! DB index for candidates.
      REAL BEST_DISPERSION( MAX_FTR_CAND  )
*          ! Dispersion for candidates.
      REAL BEST_DISTANCES( MAX_FTR_CAND )
*          ! Wegihts for candidates.
      BYTE BEST_POSINDEX( MAX_FTR_CAND, 4 )
*          ! Indicies of meta-feature components.
      INTEGER BEST_PREV_FTR( MAX_FTR_CAND )
*          ! Prev neighbour prediction for candidates.
      INTEGER BEST_PREV_FTR2( MAX_FTR_CAND )
*          ! 2nd prev neighbour for candidates.
      INTEGER BEST_NEXT_FTR( MAX_FTR_CAND  )
*          ! Next neigb prediction for candidates.
      INTEGER BEST_NEXT_FTR2( MAX_FTR_CAND )
*          ! 2nd neighb for candidates.
      INTEGER BEST_FTR( MAX_FTR_CAND  )
*          ! DB index for candidates.

*  Status:
      INTEGER STATUS

*  Local Variables:
      REAL BEST

      INTEGER I
      INTEGER BEST_COUNT
      INTEGER GOT_BEST

      LOGICAL FOUND_ALL
*.

*  Check for special case of no candidates at all.
      BEST_COUNT = 0
      FOUND_ALL = .FALSE.
      IF ( CAND_COUNT .EQ. 0 ) FOUND_ALL = .TRUE.

*  Loop until found all (or max_ftr_cand) the best candidates.
      DO WHILE ( .NOT. FOUND_ALL )

*     If top 'max_ftr_cand' candidates found.
         IF ( BEST_COUNT + 1 .GT. MAX_FTR_CAND ) THEN
            FOUND_ALL = .TRUE.

         ELSE

*        Initialise best comparator
            FOUND_ALL = .FALSE.
            BEST = 1.0E23
            GOT_BEST = 0

*        Loop through all possible candidates recorded.
            DO I = 1, CAND_COUNT

*           If best so far, record.
               IF ( BPOS_DISTANCES( I ) .GT. 0.0 .AND.
     :              BPOS_DISTANCES( I ) .LT. BEST ) THEN
                  GOT_BEST = I
                  BEST = BPOS_DISTANCES( I )
               END IF
            END DO

*        If a best candidate was found add it to list of
*        the best max_ftr_cand candidates.
            IF ( GOT_BEST .GT. 0 ) THEN
               BEST_COUNT = BEST_COUNT + 1
               BEST_DISTANCES( BEST_COUNT ) =
     :               BPOS_DISTANCES( GOT_BEST )
               BEST_DISPERSION( BEST_COUNT ) =
     :               BPOS_DISPERSION( GOT_BEST )
               BEST_FTR( BEST_COUNT ) =
     :               BPOS_FTR( GOT_BEST )
               BEST_NEXT_FTR( BEST_COUNT ) =
     :               BPOS_NEXT_FTR( GOT_BEST )
               BEST_NEXT_FTR2( BEST_COUNT ) =
     :               BPOS_NEXT_FTR2( GOT_BEST )
               BEST_PREV_FTR( BEST_COUNT ) =
     :               BPOS_PREV_FTR( GOT_BEST )
               BEST_PREV_FTR2( BEST_COUNT ) =
     :               BPOS_PREV_FTR2( GOT_BEST )
               BEST_POSINDEX( BEST_COUNT, 1 ) =
     :               BPOS_POSINDEX( GOT_BEST, 1 )
               BEST_POSINDEX( BEST_COUNT, 2 ) =
     :               BPOS_POSINDEX( GOT_BEST, 2 )
               BEST_POSINDEX( BEST_COUNT, 3 ) =
     :               BPOS_POSINDEX( GOT_BEST, 3 )
               BEST_POSINDEX( BEST_COUNT, 4 ) =
     :               BPOS_POSINDEX( GOT_BEST, 4 )

*           Disable original copy of candidate.
               BPOS_DISTANCES( GOT_BEST ) = 0.0

*        Else we've found all the candidates now.
            ELSE
               FOUND_ALL = .TRUE.
            END IF
         END IF
      END DO

*  Clear any remaining unused candidate storage.
      IF ( BEST_COUNT + 1 .LT. MAX_FTR_CAND ) THEN
         DO I = BEST_COUNT + 1, MAX_FTR_CAND
            BEST_FTR( I ) = 0
            BEST_NEXT_FTR( I ) = 0
            BEST_NEXT_FTR2( I ) = 0
            BEST_PREV_FTR( I ) = 0
            BEST_PREV_FTR2( I ) = 0
            BEST_DISPERSION( I ) = 0.0
            BEST_DISTANCES( I ) = 0.0
            BEST_POSINDEX( I, 1 ) = 0
            BEST_POSINDEX( I, 2 ) = 0
            BEST_POSINDEX( I, 3 ) = 0
            BEST_POSINDEX( I, 4 ) = 0
         END DO
      END IF

      CAND_COUNT = BEST_COUNT

      END
