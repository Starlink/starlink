      SUBROUTINE ECH_CALC_META_FTRS(
     :           MAX_FEATURES,
     :           POSITIONS,
     :           NUM_ACTV_FTRS,
     :           META_SCOPE,
     :           LEFT_OFFSET,
     :           RIGHT_OFFSET,
     :           RATIOS,
     :           N_SAMPLES,
     :           NEXT_INDEX,
     :           PREV_INDEX,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_CALC_META_FTRS

*  Purpose:
*     Calculates multi-arc line group parameters

*  Description:
*     This routine calculates the possible 'meta-features' present
*     in the observed set of features.  Meta here means that
*     multiple features are re-interpreted as a single meta feature.
*     The measure here uses combinations of 5 roughly adjacent
*     features as one meta-feature.  This routine calculates the
*     ratios which characterise such meta-features in terms of
*     ratios of distance from central component to its first and
*     second left and right neighbours.

*  Invocation:
*     CALL ECH_CALC_META_FTRS(
*     :    MAX_FEATURES,
*     :    POSITIONS,
*     :    NUM_ACTV_FTRS,
*     :    META_SCOPE,
*     :    LEFT_OFFSET,
*     :    RIGHT_OFFSET,
*     :    RATIOS,
*     :    N_SAMPLES,
*     :    NEXT_INDEX,
*     :    PREV_INDEX,
*     :    STATUS
*     :   )

*  Arguments:
*     MAX_FEATURES = INTEGER (Given)
*        Maximum number of observed features per order.
*     POSITIONS = REAL (Given)
*        Observed x-coords of active features.
*     NUM_ACTV_FTRS = INTEGER (Given)
*        Number of active features to consider.
*     META_SCOPE = INTEGER (Given)
*        Scope of meta features (ie out to n'th neighbours).
*     LEFT_OFFSET = INTEGER (Returned)
*        Which left neighbour was used.
*     RIGHT_OFFSET = INTEGER (Returned)
*        Which right neighbour was used.
*     NEXT_INDEX = INTEGER (Returned)
*        Pointer to entry with next greatest ratio.
*     PREV_INDEX = INTEGER (Returned)
*        Pointer to entry with previous greatest ratio.
*     RATIOS = REAL (Returned)
*        Ratios of features inter-neigbour distances
*        the list is not sorted into order as this
*        takes too long. Instead it is treated as
*        a linked list of entries, with next_index()
*        and prev_index() showing the order of entries
*     N_SAMPLES = INTEGER (Returned)
*        Number of entries in the above arrays.
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

*  Method:
*     Initialise start/end of the linked list
*     Loop through active features (with at least 4 neighbours)
*        Initialise linked list for this feature
*        Determine maximum number of neighbours to left/right we can use
*        Loop from 1st left hand neighbour to max left hand neighbour
*         Loop from next left hand neighbour to max left-1 hand neighbour
*          Loop from 1st right hand neighbour to max right hand neighbour
*           Loop from next right hand neighbour to max right-1 hand neighbour
*             Increment counter
*             If still room for more ratios in the arrays then
*              Calculate ratio 1 (l1/r1) and store its characteristics
*              Calculate ratio 2 (l1/r2) and store its characteristics
*              Calculate ratio 3 (l2/r1) and store its characteristics
*              Calculate ratio 4 (l2/r2) and store its characteristics
*              Insert index into linked list at appropriate place
*             Else set a flag
*             Endif
*           End loop
*          End loop
*         End loop
*        End loop
*     End loop
*     Report if we used up all available storage for ratios

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

*  Arguments Given:
      INTEGER MAX_FEATURES
      REAL POSITIONS( MAX_FEATURES ) ! Observed X-coords of active features.
      INTEGER NUM_ACTV_FTRS
      INTEGER META_SCOPE

*  Arguments Returned:
      BYTE LEFT_OFFSET( MAX_META_INDEX, 2, MAX_ID_FTRS )
*          ! Which left neighbour was used.
      BYTE RIGHT_OFFSET( MAX_META_INDEX, 2, MAX_ID_FTRS )
*          ! Which right neighbour was used.
      INTEGER NEXT_INDEX( MAX_META_INDEX, MAX_ID_FTRS )
*          ! Pointer to entry with next greatest ratio.
      INTEGER PREV_INDEX( MAX_META_INDEX, MAX_ID_FTRS )
*          ! Pointer to entry with previous greatest ratio.
      REAL RATIOS( MAX_META_INDEX, 4, MAX_ID_FTRS )

*  Status:
      INTEGER STATUS

*  Local Variables:
      REAL RATIO

      INTEGER N_SAMPLES
      INTEGER LEFT
      INTEGER RIGHT
      INTEGER ICF
      INTEGER LEFT_MAX_NEIGHB
      INTEGER RIGHT_MAX_NEIGHB
      INTEGER L1
      INTEGER L2
      INTEGER R1
      INTEGER R2
      INTEGER INDEX
      INTEGER USED_TOTAL
      INTEGER NCHAR1

      LOGICAL USED_MAX

      BYTE NL1
      BYTE NL2
      BYTE NR1
      BYTE NR2

      CHARACTER*8 REF_STR1

*  Functions Called:
      EXTERNAL ECH_FATAL_ERROR
      LOGICAL ECH_FATAL_ERROR
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

*  Initialise start/end of the linked list.
      STATUS = 0
      USED_MAX = .FALSE.
      USED_TOTAL = 0

*  Loop through active features (with at least 4 neighbours).
      DO ICF = 3, NUM_ACTV_FTRS - 2

*     Initialise linked list for this feature.
         N_SAMPLES = 2
         NEXT_INDEX( 1, ICF ) = 2
         NEXT_INDEX( 2, ICF ) = 2
         PREV_INDEX( 1, ICF ) = 1
         PREV_INDEX( 2, ICF ) = 1
         RATIOS( 1, 1, ICF ) = 0.0
         RATIOS( 2, 1, ICF ) = 1.0E20

*     Determine maximum number of neighbours to left/right we can use.
         LEFT_MAX_NEIGHB = ICF - 1
         RIGHT_MAX_NEIGHB = NUM_ACTV_FTRS - ICF

*     Loop from 1st left hand neighbour to max left hand neighbour
*     Loop from next left hand neighbour to max left-1 hand neighbour
*     Loop from 1st right hand neighbour to max right hand neighbour
*     Loop from next right hand neighbour to max right-1 hand neighbour
         DO L1 = 1, MIN( LEFT_MAX_NEIGHB, META_SCOPE ) - 1
            DO L2 = L1 + 1, MIN( LEFT_MAX_NEIGHB, META_SCOPE )
               DO R1 = 1, MIN( RIGHT_MAX_NEIGHB, META_SCOPE ) - 1
                  DO R2 = R1 + 1, MIN( RIGHT_MAX_NEIGHB, META_SCOPE )
                     N_SAMPLES = N_SAMPLES + 1

*                 If still room for more ratios in the arrays.
                     IF ( N_SAMPLES .LE. MAX_META_INDEX ) THEN
                        NL1 = L1
                        NL2 = L2
                        NR1 = R1
                        NR2 = R2

*                    Calculate ratio 1 (l1/r1) and store its characteristics.
                        LEFT = L1
                        RIGHT = R1
                        IF ( ICF - LEFT .GT. 0 .AND.
     :                       ICF + RIGHT .LE. NUM_ACTV_FTRS ) THEN
                           RATIO = ( POSITIONS( ICF ) -
     :                               POSITIONS( ICF - LEFT ) ) /
     :                             ( POSITIONS( ICF + RIGHT ) -
     :                               POSITIONS( ICF ) )
                           RATIOS( N_SAMPLES, 1, ICF ) = RATIO

*                    Calculate ratio 2 (l1/r2) and store its characteristics.
                        LEFT = L1
                        RIGHT = R2
                        IF ( ICF - LEFT .GT. 0 .AND.
     :                       ICF + RIGHT .LE. NUM_ACTV_FTRS ) THEN
                           RATIO = ( POSITIONS( ICF ) -
     :                               POSITIONS( ICF - LEFT ) ) /
     :                             ( POSITIONS( ICF + RIGHT ) -
     :                               POSITIONS( ICF ) )
                           RATIOS( N_SAMPLES, 2, ICF ) = RATIO

*                    Calculate ratio 3 (l2/r1) and store its characteristics.
                        LEFT = L2
                        RIGHT = R1
                        IF ( ICF - LEFT .GT. 0 .AND.
     :                       ICF + RIGHT .LE. NUM_ACTV_FTRS ) THEN
                           RATIO = ( POSITIONS( ICF ) -
     :                               POSITIONS( ICF - LEFT ) ) /
     :                             ( POSITIONS( ICF + RIGHT ) -
     :                               POSITIONS( ICF  ) )
                           RATIOS( N_SAMPLES, 3, ICF ) = RATIO

*                    Calculate ratio 4 (l2/r2) and store its characteristics.
                        LEFT = L2
                        RIGHT = R2
                        IF ( ICF - LEFT .GT. 0 .AND.
     :                       ICF + RIGHT .LE. NUM_ACTV_FTRS ) THEN
                           RATIO = ( POSITIONS( ICF ) -
     :                               POSITIONS( ICF - LEFT ) ) /
     :                             ( POSITIONS( ICF + RIGHT ) -
     :                               POSITIONS( ICF ) )
                           RATIOS( N_SAMPLES, 4, ICF ) = RATIO

                           LEFT_OFFSET( N_SAMPLES, 1, ICF ) = NL1
                           RIGHT_OFFSET( N_SAMPLES, 1, ICF ) = NR1
                           LEFT_OFFSET( N_SAMPLES, 2, ICF ) = NL2
                           RIGHT_OFFSET( N_SAMPLES, 2, ICF ) = NR2

*                       Insert index into linked list at appropriate place.
*                       The linked list is such that 'next_index(i)' always
*                       points to the entry with the next highest value for
*                       'ratios(??,1)', i.e. ?? = next_index(i).  Similarly
*                       with 'prev_index(i)'.
                           INDEX = 1
                           DO WHILE ( RATIOS( N_SAMPLES, 1, ICF ) .GT.
     :                                RATIOS( INDEX, 1, ICF ) )
                              INDEX = NEXT_INDEX( INDEX, ICF )
                           END DO
                           NEXT_INDEX( N_SAMPLES, ICF ) = INDEX
                           PREV_INDEX( N_SAMPLES, ICF ) =
     :                           PREV_INDEX( INDEX, ICF )
                           NEXT_INDEX( PREV_INDEX( INDEX, ICF ), ICF ) =
     :                           N_SAMPLES
                           PREV_INDEX( INDEX, ICF ) = N_SAMPLES
                        END IF
                        END IF
                        END IF
                        END IF

*                 Else set a flag
                     ELSE
                           USED_MAX = .TRUE.
                     END IF
                  END DO
               END DO
            END DO
         END DO
         USED_TOTAL = USED_TOTAL + N_SAMPLES - 2
      END DO

*  Report if we used up all available storage for ratios.
      IF ( USED_MAX ) THEN
         N_SAMPLES = MAX_META_INDEX
         CALL ECH_REPORT( 0, ' Reached maximum line-group components.' )
         STATUS = ECH__META_OUTSPACE
      END IF

      CALL CHR_ITOC( USED_TOTAL, REF_STR1, NCHAR1 )
      REPORT_STRING = ' Calculated ' // REF_STR1( :NCHAR1 ) //
     :      ' line-group component ratios.'
      CALL ECH_REPORT( 0, REPORT_STRING )

      END
