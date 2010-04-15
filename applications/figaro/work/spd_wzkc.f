      SUBROUTINE SPD_WZKC( INFO,
     :   max_features, positions, num_actv_ftrs, meta_scope,
     :   left_offset, right_offset, ratios, n_samples,
     :   next_index, prev_index, status )
*+
*  Name:
*     SPD_WZKC

*  Purpose:
*     FDB: calculates multi-arc line group parameters.

*  Language:
*     Fortran

*  Invocation:
*     CALL SPD_WZKC( INFO,
*        max_features, positions, num_actv_ftrs, meta_scope,
*        left_offset, right_offset, ratios, n_samples,
*        next_index, prev_index, status )

*  Description:
*     This is a modification of Dave Mills' routine CALC_META_FTRS (cf.
*     Mills 1992). "FDB" stands for "feature data base", which is a data
*     base of known features in arc spectra.
*
*     This routine calculates the possible 'meta-features' present in
*     the observed set of features. Meta here means that multiple
*     features are re-interpreted as a single meta feature. The measure
*     here uses combinations of 5 roughly adjacent features as one
*     meta-feature. This routine calculates the ratios which
*     characterise such meta-features in terms of ratios of distance
*     from central component to its first and second  left and right
*     neighbours.

*  Arguments:
*     INFO = LOGICAL (Given)
*        If true, some messages are issued.
*     max_features = INTEGER (Given)
*        Maximum number of observed features per order
*     positions() = REAL (Given)
*        Observed x-coords of active features
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
*     prev_index() = INTEGER (Returned)
*        Pointer to entry with previous greatest ratio
*     ratios() = REAL (Returned)
*        Ratios of features inter-neigbour distances the list is not
*        sorted into order as this  takes too long. Instead it is
*        treated as a linked list of entries , with next_index() and
*        prev_index() showing the order of entries
*     n_samples = INTEGER (Returned)
*        Number of entries in the above arrays
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
*        Original version (CALC_META_FTRS).
*     25 May 1993 (hme):
*        Disuse the status value parameters.
*     25 Jan 1995 (hme):
*        Renamed from SPADG.
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

      INTEGER max_features       !Maximum number of observed features per order
      REAL positions ( max_features ) !Observed x-coords of active features
      INTEGER num_actv_ftrs      !NUmber of active features
      INTEGER meta_scope         !Scope of meta features (ie out to n'th neighbours)
      BYTE left_offset ( max_meta_index , 2,max_id_ftrs ) !Which left neighbour was used
      BYTE right_offset ( max_meta_index , 2,max_id_ftrs ) !Which right neighbour was used
      INTEGER next_index ( max_meta_index  , max_id_ftrs ) !Pointer to entry with next greatest ratio
      INTEGER prev_index ( max_meta_index  , max_id_ftrs ) !Pointer to entry with previous greatest ratio
      REAL ratios ( max_meta_index , 4  , max_id_ftrs ) !Ratios of features inter-neigbour distances
      INTEGER status             !Input/Output status condition
      INTEGER n_samples          !number of ratios calculated

*     Local variables
      INTEGER left               !Which left hand neighbour is being used
      INTEGER right              !Which right hand neighbour is being used
      INTEGER icf                !Index of current feature
      INTEGER left_max_neighb    !Leftmost usable neighbour
      INTEGER right_max_neighb   !Rightmost usable neighbour
      BYTE nl1                   !First left hand neighbour
      BYTE nl2                   !Second left hand neighbour
      BYTE nr1                   !First right hand neighbour
      BYTE nr2                   !Second right hand neighbour
      INTEGER l1                 !First left hand neighbour
      INTEGER l2                 !Second left hand neighbour
      INTEGER r1                 !First right hand neighbour
      INTEGER r2                 !Second right hand neighbour
      INTEGER index              !Index into linked list arrays
      REAL ratio                 !Calculated n-n ratio
      REAL log1                  !LOG(1)
      LOGICAL used_max           !TRUE when array filled with ratios
      INTEGER used_total         !Total ratios calculated
      CHARACTER * ( 80 ) REPORT_STRING
      INTEGER IGNORE             ! MSG status

      IGNORE = 0

*%      Initialise start/end of the linked list
        status = 0
        used_max = .FALSE.
        log1 = LOG ( 1.0 )
        used_total = 0

*%      Loop thru active features (with at least 4 neighbours)
        DO icf = 3 , num_actv_ftrs - 2

*%         Initialise linked list for this feature
           n_samples = 2
           next_index(1,icf) = 2
           next_index(2,icf) = 2
           prev_index(1,icf) = 1
           prev_index(2,icf) = 1
           ratios(1,1,icf) = 0.0
           ratios(2,1,icf) = 1.0e20

*%         Determine maximum number of neighbours to left/right we can use
           left_max_neighb = icf - 1
           right_max_neighb = num_actv_ftrs - icf

*%         Loop from 1st left hand neighbour to max left hand neighbour
*%          Loop from next left hand neighbour to max left-1 hand neighbour
*%           Loop from 1st right hand neighbour to max right hand neighbour
*%            Loop from next right hand neighbour to max right-1 hand neighbour
           DO l1 = 1 , MIN ( left_max_neighb , meta_scope ) - 1
            DO l2 = l1 + 1 , MIN ( left_max_neighb , meta_scope )
             DO r1 = 1 , MIN ( right_max_neighb , meta_scope ) - 1
              DO r2 = r1 + 1 , MIN ( right_max_neighb , meta_scope )

*%              Increment counter
*%              If still room for more ratios in the arrays then
                n_samples = n_samples + 1

                IF ( n_samples .LE. max_meta_index ) THEN

                 nl1 = l1
                 nl2 = l2
                 nr1 = r1
                 nr2 = r2

*%               Calculate ratio 1 (l1/r1) and store its characteristics
                  left = l1
                  right = r1
                  IF ( icf-left .GT. 0 .AND.
     :                icf+right .LE. num_actv_ftrs ) THEN
                  ratio =    ( positions ( icf ) -
     :                      positions ( icf - left )   )
     :                                                              /
     :                     ( positions ( icf + right ) -
     :                     positions ( icf  )         )
                  ratios ( n_samples , 1 , icf ) = ratio

*%               Calculate ratio 2 (l1/r2) and store its characteristics
                  left = l1
                  right = r2
                  IF ( icf-left .GT. 0 .AND.
     :                icf+right .LE. num_actv_ftrs ) THEN
                  ratio =    ( positions ( icf ) -
     :                      positions ( icf - left )   )
     :                                                              /
     :                     ( positions ( icf + right ) -
     :                     positions ( icf  )         )
                  ratios ( n_samples , 2 , icf ) = ratio

*%               Calculate ratio 3 (l2/r1) and store its characteristics
                  left = l2
                  right = r1
                  IF ( icf-left .GT. 0 .AND.
     :                icf+right .LE. num_actv_ftrs ) THEN
                  ratio =    ( positions ( icf ) -
     :                      positions ( icf - left )   )
     :                                                              /
     :                     ( positions ( icf + right ) -
     :                     positions ( icf  )         )
                  ratios ( n_samples , 3 , icf ) = ratio

*%               Calculate ratio 4 (l2/r2) and store its characteristics
                  left = l2
                  right = r2
                  IF ( icf-left .GT. 0 .AND.
     :                icf+right .LE. num_actv_ftrs ) THEN
                  ratio =    ( positions ( icf ) -
     :                      positions ( icf - left )   )
     :                                                              /
     :                     ( positions ( icf + right ) -
     :                     positions ( icf  )         )
                  ratios ( n_samples , 4 , icf ) = ratio

                  left_offset ( n_samples , 1 , icf ) = nl1
                  right_offset ( n_samples , 1 , icf ) = nr1
                  left_offset ( n_samples , 2 , icf ) = nl2
                  right_offset ( n_samples , 2  , icf ) = nr2

*%               Insert index into linked list at appropriate place
*                The linked list is such that 'next_index(i)' always points to
*                the entry with the next highest value for 'ratios(??,1)'
*                ie ?? = next_index(i). Similarly with 'prev_index(i)'.
                  index = 1
                  DO WHILE ( ratios ( n_samples , 1 , icf ) .GT.
     :                      ratios ( index , 1 , icf )    )
                    index = next_index ( index , icf )
                  END DO
                  next_index ( n_samples , icf ) = index
                  prev_index ( n_samples , icf ) =
     :                                   prev_index ( index , icf )
                  next_index ( prev_index
     :                         ( index , icf ) , icf  ) = n_samples
                  prev_index ( index , icf ) = n_samples


                 ENDIF
                 ENDIF
                 ENDIF
                 ENDIF

*%              Else set a flag
                ELSE

                   used_max = .TRUE.

*%              Endif
                ENDIF

*%            End loop
*%           End loop
*%          End loop
*%         End loop
              END DO
             END DO
            END DO
           END DO

           used_total = used_total + n_samples - 2

*%      End loop
        END DO

*%      Report if we used up all available storage for ratios
        IF ( used_max ) THEN
           n_samples = max_meta_index
           IF ( INFO ) CALL MSG_OUT( 'FDB_REPORT',
     :                'Reached maximum line-group components', IGNORE )
           status = -1
        ENDIF

        IF ( INFO ) THEN
           WRITE ( report_string , 1000 ) used_total
           CALL MSG_OUT( 'FDB_REPORT', report_string, IGNORE )
        END IF

*
 1000 FORMAT ( 1X , 'Calculated ',I5,' line-group component ratios' )

      END
