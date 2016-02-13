      SUBROUTINE SPD_WZLB( INFO,
     :   DB_SCOPE, MAX_ARC_LINES, FTR_DB_INDEX_SIZE,
     :   FTR_LIST, FTR_DB,
     :   FTR_DB_INDEX_L, FTR_DB_INDEX_R, FTR_DB_INDEX_WAVE,
     :   FTR_DB_QUICK_INDEX, FTR_DB_QUICK_VALUE,
     :   FTR_DB_TEMP_L, FTR_DB_TEMP_R, FTR_DB_TEMP_WAVE,
     :   wcal_ftr, src_ftr_list, status )
*+
*  Name:
*     SPD_WZLB

*  Purpose:
*     FDB: rebuilds feature database.

*  Language:
*     Fortran

*  Invocation:
*     CALL SPD_WZLB( INFO,
*        DB_SCOPE, MAX_ARC_LINES, FTR_DB_INDEX_SIZE,
*        FTR_LIST, FTR_DB,
*        FTR_DB_INDEX_L, FTR_DB_INDEX_R, FTR_DB_INDEX_WAVE,
*        FTR_DB_QUICK_INDEX, FTR_DB_QUICK_VALUE,
*        FTR_DB_TEMP_L, FTR_DB_TEMP_R, FTR_DB_TEMP_WAVE,
*        wcal_ftr, src_ftr_list, status )

*  Description:
*     This is a modification of Dave Mills' routine GENERATE_FDB (cf.
*     Mills 1992). "FDB" stands for "feature data base", which is a data
*     base of known features in arc spectra.
*
*     This routine generates the feature database. The entries in the
*     database describe the relationships between features and their (up
*     to db_scope) nearest neighbours, in terms of ratios of
*     differences.

*  Arguments:
*     INFO = LOGICAL (Given)
*        If true, some messages are issued.
*     DB_SCOPE = INTEGER (Given)
*        An FDB array dimension.
*     MAX_ARC_LINES = INTEGER (Given)
*        An FDB array dimension.
*     FTR_DB_INDEX_SIZE = INTEGER (Given)
*        An FDB array dimension.
*     FTR_LIST( MAX_ARC_LINES ) = REAL (Returned)
*        The FDB wavelengths (FTR_WAVE).
*     FTR_DB( DB_SCOPE, DB_SCOPE, MAX_ARC_LINES ) = REAL (Returned)
*        The FDB DB array (FTR_DB).
*     FTR_DB_INDEX_L( DB_SCOPE*DB_SCOPE*MAX_ARC_LINES )
*           = BYTE (Returned)
*        The FDB left index array (FTR_LEFT).
*     FTR_DB_INDEX_R( DB_SCOPE*DB_SCOPE*MAX_ARC_LINES )
*           = BYTE (Returned)
*        The FDB right index array (FTR_LEFT).
*     FTR_DB_INDEX_WAVE( DB_SCOPE*DB_SCOPE*MAX_ARC_LINES )
*           = INTEGER * 2 (Returned)
*        The FDB wavelength index array (WAVE_INDEX).
*     FTR_DB_QUICK_INDEX( FTR_DB_INDEX_SIZE ) = INTEGER (Returned)
*        The FDB quick index array (QUICK_INDEX).
*     FTR_DB_QUICK_VALUE( FTR_DB_INDEX_SIZE ) = REAL (Returned)
*        The FDB quick wavelength array (QENTRIES).
*     FTR_DB_TEMP_L( DB_SCOPE*DB_SCOPE*MAX_ARC_LINES )
*           = BYTE (Given and Returned)
*        Workspace.
*     FTR_DB_TEMP_R( DB_SCOPE*DB_SCOPE*MAX_ARC_LINES )
*           = BYTE (Given and Returned)
*        Workspace.
*     FTR_DB_TEMP_WAVE( DB_SCOPE*DB_SCOPE*MAX_ARC_LINES )
*           = INTEGER * 2 (Given and Returned)
*        Workspace.
*     WCAL_FTR = INTEGER (Given)
*        An array dimension.
*     SRC_FTR_LIST( WCAL_FTR ) = REAL (Given)
*        The array of feature wavelenghts. The values must be strictly
*        monotonically increasing.
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions
*        This is a status for use in the FDB routines, not a Starlink
*        inherited status. The status is actually unused (neiter checked
*        nor modified).

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
*     23 Jan 1990 (djm):
*        Original version (GENERATE_FDB).
*     26 May 1993 (hme):
*        Add the permanent data base arrays to the argument list and
*        avoid the common block.
*        Replace REPORT with MSG_OUT.
*        Suppress writing the output file.
*     25 Nov 1994 (hme):
*        Renamed from SPADD.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

      IMPLICIT NONE

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
      BYTE FTR_DB_TEMP_L( DB_SCOPE * DB_SCOPE * MAX_ARC_LINES )
      BYTE FTR_DB_TEMP_R( DB_SCOPE * DB_SCOPE * MAX_ARC_LINES )
      INTEGER * 2
     :   FTR_DB_TEMP_WAVE( DB_SCOPE * DB_SCOPE * MAX_ARC_LINES )

      INTEGER status             !Input/Output status condition
      INTEGER wcal_ftr
      REAL src_ftr_list ( wcal_ftr )

*     Local variables
      INTEGER i                  !General loop counter
      REAL distance_l
      REAL distance_r
      INTEGER left_side_ftr
      INTEGER right_side_ftr
      INTEGER index_l_temp
      INTEGER index_r_temp
      INTEGER index_wave_temp
      REAL indices
      INTEGER index_count
      INTEGER quick_index_count
      LOGICAL sorted
      REAL min
      REAL max
      REAL current
      REAL threshold
      INTEGER rough_index_count ( 5000 )
      INTEGER temp_index_start
      INTEGER temp_index_count
      INTEGER ii
      CHARACTER * ( 80 ) REPORT_STRING
      INTEGER IGNORE             ! MSG status

      IGNORE = 0

      DO i = 1 , wcal_ftr
         ftr_list ( i ) = src_ftr_list ( i )
      END DO

      IF ( wcal_ftr .GE. 2*db_scope+2 ) THEN

*%         Initialise counters , min max detectors etc.
           IF ( INFO ) CALL MSG_OUT( 'FDB_REPORT',
     :        ' Composing initial database/index', IGNORE )
           i = db_scope + 1
           index_count = 0
           max = -1.0e20
           min = 1.0e20

*%         Loop through the feature list (expects increasing values)
*          In the following the 'current' shall be the entry in the list
*          array pointed to by the index checked by this loop , namely
*
*                ftr_list ( i )
* 1 >>>>>>>
           DO WHILE ( i+db_scope .LT. wcal_ftr )

*%           Loop thru lower neighbours (up to db_scope of them)
* 2 >>>>>>>>>
             DO left_side_ftr = 1 , db_scope

*%              Loop thru lower neighbours (up to db_scope of them)
* 3 >>>>>>>>>>>>
                DO right_side_ftr = 1 , db_scope

*%                 Calculate difference between 'current' and a pair of neighbours
                   distance_l = ftr_list ( i ) -
     :                              ftr_list ( i-left_side_ftr )
                   distance_r = ftr_list ( i+right_side_ftr ) -
     :                              ftr_list ( i )

*%                 Calculate the ratio of the two differences
                   ftr_db ( left_side_ftr , right_side_ftr , i )  =
     :                                        distance_l / distance_r

*%                 Update min/max detectors if this ratio is min/max so far
                   IF ( ftr_db ( left_side_ftr , right_side_ftr , i )
     :                  .GT.  max  )
     :                       max =  ftr_db ( left_side_ftr ,
     :                                       right_side_ftr , i )

                   IF ( ftr_db ( left_side_ftr , right_side_ftr , i )
     :                        .LT. min )
     :                       min =  ftr_db ( left_side_ftr ,
     :                                       right_side_ftr , i )

*%                 Setup the three array entries that describe this 'feature'
*%                      ie. Current + 2 neighbours
                   index_count = index_count + 1
                   ftr_db_index_l ( index_count ) = left_side_ftr
                   ftr_db_index_r ( index_count ) = right_side_ftr
                   ftr_db_index_wave ( index_count ) = i

*%              End loop
                END DO
* 3 <<<<<<<<<<<<

*%           End loop
             END DO
* 2 <<<<<<<<<

*%           Increment 'current' feature pointer
             i = i + 1

*%         End loop
           END DO
* 1 <<<<<<<

*%         Initialise indexing parameters
           IF ( INFO ) CALL MSG_OUT( 'FDB_REPORT',
     :        ' Composing db lookup index', IGNORE )

*          The 'indices' is the logarithmic range of the calculated ratios
           indices = ( LOG10 ( max ) - LOG10 ( min ) )

*          The 'threshold' is calculated by first scaling the size of the
*          database , and then evaluating the corresponding antilog
           threshold = 10.0 ** ( indices / SQRT (
     :              FLOAT ( db_scope * db_scope *
     :                                          wcal_ftr ) ) )

*%         Set first lookup index using smallest ratio found
           ftr_db_quick_index ( 1 ) = 1
           ftr_db_quick_value ( 1 ) = min
           quick_index_count = 1

*%         Calculate value which must be reached before we start a new lookup
*%                  index
            current = min * threshold

*%          Loop until lookup index slot starts above the maximum ratio found
            DO WHILE ( current .LT. max )

*%             Set lookup index entry
               ftr_db_quick_value ( quick_index_count ) = current
               quick_index_count = quick_index_count + 1

*%             Calculate threshold value for next lookup index slot
               current = current * threshold

*%          End loop
            END DO

*%          Setup last lookup index entry
            ftr_db_quick_value ( quick_index_count ) = current

*          Build a roughly indexed version of the database
           IF ( INFO ) CALL MSG_OUT( 'FDB_REPORT',
     :        ' Rough indexing phase started', IGNORE )
           temp_index_count = 1

*%         Loop through the lookup index
            DO ii = 1 , quick_index_count

*%             Report progress so far
               IF ( INFO ) THEN
                  WRITE ( report_string , 1001 ) ii , quick_index_count
                  CALL MSG_OUT( 'FDB_REPORT', report_string, IGNORE )
               END IF

*%             Store lookup index reached so far
               ftr_db_quick_index ( ii ) = temp_index_count
               rough_index_count ( ii ) = 0

*%             Loop thru all ratios in database
               DO i = 1 , index_count

*%                If entry not sorted yet then
                  IF ( ftr_db_index_l ( i ) .GT. 0 ) THEN

*%                   If current ratio exceeds limit for current lookup index slot then
                     IF ( ftr_db ( ftr_db_index_l ( i ) ,
     :                    ftr_db_index_r ( i ) ,
     :                    ftr_db_index_wave ( i ) )
     :                        .GT. ftr_db_quick_value ( ii ) ) THEN

*%                      Ignore entry for the time being
                        CONTINUE

*%                   Else
*%                      Copy details of feature into roughly sorted versions
                     ELSE
                        ftr_db_temp_l ( temp_index_count ) =
     :                              ftr_db_index_l ( i )
                        ftr_db_temp_r ( temp_index_count ) =
     :                              ftr_db_index_r ( i )
                        ftr_db_temp_wave ( temp_index_count ) =
     :                              ftr_db_index_wave ( i )

*%                      Update rough index size , flag feature as processed
                        rough_index_count ( ii )  =
     :                                 rough_index_count ( ii )  + 1
                        ftr_db_index_l ( i ) = -1
                        temp_index_count = temp_index_count + 1

*%                   Endif
                     ENDIF

*%                Endif
                  ENDIF

*%             End loop
               END DO

*%          End loop
            END DO

*          Build a finely sorted/indexed version of the database
*          This is done by taking each roughly sorted section in turn
*          and using a SHELL SORT to sort it completely
           IF ( INFO ) THEN
              CALL MSG_OUT( 'FDB_REPORT',
     :         ' Fine sorting phase started', IGNORE )
              WRITE ( report_string , 1002 ) quick_index_count
              CALL MSG_OUT( 'FDB_REPORT', report_string, IGNORE )
           END IF
           temp_index_start = 0

*%         Loop through lookup index slots
* 1 >>>>>>>
           DO ii = 1 , quick_index_count

*%            Clear 'this section sorted' flag
              sorted = .FALSE.

*%            Report progress so far
              IF ( INFO ) THEN
                 WRITE ( report_string , 1003 ) ii , quick_index_count,
     :                                       rough_index_count ( ii )
                 CALL MSG_OUT( 'FDB_REPORT', report_string, IGNORE )
              END IF

*%            Loop until section fully sorted
* 2 >>>>>>>>>>
              DO WHILE ( .NOT. sorted )

*%               Set 'this-section-sorted' flag to TRUE
                 sorted = .TRUE.

*%               Loop thru all ratios referenced by lookup index section
                 DO i = ftr_db_quick_index ( ii ) + 1  ,
     :                  ftr_db_quick_index ( ii + 1 ) - 1

*%                  If ratio pair out of order then
                    IF ( ftr_db_temp_wave ( i ) .LT.
     :                   ftr_db_temp_wave ( i-1 ) ) THEN

*%                     Clear 'this-section-sorted' flag and swap ratio entries
                       sorted = .FALSE.
                       index_l_temp = ftr_db_temp_l ( i )
                       index_r_temp = ftr_db_temp_r ( i )
                       index_wave_temp = ftr_db_temp_wave ( i )
                       ftr_db_temp_l ( i ) = ftr_db_temp_l ( i-1 )
                       ftr_db_temp_r ( i ) = ftr_db_temp_r ( i-1 )
                       ftr_db_temp_wave ( i ) = ftr_db_temp_wave ( i-1 )
                       ftr_db_temp_l ( i-1 ) = index_l_temp
                       ftr_db_temp_r ( i-1 ) = index_r_temp
                       ftr_db_temp_wave ( i-1 ) = index_wave_temp

*%                  Endif
                    ENDIF

*%               End loop
                 END DO

*%            End loop
              END DO
* 2 <<<<<<<<<<

*%         End loop
* 1 <<<<<<<
           END DO

*%         Copy the results into the mapped arrays
           DO i = 1 , db_scope*db_scope * wcal_ftr
              ftr_db_index_l ( i ) = ftr_db_temp_l ( i )
              ftr_db_index_r ( i ) = ftr_db_temp_r ( i )
              ftr_db_index_wave ( i ) = ftr_db_temp_wave ( i )
           END DO

        ELSE

           IF ( INFO ) CALL MSG_OUT( 'FDB_REPORT',
     :        ' Cannot create feature database file', IGNORE )

        ENDIF

*
 1001 FORMAT ( 1X , 'Building lookup index ' , I4 , ' (of ' , I4 , ')')
 1002 FORMAT ( 1X , 'Number of sections to be sorted is ' , I4 )
 1003 FORMAT ( 1X , 'Sorting lookup index ' , I4 , ' (of ' ,
     :              I4 , ')  with ' , I4 , ' members'  )

      END
