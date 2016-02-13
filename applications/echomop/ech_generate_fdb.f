      SUBROUTINE ECH_GENERATE_FDB(
     :           LIST_NAME,
     :           WCAL_FTR,
     :           DB_SCOPE,
     :           WCAL_INDEX,
     :           MAX_FEATURES,
     :           SRC_FTR_LIST,
     :           SRC_FTR_STRENGTH,
     :           FTR_LIST,
     :           FTR_STRENGTH,
     :           FTR_DB,
     :           FTR_DB_INDEX_L,
     :           FTR_DB_INDEX_R,
     :           FTR_DB_INDEX_WAVE,
     :           FTR_DB_QUICK_INDEX,
     :           FTR_DB_QUICK_VALUE,
     :           FTR_DB_TEMP_L,
     :           FTR_DB_TEMP_R,
     :           FTR_DB_TEMP_WAVE,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_GENERATE_FDB

*  Description:
*     This routine generates the feature database.  The entries in
*     the database describe the relationships between features and
*     their (up to db_scope) nearest neighbours, in terms of ratios
*     of differences.

*  Invocation:
*     CALL ECH_GENERATE_FDB(
*     :    LIST_NAME,
*     :    WCAL_FTR,
*     :    DB_SCOPE,
*     :    WCAL_INDEX,
*     :    MAX_FEATURES,
*     :    SRC_FTR_LIST,
*     :    SRC_FTR_STRENGTH,
*     :    FTR_LIST,
*     :    FTR_STRENGTH,
*     :    FTR_DB,
*     :    FTR_DB_INDEX_L,
*     :    FTR_DB_INDEX_R,
*     :    FTR_DB_INDEX_WAVE,
*     :    FTR_DB_QUICK_INDEX,
*     :    FTR_DB_QUICK_VALUE,
*     :    FTR_DB_TEMP_L,
*     :    FTR_DB_TEMP_R,
*     :    FTR_DB_TEMP_WAVE,
*     :    STATUS
*     :   )

*  Arguments:
*     LIST_NAME = CHAR (Given)
*        Name of reference list list ASCII version.
*     WCAL_FTR = INTEGER (Given and Returned)
*        Number of arc lines known.
*     DB_SCOPE = INTEGER (Temporary Workspace)
*        Maximum number of neighbours available in database.
*     WCAL_INDEX = INTEGER (Given and Returned)
*        Index of arc line features.
*     MAX_FEATURES = INTEGER (Given)
*        Maximum number of observed features per order.
*     SRC_FTR_LIST = REAL (Given and Returned)
*        Source arc line list.
*     SRC_FTR_STRENGTH = REAL (Given and Returned)
*        Source arc line strengths.
*     FTR_LIST = REAL (Given)
*        List of known arc line wavelengths.
*     FTR_STRENGTH = REAL (Given)
*        Array of arc line expected strengths.
*     FTR_DB = REAL (Given)
*        Feature ratio database main array.
*     FTR_DB_INDEX_L = BYTE (Given)
*        Feature database left neighbour index.
*     FTR_DB_INDEX_R = BYTE (Given)
*        Feature database right neighbour index.
*     FTR_DB_INDEX_WAVE = SHORT (Given)
*        Feature database wavelength list index.
*     FTR_DB_QUICK_INDEX = INTEGER (Given)
*        Feature database index to main array.
*     FTR_DB_QUICK_VALUE = REAL (Given)
*        Feature database index index starting values in main array.
*     FTR_DB_TEMP_L = BYTE (Given and Returned)
*        Temporary left neighbour array.
*     FTR_DB_TEMP_R = BYTE (Given and Returned)
*        Temporary right neighbour array.
*     FTR_DB_TEMP_WAVE = REAL (Given and Returned)
*        Temporary wavelengths.
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

*  Method:
*     Initialise counters, min max detectors etc.
*     Loop through the feature list (expects increasing values)
*       Loop through lower neighbours (up to db_scope of them)
*          Loop through lower neighbours (up to db_scope of them)
*             Calculate difference between 'current' and a pair of neighbours
*             Calculate the ratio of the two differences
*             Update min/max detectors if this ratio is min/max so far
*             Setup the three array entries that describe this 'feature'
*                  ie. Current + 2 neighbours
*          End loop
*       End loop
*       Increment 'current' feature pointer
*     End loop
*     Initialise indexing parameters
*     Set first lookup index using smallest ratio found
*     Calculate value which must be reached before we start a new lookup
*              index
*      Loop until lookup index slot starts above the maximum ratio found
*         Set lookup index entry
*         Calculate threshold value for next lookup index slot
*      End loop
*      Setup last lookup index entry
*     Loop through the lookup index
*         Report progress so far
*         Store lookup index reached so far
*         Loop through all ratios in database
*            If entry not sorted yet then
*               If current ratio exceeds limit for current lookup index slot then
*                  Ignore entry for the time being
*               Else
*                  Copy details of feature into roughly sorted versions
*                  Update rough index size, flag feature as processed
*               Endif
*            Endif
*         End loop
*      End loop
*     Loop through lookup index slots
*        Clear 'this section sorted' flag
*        Report progress so far
*        Loop until section fully sorted
*           Set 'this-section-sorted' flag to TRUE
*           Loop through all ratios referenced by lookup index section
*              If ratio pair out of order then
*                 Clear 'this-section-sorted' flag and swap ratio entries
*              Endif
*           End loop
*        End loop
*     End loop
*     Copy the results into the mapped arrays

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
      INCLUDE 'ECH_FTR_DB.INC'

*  Arguments Given:
      INTEGER WCAL_FTR
      INTEGER WCAL_INDEX
      CHARACTER*( * ) LIST_NAME

*  Arguments Returned:

*  Status:
      INTEGER STATUS

*  Local Variables:
        REAL DISTANCE_L
        REAL DISTANCE_R
        REAL INDICES
        REAL RMIN
        REAL RMAX
        REAL CURRENT
        REAL THRESHOLD

        INTEGER ROUGH_INDEX_COUNT( 5000 )
        INTEGER QUICK_INDEX_COUNT
        INTEGER TEMP_INDEX_COUNT
        INTEGER II
        INTEGER I
        INTEGER LEFT_SIDE_FTR
        INTEGER RIGHT_SIDE_FTR
        INTEGER INDEX_L_TEMP
        INTEGER INDEX_R_TEMP
        INTEGER INDEX_WAVE_TEMP
        INTEGER INDEX_COUNT
        INTEGER NCHAR1
        INTEGER NCHAR2
        INTEGER NCHAR3

        LOGICAL SORTED

        CHARACTER*16 REF_STR1
        CHARACTER*16 REF_STR2
        CHARACTER*16 REF_STR3

*  Functions Called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

      WCAL_FTR = 0
      DO WHILE ( SRC_FTR_LIST( WCAL_FTR + 1 ) .GT. 0 )
         WCAL_FTR = WCAL_FTR + 1
         FTR_LIST( WCAL_FTR ) = SRC_FTR_LIST( WCAL_FTR )
         FTR_STRENGTH( WCAL_FTR ) = SRC_FTR_STRENGTH( WCAL_FTR )
      END DO

      IF ( WCAL_FTR .GE. 2 * DB_SCOPE + 2 ) THEN

*     Initialise counters, min max detectors etc.
         CALL ECH_REPORT( 0, ' Composing initial database/index.' )
         INDEX_COUNT = 0
         RMAX = -1.0E20
         RMIN = 1.0E20

*     Loop through the feature list (expects increasing values)
*     In the following the 'current' shall be the entry in the list
*     array pointed to by the index checked by this loop, namely
*
*        FTR_LIST( I )
*
*         I = DB_SCOPE + 1
*         DO WHILE ( I + DB_SCOPE .LE. WCAL_FTR )
         DO I = 2, WCAL_FTR - 1

*        Loop through lower neighbours (up to db_scope of them).
            DO LEFT_SIDE_FTR = 1, MIN( DB_SCOPE, I - 1 )

*           Loop through lower neighbours (up to db_scope of them).
               DO RIGHT_SIDE_FTR = 1, MIN( DB_SCOPE, WCAL_FTR - I )

*              Calculate difference between 'current' and a pair of neighbours.
                  DISTANCE_L = FTR_LIST( I ) -
     :                         FTR_LIST( I - LEFT_SIDE_FTR )
                  DISTANCE_R = FTR_LIST( I + RIGHT_SIDE_FTR ) -
     :                         FTR_LIST( I )

*              Calculate the ratio of the two differences.
                  FTR_DB( LEFT_SIDE_FTR, RIGHT_SIDE_FTR, I ) =
     :                                           DISTANCE_L / DISTANCE_R

*              Update min/max detectors if this ratio is min/max so far.
                  IF ( FTR_DB( LEFT_SIDE_FTR, RIGHT_SIDE_FTR, I )
     :                 .GT. RMAX )
     :               RMAX = FTR_DB( LEFT_SIDE_FTR, RIGHT_SIDE_FTR, I )

                  IF ( FTR_DB( LEFT_SIDE_FTR, RIGHT_SIDE_FTR, I )
     :                  .LT. RMIN )
     :               RMIN = FTR_DB( LEFT_SIDE_FTR, RIGHT_SIDE_FTR, I )

*              Setup the three array entries that describe this 'feature'
*              ie. Current + 2 neighbours.
                  INDEX_COUNT = INDEX_COUNT + 1
                  FTR_DB_INDEX_L( INDEX_COUNT ) = LEFT_SIDE_FTR
                  FTR_DB_INDEX_R( INDEX_COUNT ) = RIGHT_SIDE_FTR
                  FTR_DB_INDEX_WAVE( INDEX_COUNT ) = I
               END DO
            END DO

*        Increment 'current' feature pointer.
*            I = I + 1
         END DO

*     Initialise indexing parameters.
         CALL ECH_REPORT( 0, ' Composing database lookup index.' )

*     The 'indices' is the logarithmic range of the calculated ratios.
         INDICES = LOG10( RMAX ) - LOG10( RMIN )

*     The 'threshold' is calculated by first scaling the size of the
*     database, and then evaluating the corresponding antilog.
         THRESHOLD = 10.0 ** ( INDICES / SQRT(
     :               FLOAT( DB_SCOPE * DB_SCOPE * WCAL_FTR ) ) )

*     Set first lookup index using smallest ratio found.
         FTR_DB_QUICK_INDEX( 1 ) = 1
         FTR_DB_QUICK_VALUE( 1 ) = RMIN
         QUICK_INDEX_COUNT = 1

*     Calculate value to be reached before we start a new lookup index.
         CURRENT = RMIN * THRESHOLD

*     Loop until lookup index slot starts above the maximum ratio found.
         DO WHILE ( CURRENT .LT. RMAX )

*        Set lookup index entry.
            FTR_DB_QUICK_VALUE( QUICK_INDEX_COUNT ) = CURRENT
            QUICK_INDEX_COUNT = QUICK_INDEX_COUNT + 1

*        Calculate threshold value for next lookup index slot.
            CURRENT = CURRENT * THRESHOLD
         END DO

*     Setup last lookup index entry.
         FTR_DB_QUICK_VALUE( QUICK_INDEX_COUNT ) = CURRENT

*     Build a roughly indexed version of the database.
         CALL ECH_REPORT( 0, ' Rough indexing phase started.' )
         TEMP_INDEX_COUNT = 1

*     Loop through the lookup index.
         CALL CHR_ITOC( QUICK_INDEX_COUNT, REF_STR2, NCHAR2 )
         DO II = 1, QUICK_INDEX_COUNT

*        Report progress so far - every twentieth loop.
            IF ( II .EQ. 1 .OR.
     :           MOD( II, 32 ) .EQ. 0 .OR.
     :           II .EQ. QUICK_INDEX_COUNT ) THEN
               CALL CHR_ITOC( II, REF_STR1, NCHAR1 )
               REPORT_STRING = ' Building lookup index ' //
     :               REF_STR1( :NCHAR1 ) // ' (of ' //
     :               REF_STR2( :NCHAR2 ) // ').'
               CALL ECH_REPORT( 0, REPORT_STRING )
            END IF

*        Store lookup index reached so far.
            FTR_DB_QUICK_INDEX( II ) = TEMP_INDEX_COUNT
            ROUGH_INDEX_COUNT( II ) = 0

*        Loop through all ratios in database.
            DO I = 1, INDEX_COUNT

*           If entry not sorted yet then.
               IF ( FTR_DB_INDEX_L( I ) .GT. 0 ) THEN

*              If current ratio exceeds limit for current lookup index
*              slot then ignore entry for the time being.
                   IF ( FTR_DB( FTR_DB_INDEX_L ( I ),
     :                 FTR_DB_INDEX_R( I ), FTR_DB_INDEX_WAVE( I ) )
     :                 .GT. FTR_DB_QUICK_VALUE( II ) ) THEN
                     CONTINUE

*              Copy details of feature into roughly sorted versions.
                  ELSE
                     FTR_DB_TEMP_L( TEMP_INDEX_COUNT ) =
     :                                            FTR_DB_INDEX_L( I )
                     FTR_DB_TEMP_R( TEMP_INDEX_COUNT ) =
     :                                            FTR_DB_INDEX_R( I )
                     FTR_DB_TEMP_WAVE( TEMP_INDEX_COUNT ) =
     :                                         FTR_DB_INDEX_WAVE( I )

*                 Update rough index size, flag feature as processed.
                     ROUGH_INDEX_COUNT( II ) =
     :                                    ROUGH_INDEX_COUNT( II ) + 1
                     FTR_DB_INDEX_L( I ) = -1
                     TEMP_INDEX_COUNT = TEMP_INDEX_COUNT + 1
                  END IF
               END IF
            END DO
         END DO

*     Build a finely sorted/indexed version of the database
*     This is done by taking each roughly sorted section in turn
*     and using a SHELL SORT to sort it completely.
         CALL ECH_REPORT( 0, ' Fine sorting phase started.' )
         CALL CHR_ITOC( QUICK_INDEX_COUNT, REF_STR1, NCHAR1 )
         REPORT_STRING = ' ' // REF_STR1( :NCHAR1 ) //
     :         ' sections to be sorted.'
         CALL ECH_REPORT( 0, REPORT_STRING )

*     Loop through lookup index slots.
         DO II = 1, QUICK_INDEX_COUNT

*        Clear 'this section sorted' flag.
            SORTED = .FALSE.

*        Report progress so far.
            IF ( II .EQ. 1 .OR. MOD( II, 32 ) .EQ. 0.0 ) THEN
               CALL CHR_ITOC( II, REF_STR2, NCHAR2 )
               CALL CHR_ITOC( ROUGH_INDEX_COUNT( II ), REF_STR3,
     :              NCHAR3 )
               REPORT_STRING = ' Sorting lookup index ' //
     :               REF_STR2( :NCHAR2 ) // ' (of ' //
     :               REF_STR1( :NCHAR1 ) // ') with ' //
     :               REF_STR3( :NCHAR3 ) // ' members.'
            CALL ECH_REPORT( 0, REPORT_STRING )
            END IF

*        Loop until section fully sorted.
            DO WHILE ( .NOT. SORTED )

*           Set 'this-section-sorted' flag to TRUE.
               SORTED = .TRUE.

*           Loop through all ratios referenced by lookup index section.
               DO I = FTR_DB_QUICK_INDEX( II ) + 1,
     :                FTR_DB_QUICK_INDEX( II + 1 ) - 1

*              If ratio pair out of order then.
                  IF ( FTR_DB_TEMP_WAVE( I ) .LT.
     :                 FTR_DB_TEMP_WAVE( I - 1 ) ) THEN

*                 Clear 'this-section-sorted' flag and swap ratio entries.
                     SORTED = .FALSE.
                     INDEX_L_TEMP = FTR_DB_TEMP_L( I )
                     INDEX_R_TEMP = FTR_DB_TEMP_R( I )
                     INDEX_WAVE_TEMP = FTR_DB_TEMP_WAVE( I )
                     FTR_DB_TEMP_L( I ) = FTR_DB_TEMP_L( I - 1 )
                     FTR_DB_TEMP_R( I ) = FTR_DB_TEMP_R( I - 1 )
                     FTR_DB_TEMP_WAVE( I ) = FTR_DB_TEMP_WAVE( I - 1 )
                     FTR_DB_TEMP_L( I - 1 ) = INDEX_L_TEMP
                     FTR_DB_TEMP_R( I - 1 ) = INDEX_R_TEMP
                     FTR_DB_TEMP_WAVE( I - 1 ) = INDEX_WAVE_TEMP
                  END IF
               END DO
            END DO
         END DO

*     Copy the results into the mapped arrays.
         DO I = 1, DB_SCOPE * DB_SCOPE * WCAL_FTR
            FTR_DB_INDEX_L( I ) = FTR_DB_TEMP_L( I )
            FTR_DB_INDEX_R( I ) = FTR_DB_TEMP_R( I )
            FTR_DB_INDEX_WAVE( I ) = FTR_DB_TEMP_WAVE( I )
         END DO
         WCAL_INDEX = QUICK_INDEX_COUNT

      ELSE
         CALL ECH_REPORT( 0, ' Cannot create feature database file.' )
         CALL ECH_SET_CONTEXT( 'PROBLEM', ' Feature list too small' )
         STATUS = ECH__BAD_DBCREATE
      ENDIF

      END
