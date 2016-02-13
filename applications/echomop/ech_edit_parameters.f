      SUBROUTINE ECH_EDIT_PARAMETERS( NEXT_MODULE, NEXT_MODULE_TEXT,
     :                                STATUS )
*+
*  Name:
*     ECHOMOP - ECH_EDIT_PARAMETERS

*  Purpose:
*     View/edit parameter values.

*  Description:
*     This routine supports the interactive setup/inquiry of ordinary
*     parameters for all the ECHOMOP tasks.

*  Invocation:
*     CALL ECH_EDIT_PARAMETERS( NEXT_MODULE, NEXT_MODULE_TEXT,
*    :                          STATUS )

*  Arguments:
*     NEXT_MODULE = CHAR (Given)
*
*     NEXT_MODULE_TEXT = CHAR (Given)
*        Descriptive text for next module.
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

*  Method:
*     Check for set of modules specified via a 'Submenu_' menu entry
*     Produce a menu of available tuning parameters
*     Loop through all known objects
*       If object value is allowed to be edited then
*        If sub-menu options are available for selected option then
*          Loop thru sub-menu options
*               If module uses object and its a parameter then
*                   Remember its table index
*               Endif
*          End loop
*        Else if module uses object and its a parameter then
*              Remember its table index
*        Endif
*       Endif
*     End loop
*     Loop until exit option (0) selected
*        Print menu of available parameters
*        Get number of parameter from user
*        If its a valid number then
*          Print parameters properties (min,max etc)
*          If parameter is one we look for in the data frame first then
*             Check for a mapped parameter value and retrieve it
*          Endif
*          If its a tuning parameter then
*             Print characteristics
*            Print parameter help text
*            Get new value from user
*            Update internal tables
*          Else
*            Get new value from user
*          Endif
*          If parameter is one we look for in the data frame first then
*             Check for a mapped parameter value and replace it
*          Endif
*          Update internal tables
*        Endif
*     End loop

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
      INCLUDE 'ECH_MODULES.INC'
      INCLUDE 'ECH_REPORT.INC'
      INCLUDE 'ECH_ECHOMOP.INC'
      INCLUDE 'ECH_USE_RDCTN.INC'
      INCLUDE 'PAR_ERR'

*  Arguments Given:
      CHARACTER*( * ) NEXT_MODULE
      CHARACTER*( * ) NEXT_MODULE_TEXT

*  Status:
      INTEGER STATUS

*  Local Variables:
      REAL VALUE
      REAL DUMVALUE
      REAL MIN
      REAL MAX

      INTEGER PARAMS( MAX_REQUIRED_OBJECTS )
      INTEGER I
      INTEGER INT_VALUE
      INTEGER MAPPED_ADDRESS
      INTEGER DEFAULT_INDEX
      INTEGER OPTION
      INTEGER DUMMY_AD
      INTEGER TINDEX
      INTEGER ISUBMENU
      INTEGER MINDEX
      INTEGER II
      INTEGER OBJECT_NUMBER
      INTEGER FFLMED_REF
      INTEGER NOARC_REF
      INTEGER NOFLAT_REF
      INTEGER NCHAR1

      LOGICAL BOOLEAN_VALUE
      LOGICAL FOUND
      LOGICAL ACTIVE

      CHARACTER*132 STRING
      CHARACTER*132 DEFAULT_STRING
      CHARACTER*80 HELP_TOPIC
      CHARACTER*32 TYPE
      CHARACTER*48 VAL_STR( 32 )
      CHARACTER*32 REF_NAM( 32 )
      CHARACTER*20 REF_NAME
      CHARACTER*16 REP_STR1
      CHARACTER*1 REF_ACT( 32 )

*  Functions Called:
      INTEGER ECH_MODULE_NAME_INDEX
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
      LOGICAL MODULE_NEEDS
      INTEGER CHR_LEN
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

*  Check for set of modules specified via a 'Submenu_' menu entry.
      ISUBMENU = 0
      IF ( NEXT_MODULE( :8 ) .EQ. 'Submenu_' ) THEN
         DO I = 1, NUM_SUBOPTIONS
            IF ( NEXT_MODULE .EQ. SUBMENU_OPTIONS( I ) )
     :         ISUBMENU = I + 1
         END DO
      END IF

*  Produce a menu of available parameters.
      IF ( NEXT_MODULE( :8 ) .NE. 'Submenu_' ) THEN
         MINDEX = ECH_MODULE_NAME_INDEX( NEXT_MODULE )
      END IF

*  Loop through all known objects.
      TINDEX = 0
      DO I = 1, MAX_REQUIRED_OBJECTS

*     If object value is allowed to be edited then
         IF ( REQUIRED_OBJECTS( I ) .NE. 'ECH_RDCTN' .AND.
     :        REQUIRED_OBJECTS( I ) .NE. 'ECH_FTRDB' .AND.
     :        REQUIRED_OBJECTS( I ) .NE. 'ECH_ECHAR' .AND.
     :        REQUIRED_OBJECTS( I ) .NE. 'ARC_TYPE' ) THEN

*        If sub-menu options are available for selected option then
         IF ( NEXT_MODULE( :8 ) .EQ. 'Submenu_' ) THEN
            II = ISUBMENU
            FOUND = .FALSE.

*           Loop thru sub-menu options.
            DO WHILE ( SUBMENU_OPTIONS( II )( :8 ) .NE.
     :                 'Submenu_' .AND. .NOT. FOUND )

               MINDEX = ECH_MODULE_NAME_INDEX( SUBMENU_OPTIONS( II ) )
               IF ( MINDEX .GT. 0 ) THEN
                IF ( module_needs( i, mindex ) ) THEN
                  IF ( .NOT. in_rdctn_file( i ) .AND.
     :                 .NOT. is_workspace( i ) .AND.
     :                 .NOT. ( expect_in_datafile( i ) .AND.
     :                         ws_dimensions( i ) .NE. ' ' )  ) THEN
                    tindex = tindex + 1
                    params( tindex ) = i
                    found = .TRUE.
                  END IF
                END IF
              END IF
              ii = ii + 1
           END DO

*     If module uses object and its a parameter then.
         ELSE IF ( mindex .GT. 0 ) THEN
           IF ( module_needs( i, mindex ) ) THEN
            IF ( .NOT. in_rdctn_file( i ) .AND.
     :           .NOT. is_workspace( i ) .AND.
     :           .NOT. ( expect_in_datafile( i )  .AND.
     :            ws_dimensions( i ) .NE. ' ' ) ) THEN
               tindex = tindex + 1
               params( tindex ) = i
           END IF
          END IF
         END IF
        END IF
      END DO

*  Set up internal list of options, with initial parameter values.
      FFLMED_REF = 0
      NOARC_REF = 0
      NOFLAT_REF = 0
      DO I = 1, TINDEX
         TYPE = REQUIRED_TYPE( PARAMS( I ) )
         REF_NAM( I ) = REQUIRED_OBJECTS( PARAMS( I ) )
         DEFAULT_INDEX = DEFAULTS_INDICES( PARAMS( I ) )

*     Note that TUNE_NOARC is in the menu and should be updated
*     in parallel with ARC.
         IF ( REF_NAM( I ) .EQ. 'TUNE_NOARC' ) NOARC_REF = I

*     Note that TUNE_NOFLAT is in the menu and should be updated
*     in parallel with FFIELD.
         IF ( REF_NAM( I ) .EQ. 'TUNE_NOFLAT' ) NOFLAT_REF = I

*     Note that TUNE_FFLMED is in the menu and should be updated
*     in parallel with FLTFIT.
         IF ( REF_NAM( I ) .EQ. 'TUNE_FFLMED' ) FFLMED_REF = I

*     Determine whether this parameter is already defined.
         ACTIVE = .FALSE.
         II = 1
         DO WHILE ( .NOT. ACTIVE .AND. II .LE. ACCESS_COUNT )
            IF ( REF_NAM( I ) .EQ. OBJECT_NAME( II ) ) THEN
               ACTIVE = .TRUE.
               OBJECT_NUMBER = II
            END IF
            II = II + 1
         END DO

*     If active, look up the current value.
         IF ( ACTIVE ) THEN
            CALL ECH_UPDATE_OBJECT_REF(
     :           REF_NAM( I ),
     :           DUMMY_AD,
     :           DUMMY_AD,
     :           DUMMY_AD,
     :           STRING,
     :           VALUE,
     :           BOOLEAN_VALUE,
     :           STATUS
     :          )
            REF_ACT( I ) = ' '

*     Look up default value.
         ELSE
            REF_ACT( I ) = ' '
            IF ( TYPE .EQ. 'CHAR' .OR.
     :           TYPE .EQ. 'IMAGE' .OR.
     :           TYPE .EQ. 'OUTIMAGE' ) THEN
               IF ( REF_NAM( I )( 1 : 5 ) .EQ. 'TUNE_' ) THEN
                  CALL PAR_GET0C( REF_NAM( I ), STRING, STATUS )
                  IF ( STATUS .EQ. PAR__NULL ) THEN
                     CALL ERR_FLUSH( STATUS )
                     STRING = STRING_DEFAULTS( DEFAULT_INDEX )
                     STATUS = 0
                     REF_ACT( I ) = '*'
                  END IF

               ELSE
                  STRING = STRING_DEFAULTS( DEFAULT_INDEX )
                  REF_ACT( I ) = '*'
               END IF

            ELSE IF ( TYPE .EQ. 'BYTE' .OR.
     :                TYPE .EQ. 'INT' .OR.
     :                TYPE .EQ. 'SHORT' .OR.
     :                TYPE .EQ. 'DOUBLE' .OR.
     :                TYPE .EQ. 'FLOAT' ) THEN
               IF ( REF_NAM( I )( 1 : 5 ) .EQ. 'TUNE_' ) THEN
                  CALL PAR_GET0R( REF_NAM( I ), VALUE, STATUS )
                  IF ( STATUS .EQ. PAR__NULL ) THEN
                     CALL ERR_FLUSH( STATUS )
                     VALUE = VALUE_DEFAULTS( DEFAULT_INDEX )
                     STATUS = 0
                     REF_ACT( I ) = '*'
                  END IF

               ELSE
                  VALUE = VALUE_DEFAULTS( DEFAULT_INDEX )
                  REF_ACT( I ) = '*'
               END IF

            ELSE
               IF ( REF_NAM( I )( 1 : 5 ) .EQ. 'TUNE_' ) THEN
                  CALL PAR_GET0L( REF_NAM( I ), BOOLEAN_VALUE, STATUS )
                  IF ( STATUS .EQ. PAR__NULL ) THEN
                     CALL ERR_FLUSH( STATUS )
                     BOOLEAN_VALUE = BOOLEAN_DEFAULTS( DEFAULT_INDEX )
                     STATUS = 0
                     REF_ACT( I ) = '*'
                  END IF

               ELSE
                  BOOLEAN_VALUE = BOOLEAN_DEFAULTS( DEFAULT_INDEX )
                  REF_ACT( I ) = '*'
               END IF
            END IF
         END IF

*     Integer-like objects.
         IF ( TYPE .EQ. 'BYTE' .OR.
     :        TYPE .EQ. 'INT' .OR.
     :        TYPE .EQ. 'SHORT' ) THEN
            CALL CHR_ITOC( INT( VALUE ), VAL_STR( I ), NCHAR1 )

*     String-like objects.
         ELSE IF ( TYPE .EQ. 'CHAR' .OR.
     :             TYPE .EQ. 'IMAGE' .OR.
     :             TYPE .EQ. 'OUTIMAGE' ) THEN
            VAL_STR( I )( 1 : 1 ) = ''''
            CALL CHR_CTOC( STRING, VAL_STR( I )( 2: ), NCHAR1 )
            II = CHR_LEN( VAL_STR( I ) ) + 1
            VAL_STR( I )( II : II ) = ''''

*     Float-like objects.
         ELSE IF ( TYPE .EQ. 'DOUBLE' .OR.
     :             TYPE .EQ. 'FLOAT' ) THEN
            CALL CHR_RTOC( VALUE, VAL_STR( I ), NCHAR1 )

*     Logical objects.
         ELSE IF ( TYPE .EQ. 'LOGICAL' ) THEN
            CALL CHR_LTOC( BOOLEAN_VALUE, VAL_STR( I ), NCHAR1 )
         END IF
      END DO

*  Loop until exit option (0) is selected.
      option = 1
      DO WHILE ( OPTION .GT. 0 )
         CALL ECH_REPORT( 0, ' ' )
         REPORT_STRING = ' Parameters for: ' // NEXT_MODULE_TEXT
         CALL ECH_REPORT( 0, REPORT_STRING )
         CALL ECH_REPORT( 0,
     :        ' A * means the parameter must be set;' //
     :        ' the displayed value is the default.' )
         CALL ECH_REPORT( 0,
     :        ' Select number of parameter to change:' )
         CALL ECH_REPORT( 0, ' ' )
         CALL ECH_REPORT( 0, '  0. Exit' )

*     Print menu of available parameters.
         IF ( TINDEX .LE. 18 ) THEN
            DO I = 1, TINDEX
               WRITE( REPORT_STRING, 1011 ) I, REF_NAM( I ),
     :                REF_ACT( I ), VAL_STR( I )
               CALL ECH_REPORT( 0, REPORT_STRING )
            END DO

         ELSE
            DO I = 1, TINDEX / 2
               WRITE( REPORT_STRING, 1012 )
     :                I, REF_NAM( I ), REF_ACT( I ), VAL_STR( I ),
     :                I + TINDEX / 2 + MOD( TINDEX, 2 ),
     :                REF_NAM( I + TINDEX / 2 + MOD( TINDEX, 2 ) ),
     :                REF_ACT( I + TINDEX / 2 + MOD( TINDEX, 2 ) ),
     :                VAL_STR( I + TINDEX / 2 + MOD( TINDEX, 2 ) )
               CALL ECH_REPORT( 0, REPORT_STRING )
            END DO
            IF ( MOD( TINDEX, 2 ) .EQ. 1 ) THEN
               WRITE( REPORT_STRING, 1011 ) TINDEX / 2 + 1,
     :                REF_NAM( TINDEX / 2 + 1 ),
     :                REF_ACT( TINDEX / 2 + 1 ),
     :                VAL_STR( TINDEX / 2 + 1 )
               CALL ECH_REPORT( 0, REPORT_STRING )
            END IF
         END IF

*     Get parameter selection from user.
         CALL ECH_REPORT( 0, ' ' )
         VALUE = FLOAT( OPTION )
         CALL ECH_GET_PARAMETER( 'INSTANT-PROMPT=Parameter number',
     :        'INT', VALUE, .FALSE., ' ', 0, STATUS )
         OPTION = INT( VALUE )

*     If its a valid number then.
         IF ( OPTION .GT. 0 .AND. OPTION .LE. TINDEX ) THEN

*        Print parameters properties (min,max etc)
            CALL ECH_REPORT( 0, ' ' )
            CALL ECH_REPORT( 0, ' Parameter: ' //
     :                       REQUIRED_OBJECTS( PARAMS( OPTION ) ) )
            REF_NAME = REF_NAM( OPTION )
            DEFAULT_INDEX = DEFAULTS_INDICES( PARAMS( OPTION ) )
            TYPE = REQUIRED_TYPE( PARAMS( OPTION ) )

*        If parameter is one we look for in the data frame first then
*        Check for a mapped parameter value and retrieve it
            IF ( EXPECT_IN_DATAFILE( PARAMS( OPTION ) ) ) THEN
               CALL ECH_UPDATE_OBJECT_REF( REF_NAME, MAPPED_ADDRESS,
     :              MAPPED_ADDRESS, MAPPED_ADDRESS, STRING, DUMVALUE,
     :              BOOLEAN_VALUE, STATUS )
               IF ( MAPPED_ADDRESS .EQ. 0 ) THEN
                  CALL ECH_ACCESS_DATA_FILE( REF_NAME, TYPE, .FALSE.,
     :                 .FALSE., MAPPED_ADDRESS, DUMMY_AD, DUMMY_AD,
     :                 STRING, STATUS )
               END IF
            END IF

*        If its a tuning parameter then.
            IF ( REF_NAME( : 5 ) .EQ. 'TUNE_' ) THEN

*           Display characteristics.
               MIN = VALUE_MINS( DEFAULT_INDEX )
               MAX = VALUE_MAXS( DEFAULT_INDEX )

               IF ( TYPE .EQ. 'CHAR' ) THEN
                  DEFAULT_STRING = STRING_DEFAULTS( DEFAULT_INDEX )

               ELSE IF ( TYPE .EQ. 'KEY' .OR. TYPE .EQ. 'LOGICAL' ) THEN
                  IF ( DEFAULT_INDEX .EQ. 0 ) THEN
                     DEFAULT_STRING = 'NO'

                  ELSE
                     DEFAULT_STRING = 'YES'
                  END IF

               ELSE
                  CALL CHR_RTOC( VALUE_DEFAULTS( DEFAULT_INDEX ),
     :                 DEFAULT_STRING, NCHAR1 )
               END IF

               CALL ECH_REPORT( 0,
     :              ' Suggested value: ' // DEFAULT_STRING )
               IF ( TYPE .EQ. 'FLOAT' .OR.
     :              TYPE .EQ. 'INT' .OR.
     :              TYPE .EQ. 'REAL' .OR.
     :              TYPE .EQ. 'DOUBLE' .OR.
     :              TYPE .EQ. 'SHORT' .OR.
     :              TYPE .EQ. 'LONG' .OR.
     :              TYPE .EQ. 'BYTE' ) THEN
                  IF ( DEFAULT_INDEX .GT. 0 ) THEN
                     CALL CHR_RTOC( MIN, REP_STR1, NCHAR1 )
                     WRITE ( REPORT_STRING, 1001 ) REP_STR1( :NCHAR1 )
                     CALL ECH_REPORT( 0, REPORT_STRING )
                     CALL CHR_RTOC( MAX, REP_STR1, NCHAR1 )
                     WRITE ( REPORT_STRING, 1002 ) REP_STR1( :NCHAR1 )
                     CALL ECH_REPORT( 0, REPORT_STRING )

                  ELSE
                     CALL ECH_REPORT( 0,
     :                    ' No limiting values defined.' )
                  END IF
               END IF

*           Print parameter help text.
               HELP_TOPIC = 'Tuning_parameters ' // REF_NAME
               CALL ECH_HELP( HELP_TOPIC, .FALSE., STATUS )

*           Get current value of parameter.
               CALL ECH_GET_PARAMETER( REF_NAME, TYPE, VALUE,
     :              BOOLEAN_VALUE, STRING, DEFAULT_INDEX, STATUS )

*           Get new value from user.
               CALL ECH_GET_PARAMETER( 'INSTANT-PROMPT=New value',
     :              TYPE, VALUE, BOOLEAN_VALUE, STRING, DEFAULT_INDEX,
     :              STATUS )
               CALL ECH_SET_PARAMETER( REF_NAME, TYPE, VALUE,
     :              BOOLEAN_VALUE, STRING, STATUS )

            ELSE

*           Get new value from user.
               CALL ECH_GET_PARAMETER( 'FORCE-' // REF_NAME, TYPE,
     :              VALUE, BOOLEAN_VALUE, STRING, DEFAULT_INDEX,
     :              STATUS )
            END IF

*        If parameter is one we look for in the data frame first then
*        check for a mapped parameter value and replace it
            IF ( EXPECT_IN_DATAFILE( PARAMS( OPTION ) ) ) THEN
               IF ( TYPE .EQ. 'INT' ) THEN
                  INT_VALUE = INT( VALUE )
                  CALL ECH_COPY_BYTES( 4, INT_VALUE,
     :                 %VAL( MAPPED_ADDRESS ) )
               ELSE
                  CALL ECH_COPY_BYTES( 4, VALUE,
     :                 %VAL( MAPPED_ADDRESS ) )
               END IF
            END IF

*        Update internal tables.
            IF ( REF_NAME .EQ. 'ARC' ) STATUS = ECH__IMAGE_LIST
            CALL ECH_SETUP_OBJECT_REF( REF_NAME, MAPPED_ADDRESS,
     :           MAPPED_ADDRESS, MAPPED_ADDRESS, STRING, VALUE,
     :           BOOLEAN_VALUE, STATUS )

*        Update the menu entry for this parameter.
*        Integer-like objects.
            REF_ACT( OPTION ) = ' '
            IF ( TYPE .EQ. 'BYTE' .OR.
     :           TYPE .EQ. 'INT' .OR.
     :           TYPE .EQ. 'SHORT' ) THEN
               CALL CHR_ITOC( INT( VALUE ), VAL_STR( OPTION ), NCHAR1 )

*        String-like objects.
            ELSE IF ( TYPE .EQ. 'CHAR' .OR.
     :                TYPE .EQ. 'IMAGE' .OR.
     :                TYPE .EQ. 'OUTIMAGE' ) THEN
               VAL_STR( OPTION )( 1 : 1 ) = ''''
               CALL CHR_CTOC( STRING, VAL_STR( OPTION )( 2: ), NCHAR1 )
               II = CHR_LEN( VAL_STR( OPTION ) ) + 1
               VAL_STR( OPTION )( II : II ) = ''''

*        Float-like objects.
            ELSE IF ( TYPE .EQ. 'DOUBLE' .OR.
     :                TYPE .EQ. 'FLOAT' ) THEN
               CALL CHR_RTOC( VALUE, VAL_STR( OPTION ), NCHAR1 )

*        Logical objects.
            ELSE IF ( TYPE .EQ. 'LOGICAL' ) THEN
               CALL CHR_LTOC( BOOLEAN_VALUE, VAL_STR( OPTION ), NCHAR1 )
            END IF

*        Update associated parameters FLTFIT/TUNE_FFLMED.
            IF ( REF_NAME .EQ. 'FLTFIT' .AND. FFLMED_REF .NE. 0 ) THEN
               IF ( STRING .EQ. 'MEDIAN' ) THEN
                  CALL ECH_SET_PARAMETER( 'TUNE_FFLMED','LOGICAL',
     :                 0.0, .TRUE., ' ', STATUS )

               ELSE
                  CALL ECH_SET_PARAMETER( 'TUNE_FFLMED','LOGICAL',
     :                 0.0, .FALSE., ' ', STATUS )
               END IF
               CALL ECH_UPDATE_OBJECT_REF( 'TUNE_FFLMED',
     :              MAPPED_ADDRESS,
     :              MAPPED_ADDRESS,
     :              MAPPED_ADDRESS,
     :              STRING, VALUE,
     :              BOOLEAN_VALUE,
     :              STATUS )
               CALL CHR_LTOC( BOOLEAN_VALUE, VAL_STR( FFLMED_REF ),
     :              NCHAR1 )
            END IF

*        Update associated parameters FFIELD/TUNE_NOFLAT.
            IF ( REF_NAME .EQ. 'FFIELD' .AND. NOFLAT_REF .NE. 0 ) THEN
               CALL ECH_UPDATE_OBJECT_REF( 'TUNE_NOFLAT',
     :              MAPPED_ADDRESS,
     :              MAPPED_ADDRESS,
     :              MAPPED_ADDRESS,
     :              STRING, VALUE,
     :              BOOLEAN_VALUE,
     :              STATUS )
               CALL CHR_LTOC( BOOLEAN_VALUE, VAL_STR( NOFLAT_REF ),
     :              NCHAR1 )
            END IF

*        Update associated parameters ARC/TUNE_NOARC.
            IF ( REF_NAME .EQ. 'ARC' .AND. NOARC_REF .NE. 0 ) THEN
               CALL ECH_UPDATE_OBJECT_REF( 'TUNE_NOARC',
     :              MAPPED_ADDRESS,
     :              MAPPED_ADDRESS,
     :              MAPPED_ADDRESS,
     :              STRING, VALUE,
     :              BOOLEAN_VALUE,
     :              STATUS )
               CALL CHR_LTOC( BOOLEAN_VALUE, VAL_STR( NOARC_REF ),
     :              NCHAR1 )
            END IF

*     User has entered a parameter number out of range.
         ELSE IF ( OPTION .GT. 0 ) THEN
            CALL CHR_ITOC( TINDEX, REP_STR1, NCHAR1 )
            WRITE ( REPORT_STRING, 1020 ) OPTION, REP_STR1( :NCHAR1 )
            CALL ECH_REPORT( 0, REPORT_STRING )
         END IF
      END DO

      CALL ECH_REPORT( 0, ' ' )

 1000 FORMAT ( 1X, I3, '. ', A16,
     :         2X, I3, '. ', A16,
     :         2X, I3, '. ', A16 )
 1001 FORMAT ( 1X, 'Minimum value allowed: ', A, '.' )
 1002 FORMAT ( 1X, 'Maximum value allowed: ', A, '.' )
 1003 FORMAT ( 1X, I3, '. ', A16 )
 1004 FORMAT ( 1X, I3, '. ', A16,
     :         2X, I3, '. ', A16 )

 1011 FORMAT ( 1X, I2, '. ', A16, '=', A1, A )
 1012 FORMAT ( 1X, I2, '. ', A16, '=', A1, A21,
     :             I2, '. ', A16, '=', A1, A21 )
 1020 FORMAT ( '!  Invalid option: ', I2, ', valid range is: ',
     :         '0-', A, '.' )

      END
