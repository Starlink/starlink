      SUBROUTINE ECH_ACTIVE_TUNE(
     :           NEXT_MODULE,
     :           NEXT_MODULE_TEXT,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_ACTIVE_TUNE

*  Purpose:
*     Interactive setup/inquiry of parameters.

*  Description:
*     This routine supports the interactive setup/inquiry of all the tuning
*     parameters for all the ECHOMOP tasks.

*  Invocation:
*     CALL ECH_ACTIVE_TUNE(
*     :    NEXT_MODULE,
*     :    NEXT_MODULE_TEXT,
*     :    STATUS
*     :   )

*  Arguments:
*     NEXT_MODULE = CHAR (Given)
*        Name of next processing module (the default next one).
*     NEXT_MODULE_TEXT = CHAR (Given)
*        The descriptive text for the next module.
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

*  Method:
*     Check if current menu option has sub-menu options available
*     Produce a menu of available tuning parameters
*     Loop through all known objects
*        If object is a tuning parameter then
*           Remember its position in internal tables
*           If sub-menu checking required then
*              Loop through sub-menu options
*                 If used by option, mark parameter as ACTIVE
*              End loop
*           Else if used by menu option
*           Endif
*        Endif
*     End loop
*     Loop through all options (till option 0 selected)
*        Loop through located tuning parameters
*           Write parameter name(s) in menu form
*        End loop
*        Get required parameters number from user
*        If not exit (option 0) then
*          Print list of modules using this parameter
*          Print parameter details (min,max,etc)
*          Print parameter help text
*          Get new value for parameter from user
*          Set parameter to new value to update internal tables
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

*  Global Constants:
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
      REAL MIN
      REAL MAX

      INTEGER TUNERS( MAX_REQUIRED_OBJECTS )
      INTEGER DUMMY_AD
      INTEGER I
      INTEGER II
      INTEGER DEFAULT_INDEX
      INTEGER ISUBMENU
      INTEGER OBJECT_NUMBER
      INTEGER OPTION
      INTEGER TINDEX
      INTEGER NCHAR1
      INTEGER ISWAP

      LOGICAL BOOLEAN_VALUE
      LOGICAL FOUND
      LOGICAL ACTIF

      CHARACTER*80 WORK_STRING
      CHARACTER*80 REF_NAME
      CHARACTER*80 REP_STR
      CHARACTER*64 STRING
      CHARACTER*64 DEFAULT_STRING
      CHARACTER*48 VAL_STR( 110 )
      CHARACTER*16 REF_NAM( 110 )
      CHARACTER*32 TYPE
      CHARACTER*4 REF_STR1
      CHARACTER*1 ACTIVE( MAX_REQUIRED_OBJECTS )
      CHARACTER*1 CSWAP

*  Functions Called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
      INTEGER ECH_MODULE_NAME_INDEX
      LOGICAL MODULE_NEEDS
      INTEGER CHR_LEN
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

*  Check if current menu option has sub-menu options available.
      ISUBMENU = 0
      IF ( NEXT_MODULE( :8 ) .EQ. 'Submenu_' ) THEN
         DO I = 1, NUM_SUBOPTIONS
            IF ( NEXT_MODULE .EQ. SUBMENU_OPTIONS( I ) )
     :         ISUBMENU = I + 1
         END DO
      ENDIF

*  Produce a menu of available tuning parameters.
      TINDEX = 0

*  Loop through all known objects.
      DO I = 1, MAX_REQUIRED_OBJECTS

*     If this is a tuning parameter store its position in a table.
         IF ( REQUIRED_OBJECTS( I )( :5 ) .EQ. 'TUNE_' ) THEN
            TINDEX = TINDEX + 1
            TUNERS( TINDEX ) = I
            ACTIVE( TINDEX ) = ' '

*        If sub-menu checking required.
            IF ( NEXT_MODULE( :8 ) .EQ. 'Submenu_' ) THEN
               FOUND = .FALSE.
               II = ISUBMENU

*           Loop through sub-menu options looking for parameter,
*           if used by option, mark parameter as ACTIVE.
               DO WHILE ( SUBMENU_OPTIONS( II )( :8 ) .NE.
     :                    'Submenu_' .AND. .NOT. FOUND )
                  IF ( ECH_MODULE_NAME_INDEX( SUBMENU_OPTIONS( II ) )
     :                 .GT. 0 ) THEN
                    IF ( MODULE_NEEDS( I,  ECH_MODULE_NAME_INDEX(
     :                   SUBMENU_OPTIONS( II ) ) ) ) THEN
                       ACTIVE( TINDEX ) = '*'
                       FOUND = .TRUE.
                    ENDIF
                  ENDIF
                  II = II + 1
               END DO

*        Else if used by menu option.
            ELSE IF ( ECH_MODULE_NAME_INDEX( NEXT_MODULE ) .GT. 0 ) THEN
               IF ( MODULE_NEEDS( I,
     :                ECH_MODULE_NAME_INDEX( NEXT_MODULE ) ) ) THEN
                  ACTIVE( TINDEX ) = '*'
               ENDIF
            ENDIF
         ENDIF
      END DO

*  Sort the indices into alphabetical order.
      DO I = 1, TINDEX - 1
         DO II = I + 1, TINDEX
            IF ( REQUIRED_OBJECTS( TUNERS( II ) ) .LT.
     :           REQUIRED_OBJECTS( TUNERS( I ) ) ) THEN
               CSWAP = ACTIVE( II )
               ACTIVE( II ) = ACTIVE( I )
               ACTIVE( I ) = CSWAP
               ISWAP = TUNERS( II )
               TUNERS( II ) = TUNERS( I )
               TUNERS( I ) = ISWAP
            END IF
         END DO
      END DO

*  Build initial menu.
      DO I = 1, TINDEX
         TYPE = REQUIRED_TYPE( TUNERS( I ) )
         REF_NAM( I ) = REQUIRED_OBJECTS( TUNERS( I ) )( 6 : )
         DEFAULT_INDEX = DEFAULTS_INDICES( TUNERS( I ) )
         WORK_STRING = 'TUNE_' // REF_NAM( I )

*     Determine whether this parameter is already defined.
         ACTIF = .FALSE.
         II = 1
         DO WHILE ( .NOT. ACTIF .AND. II .LE. ACCESS_COUNT )
            IF ( WORK_STRING .EQ. OBJECT_NAME( II ) ) THEN
               ACTIF = .TRUE.
               OBJECT_NUMBER = II
            END IF
            II = II + 1
         END DO

*     If active, look up the current value.
         IF ( ACTIF ) THEN
            CALL ECH_UPDATE_OBJECT_REF( WORK_STRING, DUMMY_AD, DUMMY_AD,
     :           DUMMY_AD, STRING, VALUE, BOOLEAN_VALUE, STATUS )

*     Look up default value.
         ELSE
            IF ( TYPE .EQ. 'CHAR' .OR.
     :           TYPE .EQ. 'IMAGE' .OR.
     :           TYPE .EQ. 'OUTIMAGE' ) THEN
               CALL PAR_GET0C( WORK_STRING, STRING, STATUS )
               IF ( STATUS .EQ. PAR__NULL ) THEN
                  CALL ERR_FLUSH( STATUS )
                  STRING = STRING_DEFAULTS( DEFAULT_INDEX )
                  STATUS = 0
               END IF

            ELSE IF ( TYPE .EQ. 'BYTE' .OR.
     :                TYPE .EQ. 'INT' .OR.
     :                TYPE .EQ. 'SHORT' .OR.
     :                TYPE .EQ. 'DOUBLE' .OR.
     :                TYPE .EQ. 'FLOAT' ) THEN
               CALL PAR_GET0R( WORK_STRING, VALUE, STATUS )
               IF ( STATUS .EQ. PAR__NULL ) THEN
                  CALL ERR_FLUSH( STATUS )
                  VALUE = VALUE_DEFAULTS( DEFAULT_INDEX )
                  STATUS = 0
               END IF

            ELSE
               CALL PAR_GET0L( WORK_STRING, BOOLEAN_VALUE, STATUS )
               IF ( STATUS .EQ. PAR__NULL ) THEN
                  CALL ERR_FLUSH( STATUS )
                  BOOLEAN_VALUE = BOOLEAN_DEFAULTS( DEFAULT_INDEX )
                  STATUS = 0
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
            VAL_STR( I )( :1 ) = ''''
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

*  Loop until option 0 selected.
      option = 1
      DO WHILE ( option .GT. 0 )
         CALL ECH_REPORT( 0, ' ' )
         CALL ECH_REPORT( 0, ' Tuning parameter adjustment:' )
         WORK_STRING = ' * indicates parameter used by: ' //
     :         NEXT_MODULE_TEXT
         CALL ECH_REPORT( 0, WORK_STRING )
         CALL ECH_REPORT( 0,
     :       ' The TUNE_ prefix has been removed for ease of reading.' )
         CALL ECH_REPORT( 0, ' ' )
         CALL ECH_REPORT( 0, '  0. Exit' )

*     Print menu of available parameters.
         DO I = 1, ( TINDEX - 1 ) / 3
            WRITE( REPORT_STRING, 1012 )
     :           I,
     :           REF_NAM( I ),
     :           ACTIVE( I ),
     :           VAL_STR( I ),
     :           I + TINDEX / 3 + 1,
     :           REF_NAM( I + TINDEX / 3 + 1 ),
     :           ACTIVE( I + TINDEX / 3 + 1 ),
     :           VAL_STR( I + TINDEX / 3 + 1 ),
     :           I + 2 * TINDEX / 3 + 1,
     :           REF_NAM( I + 2 * TINDEX / 3 + 1 ),
     :           ACTIVE( I + 2 * TINDEX / 3 + 1 ),
     :           VAL_STR( I + 2 * TINDEX / 3 + 1 )
            CALL ECH_REPORT( 0, REPORT_STRING )
         END DO
         IF ( MOD( TINDEX, 3 ) .EQ. 2 ) THEN
            I = TINDEX / 3 + 1
            WRITE( REPORT_STRING, 1013 )
     :           I,
     :           REF_NAM( I ),
     :           ACTIVE( I ),
     :           VAL_STR( I ),
     :           I + TINDEX / 3 + 1,
     :           REF_NAM( I + TINDEX / 3 + 1 ),
     :           ACTIVE( I + TINDEX / 3 + 1 ),
     :           VAL_STR( I + TINDEX / 3 + 1 )
            CALL ECH_REPORT( 0, REPORT_STRING )

         ELSE IF ( MOD( TINDEX, 3 ) .EQ. 1 ) THEN
            I = TINDEX / 3 + 1
            WRITE( REPORT_STRING, 1014 )
     :           I, REF_NAM( I ), ACTIVE( I ), VAL_STR( I )
            CALL ECH_REPORT( 0, REPORT_STRING )
         END IF

*     Get parameter number from user.
         CALL ECH_REPORT( 0, ' ' )
         VALUE = FLOAT( OPTION )
         CALL ECH_GET_PARAMETER( 'INSTANT-PROMPT=Parameter number',
     :        'INT', VALUE, .FALSE., ' ', 0, STATUS )
         OPTION = INT( VALUE )

*     If not exit (option 0) then
         IF ( OPTION .GT. 0 .AND. OPTION .LE. TINDEX ) THEN
            CALL ECH_REPORT( 0, ' ' )
            WORK_STRING = ' Parameter: ' //
     :            REQUIRED_OBJECTS( TUNERS( OPTION ) )
            CALL ECH_REPORT( 0, WORK_STRING )
            CALL ECH_REPORT( 0, ' ' )

            DEFAULT_INDEX = DEFAULTS_INDICES( TUNERS( OPTION ) )
            REF_NAME = REQUIRED_OBJECTS( TUNERS( OPTION ) )
            TYPE = REQUIRED_TYPE( TUNERS( OPTION ) )
            MIN = VALUE_MINS( DEFAULT_INDEX )
            MAX = VALUE_MAXS( DEFAULT_INDEX )

*        Print parameter details (min,max,etc)
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
     :                        DEFAULT_STRING, NCHAR1 )
            ENDIF

            WORK_STRING = ' Suggested value is ' // DEFAULT_STRING
            CALL ECH_REPORT( 0, WORK_STRING )
            IF ( type .EQ. 'FLOAT' .OR. type .EQ. 'INT' .OR.
     :          type .EQ. 'REAL' .OR. type .EQ. 'DOUBLE' .OR.
     :          type .EQ. 'SHORT' .OR. type .EQ. 'LONG' .OR.
     :          type .EQ. 'BYTE' ) THEN
              IF ( DEFAULT_INDEX .GT. 0 ) THEN
                 CALL CHR_RTOC( MIN, REP_STR, NCHAR1 )
                 WRITE ( REPORT_STRING, 1001 ) REP_STR( :NCHAR1 )
                 CALL ECH_REPORT( 0, REPORT_STRING )
                 CALL CHR_RTOC( MAX, REP_STR, NCHAR1 )
                 WRITE ( REPORT_STRING, 1002 ) REP_STR( :NCHAR1 )
                 CALL ECH_REPORT( 0, REPORT_STRING )

              ELSE
                 CALL ECH_REPORT( 0, ' No limiting values defined.' )
              ENDIF
            ENDIF

*        Print parameter help text.
            WORK_STRING = 'Tuning_parameters ' // REF_NAME
            CALL ECH_HELP( WORK_STRING, .FALSE., STATUS )

*        Get new value for parameter from user.
           CALL ECH_GET_PARAMETER( REF_NAME, TYPE, VALUE, BOOLEAN_VALUE,
     :          STRING, DEFAULT_INDEX, STATUS )
           CALL ECH_GET_PARAMETER( 'INSTANT-PROMPT=NEW VALUE', TYPE,
     :          VALUE, BOOLEAN_VALUE, STRING, DEFAULT_INDEX, STATUS )
           CALL ECH_SET_PARAMETER( REF_NAME, TYPE, VALUE, BOOLEAN_VALUE,
     :          STRING, STATUS )

*        Update the menu entry for this parameter.
*        Integer-like objects.
            IF ( TYPE .EQ. 'BYTE' .OR.
     :           TYPE .EQ. 'INT' .OR.
     :           TYPE .EQ. 'SHORT' ) THEN
               CALL CHR_ITOC( INT( VALUE ), VAL_STR( OPTION ), NCHAR1 )

*        String-like objects.
            ELSE IF ( TYPE .EQ. 'CHAR' .OR.
     :                TYPE .EQ. 'IMAGE' .OR.
     :                TYPE .EQ. 'OUTIMAGE' ) THEN
               VAL_STR( OPTION )( :1 ) = ''''
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

*     User has entered a parameter number out of range.
         ELSE IF ( OPTION .GT. 0 ) THEN
            CALL CHR_ITOC( TINDEX, REF_STR1, NCHAR1 )
            WRITE ( REPORT_STRING, 1020 ) OPTION, REF_STR1( :NCHAR1 )
            CALL ECH_REPORT( 0, REPORT_STRING )
         END IF
      END DO

 1000 FORMAT ( 1X, I3, '. ', A1, A16,
     :         2X, I3, '. ', A1, A16,
     :         2X, I3, '. ', A1, A16 )
 1001 FORMAT ( 1X, 'Minimum value allowed: ', A, '.' )
 1002 FORMAT ( 1X, 'Maximum value allowed: ', A, '.' )
 1003 FORMAT ( 1X, I3, '. ', A1, A16 )
 1004 FORMAT ( 1X, I3, '. ', A1, A16,
     :         2X, I3, '. ', A1, A16 )
 1012 FORMAT ( 1X, I2, '. ', A11, '=', A1, A8,
     :             I2, '. ', A11, '=', A1, A8,
     :             I3, '. ', A11, '=', A1, A8 )
 1013 FORMAT ( 1X, I2, '. ', A11, '=', A1, A8,
     :             I2, '. ', A11, '=', A1, A8 )
 1014 FORMAT ( 1X, I2, '. ', A11, '=', A1, A8 )
 1020 FORMAT ( '!  Invalid option: ', I3, ', valid range is: ',
     :         '0-', A, '.' )

      END
