      SUBROUTINE ECH_GET_PARAMETER(
     :           REQUIRED_OBJECT,
     :           TYPE,
     :           VALUE,
     :           BOOLEAN,
     :           STRING,
     :           DEFAULT_INDEX,
     :           STATUS
     :          )
*+
*  Name:
*     ECH_GET_PARAMETER

*  Purpose:
*     Call underlying parameter system to get a parameter value.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL ECH_GET_PARAMETER(
*     :     REQUIRED_OBJECT,
*     :     TYPE,
*     :     VALUE,
*     :     BOOLEAN,
*     :     STRING,
*     :     DEFAULT_INDEX,
*     :     STATUS
*     :     )

*  Arguments:
*     REQUIRED_OBJECT = CHARACTER*( * ) (Given and Returned)
*        Name of the object required and, optionally, type of effort to
*        be made to get the value.
*     TYPE = CHARACTER*( * ) (Given)
*        Data type of the object to be "got".
*     VALUE = REAL (Given and Returned)
*        Value for numerical objects.
*     BOOLEAN = LOGICAL (Given and Returned)
*        Value for LOGICAL objects.
*     STRING = CHARACTER*( * ) (Given and Returned)
*        Value for character string objects.
*     DEFAULT_INDEX = INTEGER (Given and Returned)
*        When given, is the index into the appropriate default value list.
*        When returned, is the index for an array object.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DMILLS: Dave Mills (UCL, Starlink)
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     ??-???-???? (DMILLS):
*       Initial release.
*     23-APR-1996 (MJC):
*       Added prologue.
*     21-MAR-1997 (MJC):
*       Removed (unused) TYPE=VNAME reference.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'PAR_ERR'          ! PAR status values

*  Include Files:
      INCLUDE 'ECH_MODULES.INC'
      INCLUDE 'ECH_REPORT.INC'
      INCLUDE 'ECH_INIT_RDCTN.INC'
      INCLUDE 'ECH_USE_RDCTN.INC'
      INCLUDE 'ECH_ENVIR_CONSTANTS.INC'

*  Arguments:
      CHARACTER*( * ) REQUIRED_OBJECT
      CHARACTER*( * ) TYPE
      REAL VALUE
      LOGICAL BOOLEAN
      CHARACTER*( * ) STRING
      INTEGER DEFAULT_INDEX

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      REAL VALUE_MIN
      REAL VALUE_MAX
      REAL VALUE_DEFAULT
      REAL ASIZE

      INTEGER I
      INTEGER ISTAT
      INTEGER IVALUE
      INTEGER OBJECT_NUMBER
      INTEGER NEW_ACCESS_COUNT
      INTEGER DUMMY
      INTEGER ECH_OBJ_IND
      INTEGER NCHAR1
      INTEGER NCHAR2
      INTEGER NCHAR3

      LOGICAL FORCE_REPROMPT
      LOGICAL BOOLEAN_DEFAULT
      LOGICAL USED_AS_INDEX
      LOGICAL OBJECT_ACTIVE
      LOGICAL INTERNAL

      CHARACTER*80 PROMPT_STRING
      CHARACTER*80 DUMMY_STRING
      CHARACTER*64 STRING_DEFAULT
      CHARACTER*32 VALUE_UNITS
      CHARACTER*12 REF_STR1
      CHARACTER*8 REF_STR2
      CHARACTER*8 REF_STR3

*  Functions Called:
      INTEGER CHR_LEN
      INTEGER ECH_WORD_LEN
      LOGICAL ECH_FATAL_ERROR
*.

*  Check for 'internal' requests.
*  We assign values to such parameters directly as they are used by most
*  programs.
*  These parameters are stored in the ECH_INIT_RDCTN common block.
      INTERNAL = ( REQUIRED_OBJECT .EQ. 'NX' .OR.
     :             REQUIRED_OBJECT .EQ. 'NY' .OR.
     :             ( REQUIRED_OBJECT .EQ. 'NUM_ORDERS' .AND.
     :               NUM_ORDERS .GT. 0 ) )

      DUMMY_STRING = REQUIRED_OBJECT // ' '

*  Handle three of the internals.
      IF ( INTERNAL ) THEN
         IF ( REQUIRED_OBJECT .EQ. 'NX' ) THEN
            VALUE = FLOAT( NX )

         ELSE IF ( REQUIRED_OBJECT .EQ. 'NY' ) THEN
            VALUE = FLOAT( NY )

         ELSE IF ( REQUIRED_OBJECT .EQ. 'NUM_ORDERS' ) THEN
            VALUE = FLOAT( NUM_ORDERS )
         END IF

*  Handle user-prompting.
      ELSE IF ( DUMMY_STRING( :15 ) .EQ. 'INSTANT-PROMPT=' ) THEN
         PROMPT_STRING = REQUIRED_OBJECT( 16: )
         IF ( TYPE .EQ. 'NAME' .OR. TYPE .EQ. 'CHAR' .OR.
     :        TYPE .EQ. 'ALPHA' .OR. TYPE .EQ. 'IMAGE' .OR.
     :        TYPE .EQ. 'OUTIMAGE' .OR. TYPE .EQ. 'TEXT' ) THEN
            STRING_DEFAULT = STRING
            CALL PAR_CANCL( 'ANYSTRING', ISTAT )
            ISTAT = 0
            CALL PAR_PROMT( 'ANYSTRING', PROMPT_STRING, ISTAT )
            CALL PAR_DEF0C( 'ANYSTRING', STRING_DEFAULT, ISTAT )
            ISTAT = 0
            CALL PAR_GET0C( 'ANYSTRING', STRING, ISTAT )
            STRING = STRING( : MAX( 1, CHR_LEN( STRING ) ) ) // ' '

         ELSE IF ( TYPE .EQ. 'KEY' .OR. TYPE .EQ. 'LOGICAL' ) THEN
            BOOLEAN_DEFAULT = BOOLEAN
            CALL PAR_CANCL( 'ANYLOGICAL', ISTAT )
            ISTAT = 0
            CALL PAR_PROMT( 'ANYLOGICAL', PROMPT_STRING, ISTAT )
            CALL PAR_DEF0L( 'ANYLOGICAL', BOOLEAN_DEFAULT, ISTAT )
            ISTAT = 0
            CALL PAR_GET0L( 'ANYLOGICAL', BOOLEAN, ISTAT )

         ELSE
            VALUE_UNITS = UNITS_STRINGS( DEFAULT_INDEX )
            VALUE_MIN = VALUE_MINS( DEFAULT_INDEX )
            VALUE_MAX = VALUE_MAXS( DEFAULT_INDEX )
            VALUE_DEFAULT = VALUE
            VALUE_UNITS = ' '
            VALUE = VALUE_MIN
            CALL PAR_CANCL( 'ANYNUMBER', ISTAT )
            ISTAT = 0
            CALL PAR_PROMT( 'ANYNUMBER', PROMPT_STRING, ISTAT )
            CALL PAR_DEF0R( 'ANYNUMBER', VALUE_DEFAULT, ISTAT )
            ISTAT = 0
            CALL PAR_GET0R( 'ANYNUMBER', VALUE, ISTAT )
         END IF
         IF ( ISTAT .NE. 0 ) THEN
            STATUS = ISTAT
            CALL ERR_FLUSH( STATUS )
         END IF
         STATUS = ISTAT
         IF ( ISTAT .EQ. PAR__ABORT ) RETURN

      ELSE

*     If a forced reprompt is requested, cancel the current value of
*     the parameter.
         IF ( DUMMY_STRING( :6 ) .EQ. 'FORCE-' ) THEN
            FORCE_REPROMPT = .TRUE.
            REQUIRED_OBJECT = REQUIRED_OBJECT( 7 : )
            CALL PAR_CANCL( REQUIRED_OBJECT, ISTAT )
            ISTAT = 0

         ELSE
            FORCE_REPROMPT = .FALSE.
         END IF

*     Check if object has already got an access active.
         OBJECT_ACTIVE = .FALSE.
         I = 1
         DO WHILE ( .NOT. OBJECT_ACTIVE .AND. I .LE. ACCESS_COUNT )
            IF ( REQUIRED_OBJECT .EQ. OBJECT_NAME( I ) ) THEN
               OBJECT_ACTIVE = .TRUE.
               OBJECT_NUMBER = I
            END IF
            I = I + 1
         END DO
         IF ( STATUS .EQ. ECH__WORKSPACE_PAR ) OBJECT_ACTIVE = .FALSE.
         IF ( STATUS .EQ. ECH__ARRAY_INDEX ) THEN
            USED_AS_INDEX = .TRUE.

         ELSE
            USED_AS_INDEX = .FALSE.
         END IF

*     Retrieve the parameter value.
         IF ( ( .NOT. OBJECT_ACTIVE ) .OR. FORCE_REPROMPT .OR.
     :        ( REQUIRED_OBJECT( :5 ) .EQ. 'TUNE_' ) ) THEN
            IF ( .NOT. OBJECT_ACTIVE ) THEN
               NEW_ACCESS_COUNT = ACCESS_COUNT + 1
            END IF

*        Get a CHARACTER value.
            IF ( TYPE .EQ. 'NAME' .OR. TYPE .EQ. 'CHAR' .OR.
     :           TYPE .EQ. 'ALPHA' .OR. TYPE .EQ. 'IMAGE' .OR.
     :           TYPE .EQ. 'OUTIMAGE' .OR. TYPE .EQ. 'TEXT' ) THEN
               STATUS = 0
               STRING_DEFAULT = STRING_DEFAULTS( DEFAULT_INDEX )
               CALL PAR_DEF0C( REQUIRED_OBJECT, STRING_DEFAULT,
     :                         STATUS )
               STATUS = 0
               STRING = 'POIUYTREWQ0987654321'
               CALL PAR_GET0C( REQUIRED_OBJECT, STRING, STATUS )
               STRING = STRING( : MAX( 1, CHR_LEN( STRING ) ) ) // ' '
               IF ( STATUS .EQ. PAR__NULL ) THEN
                  STRING = STRING_DEFAULT
                  CALL ERR_FLUSH( STATUS )
                  STATUS = 0

               ELSE IF ( STATUS .NE. PAR__ABORT ) THEN
                  IF ( STRING .EQ. 'POIUYTREWQ0987654321' ) THEN
                     STATUS = ECH__BAD_USERPAR
                  END IF

               ELSE
                  STRING = STRING_DEFAULT
               END IF
               IF ( STRING .NE. STRING_DEFAULT .AND.
     :              ( .NOT. USED_AS_INDEX )  .AND.
     :              REQUIRED_OBJECT( :5 ) .EQ. 'TUNE_' ) THEN
                  REPORT_STRING = ' ' //
     :                 REQUIRED_OBJECT(
     :                 :ECH_WORD_LEN( REQUIRED_OBJECT ) ) //
     :                 ' is set to non-default value: ''' //
     :                 STRING( :CHR_LEN( STRING ) ) // '''.'
                  CALL ECH_REPORT( 0, REPORT_STRING )
               END IF
               IF ( FORCE_UPPERCASE( DEFAULT_INDEX ) ) THEN
                  CALL CHR_UCASE( STRING )
               END IF

!!  This type is only usable directly from within processing routines and
!!  is not called by the interface layer.
            ELSE IF ( TYPE .EQ. 'ARRAY' ) THEN
               STATUS = 0

!! special usage for string_defaults entry here to supply the name of
!! the dimensioning variable for the array. In this case 'value' is actually
!! a REAL array passed from the caller. We just take advantage of passing by
!! reference to avoid the problem of having to declare it as an array in
!! this routine (we dont know its dimension!)
               IF ( DEFAULT_INDEX .EQ. 0 ) THEN
                  STATUS = 0
                  CALL PAR_GET0R( STRING, ASIZE, STATUS )
                  DEFAULT_INDEX = INT( ASIZE )
               END IF

               IF ( STATUS .NE. 0 ) THEN
                  REPORT_STRING = ' Failed to obtain dimension' //
     :                  ' specifier of array variable ' //
     :                  REQUIRED_OBJECT
                  CALL ECH_REPORT( 0, REPORT_STRING )

               ELSE
                  STATUS = 0
                  CALL PAR_GET1R( REQUIRED_OBJECT, INT( DEFAULT_INDEX ),
     :                 VALUE, STATUS )
               END IF

*        Get a LOGICAL value.
            ELSE IF ( TYPE .EQ. 'KEY'     .OR.
     :                TYPE .EQ. 'LOGICAL' .OR.
     :                TYPE .EQ. 'FLAG'    .OR.
     :                TYPE .EQ. 'BOOLEAN' ) THEN
               STATUS = 0
               IF ( OBJECT_ACTIVE ) THEN
                  CALL ECH_UPDATE_OBJECT_REF(
     :                 REQUIRED_OBJECT,
     :                 DUMMY,
     :                 DUMMY,
     :                 DUMMY,
     :                 STRING,
     :                 VALUE,
     :                 BOOLEAN_DEFAULT,
     :                 STATUS
     :                )

               ELSE
                  BOOLEAN_DEFAULT = BOOLEAN_DEFAULTS( DEFAULT_INDEX )
               END IF
               CALL PAR_DEF0L( REQUIRED_OBJECT, BOOLEAN_DEFAULT, ISTAT )
               ISTAT = 0
               CALL PAR_GET0L( REQUIRED_OBJECT, BOOLEAN, ISTAT )
               STATUS = ISTAT
               IF ( STATUS .EQ. PAR__NULL ) THEN
                  CALL ERR_FLUSH( STATUS )
                  BOOLEAN = BOOLEAN_DEFAULT
                  STATUS = 0
               END IF
               IF ( BOOLEAN .NEQV. BOOLEAN_DEFAULTS( DEFAULT_INDEX )
     :              .AND. ( .NOT. USED_AS_INDEX )  .AND.
     :              STRING .NE. 'SWITCH' .AND.
     :              REQUIRED_OBJECT( :5 ) .EQ. 'TUNE_' ) THEN
                  REPORT_STRING = ' ' // REQUIRED_OBJECT( :
     :                  ECH_WORD_LEN( REQUIRED_OBJECT ) ) //
     :                  ' is set to non-default value: '
                  NCHAR1 = CHR_LEN( REPORT_STRING ) + 1
                  IF ( BOOLEAN ) THEN
                     REPORT_STRING = REPORT_STRING( :NCHAR1 ) // 'YES.'

                  ELSE
                     REPORT_STRING = REPORT_STRING( :NCHAR1 ) // 'NO.'
                  ENDIF
                  CALL ECH_REPORT( 0, REPORT_STRING )
               ENDIF

*        Get an integer value.
            ELSE IF ( TYPE .EQ. 'INT' ) THEN
               VALUE_DEFAULT = VALUE_DEFAULTS( DEFAULT_INDEX )
               VALUE_UNITS = UNITS_STRINGS( DEFAULT_INDEX )
               VALUE_MIN = VALUE_MINS( DEFAULT_INDEX )
               VALUE_MAX = VALUE_MAXS( DEFAULT_INDEX )
               IF ( REQUIRED_OBJECT( :4 ) .EQ. 'IDX_' ) THEN
                  VALUE_MIN = 0
                  VALUE_UNITS = 'Array index'
               END IF
               VALUE = ECH__BAD_REAL
               IF ( STATUS .EQ. ECH__WORKSPACE_PAR ) THEN
                  IF ( DEFAULT_INDEX .GT. 0 ) THEN
                     CALL PAR_DEF0I( REQUIRED_OBJECT,
     :                    INT( VALUE_DEFAULT ), ISTAT )
                     ISTAT = 0
                     CALL PAR_GET0I( REQUIRED_OBJECT, IVALUE, ISTAT )
                     STATUS = ISTAT
                     IF ( STATUS .EQ. PAR__NULL ) THEN
                        CALL ERR_FLUSH( STATUS )
                        VALUE = VALUE_DEFAULT
                        STATUS = 0

                     ELSE
                        VALUE = FLOAT( IVALUE )
                     END IF

                  ELSE
                     CALL ECH_REPORT( 0, ' ' )
                     REPORT_STRING = ' The following object could' //
     :                    ' not be located in the input dataset'
                     CALL ECH_REPORT( 0, REPORT_STRING )
                     REPORT_STRING = ' but a value for it may now' //
     :                    ' be input from the terminal'
                     CALL ECH_REPORT( 0, REPORT_STRING )
                     CALL ECH_REPORT( 0, ' ' )
                     PROMPT_STRING = 'Value of ' // REQUIRED_OBJECT
                     VALUE = VALUE_MIN
                     CALL PAR_CANCL( 'ANYNUMBER', ISTAT )
                     ISTAT = 0
                     CALL PAR_PROMT( 'ANYNUMBER',  PROMPT_STRING,
     :                               ISTAT )
                     CALL PAR_DEF0R( 'ANYNUMBER', VALUE_DEFAULT, ISTAT )
                     ISTAT = 0
                     CALL PAR_GET0R( 'ANYNUMBER', VALUE, ISTAT )
                     STATUS = ISTAT
                     IF ( STATUS .EQ. PAR__NULL ) THEN
                        CALL ERR_FLUSH( STATUS )
                        VALUE = VALUE_DEFAULT
                        STATUS = 0
                     END IF
                  END IF

               ELSE
                  STATUS = 0
                  VALUE = VALUE_MIN
                  CALL PAR_DEF0I( REQUIRED_OBJECT, INT( VALUE_DEFAULT ),
     :                 ISTAT )
                  ISTAT = 0
                  CALL PAR_GET0I( REQUIRED_OBJECT, IVALUE, ISTAT )
                  STATUS = ISTAT
                  IF ( STATUS .EQ. PAR__NULL ) THEN
                     CALL ERR_FLUSH( STATUS )
                     VALUE = VALUE_DEFAULT
                     STATUS = 0

                  ELSE
                     VALUE = IVALUE
                  END IF
                  IF ( VALUE .NE. VALUE_DEFAULT .AND.
     :                 ( .NOT. USED_AS_INDEX )  .AND.
     :                 REQUIRED_OBJECT( :5 ) .EQ. 'TUNE_' .AND.
     :                 STRING .NE. 'SWITCH' ) THEN
                     CALL CHR_RTOC( VALUE, REF_STR1, NCHAR1 )
                     REPORT_STRING = ' ' //
     :               REQUIRED_OBJECT(
     :               :ECH_WORD_LEN( REQUIRED_OBJECT ) ) //
     :               ' is set to non-default value: ' //
     :               REF_STR1( :NCHAR1 ) // '.'
                     CALL ECH_REPORT( 0, REPORT_STRING )
                  END IF
               END IF
               IF ( VALUE .EQ. ECH__BAD_REAL .AND.
     :              STATUS .NE. PAR__ABORT )
     :            STATUS = ECH__BAD_USERPAR

*        Get a floating value.
            ELSE
               VALUE_DEFAULT = VALUE_DEFAULTS( DEFAULT_INDEX )
               VALUE_UNITS = UNITS_STRINGS( DEFAULT_INDEX )
               VALUE_MIN = VALUE_MINS( DEFAULT_INDEX )
               VALUE_MAX = VALUE_MAXS( DEFAULT_INDEX )
               VALUE = ECH__BAD_REAL
               IF ( STATUS .EQ. ECH__WORKSPACE_PAR ) THEN
                  IF ( DEFAULT_INDEX .GT. 0 ) THEN
                     CALL PAR_DEF0R( REQUIRED_OBJECT, VALUE_DEFAULT,
     :                               ISTAT )
                     ISTAT = 0
                     CALL PAR_GET0R( REQUIRED_OBJECT, VALUE, ISTAT )
                     STATUS = ISTAT
                     IF ( STATUS .EQ. PAR__NULL ) THEN
                        CALL ERR_FLUSH( STATUS )
                        VALUE = VALUE_DEFAULT
                        STATUS = 0
                     END IF

                  ELSE
                     CALL ECH_REPORT( 0, ' ' )
                     REPORT_STRING = ' The following object could' //
     :                    ' not be located in the input dataset'
                     CALL ECH_REPORT( 0, REPORT_STRING )
                     REPORT_STRING = ' but a value for it may now' //
     :                    ' be input from the terminal'
                     CALL ECH_REPORT( 0, REPORT_STRING )
                     CALL ECH_REPORT ( 0, ' ' )
                     PROMPT_STRING = 'Value of ' // REQUIRED_OBJECT
                     VALUE = VALUE_MIN
                     CALL PAR_CANCL( 'ANYNUMBER', ISTAT )
                     ISTAT = 0
                     CALL PAR_PROMT( 'ANYNUMBER',  PROMPT_STRING,
     :                    ISTAT )
                     CALL PAR_DEF0R( 'ANYNUMBER', VALUE_DEFAULT, ISTAT )
                     ISTAT = 0
                     CALL PAR_GET0R( 'ANYNUMBER', VALUE, ISTAT )
                     STATUS = ISTAT
                     IF ( STATUS .EQ. PAR__NULL ) THEN
                        CALL ERR_FLUSH( STATUS )
                        VALUE = VALUE_DEFAULT
                        STATUS = 0
                     END IF
                  END IF

               ELSE
                  STATUS = 0
                  VALUE = VALUE_MIN
                  CALL PAR_DEF0R( REQUIRED_OBJECT, VALUE_DEFAULT,
     :                            ISTAT )
                  ISTAT = 0
                  CALL PAR_GET0R( REQUIRED_OBJECT, VALUE, ISTAT )
                  STATUS = ISTAT
                  IF ( STATUS .EQ. PAR__NULL ) THEN
                     CALL ERR_FLUSH( STATUS )
                     VALUE = VALUE_DEFAULT
                     STATUS = 0
                  END IF
                  IF ( VALUE .NE. VALUE_DEFAULT .AND.
     :                 ( .NOT. USED_AS_INDEX )  .AND.
     :                 REQUIRED_OBJECT( :5 ) .EQ. 'TUNE_' .AND.
     :                 STRING .NE. 'SWITCH' ) THEN
                     CALL CHR_RTOC( VALUE, REF_STR1, NCHAR1 )
                     REPORT_STRING = ' ' //
     :               REQUIRED_OBJECT(
     :               :ECH_WORD_LEN( REQUIRED_OBJECT ) ) //
     :               ' is set to non-default value: ' //
     :               REF_STR1( :NCHAR1 ) // '.'
                     CALL ECH_REPORT( 0, REPORT_STRING )
                  END IF
               END IF
               IF ( VALUE .EQ. ECH__BAD_REAL .AND.
     :              STATUS .NE. PAR__ABORT )
     :            STATUS = ECH__BAD_USERPAR
            END IF

            IF ( STATUS .EQ. PAR__ABORT ) THEN
               ISTAT = STATUS
               CALL ERR_FLUSH( STATUS )
               STATUS = ISTAT
               RETURN
            END IF

            IF ( .NOT. ECH_FATAL_ERROR( STATUS ) ) THEN
               IF ( STATUS .EQ. 0 .AND. .NOT. OBJECT_ACTIVE ) THEN
                  ACCESS_COUNT = NEW_ACCESS_COUNT
                  OBJECT_NAME( ACCESS_COUNT ) = REQUIRED_OBJECT
                  OBJECT_TYPE( ACCESS_COUNT ) = TYPE
                  OBJECT_SIZE( ACCESS_COUNT ) = -1
                  OBJECT_ADDRESS( ACCESS_COUNT ) = -1
                  OBJECT_HANDLE( ACCESS_COUNT ) = -1
                  OBJECT_INDEX( ACCESS_COUNT ) =
     :                        ECH_OBJ_IND( REQUIRED_OBJECT )
                  OBJECT_ACTIVE = .TRUE.
                  OBJECT_NUMBER = ACCESS_COUNT
                  IF ( STRING .EQ. 'SWITCH' .OR. USED_AS_INDEX ) THEN
                     CALL ECH_SETUP_OBJECT_REF( REQUIRED_OBJECT,
     :                    DUMMY, DUMMY, DUMMY, ' ', VALUE, BOOLEAN,
     :                    STATUS )
                  END IF

               ELSE IF ( STATUS .EQ. ECH__WORKSPACE_PAR ) THEN
                  STATUS = ECH__IS_ACCESSED

               ELSE IF ( .NOT. FORCE_REPROMPT ) THEN
                  CALL ECH_UPDATE_OBJECT_REF( REQUIRED_OBJECT,
     :                 DUMMY, DUMMY, DUMMY, STRING, VALUE, BOOLEAN,
     :                 STATUS )
               END IF
            END IF

            IF ( DIAGNOSTICS_ACTIVE ) THEN
               IF ( TYPE .EQ. 'CHAR' ) THEN
                  REPORT_STRING = ' PAR Read: ' //
     :                REQUIRED_OBJECT(
     :                :ECH_WORD_LEN( REQUIRED_OBJECT ) ) //
     :                ' type _CHAR is ' //
     :                STRING( :CHR_LEN( STRING ) ) // '.'

               ELSE IF ( TYPE .EQ. 'LOGICAL' ) THEN
                  CALL CHR_LTOC( BOOLEAN, REF_STR1, NCHAR1 )
                  REPORT_STRING = ' PAR Read: ' //
     :                REQUIRED_OBJECT(
     :                :ECH_WORD_LEN( REQUIRED_OBJECT ) ) //
     :                ' type _LOGICAL is ' // REF_STR1( :NCHAR1 ) // '.'

               ELSE IF ( TYPE .EQ. 'INT' ) THEN
                  CALL CHR_RTOC( VALUE, REF_STR1, NCHAR1 )
                  REPORT_STRING = ' PAR Read: ' //
     :                REQUIRED_OBJECT(
     :                :ECH_WORD_LEN( REQUIRED_OBJECT ) ) //
     :                ' type _INTEGER is ' // REF_STR1( :NCHAR1 ) // '.'

               ELSE IF ( TYPE .EQ. 'FLOAT' ) THEN
                  CALL CHR_RTOC( VALUE, REF_STR1, NCHAR1 )
                  REPORT_STRING = ' PAR Read: ' //
     :                REQUIRED_OBJECT(
     :                :ECH_WORD_LEN( REQUIRED_OBJECT ) ) //
     :                ' type _FLOAT is ' // REF_STR1( :NCHAR1 ) // '.'

               ELSE
                  CALL CHR_RTOC( VALUE, REF_STR1, NCHAR1 )
                  CALL CHR_LTOC( BOOLEAN, REF_STR2, NCHAR2 )
                  CALL CHR_ITOC( STATUS, REF_STR3, NCHAR3 )
                  REPORT_STRING = ' PAR read: ' //
     :                 REQUIRED_OBJECT(
     :                 :ECH_WORD_LEN( REQUIRED_OBJECT ) ) //
     :                 ' type ' // TYPE( :ECH_WORD_LEN( TYPE ) ) //
     :                 ' is ' // STRING( :CHR_LEN( STRING ) ) //
     :                 ':' // REF_STR1( :NCHAR1 ) //
     :                 ':' // REF_STR2( :NCHAR2 ) //
     :                 ' status: ' // REF_STR3( :NCHAR3 ) // '.'
               END IF
               CALL ECH_REPORT( RPM_LOG, REPORT_STRING )
            END IF

         ELSE IF ( .NOT. FORCE_REPROMPT ) THEN
            IF ( TYPE .EQ. 'IMAGE' ) THEN
               STRING = 'POIUYTREWQ0987654321'
               STATUS = 0
               CALL PAR_GET0C( REQUIRED_OBJECT, STRING, STATUS )
               IF ( STATUS .EQ. PAR__ABORT ) RETURN
               IF ( STRING .EQ. 'POIUYTREWQ0987654321' )
     :            STATUS = ECH__BAD_USERPAR
            ENDIF
            CALL ECH_UPDATE_OBJECT_REF( REQUIRED_OBJECT, DUMMY,
     :           DUMMY, DUMMY, STRING, VALUE, BOOLEAN, STATUS )

*        Set status to indicate that object is already accessed.
            STATUS = ECH__IS_ACCESSED
         END IF
      END IF

      IF ( STATUS .EQ. ECH__ARRAY_INDEX ) STATUS = 0

      END
