      SUBROUTINE ECH_ACCESS_REDUCTION_FILE(
     :           REQ_OBJ,
     :           OPERATION,
     :           TYPE,
     :           MAPPED_ADDRESS,
     :           IO_STRING,
     :           MODULE_NUMBER,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_ACCESS_REDUCTION_FILE

*  Purpose:
*     Handles interface to all reduction file IO.

*  Invocation:
*      CALL ECH_ACCESS_REDUCTION_FILE(
*     :     REQ_OBJ,
*     :     OPERATION,
*     :     TYPE,
*     :     MAPPED_ADDRESS,
*     :     IO_STRING,
*     :     MODULE_NUMBER,
*     :     STATUS
*     :    )

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants and Variables:
      INCLUDE 'ECH_USE_RDCTN.INC'
      INCLUDE 'ECH_MODULES.INC'
      INCLUDE 'ECH_MAPPING.INC'
      INCLUDE 'ECH_ECHOMOP.INC'
      INCLUDE 'ECH_REPORT.INC'
      INCLUDE 'ECH_CONTEXT.INC'
      INCLUDE 'ECH_USE_DIMEN.INC'

*  Arguments:
      CHARACTER*( * ) REQ_OBJ
      CHARACTER*( * ) OPERATION
      CHARACTER*( * ) TYPE
      INTEGER MAPPED_ADDRESS
      CHARACTER*( * ) IO_STRING
      INTEGER MODULE_NUMBER
      INTEGER STATUS

*  Local Variables:
      REAL VALUE
      REAL RVALUE

      INTEGER CLN_DIMENSIONS( MAX_DIMENSIONS )
      INTEGER DUMDIM( MAX_DIMENSIONS )
      INTEGER I
      INTEGER II
      INTEGER I3
      INTEGER I4
      INTEGER I5
      INTEGER STAT2
      INTEGER ILEN
      INTEGER AUNIT
      INTEGER TYPE_CODE
      INTEGER IPLEN
      INTEGER IREG
      INTEGER ACTUAL_DIM_SIZ
      INTEGER ACTUAL_DIM
      INTEGER NEW_ACCESS_COUNT
      INTEGER LAST_BAD_VALUE
      INTEGER NEW_OBJECT_SIZE
      INTEGER NEW_HANDLE
      INTEGER OBJ_NUM
      INTEGER NEW_ADDRESS
      INTEGER DUMMY
      INTEGER CLONE_ADDRESS
      INTEGER DEFAULT_INDEX
      INTEGER USED_DIM
      INTEGER CLN_NUM_DIM
      INTEGER STATIC_INDICES
      INTEGER DIM_USE
      INTEGER NCHAR1
      INTEGER NCHAR2
      INTEGER NCHAR3

      LOGICAL INC_ACCESS
      LOGICAL OBJECT_ACTIVE
      LOGICAL USEOLD
      LOGICAL CLONEABLE
      LOGICAL REGISTERED

      CHARACTER*255 FULL_OBJECT_PATH
      CHARACTER*255 CLONE_OBJECT_PATH
      CHARACTER*255 CHAR_NAME
      CHARACTER*255 TEMP_STRING
      CHARACTER*132 WORK_STRING
      CHARACTER*80 LAST_BAD_INDEX
      CHARACTER*80 PATH_NAME
      CHARACTER*80 LAST_CLONE_USED
      CHARACTER*80 RDCTN_DEF_FILENAME
      CHARACTER*16 REF_STR1
      CHARACTER*16 REF_STR2
      CHARACTER*16 REF_STR3
      CHARACTER*9 FILETYPE
      CHARACTER*6 NEW_OBJECT_TYPE
      CHARACTER*4 CHAR_DIMEN

*  External Functions:
      INTEGER CHR_LEN
      INTEGER ECH_WORD_LEN
      LOGICAL ECH_FATAL_ERROR
      INTEGER ECH_MODULE_NAME_INDEX
      INTEGER ECH_OBJ_IND
      LOGICAL MODULE_UPDATES

*  Data statements:
      DATA CHAR_DIMEN / '[64]' /
*.

*  Zero-out INTEGERs.
      DO I = 1, MAX_DIMENSIONS
         CLN_DIMENSIONS( I ) = 0
         DUMDIM( I ) = 0
      END DO
      II = 0
      I3 = 0
      I4 = 0
      I5 = 0
      STAT2 = 0
      ILEN = 0
      AUNIT = 0
      TYPE_CODE = 0
      IPLEN = 0
      IREG = 0
      ACTUAL_DIM_SIZ = 0
      ACTUAL_DIM = 0
      NEW_ACCESS_COUNT = 0
      LAST_BAD_VALUE = 0
      NEW_OBJECT_SIZE = 0
      NEW_HANDLE = 0
      OBJ_NUM = 0
      NEW_ADDRESS = 0
      DUMMY = 0
      CLONE_ADDRESS = 0
      DEFAULT_INDEX = 0
      USED_DIM = 0
      CLN_NUM_DIM = 0
      DIM_USE = 0

*  Zero-out LOGICALs.
      OBJECT_ACTIVE = .FALSE.
      USEOLD = .FALSE.
      CLONEABLE = .FALSE.
      REGISTERED = .FALSE.

      STATUS = 0
      STATIC_INDICES = 0
      MAPPED_ADDRESS = 0
      INC_ACCESS = .FALSE.

*  Determine type of host file for required object.
      IF ( REQ_OBJ( :7 ) .EQ. 'ECHARC_' ) THEN
         FILETYPE = 'ECH_ECHAR'
         RDCTN_FILE_NAME = RDCTN_FILE_ECHARC

      ELSE IF ( REQ_OBJ( :7 ) .EQ. 'EFTRDB_' ) THEN
         FILETYPE = 'ECH_FTRDB'
         RDCTN_FILE_NAME = RDCTN_FILE_FTRDB

      ELSE IF ( REQ_OBJ( :7 ) .EQ. 'RESULT_' ) THEN
         FILETYPE = 'ECH_RDUCD'
         RDCTN_FILE_NAME = RDCTN_FILE_RDUCD

      ELSE
         FILETYPE = 'ECH_RDCTN'
         RDCTN_FILE_NAME = RDCTN_FILE_MAIN
      END IF

*  Close file used as source for clone if finished with.
      IF ( ( USR_TUNE_CLONE .NE. 'NULL' ) .AND. CLONE_FILE_OPEN .AND.
     :     ( USR_TUNE_CLONE .NE. LAST_CLONE_USED ) ) THEN
         CALL ECH_ACCESS_OBJECT( 'CLONE', 'UNMAP', 'STRUCTURE', 0,
     :        0, 0, DUMDIM, 1, 0, ' ', STATUS )
         CLONE_FILE_OPEN = .FALSE.
      END IF

*  Open file as source for clone.
      IF ( .NOT. CLONE_FILE_OPEN .AND.
     :     ( USR_TUNE_CLONE .NE. 'NULL' ) ) THEN
         CALL ECH_ACCESS_OBJECT( 'CLONE', 'OPEN', 'STRUCTURE', 0, 0, 0,
     :        DUMDIM, 1, 0, USR_TUNE_CLONE, STATUS )
         IF ( STATUS .NE. 0 ) THEN
            STATUS = ECH__NO_CLONE
            CALL ECH_REPORT( 0,
     :           ' Cannot open source reduction file to copy from.' )
            CALL ECH_SET_CONTEXT( 'PROBLEM', 'No cloneable object' )
            GO TO 999

         ELSE
            LAST_CLONE_USED = USR_TUNE_CLONE
            CALL ECH_ACCESS_OBJECT( 'CLONE', 'READ-NAME', 'STRUCTURE',
     :           0, 0, 0, DUMDIM, 1, 0,
     :           RDCTN_FILE_CLONE, STATUS )
            CLONE_FILE_OPEN = .TRUE.
         END IF

*  Close clone source file, no longer needed.
      ELSE IF ( CLONE_FILE_OPEN .AND.
     :          ( USR_TUNE_CLONE .EQ. 'NULL' ) ) THEN
         CALL ECH_ACCESS_OBJECT( 'CLONE', 'UNMAP', 'STRUCTURE', 0, 0,
     :        0, DUMDIM, 1, 0, ' ', STATUS )
         CLONE_FILE_OPEN = .FALSE.
      END IF

*  Close a result file.
      IF ( ( REQ_OBJ .EQ. 'ECH_RDUCD' ) .AND.
     :     ( OPERATION .EQ. 'CLOSE' ) .AND. RESULT_FILE_OPEN ) THEN
         RESULT_FILE_OPEN = .FALSE.
         DO I = 1, ACCESS_COUNT
            IF ( OBJECT_NAME( I )( :7 ) .EQ. 'RESULT_' .OR.
     :           OBJECT_NAME( I )( :9 ) .EQ. 'ECH_RDUCD' ) THEN
               IF ( OBJECT_TYPE( I ) .NE. 'CHAR' ) THEN
                  CALL ECH_ACCESS_OBJECT ( OBJECT_NAME( I ),
     :                 'UNMAP', OBJECT_TYPE( I ), OBJECT_SIZE( I ),
     :                 OBJECT_ADDRESS( I ), OBJECT_HANDLE( I ),
     :                 DUMDIM, 1, 0, ' ', STATUS )
                  OBJECT_ADDRESS( I ) = 0
                  OBJECT_NAME( I ) = ' '
                  OBJECT_TYPE( I ) = '-a free entry-'
                  OBJECT_SIZE( I ) = 0
                  OBJECT_STATICS( I ) = 0
                  OBJECT_INDEX( I ) = 0
                  OBJECT_SEND( I ) = 0
               END IF
            END IF
         END DO
         CALL ECH_ACCESS_OBJECT( 'ECH_RDUCD', 'UNMAP', 'STRUCTURE',
     :        0, 0, 0, DUMDIM, 1, 0, ' ', STATUS )
         GO TO 999
      END IF

*  Open a data file.
      IF ( ( .NOT. RDCTN_FILE_OPEN .AND.
     :     ( FILETYPE .EQ. 'ECH_RDCTN' ) ) .OR. (
     :     .NOT. FTRDB_FILE_OPEN .AND.
     :     ( FILETYPE .EQ. 'ECH_FTRDB' ) ) .OR. (
     :     .NOT. RESULT_FILE_OPEN .AND.
     :     ( FILETYPE .EQ. 'ECH_RDUCD' ) ) .OR. (
     :     .NOT. ECHARC_FILE_OPEN .AND.
     :     ( FILETYPE .EQ. 'ECH_ECHAR' ) ) ) THEN
         RDCTN_DEF_FILENAME = FILETYPE//'.DEF'

*     Open a line database file in read-only mode as we do not
*     require write access.  Unless we are creating the file...
         IF ( FILETYPE .EQ. 'ECH_FTRDB' ) THEN
            IF ( CONTEXT_MODE .NE. CTX_FTRDB_CREATOR ) THEN
               CALL ECH_ACCESS_OBJECT( FILETYPE, 'OPEN-READONLY',
     :              'STRUCTURE', 0, 0, 0, DUMDIM, 1, 0,
     :              RDCTN_FILE_NAME, STATUS )

            ELSE
               CALL ECH_INIT_OBJ_PATHS( RDCTN_DEF_FILENAME, STATUS )
               CALL ECH_CREATE_REDUCTION_FILE( FILETYPE, STATUS )
               CALL ECH_ACCESS_OBJECT( FILETYPE, 'OPEN',
     :              'STRUCTURE', 0, 0, 0, DUMDIM, 1, 0,
     :              RDCTN_FILE_NAME, STATUS )
            END IF

*     Open file - all cases except read-only access to a line database.
         ELSE
            STATUS = ECH__SUPRESS_ERRORS
            CALL ECH_ACCESS_OBJECT( FILETYPE, 'OPEN', 'STRUCTURE',
     :           0, 0, 0, DUMDIM, 1, 0, RDCTN_FILE_NAME, STATUS )
         END IF

*     "cludge" by MJC 17-APR-1996 - so ech_plotter gets title.
         IF ( STATUS .EQ. 0 .AND. FILETYPE .EQ. 'ECH_RDCTN' ) THEN
            GRAPH_TITLE = RDCTN_FILE_NAME
         END IF
         IF ( ECH_FATAL_ERROR( STATUS ) ) THEN
            GO TO 999
         END IF

         IF ( STATUS .NE. 0 ) THEN
            IF ( FILETYPE .EQ. 'ECH_FTRDB' .AND.
     :           CONTEXT_MODE .EQ. CTX_FTRDB_CREATOR ) THEN
               WORK_STRING = 'Cannot create ' // FILETYPE
               CALL ECH_SET_CONTEXT( 'PROBLEM', WORK_STRING )
               STATUS = ECH__CANT_CREATE

            ELSE IF ( MODULE_UPDATES(
     :                ECH_OBJ_IND( FILETYPE ),
     :                MODULE_NUMBER ) ) THEN
               CALL ECH_INIT_OBJ_PATHS( RDCTN_DEF_FILENAME, STATUS )
               CALL ECH_CREATE_REDUCTION_FILE( FILETYPE, STATUS )
               IF ( STATUS .EQ. 0 ) THEN
                  IF ( FILETYPE .EQ. 'ECH_RDCTN' ) THEN
                     RDCTN_FILE_OPEN = .TRUE.

*           "cludge" by MJC 17-APR-1996 - so ech_plotter gets title.
                     GRAPH_TITLE = RDCTN_FILE_NAME

                  ELSE IF ( FILETYPE .EQ. 'ECH_ECHAR' ) THEN
                     ECHARC_FILE_OPEN = .TRUE.

                  ELSE IF ( FILETYPE .EQ. 'ECH_FTRDB' ) THEN
                     FTRDB_FILE_OPEN = .TRUE.

                  ELSE IF ( FILETYPE .EQ. 'ECH_RDUCD' ) THEN
                     RESULT_FILE_OPEN = .TRUE.
                  END IF

               ELSE
                  CALL ECH_SET_CONTEXT( 'PROBLEM',
     :                 'Cannot create here' )
                  STATUS = ECH__NOCR_ACCESS
               END IF

            ELSE
               WORK_STRING = 'Cannot create ' // FILETYPE
               CALL ECH_SET_CONTEXT( 'PROBLEM', WORK_STRING )
               STATUS = ECH__CANT_CREATE
            END IF

         ELSE
            IF ( FILETYPE .EQ. 'ECH_RDCTN' ) THEN
               CALL ECH_INIT_OBJ_PATHS( RDCTN_DEF_FILENAME, STATUS )
               RDCTN_FILE_OPEN = .TRUE.

            ELSE IF ( FILETYPE .EQ. 'ECH_ECHAR' ) THEN
               CALL ECH_INIT_OBJ_PATHS( RDCTN_DEF_FILENAME, STATUS )
               ECHARC_FILE_OPEN = .TRUE.

            ELSE IF ( FILETYPE .EQ. 'ECH_FTRDB' ) THEN
               CALL ECH_INIT_OBJ_PATHS( RDCTN_DEF_FILENAME, STATUS )
               FTRDB_FILE_OPEN = .TRUE.

            ELSE IF ( FILETYPE .EQ. 'ECH_RDUCD' ) THEN
               CALL ECH_INIT_OBJ_PATHS( RDCTN_DEF_FILENAME, STATUS )
               RESULT_FILE_OPEN = .TRUE.
            END IF
         END IF

         IF ( FILETYPE .EQ. 'ECH_RDCTN' ) THEN
            CALL ECH_ACCESS_OBJECT( 'ECH_RDCTN', 'READ-NAME',
     :           'STRUCTURE', 0, 0, 0, DUMDIM, 1, 0, RDCTN_FILE_MAIN,
     :           STATUS )
            USEOLD = .TRUE.

         ELSE IF ( FILETYPE .EQ. 'ECH_ECHAR' ) THEN
            CALL ECH_ACCESS_OBJECT( 'ECH_ECHAR', 'READ-NAME',
     :           'STRUCTURE', 0, 0, 0, DUMDIM, 1, 0, RDCTN_FILE_ECHARC,
     :           STATUS )

         ELSE IF ( FILETYPE .EQ. 'ECH_FTRDB' ) THEN
            CALL ECH_ACCESS_OBJECT( 'ECH_FTRDB', 'READ-NAME',
     :           'STRUCTURE', 0, 0, 0, DUMDIM, 1, 0, RDCTN_FILE_FTRDB,
     :           STATUS )

         ELSE IF ( FILETYPE .EQ. 'ECH_RDUCD' ) THEN
            CALL ECH_ACCESS_OBJECT( 'ECH_RDUCD', 'READ-NAME',
     :           'STRUCTURE', DUMDIM, 1, 0, 0, 1, 0, RDCTN_FILE_RDUCD,
     :            STATUS )
         END IF
      END IF

      IF ( STATUS .NE. 0 ) THEN
         GO TO 999
      END IF

      CALL ECH_GET_OBJECT_PATH( REQ_OBJ, FULL_OBJECT_PATH,
     :     PATH_NAME, STATIC_INDICES, STATUS )

*  Check if object has already got an access active.
      NEW_ACCESS_COUNT = 0
      OBJECT_ACTIVE = .FALSE.
      IPLEN = ECH_WORD_LEN( PATH_NAME )
      I = 1
      DO WHILE ( .NOT. OBJECT_ACTIVE .AND. I .LE. ACCESS_COUNT )
         IF ( NEW_ACCESS_COUNT .EQ. 0 .AND.
     :        OBJECT_TYPE ( I ) .NE. 'IMAGE' .AND.
     :        OBJECT_ADDRESS( I ) .EQ. 0 ) THEN
            NEW_ACCESS_COUNT = I

         ELSE IF ( FULL_OBJECT_PATH .EQ. OBJECT_NAME( I ) )  THEN
            IF ( OBJECT_STATICS( I ) .EQ. STATIC_INDICES ) THEN
               OBJECT_ACTIVE = .TRUE.
               OBJ_NUM = I
               NEW_OBJECT_SIZE = OBJECT_SIZE( I )
               MAPPED_ADDRESS = OBJECT_ADDRESS( I )
               IF ( STATIC_INDICES .GT. 0 .AND.
     :              ONCE_PER_ORDER( MODULE_NUMBER ) ) THEN
                  IF ( DIMEN_INDEX( STATIC_INDICES ) .EQ.
     :                 'IDX_NUM_ORDERS' ) THEN
                     CALL ECH_TYPEINFO( OBJECT_TYPE( I ),
     :                    TYPE_CODE, AUNIT )
                     CALL ECH_ACCESS_OBJECT( FULL_OBJECT_PATH,
     :                    'READ-SIZE', ' ', 0, 0, 0, DIMENSIONS,
     :                    MAX_DIMENSIONS, NUM_DIM, ' ', STATUS )
                     IF ( OPERATION .NE. 'WRITE' ) THEN
                        IF ( MAPPED_ADDRESS .NE. 0 ) THEN
                           MAPPED_ADDRESS =
     :                       MAPPED_ADDRESS + AUNIT * (
     :                       ( DIMEN_VALUE( STATIC_INDICES ) - 1 ) *
     :                       NEW_OBJECT_SIZE /
     :                       DIMENSIONS( STATIC_INDICES ) )
                        END IF
                     END IF
                  END IF
               END IF
               IF ( OBJECT_TYPE( I ) .EQ. 'CHAR' .AND.
     :              OPERATION .NE. 'WRITE' .AND.
     :              FULL_OBJECT_PATH( :9 ) .NE. 'ECH_RDUCD' ) THEN
                  CALL ECH_ACCESS_OBJECT( FULL_OBJECT_PATH,
     :               'READ-SIZE', ' ', 0, 0, 0, DIMENSIONS,
     :               MAX_DIMENSIONS, NUM_DIM, ' ', STATUS )
                  CALL ECH_ACCESS_OBJECT( FULL_OBJECT_PATH,
     :               'READ', 'CHAR', DIMENSIONS( 1 ), 0, 0,
     :               DUMDIM, 1, 0, TEMP_STRING, STATUS )
                  IO_STRING = TEMP_STRING
                  MAPPED_ADDRESS = 1
                  IF ( TEMP_STRING( :1 ) .EQ. '?' ) THEN
                     STATUS = ECH__OBJ_UNINIT
                  END IF
               END IF

            ELSE IF ( OBJECT_NAME( I )( :IPLEN ) .EQ.
     :              PATH_NAME( :IPLEN ) ) THEN
               CALL ECH_ACCESS_OBJECT( OBJECT_NAME( I ), 'UNMAP',
     :            OBJECT_TYPE( I ), OBJECT_SIZE( I ),
     :            OBJECT_ADDRESS( I ), OBJECT_HANDLE( I ),
     :            DUMDIM, 1, 0, ' ', STATUS )
               IF ( ECH_FATAL_ERROR( STATUS ) ) THEN
                  GO TO 999
               END IF
               OBJECT_ADDRESS( I ) = 0
               OBJECT_NAME( I ) = ' '
               OBJECT_TYPE( I ) = '-a free entry-'
               OBJECT_SIZE( I ) = 0
               OBJECT_STATICS( I ) = 0
               OBJECT_INDEX( I ) = 0
               OBJECT_SEND( I ) = 0
               IF ( NEW_ACCESS_COUNT .EQ. 0 ) THEN
                  NEW_ACCESS_COUNT = I
               END IF
            END IF

         ELSE IF ( OBJECT_NAME( I )( :IPLEN ) .EQ.
     :             PATH_NAME( :IPLEN ) ) THEN
            CALL ECH_ACCESS_OBJECT( OBJECT_NAME( I ), 'UNMAP',
     :           OBJECT_TYPE( I ), OBJECT_SIZE( I ),
     :           OBJECT_ADDRESS( I ), OBJECT_HANDLE( I ), DUMDIM,
     :           1, 0, ' ', STATUS )
            IF ( ECH_FATAL_ERROR( STATUS ) ) THEN
               GO TO 999
            END IF
            OBJECT_ADDRESS( I ) = 0
            OBJECT_NAME( I ) = ' '
            OBJECT_TYPE( I ) = '-a free entry-'
            OBJECT_SIZE( I ) = 0
            OBJECT_STATICS( I ) = 0
            OBJECT_INDEX( I ) = 0
            OBJECT_SEND( I ) = 0
            IF ( NEW_ACCESS_COUNT .EQ. 0 ) THEN
               NEW_ACCESS_COUNT = I
            END IF
         END IF
         I = I + 1
      END DO

*  Check object dimensionality.
      IF ( NEW_ACCESS_COUNT .EQ. 0 ) THEN
         INC_ACCESS = .TRUE.
      END IF
 2    CONTINUE
      CALL ECH_ACCESS_OBJECT( FULL_OBJECT_PATH, 'READ-SIZE', ' ', 0,
     :     0, 0, DIMENSIONS, MAX_DIMENSIONS, NUM_DIM, ' ', STATUS )
      ACTUAL_DIM = NUM_DIM

      IF ( STATUS .EQ. 0 ) THEN
         NEW_OBJECT_SIZE = 1
         IF ( TYPE .EQ. 'CHAR' ) THEN
            NEW_OBJECT_SIZE = DIMENSIONS( 1 )
         END IF

*     Only need to add extra dimensionality when a static index
*     is being used EG. ARRAY[ 1, 1, order_number ], has a
*     size defined by the first two (static) dimensions.
*     Whereas ARRAY[ order_number ] refers to a single element.
         IF ( STATIC_INDICES .GT. 0 ) THEN
            NUM_DIM = STATIC_INDICES
            DO I = 1, NUM_DIM
               DIM_USE = DIMENSIONS( I )
               IF ( DIMEN_INDEX( I )( :4 ) .EQ. 'IDX_' ) THEN
                  CALL ECH_UPDATE_OBJECT_REF( DIMEN_INDEX( I )( 5: ),
     :                 DUMMY, DUMMY, DUMMY, ' ', RVALUE, .FALSE.,
     :                 STATUS )
                  USED_DIM = INT( RVALUE )
                  ACTUAL_DIM_SIZ = USED_DIM

               ELSE IF ( .NOT. IN_RDCTN_FILE(
     :                   ECH_OBJ_IND( DIMEN_INDEX( I ) ) ) )
     :                   THEN
                  DEFAULT_INDEX = DEFAULTS_INDICES(
     :                ECH_OBJ_IND( DIMEN_INDEX( I ) ) )
                  STATUS = ECH__ARRAY_INDEX
                  CALL ECH_GET_PARAMETER( DIMEN_INDEX( I ), 'INT',
     :                 RVALUE, .FALSE., ' ', DEFAULT_INDEX, STATUS )
                  USED_DIM = INT( RVALUE )

               ELSE
                  CALL ECH_UPDATE_OBJECT_REF( DIMEN_INDEX( I ),
     :                 DUMMY, DUMMY, DUMMY, ' ', RVALUE, .FALSE.,
     :                 STATUS )
                  CALL ECH_TRANSFER_INT( %VAL( DUMMY ), USED_DIM )
               END IF

*           This bit appears to deal with accesses to objects with
*           dimensions different to those "requested".
*           There isn't any point in displaying vast swathes of
*           redimension info, unless the user of the program is
*           going to be prompted to confirm re-dims - so I've altered
*           the messages here accordingly.  MJC.
               IF ( ( ( DIMEN_INDEX( I )( :4 ) .EQ. 'IDX_' ) .AND.
     :                  DIMENSIONS( I ) .NE. USED_DIM ) .OR.
     :              ( DIMEN_INDEX( I )( :4 ) .NE. 'IDX_' .AND.
     :              ( ( DIMEN_VALUE( I ) .GT. 1 .AND.
     :                  DIMEN_VALUE( I ) .NE. DIMENSIONS( I ) ) .OR.
     :              ( DIMEN_VALUE( I ) .EQ. 1 .AND.
     :                DIMENSIONS( I ) .NE. USED_DIM ) ) ) ) THEN
                  IF ( DIMEN_VALUE( I ) .EQ. 1 ) THEN
                     DIMEN_VALUE( I ) = USED_DIM
                  END IF

                  IF ( DIMEN_INDEX( I )( :5 ) .NE. 'TUNE_' ) THEN
                     USEOLD = .FALSE.

                  ELSE
                     CALL ECH_SET_CONTEXT(
     :                    'PROBLEM', 'Dimension conflict' )

                     REPORT_STRING =
     :                  ' Reduction object ' // REQ_OBJ(
     :                  : ECH_WORD_LEN( REQ_OBJ ) ) //
     :                  ' already created.'
                     CALL ECH_REPORT( 0, REPORT_STRING )

                     REPORT_STRING =
     :                  ' Specified object dimension in conflict' //
     :                  ' using ' // DIMEN_INDEX( I )(
     :                  : ECH_WORD_LEN( DIMEN_INDEX( I ) ) ) // '.'
                     CALL ECH_REPORT( 0, REPORT_STRING )

                     CALL CHR_ITOC( DIMENSIONS( I ), REF_STR1,
     :                    NCHAR1 )
                     CALL CHR_ITOC( DIMEN_VALUE( I ), REF_STR2,
     :                    NCHAR2 )
                     REPORT_STRING = ' Old value=' //
     :                  REF_STR1( :NCHAR1 ) //
     :                  ' requested value=' //
     :                  REF_STR2( :NCHAR2 ) // '.'
                     CALL ECH_REPORT( 0, REPORT_STRING )

                     IF ( LAST_BAD_INDEX .NE. DIMEN_INDEX( I ) .OR.
     :                    LAST_BAD_VALUE .NE. DIMENSIONS( I ) ) THEN
                        CALL ECH_GET_PARAMETER(
     :                       'INSTANT-PROMPT=Use OLD value ',
     :                       'LOGICAL', 0., USEOLD, ' ', 0, STATUS )
                        IF ( STATUS .NE. 0 ) THEN
                           GO TO 999
                        END IF
                        LAST_BAD_INDEX = DIMEN_INDEX( I )
                        LAST_BAD_VALUE = DIMENSIONS( I )
                     END IF
                  END IF
                  IF ( USEOLD ) THEN
                     VALUE = FLOAT( DIMENSIONS( I ) )
                     CALL ECH_SET_PARAMETER( DIMEN_INDEX( I ),
     :                    'INT', VALUE, 0, ' ', STATUS )
                     DIM_USE = DIMENSIONS( I )

                  ELSE
                     WORK_STRING = ' Deleting and' //
     :                    ' re-creating newly dimensioned' //
     :                    ' object: ' // PATH_NAME( :ECH_WORD_LEN(
     :                    PATH_NAME ) ) // '.'
                     CALL ECH_REPORT( 0, WORK_STRING )
                     IF ( OBJECT_ACTIVE )
     :                  CALL ECH_ACCESS_OBJECT(
     :                       OBJECT_NAME( OBJ_NUM ),
     :                       'UNMAP', OBJECT_TYPE( OBJ_NUM ),
     :                       OBJECT_SIZE( OBJ_NUM ),
     :                       OBJECT_ADDRESS( OBJ_NUM ),
     :                       OBJECT_HANDLE( OBJ_NUM ),
     :                       DUMDIM, 1, 0, ' ', STATUS )
                     CALL ECH_ACCESS_OBJECT( PATH_NAME, 'DELETE',
     :                    ' ', 0, 0, 0, DUMDIM, 1, 0, ' ', STATUS )
                     IF ( STATUS .NE. 0 ) THEN
                        GO TO 999
                     END IF
                     IF ( OBJECT_ACTIVE ) THEN
                        INC_ACCESS = .FALSE.
                        NEW_ACCESS_COUNT = OBJ_NUM
                     END IF
                     OBJECT_ACTIVE = .FALSE.
                     GO TO 2
                  END IF

               ELSE IF ( DIMEN_VALUE( I ) .EQ. 1 .AND.
     :                   ACTUAL_DIM .EQ. 1 .AND. I .EQ. 1 ) THEN
                  REGISTERED = .FALSE.
                  DO IREG = 1, NUM_REG_VARS
                    IF ( DIMEN_INDEX( I ) .EQ.
     :                   OBJ_DIMEN_VARS( IREG ) ) REGISTERED = .TRUE.
                  END DO
                  IF ( .NOT. REGISTERED ) DIM_USE = 1
               END IF
               IF ( DIMEN_INDEX( I ) .NE. 'IDX_NUM_ORDERS' ) THEN
                  NEW_OBJECT_SIZE = NEW_OBJECT_SIZE * DIM_USE
               END IF
              END DO
            END IF
            IF ( STATIC_INDICES .GT. 0 .AND.
     :           ONCE_PER_ORDER( MODULE_NUMBER ) ) THEN
               IF ( DIMEN_INDEX( STATIC_INDICES ) .EQ.
     :              'IDX_NUM_ORDERS'  ) THEN
                  NEW_OBJECT_SIZE = NEW_OBJECT_SIZE *
     :                  DIMENSIONS( STATIC_INDICES )
               END IF
            END IF

         ELSE
            IF ( .NOT. MODULE_UPDATES(
     :           ECH_OBJ_IND( REQ_OBJ ),
     :           ECH_MODULE_NAME_INDEX( CURRENT_MODULE ) ) ) THEN
               IF ( CLONE_FILE_OPEN ) GO TO 999
               report_string = ' Required reduction object ' //
     :               REQ_OBJ( :ECH_WORD_LEN( REQ_OBJ ) ) //
     :               ' not found.'
               CALL ECH_REPORT( 0, REPORT_STRING )
               I = ECH_OBJ_IND( REQ_OBJ )
               DO ii = 1, max_modules
                  IF ( module_updates( i, ii ) ) THEN
                     DO i3 = 1, num_options
                        IF ( option_module( i3 ) .EQ.
     :                       module_name( ii ) ) THEN
                          report_string =
     :               ' Can be created by ' // option_text( i3 )
                          CALL ECH_REPORT( 0, REPORT_STRING )
                          CALL ECH_SET_CONTEXT( 'REWIND',
     :                         option_module( i3 ) )
                          status = ECH__NEED_RDCOBJ
                          GO TO 999
                        END IF
                     END DO
                     DO i3 = 1, num_suboptions
                        IF ( submenu_options ( i3 ) .EQ.
     :                       module_name(ii) ) THEN
                           i4 = i3
                           DO WHILE (submenu_options
     :                           (i4)( :8).NE. 'Submenu_' )
                              i4 = i4-1
                           END DO
                           DO i5 = 1, num_options
                            IF ( option_module ( i5 ) .EQ.
     :                       submenu_options(i4)   )  THEN
                              report_string =
     :                               ' Can be created by '//
     :                               option_text( i5 )
                              CALL ECH_REPORT( 0,
     :                             report_string )
                              CALL ECH_SET_CONTEXT( 'REWIND',
     :                             option_module( i5 ) )
                              status = ECH__NEED_RDCOBJ
                              GO TO 999
                            END IF
                           END DO
                        END IF
                     END DO
                  END IF
               END DO
               GO TO 999

            ELSE
               STATUS = ECH__CRE_OBJECT
               CALL ECH_GET_OBJECT_PATH( REQ_OBJ, FULL_OBJECT_PATH,
     :              PATH_NAME, STATIC_INDICES, STATUS )
               NEW_OBJECT_SIZE = 1
               IF ( STATIC_INDICES .GT. 0 ) THEN
                  NUM_DIM = STATIC_INDICES
                  DO I = 1, NUM_DIM
                     NEW_OBJECT_SIZE = NEW_OBJECT_SIZE *
     :                                 DIMEN_VALUE( I )
                  END DO
               END IF
               IF ( TYPE .EQ. 'CHAR' ) THEN
                  CHAR_NAME = FULL_OBJECT_PATH(
     :                  :ECH_WORD_LEN( FULL_OBJECT_PATH ) ) //
     :                  CHAR_DIMEN
                  CALL ECH_ACCESS_OBJECT( CHAR_NAME, 'CREATE', TYPE,
     :                 0, 0, 0, DUMDIM, 1, 0, ' ', STATUS )

               ELSE
                  CALL ECH_ACCESS_OBJECT( FULL_OBJECT_PATH, 'CREATE',
     :                 TYPE, 0, 0, 0, DUMDIM, 1, 0, ' ', STATUS )
               END IF
               IF ( DIAGNOSTICS_ACTIVE ) THEN
                  ILEN = ECH_WORD_LEN( FULL_OBJECT_PATH )
                  CALL CHR_ITOC( STATUS, REF_STR1, NCHAR1 )
                  REPORT_STRING = ' RDC Access: Object Create of ' //
     :                  FULL_OBJECT_PATH( :ILEN ) //
     :                  ' status: ' // REF_STR1( :NCHAR1 ) // '.'
                  CALL ECH_REPORT( RPM_LOG, REPORT_STRING )
               END IF
               IF ( STATUS .EQ. 0 ) THEN
                  CALL ECH_GET_OBJECT_PATH( REQ_OBJ,
     :                 full_object_path, path_name, static_indices,
     :                 status )
                  CALL ECH_ACCESS_OBJECT( FULL_OBJECT_PATH,
     :                 'READ-SIZE', ' ', 0, 0, 0, DIMENSIONS,
     :                 MAX_DIMENSIONS, NUM_DIM, ' ', STATUS )
                  ACTUAL_DIM = NUM_DIM
                  NEW_OBJECT_SIZE = 1
                  IF ( STATIC_INDICES .GT. 0 ) THEN
                     DO I = 1, NUM_DIM
                        NEW_OBJECT_SIZE = NEW_OBJECT_SIZE *
     :                                    DIMENSIONS( I )
                     END DO
                  END IF

                  CALL ECH_ACCESS_OBJECT( FULL_OBJECT_PATH,
     :                 'READ-TYPE', NEW_OBJECT_TYPE, 0, 0, 0,
     :                 DUMDIM, 1, 0, ' ', STATUS )
                  IF ( NEW_OBJECT_TYPE .NE. 'CHAR' ) THEN
                     CALL ECH_ACCESS_OBJECT( FULL_OBJECT_PATH,
     :                    'MAP', NEW_OBJECT_TYPE, NEW_OBJECT_SIZE,
     :                    NEW_ADDRESS, NEW_HANDLE, DUMDIM, 1, 0,
     :                    ' ', STATUS )
                     IF ( STATUS .EQ. 0 ) THEN
                        CALL ECH_ACCESS_OBJECT( FULL_OBJECT_PATH,
     :                       'ZERO', NEW_OBJECT_TYPE,
     :                       NEW_OBJECT_SIZE, NEW_ADDRESS,
     :                       NEW_HANDLE, DUMDIM, 1, 0, ' ', STATUS )
                        CALL ECH_ACCESS_OBJECT( FULL_OBJECT_PATH,
     :                       'UNMAP', NEW_OBJECT_TYPE,
     :                       NEW_OBJECT_SIZE, NEW_ADDRESS,
     :                       NEW_HANDLE, DUMDIM, 1, 0, ' ', STATUS )
                     END IF
                  END IF

               ELSE
                  GO TO 999
               END IF
            END IF
         END IF

      IF ( CLONE_FILE_OPEN ) THEN
         IF ( MODULE_UPDATES( ECH_OBJ_IND( REQ_OBJ ),
     :        ECH_MODULE_NAME_INDEX( CURRENT_MODULE ) ) ) THEN
            CLONE_OBJECT_PATH = 'CLONE.' // FULL_OBJECT_PATH( 11: )
            CALL ECH_ACCESS_OBJECT( CLONE_OBJECT_PATH, 'READ-SIZE', ' ',
     :           0, 0, 0, CLN_DIMENSIONS, MAX_DIMENSIONS, CLN_NUM_DIM,
     :           ' ', STAT2 )
            CLONEABLE = .FALSE.
            IF ( STAT2 .EQ. 0 .AND. ACTUAL_DIM .EQ. CLN_NUM_DIM ) THEN
               CLONEABLE = .TRUE.
               DO I = 1, NUM_DIM
                  IF ( DIMENSIONS( I ) .NE. CLN_DIMENSIONS( I ) )
     :               CLONEABLE = .FALSE.
               END DO
            END IF
            IF ( .NOT. CLONEABLE ) THEN
               WORK_STRING = ' Unable to copy '//
     :              FULL_OBJECT_PATH(11:
     :              ECH_WORD_LEN( FULL_OBJECT_PATH ) )//
     :              ' from file ' // RDCTN_FILE_CLONE
               CALL ECH_REPORT( 0, WORK_STRING )
               CALL ECH_SET_CONTEXT( 'PROBLEM', 'No cloneable object')
               STATUS = ECH__NO_CLONE
               GO TO 999
            END IF
         END IF
      END IF

      IF ( .NOT. OBJECT_ACTIVE ) THEN
         CALL ECH_TYPEINFO( TYPE, TYPE_CODE, AUNIT )
         CALL ECH_ACCESS_OBJECT( FULL_OBJECT_PATH, 'READ-TYPE',
     :        NEW_OBJECT_TYPE, 0, 0, 0, DUMDIM, 1, 0, ' ', STATUS )
         IF ( NEW_OBJECT_TYPE .EQ. 'CHAR' .AND. TYPE .EQ. 'CHAR') THEN
            CALL ECH_ACCESS_OBJECT( FULL_OBJECT_PATH, 'READ', 'CHAR',
     :           NEW_OBJECT_SIZE, 0, 0, DUMDIM, 1, 0, TEMP_STRING,
     :           STATUS )
            IO_STRING = TEMP_STRING
            NEW_ADDRESS = 1
            MAPPED_ADDRESS = 1
            IF ( TEMP_STRING( :1 ) .EQ. '?' ) STATUS = ECH__OBJ_UNINIT

         ELSE
            IF ( FILETYPE .EQ. 'ECH_FTRDB' .AND.
     :           CONTEXT_MODE .NE. CTX_FTRDB_CREATOR ) THEN
               CALL ECH_ACCESS_OBJECT( FULL_OBJECT_PATH, 'MAP-READ',
     :              TYPE, NEW_OBJECT_SIZE, NEW_ADDRESS, NEW_HANDLE,
     :              DUMDIM, 1, 0, ' ', STATUS )

            ELSE
               CALL ECH_ACCESS_OBJECT( FULL_OBJECT_PATH, 'MAP', TYPE,
     :              NEW_OBJECT_SIZE, NEW_ADDRESS, NEW_HANDLE, DUMDIM,
     :              1, 0, ' ', STATUS )
            END IF
            MAPPED_ADDRESS = NEW_ADDRESS
            IF ( STATIC_INDICES .GT. 0 .AND.
     :           ONCE_PER_ORDER( MODULE_NUMBER ) ) THEN
               IF ( DIMEN_INDEX( STATIC_INDICES ) .EQ.
     :              'IDX_NUM_ORDERS' ) THEN
                  IF ( OPERATION .NE. 'WRITE' ) THEN
                     IF ( MAPPED_ADDRESS .NE. 0 ) THEN
                        MAPPED_ADDRESS = MAPPED_ADDRESS + AUNIT * (
     :                        ( DIMEN_VALUE( STATIC_INDICES ) - 1 ) *
     :                        NEW_OBJECT_SIZE /
     :                        DIMENSIONS( STATIC_INDICES ) )
                     END IF
                  END IF
               END IF
            END IF
            IF ( CLONE_FILE_OPEN ) THEN
               IF ( MODULE_UPDATES( ECH_OBJ_IND( REQ_OBJ ),
     :              ECH_MODULE_NAME_INDEX( CURRENT_MODULE ) ) ) THEN
                CLONE_OBJECT_PATH = 'CLONE.' // FULL_OBJECT_PATH(11:)
                CALL ECH_ACCESS_OBJECT( CLONE_OBJECT_PATH, 'MAP-READ',
     :               TYPE, NEW_OBJECT_SIZE, CLONE_ADDRESS, NEW_HANDLE,
     :               DUMDIM, 1, 0, ' ', STATUS )
                IF ( STATUS .EQ. 0 ) THEN
                   CALL ECH_COPY_BYTES( NEW_OBJECT_SIZE * AUNIT,
     :                  %VAL( CLONE_ADDRESS ), %VAL( NEW_ADDRESS ) )
                END IF
                CALL ECH_ACCESS_OBJECT( FULL_OBJECT_PATH, 'WRITE',
     :               TYPE, NEW_OBJECT_SIZE, NEW_ADDRESS, 0, DUMDIM, 1,
     :               0, ' ', STATUS )
                CALL ECH_ACCESS_OBJECT( CLONE_OBJECT_PATH, 'UNMAP',
     :               TYPE, NEW_OBJECT_SIZE, CLONE_ADDRESS, NEW_HANDLE,
     :               DUMDIM, 1, 0, ' ', STATUS )
                REPORT_STRING = ' Cloned ' // FULL_OBJECT_PATH( 11:
     :                ECH_WORD_LEN( FULL_OBJECT_PATH ) ) //
     :                ' from file ' //
     :                RDCTN_FILE_CLONE(
     :                :ECH_WORD_LEN( RDCTN_FILE_CLONE ) ) // '.'
                CALL ECH_REPORT( 0, REPORT_STRING )
               END IF
            END IF
         END IF

         IF ( STATUS .EQ. 0 ) THEN
            IF ( INC_ACCESS ) THEN
               NEW_ACCESS_COUNT = ACCESS_COUNT + 1
               ACCESS_COUNT = NEW_ACCESS_COUNT
            END IF
            OBJECT_NAME( NEW_ACCESS_COUNT ) = FULL_OBJECT_PATH
            OBJECT_TYPE( NEW_ACCESS_COUNT ) = NEW_OBJECT_TYPE
            OBJECT_SIZE( NEW_ACCESS_COUNT ) = NEW_OBJECT_SIZE
            OBJECT_INDEX( NEW_ACCESS_COUNT ) =
     :            ECH_OBJ_IND( REQ_OBJ )
            OBJECT_ADDRESS( NEW_ACCESS_COUNT ) = NEW_ADDRESS
            OBJECT_HANDLE( NEW_ACCESS_COUNT ) = NEW_HANDLE
            OBJECT_STATICS( NEW_ACCESS_COUNT ) = STATIC_INDICES
            OBJECT_ACTIVE = .TRUE.
            OBJ_NUM = NEW_ACCESS_COUNT
         END IF
         IF ( DIAGNOSTICS_ACTIVE ) THEN
            ILEN = ECH_WORD_LEN( FULL_OBJECT_PATH )
            REPORT_STRING = ' RDC Access to ' //
     :             FULL_OBJECT_PATH( :ILEN ) // '.'
            CALL ECH_REPORT( RPM_LOG, REPORT_STRING )
            CALL CHR_ITOC( NEW_OBJECT_SIZE, REF_STR1, NCHAR1 )
            CALL CHR_ITOC( MAPPED_ADDRESS, REF_STR2, NCHAR2 )
            CALL CHR_ITOC( STATUS, REF_STR3, NCHAR3 )
            REPORT_STRING = ' using ' //
     :            OPERATION( :ECH_WORD_LEN( OPERATION ) ) // ' ' //
     :            TYPE( :ECH_WORD_LEN( TYPE ) ) // ' ' //
     :            REF_STR1( :NCHAR1 ) // ' bytes @' //
     :            REF_STR2( :NCHAR2 ) // ' status: ' //
     :            REF_STR3( :NCHAR3 ) // '.'
            CALL ECH_REPORT( RPM_LOG, REPORT_STRING )
         END IF

      ELSE IF ( OPERATION .EQ. 'UNMAP' ) THEN
         IF ( OBJECT_TYPE( OBJ_NUM ) .EQ. 'CHAR' .AND.
     :        TYPE .EQ. 'CHAR' ) THEN
            NEW_OBJECT_SIZE = CHR_LEN( IO_STRING )
            IF ( NEW_OBJECT_SIZE .GT. 0 .AND. IO_STRING .NE. ' ' ) THEN
               CALL ECH_ACCESS_OBJECT( FULL_OBJECT_PATH, 'WRITE',
     :              'CHAR', NEW_OBJECT_SIZE, 0, 0, DUMDIM, 1, 0,
     :              IO_STRING, STATUS )
            END IF

*     Only unmap vectors (scalars carry no overhead and some are used
*     in between processing routines).
         ELSE IF ( OBJECT_SIZE( OBJ_NUM ) .GT. 8 ) THEN
            CALL ECH_ACCESS_OBJECT( OBJECT_NAME( OBJ_NUM ), 'UNMAP',
     :           OBJECT_TYPE( OBJ_NUM ), OBJECT_SIZE( OBJ_NUM ),
     :           OBJECT_ADDRESS( OBJ_NUM ), OBJECT_HANDLE( OBJ_NUM ),
     :           DUMDIM, 1, 0, ' ', STATUS )
            IF ( ECH_FATAL_ERROR( STATUS ) ) GO TO 999
            OBJECT_ADDRESS( OBJ_NUM ) = 0
            OBJECT_NAME( OBJ_NUM ) = ' '
            object_type( OBJ_NUM ) = '-a free entry-'
            OBJECT_SIZE( OBJ_NUM ) = 0
            OBJECT_STATICS( OBJ_NUM ) = 0
            OBJECT_INDEX( OBJ_NUM ) = 0
            OBJECT_SEND( OBJ_NUM ) = 0
         END IF

      ELSE IF ( OPERATION .EQ. 'WRITE' ) THEN
         IF ( OBJECT_TYPE( OBJ_NUM ) .EQ. 'CHAR' .AND.
     :        TYPE .EQ. 'CHAR' ) THEN
            NEW_OBJECT_SIZE = CHR_LEN( IO_STRING )
            IF ( NEW_OBJECT_SIZE .GT. 0 .AND. IO_STRING .NE. ' ' ) THEN
               CALL ECH_ACCESS_OBJECT( FULL_OBJECT_PATH, 'WRITE',
     :              'CHAR', NEW_OBJECT_SIZE, 0, 0, DUMDIM, 1, 0,
     :              IO_STRING, STATUS )
            END IF

         ELSE
            IF ( MAPPED_ADDRESS .EQ. OBJECT_ADDRESS( OBJ_NUM ) ) THEN
                CALL ECH_ACCESS_OBJECT( FULL_OBJECT_PATH, 'WRITE', TYPE,
     :               OBJECT_SIZE( OBJ_NUM ), OBJECT_ADDRESS( OBJ_NUM ),
     :               OBJECT_HANDLE( OBJ_NUM ), DUMDIM, 1, 0, ' ',
     :                STATUS )
                IF ( MAPPED_ADDRESS .NE.
     :            OBJECT_ADDRESS( OBJ_NUM ) ) THEN
                  MAPPED_ADDRESS = OBJECT_ADDRESS( OBJ_NUM )
                  CALL ECH_SETUP_OBJECT_REF( REQ_OBJ, MAPPED_ADDRESS,
     :                 0, 0, ' ', 0.0, .FALSE., STATUS )
                END IF
            END IF
         END IF

         IF ( STATUS .NE. 0 ) THEN
            ILEN = ECH_WORD_LEN( FULL_OBJECT_PATH )
            REPORT_STRING = ' Failed to write ' //
     :            FULL_OBJECT_PATH( :ILEN ) // '.'
            CALL ECH_REPORT( 0, REPORT_STRING )
         END IF
         IF ( DIAGNOSTICS_ACTIVE ) THEN
            ILEN = ECH_WORD_LEN( FULL_OBJECT_PATH )
            REPORT_STRING = ' RDC Access to ' //
     :             FULL_OBJECT_PATH( :ILEN ) // '.'
            CALL ECH_REPORT( RPM_LOG, REPORT_STRING )
            CALL CHR_ITOC( NEW_OBJECT_SIZE, REF_STR1, NCHAR1 )
            CALL CHR_ITOC( MAPPED_ADDRESS, REF_STR2, NCHAR2 )
            CALL CHR_ITOC( STATUS, REF_STR3, NCHAR3 )
            REPORT_STRING = ' using ' //
     :            OPERATION( :ECH_WORD_LEN( OPERATION ) ) // ' ' //
     :            TYPE( :ECH_WORD_LEN( TYPE ) ) // ' ' //
     :            REF_STR1( :NCHAR1 ) // ' bytes @' //
     :            REF_STR2( :NCHAR2 ) // ' status: ' //
     :            REF_STR3( :NCHAR3 ) // '.'
            CALL ECH_REPORT( RPM_LOG, REPORT_STRING )
         END IF

      ELSE

*     If object is mapped successfully then flag it.
         IF ( MAPPED_ADDRESS .NE. 0 .AND.
     :        MAPPED_ADDRESS .NE. -1 .AND. STATUS .EQ. 0 ) THEN
            STATUS = ECH__IS_ACCESSED

         ELSE
            STATUS = ECH__PAR_ACCESSED
         END IF

         IF ( DIAGNOSTICS_ACTIVE ) THEN
            ILEN = ECH_WORD_LEN( FULL_OBJECT_PATH )
            REPORT_STRING = ' RDC Access to ' //
     :            FULL_OBJECT_PATH( :ILEN ) // '.'
            CALL ECH_REPORT( RPM_LOG, REPORT_STRING )
         END IF
      END IF

  999 CONTINUE

      END
