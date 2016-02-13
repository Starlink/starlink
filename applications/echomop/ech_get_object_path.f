      SUBROUTINE ECH_GET_OBJECT_PATH(
     :           OBJECT,
     :           FINAL_PATH_SPEC,
     :           PATH_NAME,
     :           STATIC_INDICES,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_GET_OBJECT_PATH

*  Purpose:
*     Uses object ref name to determine path into reduction file.

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'SAE_PAR'
      INCLUDE 'ECH_MODULES.INC'
      INCLUDE 'ECH_USE_RDCTN.INC'
      INCLUDE 'ECH_REPORT.INC'

*  Arguments:
      CHARACTER*( * ) OBJECT
      CHARACTER*( * ) FINAL_PATH_SPEC
      CHARACTER*( * ) PATH_NAME
      INTEGER STATIC_INDICES
      INTEGER STATUS

*  Local Variables:
      REAL RVALUE

      INTEGER DUMDIM( MAX_DIMENSIONS )
      INTEGER I
      INTEGER INDEX
      INTEGER OBJNUM
      INTEGER MXLEN
      INTEGER IOBJ
      INTEGER WINDEX
      INTEGER COPY_INDEX
      INTEGER STATUS2
      INTEGER VALUE
      INTEGER VALUENOW
      INTEGER PAR_START_AT
      INTEGER PAR_END_AT
      INTEGER IREG
      INTEGER DEFAULT_INDEX
      INTEGER NCHAR1

      LOGICAL SCANNING_INDEX
      LOGICAL REG_DIMEN_VAR
      LOGICAL CREATING
      LOGICAL NO_TRANSLATE
      LOGICAL INDEXING

      CHARACTER*128 PATH_SPEC
      CHARACTER*80 WORK_STRING
      CHARACTER*80 REQUIRED_PATH
      CHARACTER*80 TPATH_NAME
      CHARACTER*32 PAR_NAME
      CHARACTER*1  PATHC

      INTEGER ECH_WORD_LEN
      INTEGER ECH_OBJ_IND
*.

      INDEXING = .FALSE.
      CREATING = ( STATUS .EQ. ECH__CRE_OBJECT )
      NO_TRANSLATE = ( STATIC_INDICES .LT. 0 )
      STATIC_INDICES = 0
      VALUENOW = 0

*  Non-workspace objects.
      IF ( .NOT. IS_WORKSPACE( ECH_OBJ_IND( OBJECT ) ) ) THEN
         WINDEX = 0
         DO I = MAX_DIMENSIONS, 1, -1
            DIMEN_INDEX( I ) = ' '
            DIMEN_VALUE( I ) = 0
         END DO

         DO I = 1, MAX_OBJECTS
            IF ( OBJECT .EQ. OBJECT_REF_NAME( I ) ) THEN
               INDEX = I
               GO TO 100
            END IF
         END DO
         IF ( OBJECT( :7 ) .EQ. 'ECHARC_' ) THEN
            FINAL_PATH_SPEC = 'ECH_ECHAR.' // OBJECT

         ELSE IF ( OBJECT( :7 ) .EQ. 'EFTRDB_' ) THEN
            FINAL_PATH_SPEC = 'ECH_FTRDB.' // OBJECT

         ELSE IF ( OBJECT( :7 ) .EQ. 'RESULT_' ) THEN
            FINAL_PATH_SPEC = 'ECH_RDUCD.' // OBJECT

         ELSE
            FINAL_PATH_SPEC = 'ECH_RDCTN.' // OBJECT
         END IF
         PATH_NAME = FINAL_PATH_SPEC
         GO TO 999

  100    CONTINUE
         PATH_SPEC = ' '
         FINAL_PATH_SPEC = ' '
         REQUIRED_PATH = OBJECT_PATH( INDEX )
         TPATH_NAME = REQUIRED_PATH

*  Workspace objects.
      ELSE
         WINDEX = ECH_OBJ_IND( OBJECT )
         I = ECH_WORD_LEN( OBJECT )
         REQUIRED_PATH = OBJECT( : I ) // WS_DIMENSIONS( WINDEX )
      END IF

      SCANNING_INDEX = .FALSE.
      COPY_INDEX = 0
      MXLEN = ECH_WORD_LEN( REQUIRED_PATH )
      DO I = 1, MXLEN
         PATHC = REQUIRED_PATH( I : I )
         IF ( .NOT. SCANNING_INDEX ) THEN
            COPY_INDEX = COPY_INDEX + 1
         END IF
         IF ( PATHC .EQ. '[' ) THEN
            TPATH_NAME = REQUIRED_PATH( : I - 1 ) // ' '

         ELSE IF ( PATHC .EQ. ']' ) THEN
            SCANNING_INDEX = .FALSE.
         ENDIF
         IF ( .NOT. SCANNING_INDEX .OR.
     :        ( SCANNING_INDEX .AND. PATHC .EQ. ',' ) ) THEN
            PATH_SPEC( COPY_INDEX : COPY_INDEX ) = PATHC
         ENDIF
         IF ( PATHC .EQ. '[' .OR.
     :        ( SCANNING_INDEX .AND. PATHC .EQ. ',' ) ) THEN
            SCANNING_INDEX = .TRUE.
            PAR_START_AT = I + 1
            PAR_END_AT = I + 1
            DO WHILE ( REQUIRED_PATH( PAR_END_AT : PAR_END_AT ) .NE.
     :                 ']' .AND.
     :                 REQUIRED_PATH( PAR_END_AT : PAR_END_AT ) .NE.
     :                 ',' )
               PAR_END_AT = PAR_END_AT + 1
            END DO
            PAR_END_AT = PAR_END_AT - 1

            PAR_NAME = REQUIRED_PATH( PAR_START_AT : PAR_END_AT )
            IF ( NO_TRANSLATE ) THEN
               STATIC_INDICES = STATIC_INDICES + 1
               DIMEN_INDEX( STATIC_INDICES ) = PAR_NAME
               PATH_SPEC( COPY_INDEX + 1: ) = PAR_NAME
               COPY_INDEX = COPY_INDEX + ECH_WORD_LEN( PAR_NAME ) + 1

            ELSE
               DO IREG = 1, NUM_REG_VARS
                  IF ( OBJ_DIMEN_VARS( IREG ) .EQ. PAR_NAME ) THEN
                     REG_DIMEN_VAR = .TRUE.
                     GO TO 500
                  END IF
               END DO
               REG_DIMEN_VAR = .FALSE.
  500          CONTINUE

               IF ( WINDEX .EQ. 0 .AND. REG_DIMEN_VAR .AND.
     :              .NOT. CREATING ) THEN
                  VALUE = 1
                  STATIC_INDICES = STATIC_INDICES + 1
                  DIMEN_INDEX( STATIC_INDICES ) = PAR_NAME
                  DIMEN_VALUE( STATIC_INDICES ) = VALUE
                  IF ( PAR_NAME( :1 ) .NE. '1' ) THEN
                     IF ( IN_RDCTN_FILE( ECH_OBJ_IND( PAR_NAME ) ) )
     :                    THEN
                        DO IOBJ = 1, MAX_OBJECTS
                           IF ( OBJECT_REF_NAME( IOBJ ) .EQ. PAR_NAME )
     :                          THEN
                              OBJNUM = IOBJ
                              GO TO 600
                           END IF
                        END DO
                        OBJNUM = 0
                        GO TO 610
  600                   CONTINUE
                        WORK_STRING = 'ECH_RDCTN.' //
     :                        OBJECT_PATH( OBJNUM )
                        CALL ECH_ACCESS_OBJECT( WORK_STRING,
     :                       'READ-INT', 'INT', 1, VALUENOW, 0,
     :                       DUMDIM, MAX_DIMENSIONS, 0, ' ', STATUS )
                        DIMEN_VALUE( STATIC_INDICES ) = VALUENOW
  610                   CONTINUE
                     END IF
                  END IF

               ELSE
                  INDEXING = .FALSE.
                  IF ( .NOT. CREATING .AND. WINDEX .EQ. 0 .AND.
     :                 PAR_NAME( :5 ) .NE. 'TUNE_' .AND.
     :                 PAR_NAME( :1 ) .NE. '1' ) THEN
                     STATUS2 = 0
                     WORK_STRING = 'IDX_' // PAR_NAME
                     CALL ECH_GET_PARAMETER( WORK_STRING,
     :                    'INT', RVALUE, .FALSE., ' ', 0, STATUS2 )
                     IF ( ( STATUS2 .EQ. 0 .OR.
     :                      STATUS2 .EQ. ECH__IS_ACCESSED ) .AND.
     :                    RVALUE .NE. 0 ) THEN
                        PAR_NAME = 'IDX_' // PAR_NAME
                        INDEXING = .TRUE.
                     END IF
                  END IF

                  IF ( PAR_NAME( :1 ) .EQ. '1' ) THEN
                     RVALUE = 1.0
                     OBJNUM = -1

                  ELSE IF ( .NOT. INDEXING  ) THEN
                     OBJNUM = 0
                     IF ( IN_RDCTN_FILE(
     :                    ECH_OBJ_IND( PAR_NAME ) ) ) THEN
                        DO IOBJ = 1, MAX_OBJECTS
                           IF ( OBJECT_REF_NAME( IOBJ ) .EQ. PAR_NAME )
     :                          THEN
                              OBJNUM = IOBJ
                              GO TO 700
                           END IF
                        END DO
                        GO TO 710
  700                   CONTINUE
                        WORK_STRING = 'ECH_RDCTN.' //
     :                        OBJECT_PATH( OBJNUM )
                        CALL ECH_ACCESS_OBJECT( WORK_STRING,
     :                       'READ-INT', 'INT', 1, VALUE, 0,
     :                       DUMDIM, MAX_DIMENSIONS, 0, ' ', STATUS )
                        RVALUE = FLOAT( VALUE )
  710                   CONTINUE

                     ELSE
                        DEFAULT_INDEX = DEFAULTS_INDICES(
     :                        ECH_OBJ_IND( PAR_NAME ) )
                        STATUS = ECH__ARRAY_INDEX
                        CALL ECH_GET_PARAMETER( PAR_NAME, 'INT', RVALUE,
     :                       .FALSE., ' ', DEFAULT_INDEX, STATUS )
                     END IF
                  END IF

                  IF ( STATUS .EQ. SAI__OK .AND. OBJNUM .EQ. 0 ) THEN
                     CALL ECH_SETUP_OBJECT_REF( PAR_NAME, 0, 0, 0, ' ',
     :                    RVALUE, .FALSE., STATUS )

                  ELSE IF ( STATUS .EQ. ECH__IS_ACCESSED ) THEN
                     CALL ECH_UPDATE_OBJECT_REF( PAR_NAME, 0, 0, 0, ' ',
     :                    RVALUE, .FALSE., STATUS )
                  END IF
                  VALUE = INT( RVALUE )
                  IF ( WINDEX .GT. 0 .OR. CREATING .OR.
     :                 .NOT. INDEXING ) THEN
                     STATIC_INDICES = STATIC_INDICES + 1
                     DIMEN_INDEX( STATIC_INDICES ) = PAR_NAME
                     DIMEN_VALUE( STATIC_INDICES ) = VALUE
                     IF ( .NOT. CREATING ) VALUE = 1
                     IF ( WINDEX .GT. 0 .AND.
     :                    STATIC_INDICES .EQ. 1 ) THEN
                        IF ( WS_DIMEN_MULT( WINDEX ) .GT. 0 ) THEN
                           DIMEN_VALUE( STATIC_INDICES ) =
     :                           DIMEN_VALUE( STATIC_INDICES )  *
     :                           WS_DIMEN_MULT( WINDEX )
                        END IF
                     END IF

                  ELSE IF ( PAR_NAME .EQ. 'IDX_NUM_ORDERS' ) THEN
                     STATIC_INDICES = STATIC_INDICES + 1
                     DIMEN_INDEX( STATIC_INDICES ) = PAR_NAME
                     DIMEN_VALUE( STATIC_INDICES ) = VALUE
                     VALUE = 1
                  END IF
               END IF

               CALL CHR_ITOC( VALUE, PATH_SPEC( COPY_INDEX + 1: ),
     :              NCHAR1 )
               COPY_INDEX = COPY_INDEX + NCHAR1 + 1
            END IF
         END IF
      END DO

      IF ( OBJECT( :7 ) .EQ. 'ECHARC_' ) THEN
         FINAL_PATH_SPEC = 'ECH_ECHAR.' // PATH_SPEC // ' '
         PATH_NAME = 'ECH_ECHARC.' // TPATH_NAME // ' '

      ELSE IF ( OBJECT( :7 ) .EQ. 'EFTRDB_' ) THEN
         FINAL_PATH_SPEC = 'ECH_FTRDB.' // PATH_SPEC // ' '
         PATH_NAME = 'ECH_ECHARC.' // TPATH_NAME // ' '

      ELSE IF ( OBJECT( :7 ) .EQ. 'RESULT_' ) THEN
         FINAL_PATH_SPEC = 'ECH_RDUCD.' // PATH_SPEC // ' '
         PATH_NAME = 'ECH_RDUCD.' // TPATH_NAME // ' '

      ELSE
         FINAL_PATH_SPEC = 'ECH_RDCTN.' // PATH_SPEC // ' '
         PATH_NAME = 'ECH_RDCTN.' // TPATH_NAME // ' '
      END IF

*  Jump to here if an error occurs.
  999 CONTINUE

      END
