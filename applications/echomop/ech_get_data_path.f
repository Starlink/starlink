      SUBROUTINE ECH_GET_DATA_PATH(
     :           REQUIRED_OBJECT,
     :           FINAL_PATH_SPEC,
     :           TYPE,
     :           STATIC_INDICES,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_GET_DATA_PATH

*  Purpose:
*     Given an obect reference name, finds a datafile path.

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_MODULES.INC'
      INCLUDE 'ECH_USE_RDCTN.INC'
      INCLUDE 'ECH_REPORT.INC'

*  Arguments:
      CHARACTER*( * ) REQUIRED_OBJECT
      CHARACTER*( * ) FINAL_PATH_SPEC
      CHARACTER*( * ) TYPE
      INTEGER STATIC_INDICES
      INTEGER STATUS

*  Local Variables:
      REAL RVALUE

      INTEGER I
      INTEGER I2
      INTEGER INDEX
      INTEGER COPY_INDEX
      INTEGER STATUS2
      INTEGER VALUE
      INTEGER MXLEN
      INTEGER PAR_START_AT
      INTEGER PAR_END_AT
      INTEGER IREG
      INTEGER DEFAULT_INDEX
      INTEGER NCHAR1

      LOGICAL SCANNING_INDEX
      LOGICAL REG_DIMEN_VAR
      LOGICAL INDEXING

      CHARACTER*80 WORK_STRING
      CHARACTER*80 REQUIRED_PATH
      CHARACTER*80 PATH_SPEC
      CHARACTER*80 PAR_NAME
      CHARACTER*1 PATHC

*  Functions Called:
      INTEGER ECH_OBJ_IND
      INTEGER ECH_WORD_LEN
*.

      STATIC_INDICES = 0

      DO I = 1, MAX_DIMENSIONS
         DIMEN_INDEX( I ) = ' '
         DIMEN_VALUE( I ) = 0
      END DO

      DO I = 1, MAX_OBJECTS
         IF ( REQUIRED_OBJECT .EQ. OBJECT_REF_NAME( I ) ) THEN
            INDEX = I
            GO TO 100
         END IF
      END DO
      FINAL_PATH_SPEC = REQUIRED_OBJECT
      STATUS = 0
      GO TO 999

  100 CONTINUE
      REQUIRED_PATH = OBJECT_PATH( INDEX )
      PATH_SPEC = ' '
      FINAL_PATH_SPEC = ' '
      SCANNING_INDEX = .FALSE.
      COPY_INDEX = 0
      MXLEN = ECH_WORD_LEN( REQUIRED_PATH )
      DO I = 1, MXLEN - 1
         PATHC = REQUIRED_PATH( I : I )
         IF ( .NOT. SCANNING_INDEX ) THEN
            COPY_INDEX = MIN( COPY_INDEX + 1, MXLEN )
         END IF
         IF ( PATHC .EQ. ']' ) THEN
            SCANNING_INDEX = .FALSE.
         END IF
         IF ( .NOT. SCANNING_INDEX .OR.
     :        ( SCANNING_INDEX .AND. PATHC .EQ. ',' ) ) THEN
            PATH_SPEC( COPY_INDEX : COPY_INDEX ) = PATHC
         END IF
         IF ( PATHC .EQ. '[' ) STATIC_INDICES = 0
         IF ( PATHC .EQ. '[' .OR.
     :        ( PATHC .EQ. ',' .AND. SCANNING_INDEX ) ) THEN
            SCANNING_INDEX = .TRUE.
            PAR_START_AT = I + 1
            PAR_END_AT = I + 1
            DO WHILE ( REQUIRED_PATH(PAR_END_AT:PAR_END_AT) .NE.
     :                 ']' .AND.
     :                 REQUIRED_PATH(PAR_END_AT:PAR_END_AT) .NE.
     :                 ',' )
               PAR_END_AT = PAR_END_AT + 1
            END DO
            PAR_END_AT = PAR_END_AT - 1

            PAR_NAME = REQUIRED_PATH( PAR_START_AT : PAR_END_AT )
            DO IREG = 1, NUM_REG_VARS
               IF ( OBJ_DIMEN_VARS( IREG ) .EQ. PAR_NAME ) THEN
                  REG_DIMEN_VAR = .TRUE.
                  GO TO 500
               END IF
            END DO
            REG_DIMEN_VAR = .FALSE.

  500       CONTINUE
            IF ( REG_DIMEN_VAR ) THEN
               VALUE = 1
               STATIC_INDICES = STATIC_INDICES + 1
               DIMEN_INDEX( STATIC_INDICES ) = PAR_NAME
               DIMEN_VALUE( STATIC_INDICES ) = VALUE

            ELSE
               INDEXING = .FALSE.
               IF ( PAR_NAME( :5 ) .NE. 'TUNE_' ) THEN
                  STATUS2 = 0
                  WORK_STRING = 'IDX_' // PAR_NAME
                  CALL ECH_GET_PARAMETER( WORK_STRING, 'INT',
     :                 RVALUE, .FALSE., ' ', 0, STATUS2 )
                  IF ( ( STATUS2 .EQ. 0 .OR.
     :                 STATUS2 .EQ. ECH__IS_ACCESSED ) .AND.
     :                 RVALUE .NE. 0 ) THEN
                     PAR_NAME = 'IDX_' // PAR_NAME
                     INDEXING = .TRUE.
                     STATIC_INDICES = STATIC_INDICES + 1
                     DIMEN_INDEX( STATIC_INDICES ) = PAR_NAME
                     DIMEN_VALUE( STATIC_INDICES ) = INT( RVALUE )
                  END IF
               END IF

               IF ( .NOT. INDEXING ) THEN
                  DEFAULT_INDEX = DEFAULTS_INDICES (
     :                  ECH_OBJ_IND( PAR_NAME ) )
                  STATUS = ECH__ARRAY_INDEX
                  CALL ECH_GET_PARAMETER( PAR_NAME, 'INT',
     :                 RVALUE, .FALSE., ' ', DEFAULT_INDEX, STATUS )
               END IF

               IF ( STATUS .EQ. 0 ) THEN
                  CALL ECH_SETUP_OBJECT_REF( PAR_NAME, 0, 0, 0, ' ',
     :                 RVALUE, .FALSE., STATUS )
               ELSE IF ( STATUS .EQ. ECH__IS_ACCESSED ) THEN
                  CALL ECH_UPDATE_OBJECT_REF( PAR_NAME, 0, 0, 0, ' ',
     :                 RVALUE, .FALSE., STATUS )
               END IF
               VALUE = INT( RVALUE )
            END IF

            CALL CHR_ITOC( VALUE, PATH_SPEC( COPY_INDEX + 1: ),
     :           NCHAR1 )
            COPY_INDEX = MIN( COPY_INDEX + NCHAR1 + 1, MXLEN )
         END IF
      END DO

      FINAL_PATH_SPEC = PATH_SPEC( : LEN( PATH_SPEC ) ) // ' '

      IF ( TYPE .EQ. 'IMAGE' ) THEN
         FINAL_PATH_SPEC = ' '
         I = 1
         I2 = 1
         DO WHILE ( I .LT. LEN( PATH_SPEC ) .AND.
     :              PATH_SPEC( I : I) .NE. ' ' )
            IF ( PATH_SPEC( I : I ) .EQ. '[' .OR.
     :           PATH_SPEC( I : I ) .EQ. ']' ) I = I + 1
            FINAL_PATH_SPEC( I2 : I2 ) = PATH_SPEC( I : I )
            I = I + 1
            I2 = I2 + 1
         END DO
      END IF

*  Jump to here if an error occurs.
  999 CONTINUE

      END
