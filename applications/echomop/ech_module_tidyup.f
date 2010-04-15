      SUBROUTINE ECH_MODULE_TIDYUP( CALLER_NAME, STATUS )
*+
*  Name:
*     ECHOMOP - ECH_MODULE_TIDYUP

*  Purpose:
*     Tidy up after a processing module.

*  Description:
*     This routine is called by every processing module to ensure that any
*     required data/reduction database updates are made after processing
*     has been completed.

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Variables and Constants:
      INCLUDE 'ECH_USE_RDCTN.INC'
      INCLUDE 'ECH_MODULES.INC'
      INCLUDE 'ECH_MAPPING.INC'
      INCLUDE 'ECH_REPORT.INC'
      INCLUDE 'ECH_ECHOMOP.INC'
      INCLUDE 'ECH_SERVER.INC'

*  Arguments:
      CHARACTER*( * ) CALLER_NAME

*  Global Status:
      INTEGER STATUS

*  Local Variables:
      REAL VALUE
      REAL SWITCH

      INTEGER DUMDIM( MAX_DIMENSIONS )
      INTEGER MODULE_NUMBER
      INTEGER I
      INTEGER MAPPED_ADDRESS
      INTEGER MAPPED_QADDRESS
      INTEGER MAPPED_EADDRESS
      INTEGER REM_STATUS
      INTEGER DUMMY_QADDRESS
      INTEGER DUMMY_EADDRESS
      INTEGER STAT2

      LOGICAL BOOLEAN_VALUE
      LOGICAL SWITCHED_OFF

      CHARACTER*80 REF_NAME
      CHARACTER*80 SERVED
      CHARACTER*80 STRING
      CHARACTER*32 TYPE

      LOGICAL USED_LAST_TIME( MAX_REQUIRED_OBJECTS )
      LOGICAL FIRST_CALL
      COMMON / MODULE_TIDY / USED_LAST_TIME, FIRST_CALL

*  External Functions:
      INTEGER CHR_LEN
      LOGICAL MODULE_NEEDS
      LOGICAL MODULE_UPDATES
      LOGICAL ECH_FATAL_ERROR

      DATA FIRST_CALL / .TRUE. /
*.

      IF ( FIRST_CALL ) THEN
         FIRST_CALL = .FALSE.
         DO I = 1, MAX_REQUIRED_OBJECTS
            USED_LAST_TIME( I ) = .FALSE.
         END DO
      END IF
      DO I = 1, MAX_DIMENSIONS
         DUMDIM( I ) = 0
      END DO
      MODULE_NUMBER = 0
      MAPPED_ADDRESS = 0
      MAPPED_QADDRESS = 0
      MAPPED_EADDRESS = 0
      REM_STATUS = 0
      DUMMY_QADDRESS = 0
      DUMMY_EADDRESS = 0
      STAT2 = 0

      BOOLEAN_VALUE = .FALSE.
      SWITCHED_OFF = .FALSE.


      IF ( STATUS .EQ. ECH__CLIENT_ACTIVE ) THEN
         STATUS = 0
         CALL ECH_ACCESS_OBJECT( 'MODULE', 'RESPONSE', 'CHAR',
     :        0, 0, 0, DUMDIM, MAX_DIMENSIONS, 0, SERVED, STATUS )
         IF ( SERVED( :CHR_LEN( CALLER_NAME ) ) .NE. CALLER_NAME ) THEN
            CALL ECH_REPORT( 0,
     :           ' Server responsed with incorrect module name.' )
         ENDIF
      ENDIF

      IF ( ECH_FATAL_ERROR( STATUS ) ) THEN
         REM_STATUS = STATUS

      ELSE
         REM_STATUS = 0
      END IF

      IF ( STATUS .EQ. ECH__ABORT_INIT ) THEN
         GO TO 999
      END IF

*  Locate module by searching the table for its name.
      IF ( CURRENT_MODULE .EQ. CALLER_NAME ) THEN
         MODULE_NUMBER = CURRENT_MODULE_INDEX

      ELSE
         MODULE_NUMBER = 0
         DO I = 1, MAX_MODULES
            IF ( MODULE_NAME( I ) .EQ. CALLER_NAME ) THEN
               MODULE_NUMBER = I
               GO TO 50
            ENDIF
         END DO

*     Do nothing if Module number is undefined.
         GO TO 999
  50     CONTINUE
      END IF

*  Loop through objects required by the module.
      DO I = 1, MAX_REQUIRED_OBJECTS
         IF ( MODULE_NEEDS( I, MODULE_NUMBER ) ) THEN
            SWITCHED_OFF = .FALSE.
            IF ( REQUIRED_OBJECTS( I )( :5 ) .NE. 'TUNE_' .AND.
     :           REQUIRED_OBJECTS( I )( :4 ) .NE. 'ECH_' .AND.
     :           USR_TUNE_CLONE .NE. 'NULL' .AND.
     :           .NOT. IN_RDCTN_FILE( I ) )
     :         SWITCHED_OFF = .TRUE.
            IF ( OBJECT_SWITCH( I ) .GT. 0 ) THEN
               CALL ECH_GET_PARAMETER(
     :              REQUIRED_OBJECTS( OBJECT_SWITCH( I ) ),
     :              'LOGICAL', 0., SWITCHED_OFF,
     :              'SWITCH', 0, STAT2 )

            ELSE IF ( OBJECT_SWITCH( I ) .LT. 0 ) THEN
               CALL ECH_GET_PARAMETER(
     :              REQUIRED_OBJECTS( -OBJECT_SWITCH( I ) ),
     :              'INT', SWITCH, .FALSE., 'SWITCH', 0, STAT2 )
                 IF ( INT( SWITCH ) .EQ. 0 ) SWITCHED_OFF = .TRUE.
            ENDIF
            IF ( .NOT. SWITCHED_OFF ) THEN
               REF_NAME = REQUIRED_OBJECTS( I )
               TYPE = REQUIRED_TYPE( I )
               IF ( USR_TUNE_SERVER ) THEN
                  IF ( IS_WORKSPACE( I ) ) THEN
                     CALL ECH_ACCESS_WORKSPACE( REF_NAME, TYPE,
     :                    MAPPED_ADDRESS, STATUS )
                  END IF

               ELSE IF ( MODULE_UPDATES( I, MODULE_NUMBER ) ) THEN
                  IF ( .NOT. USR_TUNE_CLIENT )
     :               CALL ECH_UPDATE_OBJECT_REF( REF_NAME,
     :                    MAPPED_ADDRESS, MAPPED_QADDRESS,
     :                    MAPPED_EADDRESS, STRING, VALUE, BOOLEAN_VALUE,
     :                    STATUS )

*              If the object should be in the reduction file look there.
                  IF ( IN_RDCTN_FILE( I ) ) THEN
                     IF ( USR_TUNE_CLIENT ) THEN
                        CALL ECH_ACCESS_REDUCTION_FILE( REF_NAME,
     :                       'READ', TYPE, MAPPED_ADDRESS, STRING,
     :                       MODULE_NUMBER, STATUS )
                        CALL ECH_ACCESS_OBJECT( REF_NAME, 'RESPONSE',
     :                       ' ', 0, MAPPED_ADDRESS, DUMMY_QADDRESS,
     :                       DUMDIM, MAX_DIMENSIONS, DUMMY_EADDRESS,
     :                       STRING, STATUS )
                     END IF
                     CALL ECH_ACCESS_REDUCTION_FILE( REF_NAME, 'WRITE',
     :                    TYPE, MAPPED_ADDRESS, STRING, MODULE_NUMBER,
     :                    STATUS )
                     IF ( ( USR_TUNE_CLIENT .AND.
     :                    .NOT. USED_LAST_TIME( I ) ) .OR.
     :                    .NOT. ONCE_PER_ORDER( MODULE_NUMBER ) ) THEN
                        CALL ECH_ACCESS_REDUCTION_FILE( REF_NAME,
     :                       'UNMAP', TYPE, MAPPED_ADDRESS, STRING,
     :                       MODULE_NUMBER, STATUS )
                     ENDIF

                  ELSE
                     CALL ECH_SET_PARAMETER( REF_NAME, TYPE, VALUE,
     :                    BOOLEAN_VALUE, STRING, STATUS )
                  ENDIF

               ELSE IF ( IN_RDCTN_FILE( I ) ) THEN
                  IF ( .NOT. USED_LAST_TIME( I ) )
     :               CALL ECH_ACCESS_REDUCTION_FILE( REF_NAME, 'UNMAP',
     :                    TYPE, MAPPED_ADDRESS, STRING, MODULE_NUMBER,
     :                    STATUS )

*           Else if a workspace object, call workspace manager to
*           have it released (no workspace is retained for more than
*           one main module invocation).
               ELSE IF ( IS_WORKSPACE( I ) ) THEN
                  IF ( .NOT. USR_TUNE_CLIENT ) THEN
                     CALL ECH_ACCESS_WORKSPACE( REF_NAME, TYPE,
     :                    MAPPED_ADDRESS, STATUS )
                  END IF
               ENDIF
            ENDIF
            USED_LAST_TIME( I ) = .TRUE.

         ELSE
            USED_LAST_TIME( I ) = .FALSE.
         END IF
      END DO

      IF ( CALLER_NAME .EQ. 'DUMMY_EXTRES' .OR.
     :     CALLER_NAME .EQ. 'DUMMY_SCRRES' .OR.
     :     CALLER_NAME .EQ. 'DUMMY_ESPECT' ) THEN
         CALL ECH_ACCESS_REDUCTION_FILE( 'ECH_RDUCD', 'CLOSE', TYPE,
     :        MAPPED_ADDRESS, STRING, MODULE_NUMBER, STATUS )
      ENDIF

 999  CONTINUE

      IF ( USR_TUNE_CLIENT ) MESSAGE_LENGTH = 0
      IF ( REM_STATUS .NE. 0 ) STATUS = REM_STATUS

      END
