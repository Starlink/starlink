      SUBROUTINE ECH_MODULE_INIT( CALLER_NAME, STATUS )
*+
*  Name:
*     ECHOMOP - ECH_MODULE_INIT

*  Purpose:
*     Manages object/module initialisation.

*  Description:
*     This routine is called by every processing module to ensure that any
*     required data/reduction database mappings are made.

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Variables and Constants:
      INCLUDE 'ECH_USE_RDCTN.INC'
      INCLUDE 'ECH_MODULES.INC'
      INCLUDE 'ECH_MAPPING.INC'
      INCLUDE 'ECH_REPORT.INC'
      INCLUDE 'ECH_SERVER.INC'

*  Local Constants:
      INTEGER MAX_INDEX_OBJECTS
      PARAMETER ( MAX_INDEX_OBJECTS = 42 )

*  Arguments:
      CHARACTER*( * ) CALLER_NAME
      INTEGER STATUS

*  Local Variables:
      REAL VALUE
      REAL SWITCH

      INTEGER INDICES( MAX_INDEX_OBJECTS + MAX_REQUIRED_OBJECTS )
      INTEGER DUMDIM( MAX_DIMENSIONS )
      INTEGER MODULE_NUMBER
      INTEGER I
      INTEGER J
      INTEGER EOI
      INTEGER JLIMIT
      INTEGER MAPPED_ADDRESS
      INTEGER MAPPED_QADDRESS
      INTEGER MAPPED_EADDRESS
      INTEGER DEFAULT_INDEX
      INTEGER STAT2

      LOGICAL USED( MAX_REQUIRED_OBJECTS )
      LOGICAL SWITCHED_OFF
      LOGICAL BOOLEAN_VALUE
      LOGICAL ABORTING

      CHARACTER*80 REF_NAME
      CHARACTER*80 SAVE_REF_NAME
      CHARACTER*80 STRING
      CHARACTER*32 TYPE
      CHARACTER*32 SAVE_TYPE
      CHARACTER*16 INDEX_OBJECT( MAX_INDEX_OBJECTS )

*  Functions Called:
      INTEGER ECH_OBJ_IND
      LOGICAL ECH_FATAL_ERROR
      LOGICAL MODULE_NEEDS
      LOGICAL MODULE_UPDATES
      LOGICAL MODULE_NEEDS_QUALITY
      LOGICAL MODULE_NEEDS_ERRORS
      LOGICAL NEEDS_QUALITY
      LOGICAL NEEDS_ERRORS

*  Data Statements:
      DATA INDEX_OBJECT
     :     / 'TUNE_CLONE',
     :       'ECH_RDCTN',
     :       'TUNE_DIAGNOSE',
     :       'NO_OF_ORDERS',
     :       'NX_PIXELS',
     :       'NREF_FRAME',
     :       'NO_OF_BINS',
     :       'NX_REBIN',
     :       'NXBAD',
     :       'NYBAD',
     :       'TRACIM',
     :       'OBJECT',
     :       'ARC',
     :       'STAR',
     :       'FFIELD',
     :       'TUNE_MAXPOLY',
     :       'TUNE_XZONE',
     :       'TUNE_YZONE',
     :       'TUNE_MXSKYPIX',
     :       'TUNE_PFLSSAMP',
     :       'TUNE_MAX2DPNTS',
     :       'TUNE_MAXRFLN',
     :       'BLZFIT',
     :       'TRCFIT',
     :       'OBJFIT',
     :       'SKYFIT',
     :       'FLTFIT',
     :       'TUNE_MRGWGHT',
     :       'ORDER_YPOS',
     :       'TRC_POLY',
     :       'NUM_ORDERS',
     :       'IDX_NUM_ORDERS',
     :       'NX',
     :       'NY',
     :       'IDX_NREF_FRAME',
     :       'ORDER_NUMBER',
     :       'ECH_ECHAR',
     :       'ECH_RDUCD',
     :       'ECH_FTRDB',
     :       'EFTRDB_IND_SIZ',
     :       'EFTRDB_QIND_SIZ',
     :       'WAVFIT' /
*.

*  Check Global Status.
      IF ( ECH_FATAL_ERROR ( STATUS ) ) RETURN

      DO I = 1, MAX_REQUIRED_OBJECTS
         USED( I ) = .FALSE.
      END DO
      DO I = 1, MAX_DIMENSIONS
         DUMDIM( I ) = 0
      END DO
      MODULE_NUMBER = 0
      MAPPED_ADDRESS = 0
      MAPPED_QADDRESS = 0
      MAPPED_EADDRESS = 0
      DEFAULT_INDEX = 0
      STAT2 = 0

      SWITCHED_OFF = .FALSE.
      BOOLEAN_VALUE = .FALSE.
      ABORTING = .FALSE.

      STATUS = 0
      IF ( USR_TUNE_SERVER .AND. CALLER_NAME .EQ. 'ECH_DUMMY' ) THEN
         RETURN

      ELSE IF ( USR_TUNE_CLIENT .AND. MESSAGE_LENGTH .EQ. 0 ) THEN
         CALL ECH_ACCESS_OBJECT( 'MODULE', 'SEND', 'CHAR', 0, 0, 0,
     :        DUMDIM, MAX_DIMENSIONS, 0, CALLER_NAME, STATUS )
      END IF

*  Locate module by searching the table for its name as passed by the caller.
      IF ( CURRENT_MODULE .EQ. CALLER_NAME ) THEN
         MODULE_NUMBER = CURRENT_MODULE_INDEX

      ELSE
         MODULE_NUMBER = 0
         DO I = 1, MAX_MODULES
            IF ( MODULE_NAME( I ) .EQ. CALLER_NAME ) THEN
               MODULE_NUMBER = I
               GO TO 50
            END IF
         END DO
 50      CONTINUE

*     Do nothing if Module number is undefined.
         IF ( MODULE_NUMBER .EQ. 0 ) THEN
            GO TO 900
         END IF
         CURRENT_MODULE = CALLER_NAME
         CURRENT_MODULE_INDEX = MODULE_NUMBER
      END IF

*  Loop through objects required by the module.  Check for indexing
*  objects first as they may be required to dimension other objects.
*  Other "primary" objects, e.g. TUNE_CLONE, are also checked for
*  prior to processing all other objects for the module.
      I = 0
      DO J = 1, MAX_INDEX_OBJECTS
         EOI = ECH_OBJ_IND( INDEX_OBJECT( J ) )
         IF ( MODULE_NEEDS( EOI, MODULE_NUMBER ) ) THEN
            I = I + 1
            INDICES( I ) = EOI
            USED( EOI ) = .TRUE.
         END IF
      END DO
      DO J = 1, MAX_REQUIRED_OBJECTS
         IF ( MODULE_NEEDS( J, MODULE_NUMBER ) .AND. .NOT.
     :        USED( J ) ) THEN
            I = I + 1
            INDICES( I ) = J
         END IF
      END DO
      JLIMIT = I
      DO J = 1, JLIMIT
         I = INDICES( J )
         ABORTING = ECH_FATAL_ERROR( STATUS )
         IF ( STATUS .EQ. ECH__ABORT_INIT ) THEN
            CALL ECH_REPORT( 0, '!  Abort: user initiated abort.' )
            GO TO 999

         ELSE IF ( ABORTING ) THEN
            CALL ECH_REPORT( 0, '!  Abandon: operation abandoned.' )
            GO TO 999
         END IF

         IF ( USR_TUNE_SERVER .AND. .NOT.IS_WORKSPACE( I ) ) THEN
            CALL ECH_ACCESS_OBJECT( REQUIRED_OBJECTS( I ),
     :           'RECEIVE', ' ', 0, MAPPED_ADDRESS,
     :           MAPPED_QADDRESS, DUMDIM, MAX_DIMENSIONS,
     :           MAPPED_EADDRESS, STRING, STATUS )
            CALL ECH_SETUP_OBJECT_REF( REQUIRED_OBJECTS( I ),
     :           MAPPED_ADDRESS, MAPPED_QADDRESS, MAPPED_EADDRESS,
     :           STRING, %VAL( MAPPED_ADDRESS ),
     :           %VAL( MAPPED_ADDRESS ), STATUS )

         ELSE
            IF ( REQUIRED_OBJECTS( I )( :5 ) .NE. 'TUNE_' .AND.
     :           REQUIRED_OBJECTS( I )( :4 ) .NE. 'ECH_' .AND.
     :           USR_TUNE_CLONE .NE. 'NULL' .AND.
     :           .NOT. IN_RDCTN_FILE( I ) ) THEN
               SWITCHED_OFF = .TRUE.

            ELSE
               SWITCHED_OFF = .FALSE.
               IF ( OBJECT_SWITCH( I ) .GT. 0 ) THEN
                  CALL ECH_GET_PARAMETER(
     :                 REQUIRED_OBJECTS( OBJECT_SWITCH( I ) ),
     :                 'LOGICAL', 0., SWITCHED_OFF, 'SWITCH', 0,
     :                 STAT2 )
               END IF
               IF ( OBJECT_SWITCH( I ) .LT. 0 ) THEN
                  CALL ECH_GET_PARAMETER(
     :                 REQUIRED_OBJECTS( -OBJECT_SWITCH( I ) ),
     :                 'INT', SWITCH, .FALSE., 'SWITCH', 0, STAT2 )
                  IF ( INT( SWITCH ) .EQ. 0 ) SWITCHED_OFF = .TRUE.
               END IF
            END IF
            IF ( .NOT. SWITCHED_OFF ) THEN
               IF ( STATUS .NE. ECH__RETRY_ACCESS ) STATUS = 0
               REF_NAME = REQUIRED_OBJECTS( I )
               TYPE = REQUIRED_TYPE( I )
               DEFAULT_INDEX = DEFAULTS_INDICES( I )
               NEEDS_QUALITY =
     :               MODULE_NEEDS_QUALITY( I, MODULE_NUMBER )
               NEEDS_ERRORS = MODULE_NEEDS_ERRORS( I, MODULE_NUMBER )

*           Type NAME needs translating into an image parameter name
*           (Specifically ECH_ recognises ARC,STAR,FFIELD,OBJECT at
*           present. These are the objects defined with type IMAGE
*           in ECH_DEFINE_MODULES at initialisation).
               SAVE_TYPE = TYPE
               IF ( TYPE .EQ. 'NAME' ) THEN
                  SAVE_REF_NAME = REF_NAME
                  CALL ECH_GET_PARAMETER( REF_NAME, TYPE, VALUE,
     :                 BOOLEAN_VALUE, STRING, DEFAULT_INDEX, STATUS )
                  REF_NAME = STRING
                  TYPE = 'IMAGE'
               END IF

*           If a workspace object, assign space for it.
               IF ( IS_WORKSPACE( I ) ) THEN
                  IF ( .NOT. USR_TUNE_CLIENT ) THEN
                     CALL ECH_ACCESS_WORKSPACE( REF_NAME, TYPE,
     :                    MAPPED_ADDRESS, STATUS )
                  END IF

*           If the object is expected to be in the reduction file
*           then look there.
               ELSE IF ( IN_RDCTN_FILE( I ) .AND.
     :                   STATUS .NE. ECH__IS_ACCESSED ) THEN
                  CALL ECH_ACCESS_REDUCTION_FILE( REF_NAME, 'READ',
     :                 TYPE, MAPPED_ADDRESS, STRING, MODULE_NUMBER,
     :                 STATUS )

*           Otherwise look in an input data file.
               ELSE IF ( STATUS .NE. ECH__IS_ACCESSED ) THEN
 500              CALL ECH_ACCESS_DATA_FILE( REF_NAME, TYPE,
     :                 NEEDS_QUALITY, NEEDS_ERRORS, MAPPED_ADDRESS,
     :                 MAPPED_QADDRESS, MAPPED_EADDRESS, STRING,
     :                 STATUS )
                  IF ( TYPE .EQ. 'IMAGE' .AND.
     :                 STATUS .NE. ECH__IS_ACCESSED .AND.
     :                 .NOT. ECH_FATAL_ERROR( STATUS ) .AND.
     :                 STATUS .NE. 0 ) THEN
                     STATUS = ECH__RETRY_ACCESS
                     GO TO 500
                  END IF
               END IF

*           As a last resort, try to find a user parameter.
               IF ( .NOT. IN_RDCTN_FILE( I ) ) THEN
                  IF ( STATUS .NE. 0 .AND.
     :                 STATUS .NE. ECH__IS_ACCESSED .AND.
     :                 STATUS .NE. ECH__NEED_RDCOBJ .AND.
     :                 .NOT. ECH_FATAL_ERROR( STATUS ) .AND.
     :                 STATUS .NE. ECH__RETRY_ACCESS ) THEN
                     IF ( STATUS .EQ. ECH__WORKSPACE_PAR ) THEN
                        CALL ECH_GET_PARAMETER( REF_NAME, TYPE,
     :                       %VAL( MAPPED_ADDRESS ),
     :                       %VAL( MAPPED_ADDRESS ), STRING,
     :                       DEFAULT_INDEX, STATUS )

                     ELSE
                        CALL ECH_GET_PARAMETER( REF_NAME, TYPE, VALUE,
     :                       BOOLEAN_VALUE, STRING, DEFAULT_INDEX,
     :                       STATUS )
                     END IF
                  END IF
               END IF

*           If required object has been located then set up the
*           relevant address/variable in common.
               IF ( ( STATUS .EQ. 0 .OR.
     :              STATUS .EQ. ECH__IS_ACCESSED ) .AND.
     :              .NOT. ( STATUS .EQ. ECH__IS_ACCESSED .AND.
     :              SAVE_TYPE .EQ. 'NAME' ) ) THEN
               IF ( SAVE_TYPE .EQ. 'NAME' ) THEN
                     CALL ECH_SETUP_OBJECT_REF( SAVE_REF_NAME,
     :                    MAPPED_ADDRESS, MAPPED_QADDRESS,
     :                    MAPPED_EADDRESS, STRING, VALUE,
     :                    BOOLEAN_VALUE, STATUS )
                  END IF
                  CALL ECH_SETUP_OBJECT_REF( REF_NAME,
     :                 MAPPED_ADDRESS, MAPPED_QADDRESS,
     :                 MAPPED_EADDRESS, STRING, VALUE, BOOLEAN_VALUE,
     :                 STATUS )
                  IF ( USR_TUNE_CLIENT .AND.
     :                 IN_RDCTN_FILE( I ) .AND.
     :                 MODULE_UPDATES( I, MODULE_NUMBER) ) THEN
                     CALL ECH_ACCESS_REDUCTION_FILE( REF_NAME,
     :                    'UNMAP', TYPE, MAPPED_ADDRESS, STRING,
     :                    MODULE_NUMBER, STATUS )
                  END IF
               END IF
            END IF
         END IF
      END DO

 900  CONTINUE

      IF ( STATUS .EQ. ECH__IS_ACCESSED ) STATUS = 0

 999  CONTINUE

      END
