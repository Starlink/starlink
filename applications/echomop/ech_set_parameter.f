      SUBROUTINE ECH_SET_PARAMETER(
     :           REQUIRED_OBJECT,
     :           TYPE,
     :           VALUE,
     :           BOOLEAN,
     :           STRING,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_SET_PARAMETER

*  Purpose:
*     Set parameter to required value.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ECH_SET_PARAMETER(
*     :    REQUIRED_OBJECT,
*     :    TYPE,
*     :    VALUE,
*     :    BOOLEAN,
*     :    STRING,
*     :    STATUS
*     :   )

*  Arguments:
*     REQUIRED_OBJECT = CHAR*( * ) (Given)
*        The parameter to be set.
*     TYPE = CHAR*( * ) (Given)
*        The storage type of the parameter.
*     VALUE = REAL (Given)
*        Value for numeric type parameter.
*     BOOLEAN = LOGICAL (Given)
*        Value for logical type parameter.
*     STRING = CHAR*( * ) (Given)
*        Value for character-like type parameter.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DMILLS: Dave Mills (UCL, Starlink)
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     01-SEP-1992 (DMILLS):
*       Initial release.
*     13-JUN-1996 (MJC):
*       Added checking for type 'INT' and removed redundant DST handling.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_REPORT.INC'
      INCLUDE 'ECH_USE_RDCTN.INC'

*  Arguments Given:
      CHARACTER*( * ) REQUIRED_OBJECT
      CHARACTER*( * ) TYPE
      REAL VALUE
      CHARACTER*( * ) STRING
      LOGICAL BOOLEAN

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER I
      INTEGER OBJECT_NUMBER
      INTEGER NEW_ACCESS_COUNT

      LOGICAL OBJECT_ACTIVE

*  Functions Called:
      LOGICAL ECH_FATAL_ERROR
      INTEGER ECH_OBJ_IND
*.

*  Return immediately if status is not good.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

      STATUS = 0

*  Check if object has already got an access active.
      OBJECT_ACTIVE = .FALSE.
      I = 1
      DO WHILE ( .NOT. OBJECT_ACTIVE .AND. I .LE. ACCESS_COUNT )
         IF ( REQUIRED_OBJECT .EQ. OBJECT_NAME( I ) )  THEN
           OBJECT_ACTIVE = .TRUE.
           OBJECT_NUMBER = I
         ENDIF
         I = I + 1
      END DO

*  If 'forgetting' the parameter.
      IF ( TYPE .EQ. 'CANCEL' ) THEN
         IF ( OBJECT_ACTIVE ) THEN
            OBJECT_NAME( OBJECT_NUMBER ) = ' '
            IF ( ACCESS_COUNT .EQ. OBJECT_NUMBER )
     :         ACCESS_COUNT = ACCESS_COUNT - 1
            CALL PAR_CANCL( REQUIRED_OBJECT, STATUS )
            STATUS = 0
         ENDIF

*  Retrieve the parameter value.
      ELSE
         IF ( .NOT. OBJECT_ACTIVE ) THEN
           NEW_ACCESS_COUNT = ACCESS_COUNT + 1
           ACCESS_COUNT = NEW_ACCESS_COUNT
           OBJECT_NAME( ACCESS_COUNT ) = REQUIRED_OBJECT
           OBJECT_TYPE( ACCESS_COUNT ) = TYPE
           OBJECT_SIZE( ACCESS_COUNT ) = -1
           OBJECT_ADDRESS( ACCESS_COUNT ) = -1
           OBJECT_INDEX( ACCESS_COUNT ) =
     :           ECH_OBJ_IND( REQUIRED_OBJECT )
           OBJECT_ACTIVE = .TRUE.
           OBJECT_NUMBER = ACCESS_COUNT
         END IF
         IF ( TYPE .EQ. 'NAME' .OR. TYPE .EQ. 'CHAR' .OR.
     :        TYPE .EQ. 'ALPHA' .OR. TYPE .EQ. 'TEXT' ) THEN
            IF ( REQUIRED_OBJECT( :4 ) .NE. 'ECH_' ) THEN
               CALL PAR_DEF0C( REQUIRED_OBJECT, STRING, STATUS )
               CALL PAR_PUT0C( REQUIRED_OBJECT, STRING, STATUS )
            END IF

         ELSE IF ( TYPE .EQ. 'KEY' .OR. TYPE .EQ. 'LOGICAL' .OR.
     :             TYPE .EQ. 'FLAG' .OR. TYPE .EQ. 'BOOLEAN' ) THEN
            CONTINUE

         ELSE IF ( TYPE .EQ. 'INT' ) THEN
            CALL PAR_DEF0I( REQUIRED_OBJECT, INT( VALUE ), STATUS )
            CALL PAR_PUT0I( REQUIRED_OBJECT, INT( VALUE ), STATUS )

         ELSE
            CALL PAR_DEF0R( REQUIRED_OBJECT, VALUE, STATUS )
            CALL PAR_PUT0R( REQUIRED_OBJECT, VALUE, STATUS )
         END IF

*     Objects reference variables must be set to the appropriate value.
         CALL ECH_SETUP_OBJECT_REF( REQUIRED_OBJECT, 0, 0, 0, STRING,
     :        VALUE, BOOLEAN, STATUS )
      END IF

      END
