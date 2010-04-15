*+  QMAN_SORT - Sort action for QMAN task
      SUBROUTINE QMAN_SORT( STATUS )
*    Invocation :
*     CALL QMAN_SORT( STATUS )
*    Authors :
*     P. N. Daly (PND@JACH.HAWAII.EDU)
*    History :
*     27-May-1994: Original version                                   (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'               ! Defines SAI__OK and others
      INCLUDE 'MESSYS_LEN'                 ! Defines MSG_VAL_LEN etc
*    Status :
      INTEGER STATUS                  ! Inherited global ADAM status
*    Global variables :
      INCLUDE 'QMAN_GLOBAL.PAR'       ! QMAN common block
      INCLUDE 'QMAN_COMMON.BLK'       ! QMAN global parameter constants
*    Local variables :
      CHARACTER*( MSG_VAL_LEN ) MODE  ! Sort mode
*-

*   Return immediately if status bad.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Check task has been initialised
      CALL QMAN_INIT_DONE( STATUS )

*   Check the password (if specified)
      CALL QMAN_CHECK_PWD( STATUS )

*   Get the sort mode
      CALL CHR_FILL( ' ', MODE )
      CALL PAR_GET0C( 'SORT_MODE', MODE, STATUS )
      CALL CHR_UCASE( MODE )

*   Open it for write access
      IF ( MODE .EQ. 'ASCENDING' ) THEN
        CALL QMAN_SORT_ASC( STATUS )
      ELSE IF ( MODE .EQ. 'DESCENDING' ) THEN
        CALL QMAN_SORT_DES( STATUS )
      ENDIF

*   Exit subroutine
      END
