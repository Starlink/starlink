*+  QMAN_LIST_SPEC - Lists a specified record
      SUBROUTINE QMAN_LIST_SPEC( STATUS )
*    Invocation :
*     CALL QMAN_LIST_SPEC( STATUS )
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
      INTEGER NUMBER                  ! Record number
*-

*   Return immediately if status bad.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Write out informational message
      CALL MSG_BLANK( STATUS )
      CALL MSG_OUT( ' ', 'Rec# Timestamp             Command', STATUS )
      CALL MSG_OUT( ' ', '---- ---------             -------', STATUS )
      CALL MSG_BLANK( STATUS )

*   Get the record number
      CALL PAR_GET0I( 'NUMBER', NUMBER, STATUS )

*   List the specified range of records
      CALL MSG_FMTI( 'RECNUM', 'I4', NUMBER )
      CALL MSG_FMTD( 'DATE', 'F21.14', DATEQ( NUMBER ) )
      CALL MSG_SETC( 'COMMAND', CHARQ( NUMBER ) )
      CALL MSG_OUT( ' ', '^RECNUM ^DATE ^COMMAND', STATUS )

*    Exit subroutine
      END
