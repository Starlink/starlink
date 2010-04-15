*+  CGS3DR_OBEYW - Wrap-up for TASK_OBEY, TASK_DONE.
      SUBROUTINE CGS3DR_OBEYW (TASK_NAME, ACTION, INVAL, OUTVAL,
     : TIMEOUT, STATUS)
*    Invocation:
*      CALL CGS3DR_OBEYW (TASK_NAME, ACTION, INVAL, OUTVAL,
*     : TIMEOUT, STATUS)
*    Parameters:
*     TASK_NAME = CHARACTER*(*) (READ)
*       Name of task to instruct.
*     ACTION = CHARACTER*(*) (READ)
*       Name of action to perform
*     INVAL = CHARACTER*(*) (READ)
*       Input value string, containing parameters
*     OUTVAL = CHARACTER*(*) (WRITE)
*       Output value string, containing possible responses
*     TIMEOUT = INTEGER (READ)
*       Timeout (msecs) to wait after which error is returned.
*     STATUS = INTEGER
*    Authors:
*     Alan Bridger (JAC::AB)
*    History:
*      4-Jan-93: Original (JAC::AB)
*    Type Definitions:
      IMPLICIT NONE
*    Global constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DTASK_ERR'
      INCLUDE 'CGS3DR_CMN'
*    Import:
      CHARACTER*(*)   TASK_NAME
      CHARACTER*(*)   ACTION
      CHARACTER*(*)   INVAL
      INTEGER         TIMEOUT
*    Export:
      CHARACTER*(*)   OUTVAL
*    External References:
      INTEGER CHR_LEN
*    Status:
      INTEGER  STATUS
*    Local variables:
      INTEGER         PATH
      INTEGER         MESSID
      INTEGER         TMP_STATUS
*-

      IF (STATUS .NE. SAI__OK) RETURN

*    In verbose mode, write out calling string
      IF (VERBOSE_PH) THEN
        CALL MSG_SETC( 'TASK_NAME', TASK_NAME(1:CHR_LEN(TASK_NAME)) )
        CALL MSG_SETC( 'ACTION', ACTION(1:CHR_LEN(ACTION)) )
        CALL MSG_SETC( 'INVAL', INVAL(1:CHR_LEN(INVAL)) )
        CALL MSG_SETC( 'OUTVAL', OUTVAL(1:CHR_LEN(OUTVAL)) )
        CALL MSG_SETI( 'TIMEOUT', TIMEOUT )
        CALL MSG_OUT( ' ', 'CGS3DR_OBEYW: '/
     :   /'Called ^TASK_NAME ^ACTION action with parameters INVAL = '/
     :   /'^INVAL, OUTVAL = ^OUTVAL, TIMEOUT = ^TIMEOUT', STATUS )
      ENDIF

*    Call the action
      CALL TASK_OBEY (TASK_NAME, ACTION, INVAL, OUTVAL, PATH,
     :    MESSID, STATUS)
      IF (STATUS .EQ. DTASK__ACTSTART) THEN
        CALL ERR_ANNUL( STATUS )
      ELSE
        TMP_STATUS = STATUS
        STATUS = SAI__ERROR
        CALL MSG_SETI( 'TSTAT', TMP_STATUS )
        CALL MSG_SETC( 'TNAME', TASK_NAME )
        CALL MSG_SETC( 'TACNT', ACTION )
        CALL ERR_REP( ' ', 'CGS3DR_OBEYW: Failed to start '/
     :   /'^TACNT action in ^TNAME task, Status = ^TSTAT', STATUS )
      ENDIF

*    Wait for completion
      CALL TASK_DONE (TIMEOUT, PATH, MESSID, OUTVAL, STATUS)
      IF (STATUS.EQ.DTASK__ACTCOMPLETE) THEN
        CALL ERR_ANNUL( STATUS )
      ELSE
        TMP_STATUS = STATUS
        STATUS = SAI__ERROR
        CALL MSG_SETI( 'TSTAT', TMP_STATUS )
        CALL MSG_SETC( 'TNAME', TASK_NAME )
        CALL MSG_SETC( 'TACNT', ACTION )
        CALL ERR_REP( ' ', 'CGS3DR_OBEYW: Failed to complete '/
     :   /'^TACNT action in ^TNAME task, Status = ^TSTAT', STATUS )
      ENDIF

      END
