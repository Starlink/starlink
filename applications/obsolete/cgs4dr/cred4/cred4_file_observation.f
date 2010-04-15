*+  CRED4_FILE_OBSERVATION - File observation
      SUBROUTINE CRED4_FILE_OBSERVATION( OBSERVATION, ASK, WAIT, STATUS )
*    Description :
*     This routine invokes an action in the RED4 A-task which files
*     the specified observation.
*    Invocation :
*     CALL CRED4_FILE_OBSERVATION( OBSERVATION, ASK, WAIT, STATUS )
*    Parameters :
*     OBSERVATION  = CHARACTER*(*)( READ )
*        Name of observation to file.
*     ASK          = LOGICAL( READ )
*        Logical flag indicating if the task should prompt for
*        confirmation before invoking the action.
*     WAIT         = LOGICAL( READ )
*        Logical flag indicating if the task should wait until
*        the filing operation has finished before proceeding.
*     STATUS       = INTEGER( UPDATE )
*        Global status.
*    Authors :
*     S.M.Beard (REVAD::SMB)
*     P.N.Daly (JACH::PND)
*    History :
*     10-Jan-1990 : Original version (copied from
*                   CRED4_REDUCE_INTEGRATION).                 (SMB)
*     11-Jan-1990 : Bug fix - PAR_CANCL used before asking
*                   whether to proceed.                        (SMB)
*      9-Mar-1990: DSA__ERROR parameter added. Code spaced out
*                  more. Error messages made more meaningful
*                  and tokens used. Do not allow DSA__ERROR to
*                  be returned to ADAM.                        (SMB)
*      8-Aug-1990: I have just discovered that ERR_REP resets
*                  STATUS back to SAI__OK, which messes up
*                  the error handling. Mistake fixed.          (SMB)
*      2-Oct-1990: Messages commented out to reduce verbosity. (SMB)
*     22-Nov-1990: VERBOSE flag added.                         (SMB)
*     14-Apr-1992: Add MSG_VAL_LEN.                            (SMB)
*     11-Feb-1993: Conform to error strategy                   (PND)
*      6-Dec-1993: Add TYPE to INVAL string                    (PND)
*     10-Dec-1993: Add task alias                              (PND)
*     30-Aug-1994: Port to Unix (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DTASK_ERR'
*    Import :
      LOGICAL ASK                   ! T if should ask user before proceeding
      LOGICAL WAIT                  ! T if should wait for action to finish
      CHARACTER*(*) OBSERVATION     ! The name of the observation
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'CRED4COM.INC' ! CRED4 common block
*    Local variables :
      LOGICAL PROCEED               ! T if filing is to proceed
      CHARACTER*( MSG_VAL_LEN )
     :  INVAL,                      ! Command line parameter string
     :  OUTVAL                      ! Returned string
      INTEGER ERR_STATUS            ! Temporary status for ERR_REP
*-

*    Return if status on entry is not SAI__OK
      IF (STATUS .NE. SAI__OK) RETURN

      CALL PAR_GET0C( 'RED4_ALIAS', RED4_ALIAS, STATUS )

      IF ( VERBOSE ) THEN

         CALL MSG_SETC( 'OBSERVATION', OBSERVATION )
         CALL MSG_OUT (' ', 'Filing observation ^OBSERVATION', STATUS)
      END IF

*    if necessary ask user if they wish to proceed
      IF (ASK) THEN

         CALL PAR_CANCL ('PROCEED_FILE', STATUS )
         CALL PAR_GET0L ('PROCEED_FILE', PROCEED, STATUS)
      ELSE

         PROCEED = .TRUE.
      ENDIF

      IF (PROCEED) THEN

*       if the RED4_ACTIVE flag is true then a RED4 action is in progress
*       so wait for it to finish before proceeding. Failure of that action
*       is clearly not important to the rest of the reduction sequence
*       because we haven't had to wait for it to finish before proceeding,
*       so if it has failed don't worry beyond printing a warning.
         IF (RED4_ACTIVE) THEN

            CALL TASK_DONE (-1, RED4_PATH, RED4_MESSID, OUTVAL, STATUS)

            RED4_ACTIVE = .FALSE.

            IF (STATUS .NE. DTASK__ACTCOMPLETE) THEN
               IF (STATUS .EQ. DTASK__ACTINFORM) THEN

                  ERR_STATUS = STATUS
                  STATUS = SAI__ERROR
                  CALL MSG_SETI( 'ES', ERR_STATUS )
                  CALL ERR_REP( ' ', 'CRED4_FILE_OBSERVATION: '/
     :               /'Failure reported from RED4 task '/
     :               /'(Status = ^ES, message follows)', STATUS )
                  CALL MSG_SETC( 'OUTVAL', OUTVAL )
                  CALL ERR_REP( ' ', 'CRED4_FILE_OBSERVATION: '/
     :               /'^OUTVAL', STATUS )

               ELSE IF (STATUS .EQ. DSA__ERROR) THEN

*                explicit check for DSA error, report but otherwise ignore
                  STATUS = SAI__ERROR
                  CALL ERR_REP( ' ', 'CRED4_FILE_OBSERVATION: '/
     :               /'First DSA error has occured', STATUS )
               ELSE

                  ERR_STATUS = STATUS
                  STATUS = SAI__ERROR
                  CALL MSG_SETI( 'ES', ERR_STATUS )
                  CALL ERR_REP( ' ', 'CRED4_FILE_OBSERVATION: '/
     :               /'Previous data reduction action failed '/
     :               /'(Status = ^ES)', STATUS )
               ENDIF
            ELSE

               CALL ERR_ANNUL( STATUS )
            ENDIF
         ENDIF

*       tell the RED4 A-task to file this observation
         INVAL = 'OBSERVATION="' // OBSERVATION /
     :      / '" TYPE="WHATEVER_IT_IS"'
         CALL TASK_OBEY (RED4_ALIAS, 'FILE_OBS', INVAL, OUTVAL,
     :      RED4_PATH, RED4_MESSID, STATUS)

*       check that the action started OK, if not report an error
         IF (STATUS .NE. DTASK__ACTSTART) THEN

            ERR_STATUS = STATUS
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'ES', ERR_STATUS )
            CALL ERR_REP( ' ', 'CRED4_FILE_OBSERVATION: '/
     :         /'Failed to start filing action '/
     :         /'(status = ^ES)', STATUS )
         ELSE

*          set global flag to indicate that a RED4 action is in progress
            CALL ERR_ANNUL( STATUS )
            RED4_ACTIVE = .TRUE.

*          if necessary, wait for action to finish, check completion
            IF (WAIT) THEN

               CALL TASK_DONE (-1, RED4_PATH, RED4_MESSID, OUTVAL,
     :            STATUS)
               RED4_ACTIVE = .FALSE.

               IF (STATUS .NE. DTASK__ACTCOMPLETE) THEN
                  IF (STATUS .EQ. DTASK__ACTINFORM) THEN

                     ERR_STATUS = STATUS
                     STATUS = SAI__ERROR
                     CALL MSG_SETI( 'ES', ERR_STATUS )
                     CALL ERR_REP( ' ', 'CRED4_FILE_OBSERVATION: '/
     :                  /'Failure reported from RED4 task '/
     :                  /'(Status = ^ES, message follows)', STATUS )
                     CALL MSG_SETC( 'OUTVAL', OUTVAL )
                     CALL ERR_REP( ' ', 'CRED4_FILE_OBSERVATION: '/
     :                  /'^OUTVAL', STATUS )

                  ELSE IF (STATUS .EQ. DSA__ERROR) THEN

*                   explicit check for DSA error, report but otherwise ignore
                     STATUS = SAI__ERROR
                     CALL ERR_REP( ' ', 'CRED4_FILE_OBSERVATION: '/
     :                  /'Second DSA error has occured', STATUS )
                  ELSE

                     ERR_STATUS = STATUS
                     STATUS = SAI__ERROR
                     CALL MSG_SETI( 'ES', ERR_STATUS )
                     CALL ERR_REP( ' ', 'CRED4_FILE_OBSERVATION: '/
     :                  /'Failed to file observation '/
     :                  /'(Status = ^ES)', STATUS )
                  ENDIF
               ELSE
                  CALL ERR_ANNUL( STATUS )
               ENDIF
            ENDIF
         ENDIF
      ENDIF

      END
