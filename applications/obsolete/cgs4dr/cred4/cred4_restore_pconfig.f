*+  CRED4_RESTORE_PCONFIG - Restore a plotting config
      SUBROUTINE CRED4_RESTORE_PCONFIG( STATUS )
*    Description :
*     Instructs the P4 D-task to restore a config
*    Invocation :
*     CALL CRED4_RESTORE_PCONFIG( STATUS)
*    Author(s) :
*     P.N.Daly (JACH::PND)
*    History :
*     12-Apr-1996: Original version (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
      INCLUDE 'DTASK_ERR'
*    Import :
      LOGICAL ASK               ! T if should ask user before proceeding
      LOGICAL WAIT              ! T if should wait for reduction action
      INTEGER STATUS            ! Inherited ADAM status
*    Global variables :
      INCLUDE 'CRED4COM.INC'    ! CRED4 common block
*    External references :
      INTEGER CHR_LEN
*    Local variables :
      LOGICAL PROCEED           ! T if reduction step is to proceed
      CHARACTER*( MSG_VAL_LEN )
     :  INVAL,                  ! Input string
     :  OUTVAL                  ! Returned string
      CHARACTER*100
     :  CONFIG_FILE             ! Name of configuration file
      INTEGER ERR_STATUS
*-

*    Return immediately if status on entry is bad
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Get the plotting task alias and initialise output string
      CALL PAR_GET0C( 'P4_ALIAS', P4_ALIAS, STATUS )
      CALL PAR_GET0C( 'CONFIG_FILE', CONFIG_FILE, STATUS )
      CALL CHR_FILL( ' ', INVAL )
      CALL CHR_FILL( ' ', OUTVAL )

*    Set inval to the parameter string required by P4s restore command
      INVAL = 'FILE="'//CONFIG_FILE(1:CHR_LEN(CONFIG_FILE))//'" PORT=-1'

*    if necessary ask user if they wish to proceed
      ASK = .FALSE.
      WAIT = .TRUE.
      IF ( ASK ) THEN
         CALL PAR_CANCL( 'PROCEED_DISP', STATUS )
         CALL PAR_GET0L( 'PROCEED_DISP', PROCEED, STATUS )
      ELSE
         PROCEED = .TRUE.
      ENDIF

      IF ( PROCEED ) THEN

*       Ensure that the P4_BUSY flag is set for the duration of this routine at least.
         CALL NBS_PUT_VALUE( P4_BUSY_ID, 0, VAL__NBI, .TRUE., STATUS )

*       if the P4_ACTIVE flag is true then a P4 action is in progress
*       so wait for it to finish before proceeding. Failure of that action
*       is clearly not important to the rest of the reduction sequence
*       because we haven't had to wait for it to finish before proceeding,
         IF ( P4_ACTIVE ) THEN

            CALL TASK_DONE( -1, P4_PATH, P4_MESSID, OUTVAL, STATUS )

            P4_ACTIVE = .FALSE.

            IF ( STATUS.NE.DTASK__ACTCOMPLETE ) THEN
               IF ( STATUS.EQ.DTASK__ACTINFORM ) THEN
                  ERR_STATUS = STATUS
                  STATUS = SAI__ERROR
                  CALL MSG_SETI( 'ES', ERR_STATUS )
                  CALL ERR_REP( ' ', 'CRED4_RESTORE_PCONFIG: '/
     :                /'Failure reported from plot task '/
     :               /'(Status = ^ES, message follows)', STATUS )
                  CALL MSG_SETC( 'OUTVAL', OUTVAL )
                  CALL ERR_REP( ' ', 'CRED4_RESTORE_PCONFIG: '/
     :               /'^OUTVAL', STATUS )
               ELSE IF ( STATUS.EQ.DSA__ERROR ) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP( ' ', 'CRED4_RESTORE_PCONFIG: '/
     :               /'DSA error occurred in plot task', STATUS )
               ELSE
                  ERR_STATUS = STATUS
                  STATUS = SAI__ERROR
                  CALL MSG_SETI( 'ES', ERR_STATUS )
                  CALL ERR_REP( ' ', 'CRED4_RESTORE_PCONFIG: '/
     :              /'Plot task failed in some way (Status = ^ES)', STATUS )
               ENDIF
            ELSE
               CALL ERR_ANNUL( STATUS )
            ENDIF
         ENDIF

*       Set the action going
         IF ( VERBOSE) THEN
           CALL MSG_SETC( 'CONFIG', CONFIG_FILE )
           CALL MSG_OUT( ' ', 'Restoring plotting configuration from ^CONFIG', STATUS )
         ENDIF
         CALL TASK_OBEY( P4_ALIAS(1:CHR_LEN(P4_ALIAS)), 'RESTORE',
     :      INVAL(1:CHR_LEN(INVAL)), OUTVAL, P4_PATH, P4_MESSID, STATUS )

         IF (STATUS .NE. DTASK__ACTSTART) THEN
            ERR_STATUS = STATUS
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'ES', ERR_STATUS )
            CALL ERR_REP( ' ', 'CRED4_RESTORE_PCONFIG: '/
     :         /'Failed to start plotting action (Status = ^ES)', STATUS )
         ELSE

*          reset status and set global flag to indicate that a P4 action is in progress
            CALL ERR_ANNUL( STATUS )
            P4_ACTIVE = .TRUE.

*          if necessary, wait for action to finish, check completion
            IF ( WAIT ) THEN

               CALL TASK_DONE( -1, P4_PATH, P4_MESSID, OUTVAL, STATUS )
               P4_ACTIVE = .FALSE.

               IF ( STATUS.NE.DTASK__ACTCOMPLETE ) THEN
                  IF ( STATUS.EQ.DTASK__ACTINFORM ) THEN
                     ERR_STATUS = STATUS
                     STATUS = SAI__ERROR
                     CALL MSG_SETI( 'ES', ERR_STATUS )
                     CALL ERR_REP( ' ', 'CRED4_RESTORE_PCONFIG: '/
     :                  /'Failure reported by P4  task '/
     :                  /'(Status = ^ES, message follows)', STATUS )
                     CALL MSG_SETC( 'OUTVAL', OUTVAL )
                     CALL ERR_REP( ' ', 'CRED4_RESTORE_PCONFIG: '/
     :                  /'^OUTVAL', STATUS )
                  ELSE IF ( STATUS.EQ.DSA__ERROR ) THEN
                     STATUS = SAI__ERROR
                     CALL ERR_REP( ' ', 'CRED4_RESTORE_PCONFIG: '/
     :                  /'DSA error occurred', STATUS )
                  ELSE
                     ERR_STATUS = STATUS
                     STATUS = SAI__ERROR
                     CALL MSG_SETI( 'ES', ERR_STATUS )
                     CALL ERR_REP( ' ', 'CRED4_RESTORE_PCONFIG: '/
     :                  /'Plot task failed to display data (Status =^ES)', STATUS )
                  ENDIF
               ELSE
                  CALL ERR_ANNUL( STATUS )
               ENDIF

*            Set a dummy success status, so the P4_BUSY flag will be unset.
               CALL ERR_ANNUL( STATUS )
               CALL MSG_OUT( ' ', 'Plotting configuration restored OK', STATUS )
               CALL NBS_PUT_VALUE( P4_BUSY_ID, 0, VAL__NBI, .FALSE., STATUS )
            ENDIF
         ENDIF
      ENDIF
      END
