*+  CRED4_ARCHIVE_OBSERVATION - Archive observation
      SUBROUTINE CRED4_ARCHIVE_OBSERVATION( OBSERVATION, WAIT, STATUS )
*    Description :
*     This routine invokes an action in the RED4 A-task which records
*     the specified observation to the CGS4 observations catalogue.
*
*     (Note that this routine has no ASK argument. Its execution is
*     compulsory).
*    Invocation :
*     CALL CRED4_ARCHIVE_OBSERVATION (OBSERVATION, WAIT, STATUS)
*    Parameters :
*     OBSERVATION  = CHARACTER*(*)( READ )
*        Name of observation to archive.
*     WAIT         = LOGICAL( READ )
*        Logical flag indicating if the task should wait until
*        the archiving operation has finished before proceeding.
*     STATUS       = INTEGER( UPDATE )
*        Global status.
*    Authors :
*     S.M.Beard (REVAD::SMB)
*     P.N.Daly (REVAD::PND)
*    History :
*     31-Jul-1990: Original version (copied from
*                  CRED4_FILE_OBSERVATION).                   (SMB)
*      8-Aug-1990: I have just discovered that ERR_REP resets
*                  STATUS back to SAI__OK, which messes up
*                  the error handling. Mistake fixed.         (SMB)
*     14-Apr-1992: Add MSG_VAL_LEN.                           (SMB)
*     11-Feb-1993: Conform to error strategy                  (PND)
*     28-Jun-1993: Pass full observation string               (PND)
*     10-Dec-1993: Add task alias                             (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DTASK_ERR'
*    Import :
      LOGICAL WAIT                  ! T if should wait for action to finish
      CHARACTER*(*) OBSERVATION     ! The name of the observation to be archived
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'CRED4COM.INC' ! CRED4 common block
*    Local variables :
      INTEGER ERR_STATUS             ! Temporary status for ERR_REP
      CHARACTER*( MSG_VAL_LEN)
     :  INVAL,                      ! Command line parameter string
     :  OUTVAL                      ! Returned string
*-

      IF (STATUS .NE. SAI__OK) RETURN

      CALL PAR_GET0C( 'RED4_ALIAS', RED4_ALIAS, STATUS )

      CALL MSG_SETC( 'OBSERVATION', OBSERVATION )
      CALL MSG_OUT (' ', 'Recording ^OBSERVATION in observation '/
     :  /'catalogue', STATUS)

*    if the RED4_ACTIVE flag is true then a RED4 action is in progress
*    so wait for it to finish before proceeding. Failure of that action
*    is clearly not important to the rest of the reduction sequence
*    because we haven't had to wait for it to finish before proceeding,
*    so if it has failed don't worry beyond printing a warning.

      IF (RED4_ACTIVE) THEN

         CALL TASK_DONE (-1, RED4_PATH, RED4_MESSID, OUTVAL, STATUS)

         RED4_ACTIVE = .FALSE.

         IF (STATUS .NE. DTASK__ACTCOMPLETE) THEN
            IF (STATUS .EQ. DTASK__ACTINFORM) THEN

               ERR_STATUS = STATUS
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'ES', ERR_STATUS )
               CALL ERR_REP( ' ', 'CRED4_ARCHIVE_OBSERVATION: '/
     :            /'Failure reported from RED4 task '/
     :            /'(Status = ^ES, message follows)',STATUS )
               CALL MSG_SETC( 'OUTVAL', OUTVAL )
               CALL ERR_REP( ' ', 'CRED4_ARCHIVE_OBSERVATION: '/
     :            /'^OUTVAL', STATUS )
            ELSE IF (STATUS .EQ. DSA__ERROR) THEN

*             explicit check for DSA error, report but otherwise ignore
               STATUS = SAI__ERROR
               CALL ERR_REP( ' ', 'CRED4_ARCHIVE_OBSERVATION: '/
     :            /'First DSA error has occured', STATUS )
            ELSE

               ERR_STATUS = STATUS
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'ES', ERR_STATUS )
               CALL ERR_REP( ' ', 'CRED4_ARCHIVE_OBSERVATION: '/
     :            /'Failed to archive observation '/
     :            /'(Status = ^ES)', STATUS )
            ENDIF
         ELSE
            CALL ERR_ANNUL( STATUS )
         ENDIF
      ENDIF

*    Tell the RED4 A-task to archive this observation
      INVAL = 'OBSERVATION="' // OBSERVATION // '"'
      CALL TASK_OBEY( RED4_ALIAS, 'ARCHIVE_OBS', INVAL, OUTVAL,
     :   RED4_PATH, RED4_MESSID, STATUS )

*    check that the action started OK, if not report an error
      IF (STATUS .NE. DTASK__ACTSTART) THEN

         ERR_STATUS = STATUS
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'ES', ERR_STATUS )
         CALL ERR_REP( ' ', 'CRED4_ARCHIVE_OBSERVATION: '/
     :      /'Failed to start archiving action '/
     :      /'(Status = ^ES)', STATUS )
      ELSE

*       set global flag to indicate that a RED4 action is in progress
         CALL ERR_ANNUL( STATUS )
         RED4_ACTIVE = .TRUE.

*       if necessary, wait for action to finish, check completion
         IF ( WAIT ) THEN

            CALL TASK_DONE( -1, RED4_PATH, RED4_MESSID, OUTVAL,
     :         STATUS )

            RED4_ACTIVE = .FALSE.

            IF (STATUS .NE. DTASK__ACTCOMPLETE) THEN
               IF (STATUS .EQ. DTASK__ACTINFORM) THEN

                  ERR_STATUS = STATUS
                  STATUS = SAI__ERROR
                  CALL MSG_SETI( 'ES', ERR_STATUS )
                  CALL ERR_REP( ' ', 'CRED4_ARCHIVE_OBSERVATION: '/
     :               /'First failure reported from RED4 task'/
     :               /'(Status = ^ES, message follows)', STATUS )
                  CALL MSG_SETC( 'OUTVAL', OUTVAL )
                  CALL ERR_REP (' ', 'CRED4_ARCHIVE_OBSERVATION: '/
     :               /'^OUTVAL', STATUS )

               ELSE IF (STATUS .EQ. DSA__ERROR) THEN

*                explicit check for DSA error, report but otherwise ignore
                  STATUS = SAI__ERROR
                  CALL ERR_REP( ' ', 'CRED4_ARCHIVE_OBSERVATION '/
     :               /'First DSA error has occured', STATUS )
               ELSE

                  ERR_STATUS = STATUS
                  STATUS = SAI__ERROR
                  CALL MSG_SETI( 'ES', ERR_STATUS )
                  CALL ERR_REP( ' ', 'CRED4_ARCHIVE_OBSERVATION: '/
     :               /'Failed to archive observation'/
     :               /'(Status = ^ES)', STATUS )
               ENDIF
            ELSE

               CALL ERR_ANNUL( STATUS )
            ENDIF
         ENDIF
      ENDIF

      END
