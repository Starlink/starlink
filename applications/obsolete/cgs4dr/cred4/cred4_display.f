*+  CRED4_DISPLAY - Display the named observation
      SUBROUTINE CRED4_DISPLAY( INVAL, TYPE, ASK, WAIT, STATUS )
*    Description :
*     Instructs the P4 D-task to plot the given data
*    Invocation :
*     CALL CRED4_DISPLAY( INVAL, TYPE, ASK, WAIT, STATUS)
*    Bugs :
*     P.N.Daly (JACH::PND)
*    History :
*     01-Aug-1994: New routine to replace CRED4_DISPLAY_xxx   (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
      INCLUDE 'DTASK_ERR'
*    Import :
      CHARACTER*(*) INVAL       ! Input string (passed straight through to task)
      CHARACTER*(*) TYPE        ! The type of the data to be plotted
      LOGICAL ASK               ! T if should ask user before proceeding
      LOGICAL WAIT              ! T if should wait for reduction action
      INTEGER STATUS            ! Inherited ADAM status
*    Global variables :
      INCLUDE 'CRED4COM.INC'    ! CRED4 common block
*    External references :
      INTEGER CHR_LEN
*    Local variables :
      LOGICAL PROCEED           ! T if reduction step is to proceed
      LOGICAL AVAILABLE         ! T if a file is available to be displayed
      CHARACTER*( MSG_VAL_LEN )
     :  OUTVAL                  ! Returned string
      CHARACTER*20 COMMAND      ! Command to send
      INTEGER ERR_STATUS, IPOS
*-

*    Return immediately if status on entry is bad
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Get the plotting task alias and initialise output string
      CALL PAR_GET0C( 'P4_ALIAS', P4_ALIAS, STATUS )
      CALL CHR_FILL( ' ', OUTVAL )

*    Make sure INVAL contains parameters only and not DISPLAY  or OVERGRAPH command
      IPOS = 0
      CALL CHR_LDBLK( INVAL )
      IPOS = INDEX( INVAL, 'DISPLAY' )
      IF ( IPOS .NE. 0 ) THEN
        COMMAND = 'DISPLAY'
        CALL CHR_PUTC( '       ', INVAL, IPOS-1 )
      ENDIF
      IPOS = INDEX( INVAL, 'display' )
      IF ( IPOS .NE. 0 ) THEN
        COMMAND = 'DISPLAY'
        CALL CHR_PUTC( '       ', INVAL, IPOS-1 )
      ENDIF
      IPOS = INDEX( INVAL, 'OVERGRAPH' )
      IF ( IPOS .NE. 0 ) THEN
        COMMAND = 'OVERGRAPH'
        CALL CHR_PUTC( '         ', INVAL, IPOS-1 )
      ENDIF
      IPOS = INDEX( INVAL, 'overgraph' )
      IF ( IPOS .NE. 0 ) THEN
        COMMAND = 'OVERGRAPH'
        CALL CHR_PUTC( '         ', INVAL, IPOS-1 )
      ENDIF
      CALL CHR_LDBLK( INVAL )

*    See if we have a suitable plot-file
      IF ( TYPE.EQ.'INTEGRATION' .AND. INTEGRATION_AVAILABLE ) THEN
        AVAILABLE = .TRUE.
      ELSE IF ( TYPE.EQ.'integration' .AND. INTEGRATION_AVAILABLE ) THEN
        AVAILABLE = .TRUE.
      ELSE IF ( TYPE.EQ.'OBSERVATION' .AND. OBSERVATION_AVAILABLE ) THEN
        AVAILABLE = .TRUE.
      ELSE IF ( TYPE.EQ.'observation' .AND. OBSERVATION_AVAILABLE ) THEN
        AVAILABLE = .TRUE.
      ELSE IF ( TYPE.EQ.'GROUP' .AND. GROUP_AVAILABLE ) THEN
        AVAILABLE = .TRUE.
      ELSE IF ( TYPE.EQ.'group' .AND. GROUP_AVAILABLE ) THEN
        AVAILABLE = .TRUE.
      ELSE IF ( TYPE.EQ.'SPECTRUM' .AND. SPECTRUM_AVAILABLE ) THEN
        AVAILABLE = .TRUE.
      ELSE IF ( TYPE.EQ.'spectrum' .AND. SPECTRUM_AVAILABLE ) THEN
        AVAILABLE = .TRUE.
      ELSE IF ( TYPE.EQ.'FILE' ) THEN
        AVAILABLE = .TRUE.
      ELSE IF ( TYPE.EQ.'file' ) THEN
        AVAILABLE = .TRUE.
      ELSE
        AVAILABLE = .FALSE.
      ENDIF

      IF ( AVAILABLE ) THEN

*      Issue a message
         CALL MSG_SETC( 'INVAL', INVAL )
         CALL MSG_OUT( ' ', 'Displaying ^INVAL', STATUS )

*       if necessary ask user if they wish to proceed
         IF ( ASK ) THEN
            CALL PAR_CANCL( 'PROCEED_DISP', STATUS )
            CALL PAR_GET0L( 'PROCEED_DISP', PROCEED, STATUS )
         ELSE
            PROCEED = .TRUE.
         ENDIF

         IF ( PROCEED ) THEN

*          Ensure that the P4_BUSY flag is set for the duration of this routine at least.
            CALL NBS_PUT_VALUE( P4_BUSY_ID, 0, VAL__NBI, .TRUE., STATUS )

*          if the P4_ACTIVE flag is true then a P4 action is in progress
*          so wait for it to finish before proceeding. Failure of that action
*          is clearly not important to the rest of the reduction sequence
*          because we haven't had to wait for it to finish before proceeding,
            IF ( P4_ACTIVE ) THEN

               CALL TASK_DONE( -1, P4_PATH, P4_MESSID, OUTVAL, STATUS )

               P4_ACTIVE = .FALSE.

               IF ( STATUS.NE.DTASK__ACTCOMPLETE ) THEN
                  IF ( STATUS.EQ.DTASK__ACTINFORM ) THEN
                     ERR_STATUS = STATUS
                     STATUS = SAI__ERROR
                     CALL MSG_SETI( 'ES', ERR_STATUS )
                     CALL ERR_REP( ' ', 'CRED4_DISPLAY: '/
     :                  /'Failure reported from plot task '/
     :                  /'(Status = ^ES, message follows)', STATUS )
                     CALL MSG_SETC( 'OUTVAL', OUTVAL )
                     CALL ERR_REP( ' ', 'CRED4_DISPLAY: '/
     :                  /'^OUTVAL', STATUS )
                  ELSE IF ( STATUS.EQ.DSA__ERROR ) THEN
                     STATUS = SAI__ERROR
                     CALL ERR_REP( ' ', 'CRED4_DISPLAY: '/
     :                  /'DSA error occurred in plot task', STATUS )
                  ELSE
                     ERR_STATUS = STATUS
                     STATUS = SAI__ERROR
                     CALL MSG_SETI( 'ES', ERR_STATUS )
                     CALL ERR_REP( ' ', 'CRED4_DISPLAY: '/
     :                 /'Plot task failed to display data (Status = ^ES)', STATUS )
                  ENDIF
               ELSE
                  CALL ERR_ANNUL( STATUS )
               ENDIF
            ENDIF

*          Set the action going
            CALL TASK_OBEY( P4_ALIAS(1:CHR_LEN(P4_ALIAS)), COMMAND(1:CHR_LEN(COMMAND)),
     :         INVAL(1:CHR_LEN(INVAL)), OUTVAL, P4_PATH, P4_MESSID, STATUS )

            IF (STATUS .NE. DTASK__ACTSTART) THEN
               ERR_STATUS = STATUS
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'ES', ERR_STATUS )
               CALL ERR_REP( ' ', 'CRED4_DISPLAY: '/
     :            /'Failed to start plotting action (Status = ^ES)', STATUS )
               IF ( WAIT ) REDUCTION_OK = .FALSE.
            ELSE

*             reset status and set global flag to indicate that a P4 action is in progress
               CALL ERR_ANNUL( STATUS )
               P4_ACTIVE = .TRUE.

*             if necessary, wait for action to finish, check completion
               IF ( WAIT ) THEN

                  CALL TASK_DONE( -1, P4_PATH, P4_MESSID, OUTVAL, STATUS )
                  P4_ACTIVE = .FALSE.

                  IF ( STATUS.NE.DTASK__ACTCOMPLETE ) THEN
                     REDUCTION_OK = .FALSE.
                     IF ( STATUS.EQ.DTASK__ACTINFORM ) THEN
                        ERR_STATUS = STATUS
                        STATUS = SAI__ERROR
                        CALL MSG_SETI( 'ES', ERR_STATUS )
                        CALL ERR_REP( ' ', 'CRED4_DISPLAY: '/
     :                     /'Failure reported by P4  task '/
     :                     /'(Status = ^ES, message follows)', STATUS )
                        CALL MSG_SETC( 'OUTVAL', OUTVAL )
                        CALL ERR_REP( ' ', 'CRED4_DISPLAY: '/
     :                     /'^OUTVAL', STATUS )
                     ELSE IF ( STATUS.EQ.DSA__ERROR ) THEN
                        STATUS = SAI__ERROR
                        CALL ERR_REP( ' ', 'CRED4_DISPLAY: '/
     :                     /'DSA error occurred', STATUS )
                     ELSE
                        ERR_STATUS = STATUS
                        STATUS = SAI__ERROR
                        CALL MSG_SETI( 'ES', ERR_STATUS )
                        CALL ERR_REP( ' ', 'CRED4_DISPLAY: '/
     :                     /'Plot task failed to display data (Status =^ES)', STATUS )
                     ENDIF
                  ELSE
                     CALL ERR_ANNUL( STATUS )
                  ENDIF

*               Set a dummy success status, so the P4_BUSY flag will be unset.
                  CALL ERR_ANNUL( STATUS )
                  CALL NBS_PUT_VALUE( P4_BUSY_ID, 0, VAL__NBI, .FALSE., STATUS )
               ENDIF
            ENDIF
         ENDIF
      ELSE

*      There is no file available to be displayed.
         CALL MSG_OUT( ' ', 'There is no data available to be displayed yet', STATUS )
      ENDIF
      END
