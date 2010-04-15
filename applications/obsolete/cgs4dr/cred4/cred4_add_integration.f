*+  CRED4_ADD_INTEGRATION - Add integration to observation
      SUBROUTINE CRED4_ADD_INTEGRATION( INTEGRATION, ASK, WAIT, STATUS )
*    Description :
*     Makes the RED4 task add the given reduced integration into the
*     co-added observation.
*    Invocation :
*     CALL CRED4_ADD_INTEGRATION (INTEGRATION, ASK, WAIT, STATUS)
*    Deficiencies :
*     The OBSERVATION_AVAILABLE flag is only set if this routine
*     waits for the addition to complete. Otherwise it does not
*     know if the task has been completed successfully.
*    Authors :
*     J.Lightfoot (REVAD::JFL)
*     S.M.Beard (REVAD::SMB)
*     P.N.Daly (JACH::PND)
*    History :
*     September 1989 : Original version.                      (JFL)
*     10-Jan-1990: History added. ERR_REP used instead of
*                  MSG_OUT for errors. More specific PROCEED
*                  parameter used.                            (SMB)
*     11-Jan-1990: Bug fix - PAR_CANCL used before asking
*                  whether to proceed.                        (SMB)
*      9-Mar-1990: DSA__ERROR parameter added. Code spaced out
*                  more. Error messages made more meaningful
*                  and tokens used. Do not allow DSA__ERROR to
*                  be returned to ADAM.                       (SMB)
*     29-Mar-1990: OBSERVATION_AVAILABLE flag added.          (SMB)
*     18-Jun-1990: Minor changes.                             (SMB)
*      8-Aug-1990: I have just discovered that ERR_REP resets
*                  STATUS back to SAI__OK, which messes up
*                  the error handling. Mistake fixed.         (SMB)
*      2-Oct-1990: Spotted some typing mistakes and fixed them.
*                  Messages commented out to make the routine
*                  less verbose.                              (SMB)
*     22-Nov-1990: Wavelength calibration parameters added.   (SMB)
*     22-Nov-1990: VERBOSE flag added.                        (SMB)
*     26-Nov-1990: Typing mistake fixed.                      (SMB)
*     14-Apr-1992: Add BIAS_MODE, DARK_MODE, FLAT_MODE,
*                  CALIB_MODE, STANDARD_MODE, SPECIFIED_BIAS,
*                  SPECIFIED_DARK, SPECIFIED_FLAT,
*                  SPECIFIED_CALIB and SPECIFIED_STD
*                  parameters, for DRIMP/5.1 and DRIMP/5.2.   (SMB)
*     11-Feb-1993: Enforce error strategy                     (PND)
*     28-Jun-1993: Pass full string                           (PND)
*     10-Dec-1993: Add task aliasing                          (PND)
*     30-Aug-1994: Port to Unix (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DTASK_ERR'
*    Import :
      CHARACTER*(*) INTEGRATION     ! The name of the integration
      LOGICAL ASK                   ! T if should ask user before proceeding
      LOGICAL WAIT                  ! T if should wait for reduction action
*    Status :
      INTEGER STATUS
*    External references :
      INTEGER CHR_LEN               ! Character length determining function
*    Global variables :
      INCLUDE 'CRED4COM.INC'        ! CRED4 common block
*    Local variables :
      LOGICAL PROCEED               ! T if reduction step is to proceed
      INTEGER ERR_STATUS            ! An error status
      INTEGER CPOS                  ! Position in character string
      INTEGER CLEN                  ! Non-blank length of character string
      CHARACTER*( MSG_VAL_LEN )
     :  INVAL,                      ! Command line parameter string
     :  OUTVAL                      ! Returned string
*-

*    Return if status on entry is bad
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Get task alias required by this routine
      CALL PAR_GET0C( 'RED4_ALIAS', RED4_ALIAS, STATUS )

*    Check that the REDUCTION_OK flag is good, if not return.
      IF ( .NOT. REDUCTION_OK ) RETURN

*    Issue a message
      IF ( VERBOSE ) THEN
         CALL MSG_SETC( 'INTEGRATION', INTEGRATION )
         CALL MSG_OUT (' ', 'Adding integration ^INTEGRATION', STATUS)
      END IF

*    If necessary ask user if they wish to proceed
      IF ( ASK ) THEN
         CALL PAR_CANCL ('PROCEED_ADD', STATUS )
         CALL PAR_GET0L ('PROCEED_ADD', PROCEED, STATUS)
      ELSE
         PROCEED = .TRUE.
      ENDIF

      IF ( PROCEED ) THEN

*       If the RED4_ACTIVE is true then a RED4 action is in progress so wait
         IF ( RED4_ACTIVE ) THEN

            CALL TASK_DONE( -1, RED4_PATH, RED4_MESSID, OUTVAL, STATUS )

            RED4_ACTIVE = .FALSE.

            IF ( STATUS .NE. DTASK__ACTCOMPLETE ) THEN
               IF ( STATUS .EQ. DTASK__ACTINFORM ) THEN
                  ERR_STATUS = STATUS
                  STATUS = SAI__ERROR
                  CALL MSG_SETI( 'ES', ERR_STATUS )
                  CALL ERR_REP (' ', 'CRED4_ADD_INTEGRATION: '/
     :              /'First failure reported by RED4 '/
     :              /'(Status = ^ES< message follows)', STATUS )
                  CALL MSG_SETC( 'OUTVAL', OUTVAL )
                  CALL ERR_REP( ' ', 'CRED4_ADD_INTEGRATION: '/
     :              /'^OUTVAL', STATUS )
               ELSE IF ( STATUS .EQ. DSA__ERROR ) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP( ' ', 'CRED4_ADD_INTEGRATION: '/
     :               /'First DSA error has occured', STATUS )
               ELSE
                  ERR_STATUS = STATUS
                  STATUS = SAI__ERROR
                  CALL MSG_SETI( 'ES', ERR_STATUS )
                  CALL ERR_REP( ' ', 'CRED4_ADD_INTEGRATION: '/
     :               /'Failed to finish previous action (Status = ^ES)', STATUS )
               ENDIF
            ELSE
               CALL ERR_ANNUL( STATUS )
            ENDIF
         ENDIF

*       Tell the RED4 task to add this integration
         INVAL =  ' '
         CPOS = 0
         CALL CHR_PUTC( 'INTFILE="', INVAL, CPOS )
         CLEN = LEN( INTEGRATION )
         CALL CHR_PUTC( INTEGRATION(1:CLEN), INVAL, CPOS )

         CALL CHR_PUTC( '" TO_WAVELENGTH=', INVAL, CPOS )
         CLEN = MAX( 1, CHR_LEN( TO_WAVELENGTH ) )
         CALL CHR_PUTC( TO_WAVELENGTH(1:CLEN), INVAL, CPOS )

         IF ( TO_WAVELENGTH .NE. 'NO' ) THEN

            CALL CHR_PUTC( ' LAMBDA_METHOD=', INVAL, CPOS )
            CLEN = MAX( 1, CHR_LEN( LAMBDA_METHOD ) )
            CALL CHR_PUTC( LAMBDA_METHOD(1:CLEN), INVAL, CPOS )

            CALL CHR_PUTC( ' CALIB_MODE=', INVAL, CPOS )
            CLEN = MAX( 1, CHR_LEN( CALIB_MODE ) )
            CALL CHR_PUTC( CALIB_MODE(1:CLEN), INVAL, CPOS )

            IF ( CALIB_MODE .EQ. 'SPECIFIED' ) THEN

               CALL CHR_PUTC( ' SPECIFIED_CALIB=', INVAL, CPOS )
               CLEN = MAX( 1, CHR_LEN( SPECIFIED_CALIB ) )
               CALL CHR_PUTC( SPECIFIED_CALIB(1:CLEN), INVAL, CPOS )
            END IF
         END IF

         CALL TASK_OBEY( RED4_ALIAS, 'ADD_INT', INVAL(1:CPOS), OUTVAL,
     :      RED4_PATH, RED4_MESSID, STATUS )

*       Check that the action started OK, if not set the REDUCTION_OK flag
         IF ( STATUS .NE. DTASK__ACTSTART ) THEN
            ERR_STATUS = STATUS
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'ES', ERR_STATUS )
            CALL ERR_REP( ' ', 'CRED4_ADD_INTEGRATION: '/
     :         /'Failed to start ADD_INT action (Status = ^ES)', STATUS )
            IF ( WAIT ) REDUCTION_OK = .FALSE.
         ELSE

*          Set global flag to indicate that a RED4 action is in progress
            CALL ERR_ANNUL( STATUS )
            RED4_ACTIVE = .TRUE.

*          If necessary, wait for action to finish, check completion
            IF ( WAIT ) THEN

               CALL TASK_DONE( -1, RED4_PATH, RED4_MESSID, OUTVAL, STATUS )

               RED4_ACTIVE = .FALSE.

               IF ( STATUS .NE. DTASK__ACTCOMPLETE ) THEN

                  REDUCTION_OK = .FALSE.

                  IF ( STATUS .EQ. DTASK__ACTINFORM ) THEN
                     ERR_STATUS = STATUS
                     STATUS = SAI__ERROR
                     CALL MSG_SETI( 'ES', ERR_STATUS )
                     CALL ERR_REP( ' ', 'CRED4_ADD_INTEGRATION: '/
     :                 /'Second failure reported by RED4 task '/
     :                 /'(Status = ^ES, message follows)', STATUS )
                     CALL MSG_SETC( 'OUTVAL', OUTVAL )
                     CALL ERR_REP( ' ', 'CRED4_ADD_INTEGRATION: '/
     :                 /'OUTVAL', STATUS )
                  ELSE IF (STATUS .EQ. DSA__ERROR) THEN
                     STATUS = SAI__ERROR
                     CALL ERR_REP( ' ', 'CRED4_ADD_INTEGRATION: '/
     :                  /'Second DSA error has occured', STATUS )
                  ELSE
                     ERR_STATUS = STATUS
                     STATUS = SAI__ERROR
                     CALL MSG_SETI( 'ES', ERR_STATUS )
                     CALL ERR_REP( ' ', 'CRED4_ADD_INTEGRATION: '/
     :                  /'Failed to add integration (Status = ^ES)', STATUS )
                  ENDIF
               ELSE

*               The integration has been added OK.
                  CALL ERR_ANNUL( STATUS )
                  OBSERVATION_AVAILABLE = .TRUE.
                  REDUCTION_OK = .TRUE.
               ENDIF
            ENDIF
         ENDIF
      ENDIF

      END
