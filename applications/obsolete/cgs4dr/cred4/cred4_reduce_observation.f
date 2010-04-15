*+  CRED4_REDUCE_OBSERVATION: Reduce an observation
      SUBROUTINE CRED4_REDUCE_OBSERVATION( OBSERVATION, ASK, WAIT, STATUS)
*    Description :
*     Instructions the RED4 data reduction A-task to reduce the
*     specified observation.
*    Invocation :
*     CALL CRED4_REDUCE_OBSERVATION( OBSERVATION, ASK, WAIT, STATUS)
*    Deficiencies :
*     This routine can only set the OBSERVATION_AVAILABLE flag if asked
*     to wait until the reduction is complete. Otherwise how can it
*     know the reduction has completed REDUCTION_OKly.
*     The variable FF_NORM is currently fudged.
*    Authors :
*     J.Lightfoot (REVAD::JFL)
*     S.M.Beard (REVAD::SMB)
*     P.N.Daly (JACH::PND)
*    History :
*     15-Jan-1990 : Original version, copied from
*                   CRED4_REDUCE_INTEGRATION.             (JFL, SMB)
*      2-Feb-1990: PROCEED_FF flag included.                   (SMB)
*      8-Feb-1990: PROCEED_FF flag moved to RED4. PROMPT_FF
*                  flag added to RED4.                         (SMB)
*     20-Feb-1990: Ability to specify a bad pixel mask added.  (SMB)
*     20-Feb-1990: Null bad pixel mask changed from ' ' to '#' (SMB)
*      8-Mar-1990: Specifying MASK=# on the command line to
*                  RED4 was confusing the parameter system.
*                  Bug fixed by putting the # in quotes, '#'.  (SMB)
*      9-Mar-1990: DSA__ERROR parameter added. Code spaced out
*                  more. Error messages made more meaningful
*                  and tokens used. Do not allow DSA__ERROR to
*                  be returned to ADAM.                        (SMB)
*      4-Apr-1990: Modified to set OBSERVATION_AVAILABLE flag. (SMB)
*      4-May-1990: FF_NORM and ORDER parameters added.         (SMB)
*     20-Jun-1990: Phase 2 of major changes: FF_NORM has
*                  become NORMALISE_FF, which is now part of
*                  the data reduction sequence. The
*                  normalisation method is now given in
*                  NORM_METHOD. As RED4 still expects FF_NORM
*                  a temporary one is set up here.             (SMB)
*     11-Jul-1990: RED4 parameters have now been brought into
*                  line. Modified to use these new parameters. (SMB)
*      8-Aug-1990: I have just discovered that ERR_REP resets
*                  STATUS back to SAI__OK, which messes up
*                  the error handling. Mistake fixed.          (SMB)
*     22-Nov-1990: Wavelength calibration parameters passed
*                  to RED4's RED_OBS action.                   (SMB)
*     26-Nov-1990: Typing mistake fixed.                       (SMB)
*     30-Nov-1990: LINCOEFFS parameter added.                  (SMB)
*      3-Jan-1991: Bad pixel mask and linearisation cofficients
*                  included in noticeboard and configuration.  (SMB)
*     31-Jul-1991: Ability to normalise flat-fields by
*                  smoothing added.                            (SMB)
*     14-Apr-1992: Add BIAS_MODE, DARK_MODE, FLAT_MODE,
*                  CALIB_MODE, STANDARD_MODE, SPECIFIED_BIAS,
*                  SPECIFIED_DARK, SPECIFIED_FLAT,
*                  SPECIFIED_CALIB and SPECIFIED_STD
*                  parameters, for DRIMP/5.1 and DRIMP/5.2.
*                  MSG_VAL_LEN added.                          (SMB)
*     17-Jul-1992: Add SUBTRACT_BIAS option                    (PND)
*     12-Feb-1993: Conform to error strategy                   (PND)
*     28-Jun-1993: Pass full observation string                (PND)
*     10-Dec-1993: Add task alias                              (PND)
*     30-AUG-1994: Port to Unix (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DTASK_ERR'
*    Import :
      LOGICAL ASK                   ! T if should ask user before proceeding
      LOGICAL WAIT                  ! T if should wait for reduction action
      CHARACTER*(*) OBSERVATION     ! The name of the observation to be reduced
*    Status :
      INTEGER STATUS
*    External references :
      INTEGER CHR_LEN               ! Character length determining function
*    Global variables :
      INCLUDE 'CRED4COM.INC'        ! CRED4 common block
*    Local variables :
      LOGICAL PROCEED               ! T if reduction step is to proceed
      CHARACTER*( MSG_VAL_LEN )
     :  INVAL,                      ! Command line parameter string
     :  OUTVAL                      ! Returned string
      INTEGER CPOS                  ! Character position
      INTEGER CLEN                  ! Character length
      INTEGER ERR_STATUS            ! Temporary status for ERR_REP
*-

*    Return if status on entry is bad
      IF (STATUS .NE. SAI__OK) RETURN

*    Get the RED4 task alias
      CALL PAR_GET0C( 'RED4_ALIAS', RED4_ALIAS, STATUS )

*    check that the REDUCTION_OK flag is good, if not return. This flag
*    indicates that a problem has occured in the reduction sequence
*    that means this sequence should progress no further, but not serious
*    enough to imply no other sequences will succeed
      IF ( .NOT. REDUCTION_OK ) RETURN

      CALL MSG_SETC ( 'OBSERVATION', OBSERVATION )
      CALL MSG_OUT (' ', 'Reducing observation ^OBSERVATION',
     :  STATUS)

*    if necessary ask user if they wish to proceed
      IF (ASK) THEN
         CALL PAR_CANCL ('PROCEED_RED', STATUS )
         CALL PAR_GET0L ('PROCEED_RED', PROCEED, STATUS)
      ELSE
         PROCEED = .TRUE.
      ENDIF

*   Check the parameters have been obtained succesfully
      IF ( STATUS .EQ. SAI__OK ) THEN

         IF (PROCEED) THEN

*          if the RED4_ACTIVE flag is true then a RED4 action is in progress
*          so wait for it to finish before proceeding. Failure of that action
*          is clearly not important to the rest of the reduction sequence
*          because we haven't had to wait for it to finish before proceeding,
*          so if it has failed don't worry beyond printing a warning.

            IF (RED4_ACTIVE) THEN

               CALL TASK_DONE (-1, RED4_PATH, RED4_MESSID,
     :           OUTVAL, STATUS)

               RED4_ACTIVE = .FALSE.

               IF (STATUS .NE. DTASK__ACTCOMPLETE) THEN
                  IF (STATUS .EQ. DTASK__ACTINFORM) THEN

                     ERR_STATUS = STATUS
                     STATUS = SAI__ERROR
                     CALL MSG_SETI( 'ES', ERR_STATUS )
                     CALL ERR_REP( ' ', 'CRED4_REDUCE_OBSERVATION: '/
     :                  /'Failure reported by RED4 task '/
     :                  /'(Status = ^ES, message follows)', STATUS )
                     CALL MSG_SETC( 'OUTVAL', OUTVAL )
                     CALL ERR_REP (' ', 'CRED4_REDUCE_OBSERVATION: '/
     :                  /'^OUTVAL', STATUS )

                  ELSE IF (STATUS .EQ. DSA__ERROR) THEN

*                   explicit check for DSA error, report but otherwise ignore
                     STATUS = SAI__ERROR
                     CALL ERR_REP( ' ', 'CRED4_REDUCE_OBSERVATION: '/
     :                  /'First DSA error has occured', STATUS )

                  ELSE

                     ERR_STATUS = STATUS
                     STATUS = SAI__ERROR
                     CALL MSG_SETI( 'ES', ERR_STATUS )
                     CALL ERR_REP( ' ', 'CRED4_REDUCE_OBSERVATION: '/
     :                  /'Failed to reduce observation '/
     :                  /'(Status = ^ES)', STATUS )
                  ENDIF
               ELSE

                  CALL ERR_ANNUL( STATUS )
               ENDIF
            ENDIF

*          Tell the RED4 task to reduce this observation.
*          Make up a command line containing the observation name
*          and the required parameters:
*          The observation name
            CPOS = 0
            CALL CHR_PUTC( 'OBS_NAME="', INVAL, CPOS )
            CLEN = MAX( 1, CHR_LEN( OBSERVATION ) )
            CALL CHR_PUTC( OBSERVATION(1:CLEN), INVAL, CPOS )

*          The bad pixel mask required (# for none).
*           CALL CRED4_CHECK_INPUT( MASK, STATUS )
            CALL CHR_PUTC( '" MASK="', INVAL, CPOS )
            CLEN = MAX(1, CHR_LEN( MASK ) )
            CALL CHR_PUTC( MASK(1:CLEN), INVAL, CPOS )

*          The linearisation coefficients file (# for none).
            CALL CHR_PUTC( '" LINCOEFFS="', INVAL, CPOS )
            CLEN = MAX( 1, CHR_LEN( LINCOEFFS ) )
            CALL CHR_PUTC( LINCOEFFS(1:CLEN), INVAL, CPOS )

*          Bias subtraction parameters
            CALL CHR_PUTC( '" BIAS_MODE="', INVAL, CPOS )
            CLEN = MAX( 1, CHR_LEN( BIAS_MODE ) )
            CALL CHR_PUTC( BIAS_MODE(1:CLEN), INVAL, CPOS )
            CALL CHR_PUTC( '" SUBTRACT_BIAS="', INVAL, CPOS )
            CLEN = MAX( 1, CHR_LEN( SUBTRACT_BIAS ) )
            CALL CHR_PUTC( SUBTRACT_BIAS(1:CLEN), INVAL, CPOS )
            CALL CHR_PUTC( '" SPECIFIED_BIAS="', INVAL, CPOS )
            CLEN = MAX( 1, CHR_LEN( SPECIFIED_BIAS ) )
            CALL CHR_PUTC( SPECIFIED_BIAS(1:CLEN), INVAL, CPOS )

*          Dark subtraction parameters
            CALL CHR_PUTC( '" SUBTRACT_DARK="', INVAL, CPOS )
            CLEN = MAX( 1, CHR_LEN( SUBTRACT_DARK ) )
            CALL CHR_PUTC( SUBTRACT_DARK(1:CLEN), INVAL, CPOS )
            CALL CHR_PUTC( '" DARK_MODE="', INVAL, CPOS )
            CLEN = MAX( 1, CHR_LEN( DARK_MODE ) )
            CALL CHR_PUTC( DARK_MODE(1:CLEN), INVAL, CPOS )
            CALL CHR_PUTC( '" SPECIFIED_DARK="', INVAL, CPOS )
            CLEN = MAX( 1, CHR_LEN( SPECIFIED_DARK ) )
            CALL CHR_PUTC( SPECIFIED_DARK(1:CLEN), INVAL, CPOS )

*          FF normalisation and division parameters
            CALL CHR_PUTC( '" NORMALISE_FF="', INVAL, CPOS )
            CLEN = MAX( 1, CHR_LEN( NORMALISE_FF ) )
            CALL CHR_PUTC( NORMALISE_FF(1:CLEN), INVAL, CPOS )
            CALL CHR_PUTC( '" DIVIDE_BY_FF="', INVAL, CPOS )
            CLEN = MAX( 1, CHR_LEN( DIVIDE_BY_FF ) )
            CALL CHR_PUTC( DIVIDE_BY_FF(1:CLEN), INVAL, CPOS )
            CALL CHR_PUTC( '" NORM_METHOD="', INVAL, CPOS )
            CLEN = MAX( 1, CHR_LEN( NORM_METHOD ) )
            CALL CHR_PUTC( NORM_METHOD(1:CLEN), INVAL, CPOS )
            CALL CHR_PUTC( '" ORDER=', INVAL, CPOS )
            CALL CHR_PUTI( ORDER, INVAL, CPOS )
            CALL CHR_PUTC( ' BOXSIZE=', INVAL, CPOS )
            CALL CHR_PUTI( BOXSIZE, INVAL, CPOS )
            CALL CHR_PUTC( ' FLAT_MODE="', INVAL, CPOS )
            CLEN = MAX( 1, CHR_LEN( FLAT_MODE ) )
            CALL CHR_PUTC( FLAT_MODE(1:CLEN), INVAL, CPOS )
            CALL CHR_PUTC( '" SPECIFIED_FLAT="', INVAL, CPOS )
            CLEN = MAX( 1, CHR_LEN( SPECIFIED_FLAT ) )
            CALL CHR_PUTC( SPECIFIED_FLAT(1:CLEN), INVAL, CPOS )

*          Wavelength calibration parameters.
            CALL CHR_PUTC( '" TO_WAVELENGTH="', INVAL, CPOS )
            CLEN = MAX( 1, CHR_LEN( TO_WAVELENGTH ) )
            CALL CHR_PUTC( TO_WAVELENGTH(1:CLEN), INVAL, CPOS )
            CALL CHR_PUTC( '" LAMBDA_METHOD="', INVAL, CPOS )
            CLEN = MAX( 1, CHR_LEN( LAMBDA_METHOD ) )
            CALL CHR_PUTC( LAMBDA_METHOD(1:CLEN), INVAL, CPOS )
            CALL CHR_PUTC( '" CALIB_MODE="', INVAL, CPOS )
            CLEN = MAX( 1, CHR_LEN( CALIB_MODE ) )
            CALL CHR_PUTC( CALIB_MODE(1:CLEN), INVAL, CPOS )
            CALL CHR_PUTC( '" SPECIFIED_CALIB="', INVAL, CPOS )
            CLEN = MAX( 1, CHR_LEN( SPECIFIED_CALIB ) )
            CALL CHR_PUTC( SPECIFIED_CALIB(1:CLEN)//'"', INVAL, CPOS )

            IF ( VERBOSE ) THEN
              CALL MSG_SETC( 'RED4_ALIAS', RED4_ALIAS )
              CALL MSG_OUT( ' ', 'Sent to ^RED4_ALIAS: ', STATUS )
              CALL MSG_SETC( 'INVAL', INVAL )
              CALL MSG_OUT( ' ', '^INVAL', STATUS )
            ENDIF

            CALL TASK_OBEY (RED4_ALIAS, 'RED_OBS', INVAL(1:CPOS),
     :        OUTVAL, RED4_PATH, RED4_MESSID, STATUS)

*          check that the action started OK, if not set the REDUCTION_OK flag
*          to show that a failure has occured (unless the sequence setup
*          is such that we're not waiting for this action to finish before
*          proceeding; in which case it can't be important that it has failed)
            IF (STATUS .NE. DTASK__ACTSTART) THEN

               ERR_STATUS = STATUS
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'ES', ERR_STATUS )
               CALL ERR_REP( ' ', 'CRED4_REDUCE_OBSERVATION: '/
     :            /'Failed to start observation reduction '/
     :            /'(Status = ^ES)', STATUS )
               IF (WAIT) REDUCTION_OK = .FALSE.
            ELSE

*             set global flag to indicate that a RED4 action is in progress
               CALL ERR_ANNUL( STATUS )
               IF ( VERBOSE ) CALL MSG_OUT( ' ', 'Called RED4 Task OK', STATUS )
               RED4_ACTIVE = .TRUE.

*             if necessary, wait for action to finish, check completion
               IF (WAIT) THEN

                  IF ( VERBOSE ) CALL MSG_OUT( ' ', 'Waiting for RED4 action to complete', STATUS )
                  CALL TASK_DONE (-1, RED4_PATH, RED4_MESSID,
     :              OUTVAL, STATUS)

                  RED4_ACTIVE = .FALSE.

                  IF (STATUS .NE. DTASK__ACTCOMPLETE) THEN

                     REDUCTION_OK = .FALSE.

                     IF (STATUS .EQ. DTASK__ACTINFORM) THEN

                        ERR_STATUS = STATUS
                        STATUS = SAI__ERROR
                        CALL MSG_SETI( 'ES', ERR_STATUS )
                        CALL ERR_REP( ' ', 'CRED4_REDUCE_OBSERVATION: '/
     :                     /'Failure reported by RED4 task '/
     :                     /'(Status = ^ES, message follows)', STATUS )
                        CALL MSG_SETC( 'OUTVAL', OUTVAL )
                        CALL ERR_REP (' ', 'CRED4_REDUCE_OBSERVATION: '/
     :                     /'^OUTVAL', STATUS )
                     ELSE IF (STATUS .EQ. DSA__ERROR) THEN

*                      explicit check for DSA error, report but otherwise ignore
                        STATUS = SAI__ERROR
                        CALL ERR_REP( ' ', 'CRED4_REDUCE_OBSERVATION: '/
     :                     /'Second DSA error has occured', STATUS )
                     ELSE

                        ERR_STATUS = STATUS
                        STATUS = SAI__ERROR
                        CALL MSG_SETI( 'ES', ERR_STATUS )
                        CALL ERR_REP( ' ', 'CRED4_REDUCE_OBSERVATION '/
     :                     /'Failed to reduce observation '/
     :                     /'(Status = ^ES)', STATUS )
                     ENDIF
                  ELSE

*                  The reduction has completed succesfully.
*                  Return a good status and set the flag indicating
*                  a reduced observation is available for display.
                     CALL ERR_ANNUL( STATUS )
                     IF ( VERBOSE ) CALL MSG_OUT( ' ', 'CRED4_REDUCE_OBSERVATION: Reduction completed OK', STATUS )
                     OBSERVATION_AVAILABLE = .TRUE.
                     REDUCTION_OK = .TRUE.
                 ENDIF
               ENDIF
            ENDIF
         ENDIF
      ELSE

         ERR_STATUS = STATUS
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'ES', STATUS )
         CALL ERR_REP( ' ', 'CRED4_REDUCE_OBSERVATION: '/
     :     /'Failed to obtain input parameters '/
     :     /'(Status = ^ES)', STATUS )
      ENDIF

      END
