*+  CRED4_ADD_OBSERVATION - Add observation to reduced group
      SUBROUTINE CRED4_ADD_OBSERVATION( OBSERVATION, ASK, WAIT, STATUS )
*    Description :
*     This routine instructs the RED4 task to add the specified
*     reduced observation to its corresponding reduced group file.
*    Invocation :
*     CALL CRED4_ADD_OBSERVATION (OBSERVATION, ASK, WAIT, STATUS)
*    Parameters :
*     OBSERVATION  = CHARACTER*(*)( READ )
*           The name of the observation to be added.
*     ASK          = LOGICAL( READ )
*           A flag which is .TRUE. if the user should be prompted
*           for confirmation before proceeding.
*     WAIT         = LOGICAL( READ )
*           A flag which is .TRUE. if the routine should not return
*           until the action has been completed.
*     STATUS       = INTEGER( UPDATE )
*           Global ADAM status.
*    Deficiencies :
*     The GROUP_AVAILABLE flag is only set if this routine waits for
*     the addition to complete. Otherwise it does not know if the task
*     has been completed successfully.
*    Authors :
*     S.M.Beard (REVAD::SMB)
*     P.N.Daly (JACH::PND)
*    History :
*      2-Oct-1990: Original version, based on CRED4_ADD_INTEGRATION. (SMB)
*      3-Oct-1990: Modified to ignore observations with zero group
*                  number.                                           (SMB)
*     29-Oct-1990: Code which ignores observations with zero group
*                  number commented out for test purposes.           (SMB)
*      5-Nov-1990: VARIANCE_WT and SKY_WT parameters added.          (SMB)
*      6-Nov-1990: Typing mistake fixed.                             (SMB)
*     22-Nov-1990: VERBOSE flag added.                               (SMB)
*     31-Dec-1990: Typing mistake fixed.                             (SMB)
*      2-Feb-1991: Option of adding OBJECT/SKY observations together
*                  in pairs added. Rejection of zero groups restored.(SMB)
*     24-Jan-1992: Pass polyfit parameters                           (PND)
*     14-Apr-1992: Add MSG_VAL_LEN.                                  (SMB)
*      9-Jul-1992: Comment out some debugging VERBOSE code           (PND)
*     11-Feb-1993: Conform to error strategy                         (PND)
*     18-Feb-1993: Remove STR$ calls, add CHECK_INPUT                (PND)
*     28-Jun-1993: Pass full observation string                      (PND)
*     10-Dec-1993: Add task alias                                    (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DTASK_ERR'
*    Import :
      LOGICAL ASK                   ! T if should ask user before proceeding
      LOGICAL WAIT                  ! T if should wait for reduction action
      CHARACTER*(*) OBSERVATION     ! The name of the observation to be added
*    Status :
      INTEGER STATUS
*    External references :
      INTEGER CHR_LEN               ! Character length determining function.
*    Global variables :
      INCLUDE 'CRED4COM.INC'        ! CRED4 common block
*    Local Constants :
      INTEGER DSA__OK
      PARAMETER ( DSA__OK = 0 )
*    Local variables :
      LOGICAL OK                    ! T if the observation is ok
      LOGICAL PROCEED               ! T if reduction step is to proceed
      CHARACTER*( MSG_VAL_LEN )
     :  INVAL,                      ! Command line parameter string
     :  OUTVAL                      ! Returned string
      CHARACTER*32 ROBSERVATION     ! Name of reduced polyfitted observation
      CHARACTER*20 OTYPE            ! Observation type
      CHARACTER*4 COMMENT           ! Dummy comment
      INTEGER GRPNUM                ! Group number
      INTEGER CPOS                  ! Position in character string
      INTEGER CLEN                  ! Non-blank length of character string
      INTEGER DSA_STATUS            ! DSA status
      INTEGER ERR_STATUS            ! Temporary status for ERR_REP
*-

*   Check for error on entry
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Get task alias required by this task
      CALL PAR_GET0C( 'RED4_ALIAS', RED4_ALIAS, STATUS )

*   Initialise the OK flag and open DSA
      OK = .FALSE.
      DSA_STATUS = DSA__OK
      CALL DSA_OPEN( DSA_STATUS )

*    Open the observation file
      CALL DSA_NAMED_INPUT( 'OBS_IN', OBSERVATION, DSA_STATUS )

*   Read the group number from the GRPNUM element within the .FITS structure.
      CALL DSA_GET_FITS_I( 'OBS_IN', 'GRPNUM', 0, GRPNUM, COMMENT, DSA_STATUS )

*   Read the observation type from the OBSTYPE element within .FITS structure.
      CALL DSA_GET_FITS_C( 'OBS_IN', 'OBSTYPE', 0, OTYPE, COMMENT, DSA_STATUS )

*   Check the observation has been accessed successfully.
      IF ( DSA_STATUS .EQ. DSA__OK ) THEN

*      Convert the observation type string obtained to upper case.
         CALL CHR_UCASE( OTYPE )

*      Check this observation type is either SKY or OBJECT, as only
*      these can be coadded to form a group. Also check that the
*      group number is non-zero.
         IF ( ( (OTYPE .EQ. 'SKY') .OR. (OTYPE .EQ. 'OBJECT') ) .AND.
     :        ( GRPNUM .GT. 0 ) ) THEN

*      This is a suitable observation. Set the OK flag, so the routine may proceed.
            OK = .TRUE.
         END IF
      ELSE

         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'CRED4_ADD_OBSERVATION: '/
     :     /'Error accessing observation file '/
     :     /'(DSA status follows)', STATUS )
         CALL MSG_SETI( 'DSA_STATUS', DSA_STATUS )
         CALL ERR_REP( ' ', 'CRED4_ADD_OBSERVATION: '/
     :     /'DSA status = ^DSA_STATUS', STATUS )
      END IF

*   Close DSA, regardless of the current status.
      CALL DSA_CLOSE( DSA_STATUS )

*   Only proceed if everything has been successful and the
*   observation is suitable.
      IF ( ( STATUS .EQ. SAI__OK ) .AND. OK ) THEN

*      Issue a message to the user
         IF ( VERBOSE ) THEN

            CALL MSG_SETC( 'OBSERVATION', OBSERVATION )
            CALL MSG_OUT (' ', 'Adding observation ^OBSERVATION',
     :        STATUS)
         END IF

*      If necessary ask user if they wish to proceeD
         IF ( ASK ) THEN

            CALL PAR_CANCL( 'PROCEED_GRP', STATUS )
            CALL PAR_GET0L( 'PROCEED_GRP', PROCEED, STATUS )
         ELSE

            PROCEED = .TRUE.
         ENDIF

         IF ( PROCEED ) THEN

*          If the RED4_ACTIVE flag is true then a RED4 action is in progress
*          so wait for it to finish before proceeding. Failure of that action
*          is clearly not important to the rest of the reduction sequence
*          because we haven't had to wait for it to finish before proceeding,
*          so if it has failed don't worry beyond printing a warning.
            IF ( RED4_ACTIVE ) THEN

               CALL TASK_DONE (-1, RED4_PATH, RED4_MESSID, OUTVAL,
     :           STATUS)

               RED4_ACTIVE = .FALSE.

               IF (STATUS .NE. DTASK__ACTCOMPLETE) THEN
                  IF (STATUS .EQ. DTASK__ACTINFORM) THEN

                     ERR_STATUS = STATUS
                     STATUS = SAI__ERROR
                     CALL MSG_SETI( 'ES', ERR_STATUS )
                     CALL ERR_REP( ' ', 'CRED4_ADD_OBSERVATION: '/
     :                 /'First failure reported by RED4 task '/
     :                 /'(Status = ^ES, message follows)', STATUS )
                     CALL MSG_SETC( 'OUTVAL', OUTVAL )
                     CALL ERR_REP(' ', 'CRED4_ADD_OBSERVATION: '/
     :                 /'^OUTVAL', STATUS )
                  ELSE IF ( STATUS .EQ. DSA__ERROR ) THEN

*                  Explicit check for DSA error, report but otherwise ignore
                     STATUS = SAI__ERROR
                     CALL ERR_REP(' ', 'CRED4_ADD_OBSERVATION: '/
     :                  /'First DSA error has occured', STATUS )
                  ELSE

                     ERR_STATUS = STATUS
                     STATUS = SAI__ERROR
                     CALL MSG_SETI( 'ES', ERR_STATUS )
                     CALL ERR_REP( ' ', 'CRED4_ADD_OBSERVATION: '/
     :                  /'Failed to finish previous action '/
     :                  /'(Status = ^ES)', STATUS )
                  ENDIF
               ELSE

                  CALL ERR_ANNUL( STATUS )
               ENDIF
            ENDIF

*         Alter the OBSERVATION name if polyfitting has succeeded
            INVAL = ' '
            CPOS = 0
            CALL CHR_PUTC( 'OBSFILE="', INVAL, CPOS )
            IF ( (POLYFITTED) .AND. (OTYPE.EQ.'OBJECT') .AND.
     :           (PF_POLYFIT(1:3).EQ.'OBJ') ) THEN

               CLEN = CHR_LEN( OBSERVATION )
               ROBSERVATION = OBSERVATION(1:CLEN) // '_psky'
               CLEN = LEN( ROBSERVATION )
               CALL CHR_PUTC( ROBSERVATION(1:CLEN), INVAL, CPOS )
            ELSE

               CLEN = LEN( OBSERVATION )
               CALL CHR_PUTC( OBSERVATION(1:CLEN), INVAL, CPOS )
            ENDIF

*         Tell the RED4 task to add this observation
            IF ( ADD_IN_PAIRS)  THEN

               CALL CHR_PUTC( '" ERRORS=', INVAL, CPOS )
               CLEN = MAX( 1, CHR_LEN( ERRORS ) )
               CALL CHR_PUTC( ERRORS(1:CLEN), INVAL, CPOS )
               CALL CHR_PUTC( ' SKY_WT=', INVAL, CPOS )
               CALL CHR_PUTR( SKY_WT, INVAL, CPOS )

            ELSE
               CALL CHR_PUTC( '" SKY_WT=', INVAL, CPOS )
               CALL CHR_PUTR( SKY_WT, INVAL, CPOS )
            END IF

            CALL CHR_PUTC( ' VARIANCE_WT=', INVAL, CPOS )
            CALL CHR_PUTL( VARIANCE_WT, INVAL, CPOS )

            CALL CHR_PUTC( ' PF_POLYFIT=', INVAL, CPOS )
            CLEN = MAX( 1, CHR_LEN( PF_POLYFIT ) )
            CALL CHR_PUTC( PF_POLYFIT(1:CLEN), INVAL, CPOS )

            CALL CHR_PUTC( ' PF_WEIGHT=', INVAL, CPOS )
            CALL CHR_PUTL( PF_WEIGHT, INVAL, CPOS )

            CALL CHR_PUTC( ' PF_DEGREE=', INVAL, CPOS )
            CALL CHR_PUTI( PF_DEGREE, INVAL, CPOS )

            CALL CHR_PUTC( ' PF_NREJECT=', INVAL, CPOS )
            CALL CHR_PUTI( PF_NREJECT, INVAL, CPOS )

            CALL CHR_PUTC( ' PF_SAYS1=', INVAL, CPOS )
            CALL CHR_PUTI( PF_SAYS1, INVAL, CPOS )

            CALL CHR_PUTC( ' PF_SAYE1=', INVAL, CPOS )
            CALL CHR_PUTI( PF_SAYE1, INVAL, CPOS )

            CALL CHR_PUTC( ' PF_SAYS2=', INVAL, CPOS )
            CALL CHR_PUTI( PF_SAYS2, INVAL, CPOS )

            CALL CHR_PUTC( ' PF_SAYE2=', INVAL, CPOS )
            CALL CHR_PUTI( PF_SAYE2, INVAL, CPOS )

            CALL CHR_PUTC( ' PF_SAYS3=', INVAL, CPOS )
            CALL CHR_PUTI( PF_SAYS3, INVAL, CPOS )

            CALL CHR_PUTC( ' PF_SAYE3=', INVAL, CPOS )
            CALL CHR_PUTI( PF_SAYE3, INVAL, CPOS )

            CALL CHR_PUTC( ' PF_SAYS4=', INVAL, CPOS )
            CALL CHR_PUTI( PF_SAYS4, INVAL, CPOS )

            CALL CHR_PUTC( ' PF_SAYE4=', INVAL, CPOS )
            CALL CHR_PUTI( PF_SAYE4, INVAL, CPOS )

*         Add the observation either individually, or as part of
*         a pair, according to the ADD_IN_PAIRS parameter.
            IF ( ADD_IN_PAIRS ) THEN

               CALL TASK_OBEY( RED4_ALIAS, 'ADD_PAIR', INVAL(1:CPOS),
     :           OUTVAL, RED4_PATH, RED4_MESSID, STATUS )
            ELSE

               CALL TASK_OBEY( RED4_ALIAS, 'ADD_OBS', INVAL(1:CPOS),
     :           OUTVAL, RED4_PATH, RED4_MESSID, STATUS )
            END IF

*         Check that the action started OK,
            IF (STATUS .NE. DTASK__ACTSTART) THEN

               IF (WAIT) REDUCTION_OK = .FALSE.
               ERR_STATUS = STATUS
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'ES', ERR_STATUS )
               CALL ERR_REP( ' ', 'CRED4_ADD_OBSERVATION: '/
     :            /'Failed to start ADD_OBS action '/
     :            /'(Status = ^ES)', STATUS )
            ELSE

*            Set global flag to indicate that a RED4 action is in progress
               CALL ERR_ANNUL( STATUS )
               RED4_ACTIVE = .TRUE.

*            If necessary, wait for action to finish, check completion
               IF ( WAIT ) THEN

                  CALL TASK_DONE( -1, RED4_PATH, RED4_MESSID,
     :               OUTVAL, STATUS )

                  RED4_ACTIVE = .FALSE.

                  IF (STATUS .NE. DTASK__ACTCOMPLETE) THEN
                     IF (STATUS .EQ. DTASK__ACTINFORM) THEN

                        REDUCTION_OK = .FALSE.
                        ERR_STATUS = STATUS
                        STATUS = SAI__ERROR
                        CALL MSG_SETI( 'ES', ERR_STATUS )
                        CALL ERR_REP( ' ', 'CRED4_ADD_OBSERVATION: '/
     :                     /'Second failure reported by RED4 task '/
     :                     /'(Status = ^ES, message follows)', STATUS )
                        CALL MSG_SETC( 'OUTVAL', OUTVAL )
                        CALL ERR_REP(' ', 'CRED4_ADD_OBSERVATION: '/
     :                     /'^OUTVAL', STATUS )
                     ELSE IF (STATUS .EQ. DSA__ERROR) THEN
*                     Explicit check for DSA error.

                        STATUS = SAI__ERROR
                        CALL ERR_REP( ' ', 'CRED4_ADD_OBSERVATION: '/
     :                     /'Second DSA error has occured', STATUS )
                     ELSE

                        ERR_STATUS = STATUS
                        STATUS = SAI__ERROR
                        CALL MSG_SETI( 'ES', ERR_STATUS )
                        CALL ERR_REP( ' ', 'CRED4_ADD_OBSERVATION: '/
     :                     /'Failed to add observation '/
     :                     /'(Status = ^ES)', STATUS )
                     ENDIF
                  ELSE

*                  The observation has been added successfully. Set a
*                  good status and set the GROUP_AVAILABLE flag.
                     CALL ERR_ANNUL( STATUS )
                     GROUP_AVAILABLE = .TRUE.
                     REDUCTION_OK = .TRUE.
                  END IF
               END IF
            ENDIF
         ENDIF
      ENDIF

      END
