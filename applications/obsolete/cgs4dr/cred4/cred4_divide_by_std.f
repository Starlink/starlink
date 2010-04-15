*+  CRED4_DIVIDE_BY_STD:Divide a reduced group by a suitable standard
      SUBROUTINE CRED4_DIVIDE_BY_STD( INPUT, OUTPUT, ASK, WAIT, DIVIDED, STATUS )
*    Description :
*     This routine instructs the RED4 task to divide the INPUT group
*     by a suitable flux standard, to make the OUTPUT group.
*    Invocation :
*     CALL CRED4_DIVIDE_BY_STD( INPUT, OUTPUT, ASK, WAIT, DIVIDED,
*     :  STATUS )
*    Parameters :
*     INPUT     = CHARACTER*(*)( READ )
*           The name of the reduced group to be divided by a suitable standard.
*     OUTPUT    = CHARACTER*(*)( READ )
*           The name of the resultant group. (This may be the same as INPUT).
*     ASK       = LOGICAL( READ )
*           A flag which is .TRUE. if the user should be prompted
*           for confirmation before proceeding.
*     WAIT      = LOGICAL( READ )
*           A flag which is .TRUE. if the routine should not return
*           until the action has been completed.
*     DIVIDED   = LOGICAL( WRITE )
*           A flag indicating if a division has taken place.
*           It will be returned .TRUE. if INPUT has been divided by a
*           standard to make OUTPUT.
*           If will be returned .FALSE. if an error occurred during the
*           division, or if ASK is .TRUE. and the user responded with
*           'NO' to the confirmation.
*           Note that the value of this flag is only valid if WAIT is .TRUE.
*     STATUS    = INTEGER( UPDATE )
*           Global ADAM status.
*    Method :
*    Deficiencies :
*     The DIVIDED flag can only be returned with a valid value if WAIT
*     is .TRUE., otherwise the routine has no way of knowing whether the
*     division has been successful.
*    Bugs :
*    Authors :
*     S.M.Beard (REVAD::SMB)
*     P.N.Daly (JACH::PND)
*    History :
*     31-Dec-1990: Original version, based on CRED4_ADD_OBSERVATION.  (SMB)
*     14-Apr-1992: Add BIAS_MODE, DARK_MODE, FLAT_MODE,
*                  CALIB_MODE, STANDARD_MODE, SPECIFIED_BIAS,
*                  SPECIFIED_DARK, SPECIFIED_FLAT,
*                  SPECIFIED_CALIB and SPECIFIED_STD
*                  parameters, for DRIMP/5.1 and DRIMP/5.2.
*                  Add MSG_VAL_LEN.                                   (SMB)
*     11-Feb-1993: Conform to error strategy                          (PND)
*     28-Jun-1993: Pass full observation string                       (PND)
*     10-Dec-1993: Add task alias                                     (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DTASK_ERR'
*    Import :
      CHARACTER*(*)
     :  INPUT,                      ! Name of input group.
     :  OUTPUT                      ! Name of output group.
      LOGICAL ASK                   ! T if should ask user before proceeding
      LOGICAL WAIT                  ! T if should wait for reduction action
*    Export :
      LOGICAL DIVIDED               ! T if the division has taken place.
*    Status :
      INTEGER STATUS
*    External references :
      INTEGER CHR_LEN               ! Character length determining function.
*    Global variables :
      INCLUDE 'CRED4COM.INC'        ! CRED4 common block
*    Local variables :
      LOGICAL PROCEED               ! T if reduction step is to proceed
      CHARACTER*( MSG_VAL_LEN )
     :  INVAL,                      ! Command line parameter string
     :  OUTVAL                      ! Returned string
      INTEGER CPOS                  ! Position in character string
      INTEGER CLEN                  ! Non-blank length of character string
      INTEGER ERR_STATUS             ! Temporary status for ERR_REP
*-

*   Check for error on entry
      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL PAR_GET0C( 'RED4_ALIAS', RED4_ALIAS, STATUS )

*   Initialise the DIVIDED flag.
      DIVIDED = .FALSE.

*   Issue a message to the user
      IF ( VERBOSE ) THEN

         IF ( INPUT .EQ. OUTPUT ) THEN

            CALL MSG_SETC( 'INPUT', INPUT )
            CALL MSG_OUT (' ', 'Dividing group ^INPUT by '/
     :        /'STANDARD in situ', STATUS )
         ELSE

            CALL MSG_SETC( 'INPUT', INPUT )
            CALL MSG_SETC( 'OUTPUT', OUTPUT )
            CALL MSG_OUT (' ', 'Dividing group ^INPUT by '/
     :        /'STANDARD to make ^OUTPUT', STATUS )
         END IF
      END IF

*   If necessary ask user if they wish to proceed
      IF (ASK) THEN

         CALL PAR_CANCL( 'PROCEED_STD', STATUS )
         CALL PAR_GET0L( 'PROCEED_STD', PROCEED, STATUS )
      ELSE

         PROCEED = .TRUE.
      ENDIF

      IF ( PROCEED ) THEN

*       If the RED4_ACTIVE flag is true then a RED4 action is in progress
*       so wait for it to finish before proceeding. Failure of that action
*       is clearly not important to the rest of the reduction sequence
*       because we haven't had to wait for it to finish before proceeding,
*       so if it has failed don't worry beyond printing a warning.
         IF ( RED4_ACTIVE ) THEN

            CALL TASK_DONE (-1, RED4_PATH, RED4_MESSID, OUTVAL,
     :        STATUS)

            RED4_ACTIVE = .FALSE.

            IF (STATUS .NE. DTASK__ACTCOMPLETE) THEN
               IF (STATUS .EQ. DTASK__ACTINFORM) THEN

                  ERR_STATUS = STATUS
                  STATUS = SAI__ERROR
                  CALL MSG_SETI( 'ES', ERR_STATUS )
                  CALL ERR_REP( ' ', 'CRED4_DIVIDE_BY_STD: '/
     :              /'Failure reported by data reduction task '/
     :              /'(Status = ^ES, message follows)', STATUS )
                  CALL MSG_SETC( 'OUTVAL', OUTVAL )
                  CALL ERR_REP( ' ', 'CRED4_DIVIDE_BY_STD: '/
     :              /'^OUTVAL', STATUS )

               ELSE IF ( STATUS .EQ. DSA__ERROR ) THEN

*               Explicit check for DSA error, report but otherwise ignore
                  STATUS = SAI__ERROR
                  CALL ERR_REP( ' ', 'CRED4_DIVIDE_BY_STD:'/
     :               /'First DSA error has occured', STATUS )
               ELSE

                  ERR_STATUS = STATUS
                  STATUS = SAI__ERROR
                  CALL MSG_SETI( 'ES', ERR_STATUS )
                  CALL ERR_REP( ' ', 'CRED4_DIVIDE_BY_STD: '/
     :               /'Failed to finish previous action '/
     :               /'(Status = ^ES)', STATUS )
               ENDIF
            ELSE
               CALL ERR_ANNUL( STATUS )
            ENDIF
         ENDIF

*      Tell the RED4 task to divide the group by a standard, indicating
*      how to search for a suitable standard.
         INVAL = ' '
         CPOS = 0
         CALL CHR_PUTC( 'GROUP="', INVAL, CPOS )
         CLEN = LEN( INPUT )
         CALL CHR_PUTC( INPUT, INVAL, CPOS )
         CALL CHR_PUTC( '" OUTPUT="', INVAL, CPOS )
         CLEN = LEN( OUTPUT )
         CALL CHR_PUTC( OUTPUT, INVAL, CPOS )
         CALL CHR_PUTC( '" STANDARD_MODE=', INVAL, CPOS )
         CLEN = MAX( 1, CHR_LEN( STANDARD_MODE ) )
         CALL CHR_PUTC( STANDARD_MODE(1:CLEN), INVAL, CPOS )

         IF ( STANDARD_MODE .EQ. 'SPECIFIED' ) THEN

            CALL CHR_PUTC( ' SPECIFIED_STD="', INVAL, CPOS )
            CLEN = LEN( SPECIFIED_STD )
            CALL CHR_PUTC( SPECIFIED_STD(1:CLEN)//'"', INVAL, CPOS )
         END IF

         CALL TASK_OBEY( RED4_ALIAS, 'DIVIDE_BY_STD', INVAL(1:CPOS),
     :      OUTVAL, RED4_PATH, RED4_MESSID, STATUS)

*      Check that the action started OK,
         IF (STATUS .NE. DTASK__ACTSTART) THEN

            ERR_STATUS = STATUS
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'ES', ERR_STATUS )
            CALL ERR_REP( ' ', 'CRED4_DIVIDE_BY_STD: '/
     :         /'Failed to start DIVIDE_BY_STD action '/
     :         /'(Status = ^ES)', STATUS )
         ELSE

*         Set global flag to indicate that a RED4 action is in progress
            CALL ERR_ANNUL( STATUS )
            RED4_ACTIVE = .TRUE.

*         If necessary, wait for action to finish, check completion
            IF (WAIT) THEN

               CALL TASK_DONE (-1, RED4_PATH, RED4_MESSID,
     :            OUTVAL, STATUS)

               RED4_ACTIVE = .FALSE.

               IF (STATUS .NE. DTASK__ACTCOMPLETE) THEN
                  IF (STATUS .EQ. DTASK__ACTINFORM) THEN

                     ERR_STATUS = STATUS
                     STATUS = SAI__ERROR
                     CALL MSG_SETI( 'ES', ERR_STATUS )
                     CALL ERR_REP( ' ', 'CRED4_DIVIDE_BY_STD: '/
     :                 /'Failure reported by RED4 task '/
     :                 /'(Status = ^ES< message follows)', STATUS )
                     CALL MSG_SETC( 'OUTVAL', OUTVAL )
                     CALL ERR_REP (' ', 'CRED4_DIVIDE_BY_STD: '/
     :                 /'^OUTVAL', STATUS )

                  ELSE IF (STATUS .EQ. DSA__ERROR) THEN

*                  Explicit check for DSA error.
                     STATUS = SAI__ERROR
                     CALL ERR_REP( ' ', 'CRED4_DIVIDE_BY_STD: '/
     :                  /'Second DSA error has occured', STATUS )
                  ELSE

                     ERR_STATUS = STATUS
                     STATUS = SAI__ERROR
                     CALL MSG_SETI( 'ES', ERR_STATUS )
                     CALL ERR_REP( ' ', 'CRED4_DIVIDE_BY_STD: '/
     :                  /'Failed to divide by standard '/
     :                  /'(Status = ^ES)', STATUS )
                  ENDIF
               ELSE

*               The observation has been added successfully. Set a
*               good status and set the DIVIDED flag.
                  CALL ERR_ANNUL( STATUS )
                  DIVIDED = .TRUE.
               END IF
            ENDIF
         ENDIF
      ENDIF

      END
