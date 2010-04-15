*+  CRED4_EXTRACT_SPC - Extracts a spectrum
      SUBROUTINE CRED4_EXTRACT_SPC( INPUT, OUTPUT1, OUTPUT2, ASK, WAIT,
     :  LEXTRACTED, STATUS )
*    Description :
*     This routine instructs the RED4 task to extract a spectrum from an RG
*    Invocation :
*     CALL CRED4_EXTRACT_SPC( INPUT, OUTPUT, ASK, WAIT, LEXTRACTED, STATUS )
*    Parameters :
*     INPUT     = CHARACTER*(*)( READ )
*           The name of the reduced group to be polynomial fitted
*     OUTPUT1   = CHARACTER*(*)( READ )
*           The name of the resultant spectrum
*     OUTPUT2   = CHARACTER*(*)( READ )
*           The name of the resultant spectral image.
*     ASK       = LOGICAL( READ )
*           A flag which is .TRUE. if the user should be prompted
*           for confirmation before proceeding.
*           (In this subroutine, ASK is always FALSE as it is really not needed)
*     WAIT      = LOGICAL( READ )
*           A flag which is .TRUE. if the routine should not return
*           until the action has been completed.
*     LEXTRACTED   = LOGICAL( WRITE )
*           A flag indicating if extraction has taken place.
*     STATUS    = INTEGER( UPDATE )
*           Global ADAM status.
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     P.N.Daly (JACH::PND)
*     K.Krisciunas (JACH::PND)
*    History :
*     24-Mar-1994: Original version, based on CRED4_POLYFIT         (PND,KLK)
*     24-May-1994: Add bright or faint source algorithm             (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DTASK_ERR'
*    Import :
      CHARACTER*(*)
     :  INPUT,                      ! Name of input group.
     :  OUTPUT1,                    ! Name of output spectrum
     :  OUTPUT2                     ! Name of output spectral image
      LOGICAL ASK                   ! T if should ask user before proceeding
      LOGICAL LEXTRACTED            ! T if EXTRACT_SPCting was successfull
      LOGICAL WAIT                  ! T if should wait for reduction action
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'CRED4COM.INC'        ! CRED4 common block
*    Local variables :
      LOGICAL PROCEED               ! T if reduction step is to proceed
      CHARACTER*( MSG_VAL_LEN )
     :  INVAL,                      ! Command line parameter string
     :  OUTVAL                      ! Returned string
      INTEGER CPOS                  ! Position in character string
      INTEGER CLEN                  ! Non-blank length of character string
      INTEGER ERR_STATUS            ! Temporary status for ERR_REP
*-

*   Check for error on entry
      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL PAR_GET0C( 'RED4_ALIAS', RED4_ALIAS, STATUS )

*   Initialise the LEXTRACTED flag.
      LEXTRACTED = .FALSE.

*   Issue a message to the user
      CALL MSG_SETC( 'INPUT', INPUT )
      CALL MSG_SETC( 'OUTPUT1', OUTPUT1 )
      CALL MSG_SETC( 'OUTPUT2', OUTPUT2 )
      CALL MSG_OUT ( ' ',
     :  'Extracting spectrum from ^INPUT to '/
     :  /'^OUTPUT1 and ^OUTPUT2', STATUS )

*    Should we continue?
      IF (ASK) THEN
         CALL PAR_CANCL( 'PROCEED_EXTRACT', STATUS )
         CALL PAR_GET0L( 'PROCEED_EXTRACT', PROCEED, STATUS )
      ELSE
         PROCEED = .TRUE.
      END IF

      IF ( PROCEED ) THEN

*       If the RED4_ACTIVE flag is true then a RED4 action is in progress
*       so wait for it to finish before proceeding. Failure of that action
*       is clearly not important to the rest of the reduction sequence
*       because we haven't had to wait for it to finish before proceeding,
*       so if it has failed don't worry beyond printing a warning.
         IF ( RED4_ACTIVE ) THEN

            CALL TASK_DONE( -1, RED4_PATH, RED4_MESSID, OUTVAL, STATUS )

            RED4_ACTIVE = .FALSE.

            IF (STATUS .NE. DTASK__ACTCOMPLETE) THEN
               IF (STATUS .EQ. DTASK__ACTINFORM) THEN

                  ERR_STATUS = STATUS
                  STATUS = SAI__ERROR
                  CALL MSG_SETI( 'ES', ERR_STATUS )
                  CALL ERR_REP (' ', 'CRED4_EXTRACT_SPC: '/
     :              /'Failure reported by RED4 task '/
     :              /'(Status = ^ES, message follows)', STATUS )
                  CALL MSG_SETC( 'OUTVAL', OUTVAL )
                  CALL ERR_REP( ' ', 'CRED4_EXTRACT_SPC: '/
     :              /'^OUTVAL', STATUS )

               ELSE IF ( STATUS .EQ. DSA__ERROR ) THEN

*               Explicit check for DSA error, report but otherwise ignore
                  STATUS = SAI__ERROR
                  CALL ERR_REP( ' ', 'CRED4_EXTRACT_SPC: '/
     :               /'First DSA error has occured', STATUS )
               ELSE

                  ERR_STATUS = STATUS
                  STATUS = SAI__ERROR
                  CALL MSG_SETI( 'ES', ERR_STATUS )
                  CALL ERR_REP( ' ', 'CRED4_EXTRACT_SPC: '/
     :               /'Failed to finish previous action '/
     :               /'(Status = ^ES)', STATUS )
               ENDIF
            ELSE

               CALL ERR_ANNUL( STATUS )
            ENDIF
         ENDIF

*      Get a parameter list for RED4 action
         INVAL = ' '
         CPOS = 0
         CALL CHR_PUTC( 'IMAGE="', INVAL, CPOS )
         CLEN = LEN( INPUT )
         CALL CHR_PUTC( INPUT(1:CLEN), INVAL, CPOS )
         CALL CHR_PUTC( '" INVERT_SPEC=', INVAL, CPOS )
         CALL CHR_PUTL( SPC_INVERT, INVAL, CPOS )
         CALL CHR_PUTC( ' ALGORITHM="', INVAL, CPOS )
         CLEN = LEN( SPC_ALGORITHM )
         CALL CHR_PUTC( SPC_ALGORITHM(1:CLEN), INVAL, CPOS )
         CALL CHR_PUTC( '" ROW1S=', INVAL, CPOS )
         CALL CHR_PUTR( SPC_ROW1S, INVAL, CPOS )
         CALL CHR_PUTC( ' ROW1E=', INVAL, CPOS )
         CALL CHR_PUTR( SPC_ROW1E, INVAL, CPOS )
         CALL CHR_PUTC( ' ROW2S=', INVAL, CPOS )
         CALL CHR_PUTR( SPC_ROW2S, INVAL, CPOS )
         CALL CHR_PUTC( ' ROW2E=', INVAL, CPOS )
         CALL CHR_PUTR( SPC_ROW2E, INVAL, CPOS )
         CALL CHR_PUTC( ' ROW3S=', INVAL, CPOS )
         CALL CHR_PUTR( SPC_ROW3S, INVAL, CPOS )
         CALL CHR_PUTC( ' ROW3E=', INVAL, CPOS )
         CALL CHR_PUTR( SPC_ROW3E, INVAL, CPOS )
         CALL CHR_PUTC( ' SPECT="', INVAL, CPOS )
         CLEN = LEN( OUTPUT1 )
         CALL CHR_PUTC( OUTPUT1(1:CLEN), INVAL, CPOS )
         CALL CHR_PUTC( '" ISPECT="', INVAL, CPOS )
         CLEN = LEN( OUTPUT2 )
         CALL CHR_PUTC( OUTPUT2(1:CLEN), INVAL, CPOS )
         CALL CHR_PUTC( '"', INVAL, CPOS )

*      Tell the RED4 task to fit a polynomial to the group
         CALL TASK_OBEY( RED4_ALIAS, 'NODEXTRACT4', INVAL(1:CPOS),
     :      OUTVAL, RED4_PATH, RED4_MESSID, STATUS)

*      Check that the action started OK,
         IF ( STATUS .NE. DTASK__ACTSTART ) THEN

            ERR_STATUS = STATUS
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'ES', ERR_STATUS )
            CALL ERR_REP (' ', 'CRED4_EXTRACT_SPC: '/
     :         /'Failed to start NODEXTRACT4 action '/
     :         /'(Status = ^ES)', STATUS )
         ELSE

*         Set global flag to indicate that a RED4 action is in progress
            CALL ERR_ANNUL( STATUS )
            RED4_ACTIVE = .TRUE.

*         If necessary, wait for action to finish, check completion
            IF ( WAIT ) THEN

               CALL TASK_DONE (-1, RED4_PATH, RED4_MESSID,
     :            OUTVAL, STATUS)

               RED4_ACTIVE = .FALSE.

               IF (STATUS .NE. DTASK__ACTCOMPLETE) THEN
                  IF (STATUS .EQ. DTASK__ACTINFORM) THEN

                     ERR_STATUS = STATUS
                     STATUS = SAI__ERROR
                     CALL MSG_SETI( 'ES', ERR_STATUS )
                     CALL ERR_REP( ' ', 'CRED4_EXTRACT_SPC:  '/
     :                 /'Failure reported by RED4 task '/
     :                 /'(Status = ^ES, message follows)', STATUS )
                     CALL MSG_SETC( 'OUTVAL', OUTVAL )
                     CALL ERR_REP (' ', 'CRED4_EXTRACT_SPC:  '/
     :                 /'^OUTVAL', STATUS )

                  ELSE IF (STATUS .EQ. DSA__ERROR) THEN

*                  Explicit check for DSA error.
                     STATUS = SAI__ERROR
                     CALL ERR_REP( ' ', 'CRED4_EXTRACT_SPC: '/
     :                  /'First DSA error has occured', STATUS )
                  ELSE

                     ERR_STATUS = STATUS
                     STATUS = SAI__ERROR
                     CALL MSG_SETI( 'ES', ERR_STATUS )
                     CALL ERR_REP( ' ', 'CRED4_EXTRACT_SPC: '/
     :                  /'Unknown failure  (Status = ^ES)', STATUS )
                  ENDIF
               ELSE

*               The observation has been added successfully. Set a
*               good status and set the EXTRACT_SPCTED flag.
                  CALL ERR_ANNUL( STATUS )
                  LEXTRACTED = .TRUE.
               END IF
            ENDIF
         ENDIF
      ENDIF

      IF ( VERBOSE .AND. LEXTRACTED .AND. STATUS .EQ. SAI__OK ) THEN
         CALL MSG_OUT( ' ', 'Spectrum extracted OK', STATUS )
      END IF

      END
