*+  CRED4_POLYFIT - Enhances the sky subtraction with a polynomial fit
      SUBROUTINE CRED4_POLYFIT( INPUT, OUTPUT, ASK, WAIT, LPOLYFITTED,
     :  STATUS )
*    Description :
*     This routine instructs the RED4 task to fit a polynomial to
*     data points taken from several sky areas within the INPUT group
*     to make the OUTPUT group.
*    Invocation :
*     CALL CRED4_POLYFIT( INPUT, OUTPUT, ASK, WAIT, LPOLYFITTED,
*     :  STATUS )
*    Parameters :
*     INPUT     = CHARACTER*(*)( READ )
*           The name of the reduced group to be polynomial fitted
*     OUTPUT    = CHARACTER*(*)( READ )
*           The name of the resultant group. (This may be the same as INPUT).
*     ASK       = LOGICAL( READ )
*           A flag which is .TRUE. if the user should be prompted
*           for confirmation before proceeding.
*           (In this subroutine, ASK is always FALSE as it is really not needed)
*     WAIT      = LOGICAL( READ )
*           A flag which is .TRUE. if the routine should not return
*           until the action has been completed.
*     LPOLYFITTED   = LOGICAL( WRITE )
*           A flag indicating if a polynomial fitting has taken place.
*           It will be returned .TRUE. if INPUT has been polyfitted by a
*           standard to make OUTPUT.
*           If will be returned .FALSE. if an error occurred during the
*           division, or if ASK is .TRUE. and the user responded with
*           'NO' to the confirmation.
*           Note that the value of this flag is only valid if WAIT is .TRUE.
*     STATUS    = INTEGER( UPDATE )
*           Global ADAM status.
*    Method :
*    Deficiencies :
*     The POLYFITTED flag can only be returned with a valid value if WAIT
*     is .TRUE., otherwise the routine has no way of knowing whether the
*     division has been successful.
*    Bugs :
*    Authors :
*     P.N.Daly (JACH::PND)
*     S.M.Beard (ROE::SMB)
*    History :
*     01-Jul-1991: Original version, based on CRED4_DIVIDE_BY_STD.  (PND)
*     10-Jan-1992: Put in some verbose mode messages                (PND)
*     14-Apr-1992: Add MSG_VAL_LEN.                                 (SMB)
*      9-Jul-1992: Comment out some VERBOSE debugging code          (PND)
*     11-Feb-1993: Conform to error strategy                        (PND)
*     28-Jun-1993: Pass full observation string                     (PND)
*     10-Dec-1993: Add task alias                                   (PND)
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
      LOGICAL LPOLYFITTED           ! T if polyfitting was successfull
      LOGICAL WAIT                  ! T if should wait for reduction action
*    Status :
      INTEGER STATUS
*    External references :
      INTEGER CHR_LEN               ! Character length determining function.
*    Global variables :
      INCLUDE 'CRED4COM.INC'        ! CRED4 common block
*    Local variables :
      LOGICAL PROCEED               ! T if reduction step is to proceed
      CHARACTER*( MSG_VAl_LEN )
     :  INVAL,                      ! Command line parameter string
     :  OUTVAL                      ! Returned string
      INTEGER CPOS                  ! Position in character string
      INTEGER CLEN                  ! Non-blank length of character string
      INTEGER ERR_STATUS             ! Temporary status for ERR_REP
      INTEGER VSTATUS               ! Temporary status for VERBOSE output
*-

*   Check for error on entry
      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL PAR_GET0C( 'RED4_ALIAS', RED4_ALIAS, STATUS )

*   Initialise the POLYFITTED flag.
      LPOLYFITTED = .FALSE.

*   Issue a message to the user
      IF ( VERBOSE ) THEN

         CALL MSG_OUT ( ' ',
     :     'Fitting sky polynomial ', VSTATUS )
         CLEN = MAX( 1, CHR_LEN ( INPUT ) )
         CALL MSG_SETC( 'INPUT', INPUT(1:CLEN) )
         CLEN = MAX( 1, CHR_LEN ( OUTPUT ) )
         CALL MSG_SETC( 'OUTPUT', OUTPUT(1:CLEN) )
         CALL MSG_OUT ( ' ',
     :     'Input = ^INPUT   Output = ^OUTPUT', VSTATUS )
      ENDIF

*    Should we continue?
      IF (ASK) THEN
         CALL PAR_CANCL( 'PROCEED_POLYFIT', STATUS )
         CALL PAR_GET0L( 'PROCEED_POLYFIT', PROCEED, STATUS )
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
                  CALL ERR_REP( ' ', 'CRED4_POLYFIT: '/
     :              /'Failure reported by RED4 task '/
     :              /'(Status = ^ES, message follows)', STATUS )
                  CALL MSG_SETC( 'OUTVAL', OUTVAL )
                  CALL ERR_REP( ' ', 'CRED4_POLYFIT: '/
     :              /'^OUTVAL', STATUS )

               ELSE IF ( STATUS .EQ. DSA__ERROR ) THEN

*               Explicit check for DSA error, report but otherwise ignore
                  STATUS = SAI__ERROR
                  CALL ERR_REP( ' ', 'CRED4_POLYFIT: '/
     :               /'First DSA error has occured', STATUS )
               ELSE

                  ERR_STATUS = STATUS
                  STATUS = SAI__ERROR
                  CALL MSG_SETI( 'ES', ERR_STATUS )
                  CALL ERR_REP( ' ', 'CRED4_POLYFIT: '/
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
         CALL CHR_PUTC( 'INPUT="', INVAL, CPOS )
         CLEN = LEN( INPUT )
         CALL CHR_PUTC( INPUT(1:CLEN), INVAL, CPOS )
         CALL CHR_PUTC( '" OUTPUT="', INVAL, CPOS )
         CLEN = LEN( OUTPUT )
         CALL CHR_PUTC( OUTPUT(1:CLEN), INVAL, CPOS )
         CALL CHR_PUTC( '" PF_POLYFIT=', INVAL, CPOS )
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

*      Tell the RED4 task to fit a polynomial to the group
         CALL TASK_OBEY( RED4_ALIAS, 'POLYFIT', INVAL(1:CPOS),
     :      OUTVAL, RED4_PATH, RED4_MESSID, STATUS)

*      Check that the action started OK,
         IF ( STATUS .NE. DTASK__ACTSTART ) THEN

            ERR_STATUS = STATUS
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'ES', ERR_STATUS )
            CALL ERR_REP( ' ', 'CRED4_POLYFIT: '/
     :         /'Failed to start POLYFIT action '/
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
                     CALL ERR_REP( ' ', 'CRED4_POLYFIT:  '/
     :                 /'Failure reported by RED4 task '/
     :                 /'(Status = ^ES, message follows)', STATUS )
                     CALL MSG_SETC( 'OUTVAL', OUTVAL )
                     CALL ERR_REP( ' ', 'CRED4_POLYFIT:  '/
     :                 /'^OUTVAL', STATUS )

                  ELSE IF (STATUS .EQ. DSA__ERROR) THEN

*                  Explicit check for DSA error.
                     STATUS = SAI__ERROR
                     CALL ERR_REP( ' ', 'CRED4_POLYFIT: '/
     :                  /'First DSA error has occured', STATUS )
                  ELSE

                     ERR_STATUS = STATUS
                     STATUS = SAI__ERROR
                     CALL MSG_SETI( 'ES', ERR_STATUS )
                     CALL ERR_REP (' ', 'CRED4_POLYFIT: '/
     :                  /'Failed to polyfit the sky '/
     :                  /'(Status = ^ES)', STATUS )
                  ENDIF
               ELSE

*               The observation has been added successfully. Set a
*               good status and set the POLYFITTED flag.
                  CALL ERR_ANNUL( STATUS )
                  LPOLYFITTED = .TRUE.
               ENDIF
            ENDIF
         ENDIF
      ENDIF


      END
