*+  DTASK_GSOC - interpret a GET/SET/OBEY/CANCEL/CONTROL message
      SUBROUTINE DTASK_GSOC ( DTASK_APPLIC, PATH, MESSID, CONTEXT, NAME,
     :  VALUE, STATUS ) 
*    Description :
*     Interpret a message requesting GET, SET, OBEY, CANCEL or CONTROL. If 
*     necessary, activate the application.
*    Invocation :
*     CALL DTASK_GSOC ( DTASK_APPLIC, PATH, MESSID, CONTEXT, NAME,
*    :  VALUE, STATUS ) 
*    Parameters :
*     DTASK_APPLIC=EXTERNAL (given)
*           application calling routine
*     PATH=INTEGER (given)
*           path for message received
*     MESSID=INTEGER (given)
*           transaction number for message received
*     CONTEXT=INTEGER (given)
*           context of message received
*     NAME=CHARACTER*(*) (given)
*           name field in received message
*     VALUE=CHARACTER*(*) (given)
*           command-line parameter string
*     STATUS=INTEGER
*    Method :
*     A GET, SET or CONTROL is passed to the routine which handles it. For
*     OBEY and CANCEL check whether this is a valid request before calling 
*     the corresponding routine.
*    Deficiencies :
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     John Cooke (REVS::JAC) 22May84
*     B.D.Kelly (REVAD::BDK)
*    History :
*     22-MAY-1984  first insertion (REVA::ADAM])
*     10.06.1991:  DTASK_GSOC developed from various earlier routines 
*                  (REVAD::BDK)
*     27.06.1991:  call DTASK_SRCHKEY - name was wrong (REVAD::BDK)
*     25.11.1991:  use ADAM_ACKNOW (REVAD::BDK)
*     28.02.1992:  Add ACODE argument to DTASK_ADDLST (AAO::TJF)
*     07.04.1992:  Add CONTROL context and CONTROL DEFAULT action (RLVAD::BKM)
*     15.07.1992:  Correct handling of unknown context messages
*     15.07.1992:  Try to recover from GSOC errors
*     13.10.1992:  Add INCLUDE 'PAR_PAR' (RLVAD::AJC)
*     23.04.1993:  Remove unused variable declarations (RLVAD::AJC)
*     23.08.1993:  Replace PAR_PAR with SUBPAR_SYS  (RLVAD::AJC)
*                  Replace PAR__SZNAM with SUBPAR__NAMELEN  (RLVAD::AJC)
*     11.06.2001:  Call AMS_REPLY (FAMS) directly (AJC)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'SUBPAR_SYS'
      INCLUDE 'ADAM_DEFNS'
      INCLUDE 'DTASK_SYS'
      INCLUDE 'DTASK_ERR'
      INCLUDE 'MESSYS_PAR'
      INCLUDE 'MESSYS_ERR'
*    Import :
      EXTERNAL DTASK_APPLIC        ! application calling routine
      INTEGER PATH                 ! path for message received
      INTEGER MESSID               ! transaction number for message
                                   ! received 
      INTEGER CONTEXT              ! context of message received
      CHARACTER*(*) NAME           ! name field in received message
      CHARACTER*(*) VALUE          ! command-line parameter string
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'DTASK_CMN'
*    Local variables :
      INTEGER ACTPTR               ! action pointer
      CHARACTER*(SUBPAR__NAMELEN) ANAME ! action name
      INTEGER ACTLEN               ! length of action name
      CHARACTER*(SUBPAR__NAMELEN) AKEY  ! action keyword
      INTEGER MESSTATUS            ! message status sent out
      INTEGER MESLEN               ! length of VALUE
      INTEGER SEQ                  ! sequence number for action
      INTEGER ACODE                ! number for the action in the 
                                   ! parameter system
*-

      IF ( STATUS .NE. SAI__OK ) RETURN 
*
*   Enable communications for error reporting
*
      CALL SUBPAR_PUTPATH ( PATH, MESSID, STATUS )
*
*      Check for SET, GET or CONTROL
*
      IF ( CONTEXT .EQ. SET ) THEN
         CALL DTASK_SET ( PATH, NAME, VALUE, MESSID, STATUS )
      ELSE IF ( CONTEXT .EQ. GET ) THEN
         CALL DTASK_GET ( PATH, NAME, MESSID, STATUS )
      ELSE IF ( CONTEXT .EQ. CONTROL) THEN
         CALL DTASK_CONTROL( PATH, NAME, VALUE, MESSID, STATUS )
*
*      An OBEY or CANCEL. Check if the action is active.
*
      ELSE IF ( CONTEXT .EQ. OBEY .OR. CONTEXT .EQ. CANCEL ) THEN
         CALL DTASK_SRCHKEY ( NAME, ACTPTR, STATUS )
         IF ( ( CONTEXT .EQ. OBEY ) .AND. 
     :     ( STATUS .EQ. DTASK__ACTACTIVE ) ) THEN 
            MESSTATUS = DTASK__REJECTED
            STATUS = SAI__OK
            CALL DTASK_COMSHUT ( PATH, MESSID, MESSTATUS, CONTEXT, NAME,
     :        VALUE, STATUS )

         ELSE IF ( ( CONTEXT .EQ. CANCEL ) .AND. 
     :     ( STATUS .EQ. DTASK__NOTFOUND ) ) THEN 
            MESSTATUS = DTASK__NOTACTIVE
            STATUS = SAI__OK
            CALL DTASK_COMSHUT ( PATH, MESSID, MESSTATUS, CONTEXT, NAME,
     :        VALUE, STATUS )

         ELSE
*
*         Valid OBEY or CANCEL.
*         Look-up the action keyword in the parameter system.
*
            STATUS = SAI__OK
            AKEY = NAME
            CALL SUBPAR_FINDACT ( AKEY, ACODE, STATUS )
            CALL SUBPAR_ACTNAME ( ACODE, ANAME, ACTLEN, STATUS )
*
*         Give the command-line parameter string to the parameter system
*
            CALL SUBPAR_CMDLINE ( ACODE, CONTEXT, VALUE, STATUS )

            IF ( STATUS .EQ. SAI__OK ) THEN
               IF ( CONTEXT .EQ. OBEY ) THEN
*
*               Start-up a new action.
*               Mark the action as active and clear the list of
*               subsidiary tasks for it.
*
                  SEQ = 0
                  CALL DTASK_ADDLST ( ANAME, AKEY, PATH, MESSID, SEQ,
     :              ACODE, ACTPTR, STATUS )
                  CALL TASK_CLEAR_MESSINFO ( ACTPTR, STATUS )
                  IF ( STATUS .EQ. SAI__OK ) THEN
*
*                   Send the initial acknowledgement
*
                     MESSTATUS = DTASK__ACTSTART
                     MESLEN = MIN( LEN(VALUE), MESSYS__VAL_LEN )
                     CALL FAMS_REPLY( PATH, MESSID, MESSYS__MESSAGE,
     :                 MESSTATUS, CONTEXT, AKEY, MESLEN, VALUE, STATUS )
                     IF ( STATUS .NE. SAI__OK ) THEN
                        MESSTATUS = STATUS
                        CALL ERR_REP ( ' ', 
     :                    'failed to send initial acknowledgement',
     :                    STATUS ) 
                        STATUS = SAI__OK
                        CALL DTASK_ACTSHUT ( PATH, MESSID, MESSTATUS,
     :                    CONTEXT, ACTPTR, ANAME, AKEY, VALUE, STATUS ) 
                     ELSE
*
*                     Can start the OBEY
*
                        CALL DTASK_OBEY ( DTASK_APPLIC, ACTPTR, VALUE, 
     :                    STATUS )
                     ENDIF
                  ELSE
                     CALL ERR_REP ( ' ', 'failed to start action',
     :                 STATUS )
                     MESSTATUS = STATUS
                     STATUS = SAI__OK
                     CALL DTASK_COMSHUT ( PATH, MESSID, MESSTATUS, 
     :                 CONTEXT, NAME, VALUE, STATUS )
                  ENDIF
               ELSE
*
*                Cancel was requested
*
                  CALL DTASK_CANCEL ( DTASK_APPLIC, PATH, MESSID,
     :              ACTPTR, VALUE, STATUS )

               ENDIF

            ELSE
*
*             Error returned from parameter system
*
               MESSTATUS = STATUS
               STATUS = SAI__OK
               CALL DTASK_COMSHUT ( PATH, MESSID, MESSTATUS, 
     :            CONTEXT, NAME, VALUE, STATUS )

            ENDIF

         ENDIF
      ELSE
*
*      Illegal CONTEXT message
*        
         MESSTATUS = DTASK__ILLCONTEXT      
         STATUS = SAI__OK
         CALL DTASK_COMSHUT ( PATH, MESSID, MESSTATUS, 
     :      CONTEXT, NAME, VALUE, STATUS )
      ENDIF

      END
