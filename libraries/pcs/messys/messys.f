*+  MESSYS_ASTINT - cause MESSYS_RECEIVE to exit
      SUBROUTINE MESSYS_ASTINT ( STATUS )
*    Description :
*     This routine should only be called from within an AST handler 
*     routine. Its purpose is to enable an "ASTINT" event to be 
*     signalled to the main-line code.
*     It causes MESSYS_RECEIVE to detect an "ASTINT" condition 
*     and to return with a suitable status. No information is passed 
*     using this routine other than the fact that an interrupt has 
*     occurred.
*     See also MESSYS_ASTMSG.
*    Invocation :
*     CALL MESSYS_ASTINT ( STATUS )
*    Parameters :
*     STATUS=INTEGER
*    Method :
*     Call the FAMS library.
*    Deficiencies :
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     B.D.Kelly (REVAD::BDK)
*    History :
*     09.03.1988:  original (REVAD::BDK)
*     14.03.1988: don't use NULL_Q as reply queue (REVAD::BDK)
*      2.11.1992: implement using MESSYS_ASTMSG prior to retirement
*                 (RLVAD::AJC)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'

*    Status :
      INTEGER STATUS

*-

      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL MESSYS_ASTMSG( ' ', 1, ' ', STATUS )

      END


*+  MESSYS_ASTMSG - cause MESSYS_RECEIVE to return an AST message
      SUBROUTINE MESSYS_ASTMSG ( NAME, LENGTH, VALUE, STATUS )
*    Description :
*     This routine should only be called from within an AST handler 
*     routine. Its purpose is to enable an "ASTINT" event to be 
*     signalled to the main-line code.
*     It causes MESSYS_RECEIVE to detect an "ASTINT" condition 
*     and to return with a suitable status and the content of the AST 
*     message.
*    Invocation :
*     CALL MESSYS_ASTMSG ( NAME, LENGTH, VALUE, STATUS )
*    Parameters :
*     NAME=CHARACTER*(*) (given)
*          name of the action to be rescheduled
*     LENGTH=INTEGER (given)
*          number of significant bytes in VALUE
*     VALUE=CHARACTER*(*) (given)

*          message to be passed to main-line code
*     STATUS=INTEGER
*    Method :
*     Call the FAMS library.
*    Deficiencies :
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     B.D.Kelly (REVAD::BDK)
*    History :
*     09.04.1991:  original (REVAD::BDK)
*     12.04.1994: unix version (REVAD::BDK)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'

*    Import :
      CHARACTER*(*) NAME     ! name of the action to be rescheduled
      INTEGER LENGTH         ! number of significant bytes in VALUE
      CHARACTER*(*) VALUE    ! message to be passed to main-line code

*    Status :
      INTEGER STATUS

*-

      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL FAMS_ASTMSG ( NAME, LENGTH, VALUE, STATUS )

      END


*+  MESSYS_EXTINT - cause MESSYS_RECEIVE to exit
      SUBROUTINE MESSYS_EXTINT ( STATUS )
*    Description :
*     This routine should only be called from within an AST handler 
*     routine. Its purpose is to enable an "EXTINT" event to be 
*     signalled to the main-line code.
*     It causes MESSYS_RECEIVE or MESSYS_GETREPLY to detect an "EXTINT" 
*     condition and to return with a suitable status. No information is 
*     passed using this routine other than the fact that an interrupt 
*     has occurred.
*    Invocation :
*     CALL MESSYS_EXTINT ( STATUS )
*    Parameters :
*     STATUS=INTEGER
*    Method :
*     Call the FAMS library.
*    Deficiencies :
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     B.D.Kelly (REVAD::BDK)
*    History :
*     09.03.1988:  original (REVAD::BDK)
*     14.03.1988: don't use NULL_Q as the reply queue (REVAD::BDK)
*     12.04.1994: unix version (REVAD::BDK)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'

*    Status :
      INTEGER STATUS

*-

      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL FAMS_EXTINT ( STATUS )

      END


*+  MESSYS_GETREPLY - receive a message from a defined path and messid
      SUBROUTINE MESSYS_GETREPLY ( TIMEOUT, PATH, MESSID, STUFF, 
     :  STATUS )
*    Description :
*     Receive a data-structure as a message from a specified other 
*     process as part of a specified transaction or from an AST event. 
*     The receive is performed with a timeout facility.
*    Invocation :
*     CALL MESSYS_GETREPLY ( TIMEOUT, PATH, MESSID, STUFF, 
*    :  STATUS )
*    Parameters :
*     TIMEOUT=INTEGER (given)
*            timeout time in milliseconds
*     PATH=INTEGER (given)
*            path number
*     MESSID=INTEGER (given)
*            message number
*     STUFF=CHARACTER*(*) (returned)
*            message structure received
*     STATUS=INTEGER
*    Method :
*     Call the FAMS library.
*     If a message is received which has to be returned to the software 
*     calling this routine, then translate it from internal to external 
*     message structure format and return it along with status 
*     information.
*    Deficiencies :
*     NB. Unlike ADAM v1, GETREPLY no longer handles INIT requests.
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     Charlie Richardson (REVS::DCR) 18Feb85
*     B.D.Kelly (REVAD::BDK) 7Mar88
*    History :
*     07.03.1988:  total rewrite to use MSP (REVAD::BDK)
*     17.03.1988:  return MSGFUNC status instead of signalling 
*                  (REVAD::BDK)
*     17.03.1988:  pass MESSID to ADDREST (REVAD::BDK)
*     15.04.1988:  add networking (REVAD::BDK)
*     10.05.1988:  improve error handling and comments (REVAD::BDK)
*     10.05.1988:  don't trample PATH and MESSID (REVAD::BDK)
*     12.04.1994: unix version (REVAD::BDK)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'MESSYS_PAR'
      INCLUDE 'MESSYS_LEN'

*    Import :
      INTEGER TIMEOUT          ! timeout time in milliseconds
      INTEGER PATH             ! pointer to the path
      INTEGER MESSID           ! message number of incoming message

*    Export :
      CHARACTER STUFF*(*)      ! message structure received in external 
                               ! format
*    Status :
      INTEGER STATUS

*    Local variables :
      INTEGER MESSAGE_FUNCTION         ! message function
      INTEGER MESSAGE_STATUS           ! message status
      INTEGER MESSAGE_CONTEXT          ! message context
      CHARACTER*(MSG_NAME_LEN-1) MESSAGE_NAME       ! message name
      INTEGER MESSAGE_LENGTH           ! length of value string
      CHARACTER*(MSG_VAL_LEN) MESSAGE_VALUE      ! value string
*-

      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL FAMS_GETREPLY ( TIMEOUT, PATH, MESSID, MESSAGE_STATUS,
     :  MESSAGE_CONTEXT, MESSAGE_NAME, MESSAGE_LENGTH, MESSAGE_VALUE,
     :  STATUS )
      MESSAGE_FUNCTION = MESSYS__MESSAGE
      CALL MESSYS_PACK ( MESSAGE_FUNCTION, MESSAGE_STATUS,
     :  MESSAGE_CONTEXT, MESSAGE_NAME, MESSAGE_LENGTH, MESSAGE_VALUE,
     :  STUFF, STATUS )

      END


*+  MESSYS_INIT - task initialisation routine
      SUBROUTINE MESSYS_INIT ( OWN_NAME, STATUS )
*   Description:
*    Initialises the task into the message system. 
*   Invocation:
*     CALL MESSYS_INIT ( OWN_NAME, STATUS )
*   Method:
*     Call the FAMS library.
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     John Cooke (REVS::JAC) 21Mar84
*     B.D.Kelly (REVAD::BDK) 07Mar88
*    History :
*     07.03.1987:  total rewrite to use MSP (REVAD::BDK)
*     21.03.1988:  call MESSYS_PREFIX (REVAD::BDK)
*     15.04.1988:  initialise MACHINE_NAMES (REVAD::BDK)
*     04.05.1988:  initialise ADAMNET_Q, remove THIS_TASK_INIT_ACK_Q, 
*                  change OTHER_TASK_T_TRANS_NUM to OTHER_TRANSNUM 
*                  (REVAD::BDK)
*     10.05.1988:  check status before using DCLEXH (REVAD::BDK)
*     23.05.1991:  add KICK_Q (REVAD::BDK)
*     23.07.1991:  add multiple networks (REVAD::BMC)
*     21.01.1993:  use MESSYS__NETNAME as root of network process names
*                  (RLVAD::AJC)
*     12.04.1994: unix version (REVAD::BDK)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'

*    Import :
      CHARACTER OWN_NAME*(*)     ! name of this task 

*    Status :
      INTEGER STATUS
*-

      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL FAMS_INIT ( OWN_NAME, STATUS )

      END


*+  MESSYS_KICK - cause MESSYS_RECEIVE to return a KICK message
      SUBROUTINE MESSYS_KICK ( NAME, LENGTH, VALUE, STATUS )
*    Description :
*     This routine should only be called from main-line code.
*     Its purpose is to enable a "KICK" event to be queued to the task.
*     It causes MESSYS_RECEIVE to detect a "KICK" condition 
*     and to return with a suitable status and the content of the KICK
*     message.
*    Invocation :
*     CALL MESSYS_KICK ( NAME, LENGTH, VALUE, STATUS )
*    Parameters :
*     NAME=CHARACTER*(*) (given)
*          name of the action to be rescheduled
*     LENGTH=INTEGER (given)
*          number of significant bytes in VALUE
*     VALUE=CHARACTER*(*) (given)
*          message to be passed to application code
*     STATUS=INTEGER
*    Method :
*     Call the FAMS library.
*    Deficiencies :
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     B.D.Kelly (REVAD::BDK)
*    History :
*     23.05.1991:  original (REVAD::BDK)
*     12.04.1994: unix version (REVAD::BDK)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      CHARACTER*(*) NAME     ! name of the action to be rescheduled
      INTEGER LENGTH         ! number of significant bytes in VALUE
      CHARACTER*(*) VALUE    ! message to be passed to application code

*    Status :
      INTEGER STATUS

*-

      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL FAMS_KICK ( NAME, LENGTH, VALUE, STATUS )

      END

*+  MESSYS_PACK - convert a message from internal to external format
      SUBROUTINE MESSYS_PACK ( MESSAGE_FUNCTION, MESSAGE_STATUS,
     :  MESSAGE_CONTEXT, MESSAGE_NAME, MESSAGE_LENGTH, MESSAGE_VALUE,
     :  STUFF, STATUS )
*    Description :
*     Translate an incoming message from internal to external format.
*    Invocation :
*     SUBROUTINE MESSYS_PACK ( MESSAGE_FUNCTION, MESSAGE_STATUS,
*    :  MESSAGE_CONTEXT, MESSAGE_NAME, MESSAGE_LENGTH, MESSAGE_VALUE,
*    :  STUFF, STATUS )
*    Parameters :
*     MESSAGE_FUNCTION=INTEGER (given)
*           message function
*     MESSAGE_STATUS=INTEGER (given)
*           message status
*     MESSAGE_CONTEXT=INTEGER (given)
*           message context
*     MESSAGE_NAME=CHARACTER*(*) (given)
*           message name
*     MESSAGE_LENGTH=INTEGER (given)
*           length of value string
*     MESSAGE_VALUE=CHARACTER*(*) (given)
*           value string
*     STUFF=CHARACTER*(*) (returned)
*           the message in external format
*     STATUS=INTEGER
*    Method :
*     Interpret the message.
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     B.D.Kelly (REVAD::BDK)
*    History :
*     03.03.1988:  original (REVAD::BDK)
*     11.05.1988:  trap invalid messid (REVAD::BDK)
*     13.05.1988:  store other task's transaction details (REVAD::BDK)
*     12.04.1994:  unix version (REVAD::BDK)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'MESSYS_LEN'

*    Data structures :
      INCLUDE 'MESSYS_STRUC'

*    Import :
      INTEGER MESSAGE_FUNCTION         ! message function
      INTEGER MESSAGE_STATUS           ! message status
      INTEGER MESSAGE_CONTEXT          ! message context
      CHARACTER*(*) MESSAGE_NAME       ! message name
      INTEGER MESSAGE_LENGTH           ! length of value string
      CHARACTER*(*) MESSAGE_VALUE      ! value string

*    Export :
      CHARACTER*(*) STUFF       ! the message in external format

*    Status :
      INTEGER STATUS
*-

      IF ( STATUS .NE. SAI__OK ) RETURN

      MSG_NUMBER = 0
      MSG_STATUS = MESSAGE_STATUS
      MSG_FUNCTION = MESSAGE_FUNCTION
      MSG_CONTEXT = MESSAGE_CONTEXT
      MSG_NAME = MESSAGE_NAME
      MSG_LENGTH = MESSAGE_LENGTH
      MSG_VAL = MESSAGE_VALUE(1:MSG_LENGTH)

      STUFF = MSG

      END


*+  MESSYS_PATH - obtain message path to/from another task
      SUBROUTINE MESSYS_PATH ( TASK_NAME, PATH, STATUS )
*    Description :
*     Obtains a pointer to path information ( stored in common in the 
*     message system routines ).  This pointer points to complete 
*     round-path information.  The task to which the path is being set 
*     up is also requested to initialise a connection with this calling 
*     task.
*    Invocation :
*     CALL MESSYS_PATH ( TASK_NAME, PATH, STATUS )
*    Parameters :
*     TASK_NAME=CHARACTER*(*) (given)
*            name of task to which path is required
*            including machine name if networking
*     PATH=INTEGER ( returned)
*            pointer to the path
*     STATUS=INTEGER
*    Method :
*     Call the FAMS library.
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     John Cooke (REVS::JAC) 22Mar84
*     B.D.Kelly (REVAD::BDK) 07Mar88
*    History :
*     07.03.1988:  total rewrite to use MSP (REVAD::BDK)
*     15.03.1988:  parameterise RMTFLAG values (REVAD::BDK)
*     15.03.1988:  ignore returned MSG_STATUS - unused for ACK_INIT 
*                  (REVAD::BDK)
*     11.04.1988:  add networking (REVAD::BDK)
*     28.04.1988:  split remote taskname from nodename (REVAD::BDK)
*     10.05.1988:  trap overlength taskname (REVAD::BDK)
*     23.07.1991:  handle multiple networks (REVAD::BMC)
*     17.08.1992:  correct to allow ADAMNET_4 (RLVAD::AJC/AAOEPP:TJF)
*     12.04.1994: unix version (REVAD::BDK)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'

*    Import :
      CHARACTER*(*) TASK_NAME     ! name of task to which path is 
                                  ! required, including machine name if 
                                  ! networking
*    Export :
      INTEGER PATH                ! pointer to the path

*    Status :
      INTEGER STATUS
*-

      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL FAMS_PATH ( TASK_NAME, PATH, STATUS )

      END


*+  MESSYS_PLOOKUP - look up task name for given path
      SUBROUTINE MESSYS_PLOOKUP ( PATH, NAME, STATUS )
*    Description :
*     Return the task name corresponding to the given path.
*    Invocation :
*     CALL MESSYS_PLOOKUP ( PATH, NAME, STATUS )
*    Parameters :
*     PATH=INTEGER (given)
*           the path number
*     NAME=CHARACTER*(*) (returned)
*           the task name
*     STATUS=INTEGER
*    Method :
*     Call the FAMS library.
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     John Cooke (REVS::JAC) 27Mar84
*     B.D.Kelly (REVAD::BDK) 14Mar88
*    History :
*     14.03.1988:  rewrite for MSP (REVAD::BDK)
*     10.05.1988:  concatenate machine name if networking (REVAD::BDK)
*     12.04.1994: unix version (REVAD::BDK)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'

*    Import :
      INTEGER PATH        ! the path number

*    Export :
      CHARACTER*(*) NAME  ! the task name

*    Status :
      INTEGER STATUS

*-

      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL FAMS_PLOOKUP ( PATH, NAME, STATUS )

      END


*+  MESSYS_RECEIVE - receive a message 
      SUBROUTINE MESSYS_RECEIVE ( TIMEOUT, PATH, STUFF, MESSID, STATUS )
*    Description :
*     Receive a data-structure as a message from another process or from 
*     an AST event. The path and messid associated with the message are 
*     returned to enable the higher-level software to generate replies.
*     The receive is performed with a timeout facility.
*     RECEIVE also handles initialisation of tasks with each other. This 
*     is done "invisibly" at lower levels.
*    Invocation :
*     CALL MESSYS_RECEIVE ( TIMEOUT, PATH, STUFF, MESSID, STATUS )
*    Parameters :
*     TIMEOUT=INTEGER (given)
*            timeout time in milliseconds
*     PATH=INTEGER (returned)
*            pointer to the path
*     STUFF=CHARACTER*(*) (returned)
*            message structure received
*     MESSID=INTEGER (returned)
*            message number of incoming message
*     STATUS=INTEGER
*    Method :
*     Call the FAMS library.
*     If a message is received which has to be returned to the software 
*     calling this routine, then translate it from internal to external 
*     message structure format and return it along with status information.
*    Deficiencies :
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     John Cooke (REVS::JAC) 27Mar84
*     Charlie Richardson (REVS::DCR) 5Mar85
*     B.D.Kelly (REVAD::BDK) 2Mar88
*    History :
*     02.03.1988:  total rewrite to use MSP (REVAD::BDK)
*     15.04.1988:  add networking (REVAD::BDK)
*     11.05.1988:  improve error handling (REVAD::BDK)
*     23.05.1991:  add KICK_Q (REVAD::BDK)
*     12.04.1994: unix version (REVAD::BDK)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'MESSYS_PAR'
      INCLUDE 'MESSYS_ERR'
      INCLUDE 'MESSYS_LEN'

*    Import :
      INTEGER TIMEOUT          ! timeout time in milliseconds

*    Export :
      INTEGER PATH             ! pointer to the path
      CHARACTER*(*) STUFF      ! message structure received in external 
                               ! format
      INTEGER MESSID           ! message number of incoming message

*    Status :
      INTEGER STATUS

*    Local variables :
      INTEGER MESSAGE_FUNCTION         ! message function
      INTEGER MESSAGE_STATUS           ! message status
      INTEGER MESSAGE_CONTEXT          ! message context
      CHARACTER*(MSG_NAME_LEN-1) MESSAGE_NAME       ! message name
      INTEGER MESSAGE_LENGTH           ! length of value string
      CHARACTER*(MSG_VAL_LEN) MESSAGE_VALUE      ! value string
*-

      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL FAMS_RECEIVE ( TIMEOUT, MESSAGE_STATUS,
     :  MESSAGE_CONTEXT, MESSAGE_NAME, MESSAGE_LENGTH, MESSAGE_VALUE,
     :  PATH, MESSID, STATUS )
      MESSAGE_FUNCTION = MESSYS__MESSAGE
      CALL MESSYS_PACK ( MESSAGE_FUNCTION, MESSAGE_STATUS,
     :  MESSAGE_CONTEXT, MESSAGE_NAME, MESSAGE_LENGTH, MESSAGE_VALUE,
     :  STUFF, STATUS )

      END


*+  MESSYS_REPLY - send a reply as part of a continuing transaction
      SUBROUTINE MESSYS_REPLY ( PATH, STUFF, MESSID, STATUS )
*    Description :
*     Send a data-structure as a message to another process. This must 
*     occur as part of a previously initiated transaction - ie both PATH 
*     and MESSID are valid.
*    Invocation :
*     CALL MESSYS_REPLY ( PATH, STUFF, MESSID, STATUS )
*    Parameters :
*     PATH=INTEGER (given)
*            the path number for communicating with the other task
*     STUFF=CHARACTER*(*) (given)
*            the message to be sent
*     MESSID=INTEGER (given)
*            the number identifying the transaction
*     STATUS=INTEGER
*    Method :
*     Construct the message
*     Call the FAMS library.
*    Deficiencies :
*     Note that the concept of END-OF-TRANSACTION implies an unfortunate 
*     link between MESSYS and the tasking model.
*    Bugs :
*     None known
*    Authors :
*     John Cooke (REVS::JAC) 22Mar84
*     B.D.Kelly (REVAD::BDK) 09Mar88
*    History :
*     09.03.1988: total rewrite to use MSP (REVAD::BDK)
*     14.03.1988: don't use NULL_Q as the reply queue (REVAD::BDK)
*     15.03.1988: don't try to delete a NULL_Q (REVAD::BDK)
*     16.03.1988: copy the value string into the message! (REVAD::BDK)
*     15.04.1988: add networking (REVAD::BDK)
*     11.05.1988: remove ACK_INIT (REVAD::BDK)
*     19.04.1990: Add MESSYS__TRIGGER to the list of continuing transaction
*	          types (AAO::TJF)
*     19.04.1990: Add MESSYS__SYNCREP to the list of continuing transaction
*	          types (BDK)
*     12.04.1994: unix version (REVAD::BDK)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'MESSYS_LEN'

*    Import :
      INTEGER PATH         ! the path number for communicating with the 
                           ! other task

      CHARACTER*(*) STUFF  ! the message to be sent

      INTEGER MESSID       ! the number identifying the transaction

*    Status :
      INTEGER     STATUS

*    Local variables :
      INTEGER MESSAGE_FUNCTION         ! message function
      INTEGER MESSAGE_STATUS           ! message status
      INTEGER MESSAGE_CONTEXT          ! message context
      CHARACTER*(MSG_NAME_LEN-1) MESSAGE_NAME       ! message name
      INTEGER MESSAGE_LENGTH           ! length of value string
      CHARACTER*(MSG_VAL_LEN) MESSAGE_VALUE      ! value string
*-

      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL MESSYS_UNPACK ( STUFF, MESSAGE_FUNCTION, MESSAGE_STATUS,
     :  MESSAGE_CONTEXT, MESSAGE_NAME, MESSAGE_LENGTH, MESSAGE_VALUE,
     :  STATUS )
      CALL FAMS_REPLY ( PATH, MESSID, MESSAGE_FUNCTION, MESSAGE_STATUS,
     :  MESSAGE_CONTEXT, MESSAGE_NAME, MESSAGE_LENGTH, MESSAGE_VALUE,
     :  STATUS )

      END


*+  MESSYS_RESMSG - cause MESSYS_RECEIVE to return a reschedule message
      SUBROUTINE MESSYS_RESMSG ( LENGTH, VALUE, STATUS )
*    Description :
*     This routine should only be called from within an AST handler 
*     routine. Its purpose is to enable a "RESCHED" event to be 
*     signalled to the main-line code.
*     It causes MESSYS_RECEIVE to detect a "RESCHED" condition 
*     and to return with a suitable status and the content of the 
*     RESCHED message.
*    Invocation :
*     CALL MESSYS_RESMSG ( LENGTH, VALUE, STATUS )
*    Parameters :
*     LENGTH=INTEGER (given)
*          number of significant bytes in VALUE
*     VALUE=CHARACTER*(*) (given)
*          message to be passed to main-line code
*     STATUS=INTEGER
*    Method :
*     Call the FAMS library.
*    Deficiencies :
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     B.D.Kelly (REVAD::BDK)
*    History :
*     13.05.1991:  original (REVAD::BDK)
*     12.04.1994: unix version (REVAD::BDK)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'

*    Import :
      INTEGER LENGTH         ! number of significant bytes in VALUE
      CHARACTER*(*) VALUE    ! message to be passed to main-line code

*    Status :
      INTEGER STATUS

*-

      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL FAMS_RESMSG ( LENGTH, VALUE, STATUS )

      END


*+  MESSYS_SEND - message system low-level routine to send a message
      SUBROUTINE MESSYS_SEND ( PATH, STUFF, MESSID, STATUS )
*    Description :
*     Send a MSG data-structure as a message to another process. 
*     The path to this process must already have been established using 
*     the PATH call.
*     Use of SEND implies that the other process is expected to send an
*     acknowledgment.
*     The path structure is added within this routine.
*    Invocation :
*     CALL MESSYS_SEND ( PATH, STUFF, MESSID, STATUS )
*    Method :
*     translate the message to internal format
*     Call the FAMS library.
*    Deficiencies :
*    Bugs :
*     None known
*    Authors :
*     John Cooke (REVS::JAC) 22Mar84
*     B.D.Kelly (REVAD::BDK) 08Mar88
*    History :
*     08.03.1988:  total rewrite to use MSP (REVAD::BDK)
*     14.03.1988:  don't use NULL_Q as reply queue (REVAD::BDK)
*     17.03.1988:  don't create ACK_Q for DE_INIT (REVAD::BDK)
*     11.04.1988:  add networking (REVAD::BDK)
*     28.04.1988:  don't append nodename to task when networking 
*                  (REVAD::BDK)
*     04.05.1988:  change OTHER_TASK_T_TRANS_NUM to OTHER_TRANSNUM 
*                  (REVAD::BDK)
*     12.04.1994: unix version (REVAD::BDK)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'MESSYS_LEN'

*    Import :
      INTEGER PATH             ! pointer to the path
      CHARACTER*(*) STUFF      ! structure to be sent

*    Export :
      INTEGER MESSID           ! message number issued by this task

*    Status :
      INTEGER STATUS

*    Local variables :
      INTEGER MESSAGE_FUNCTION         ! message function
      INTEGER MESSAGE_STATUS           ! message status
      INTEGER MESSAGE_CONTEXT          ! message context
      CHARACTER*(MSG_NAME_LEN-1) MESSAGE_NAME       ! message name
      INTEGER MESSAGE_LENGTH           ! length of value string
      CHARACTER*(MSG_VAL_LEN) MESSAGE_VALUE      ! value string
*-

      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL MESSYS_UNPACK ( STUFF, MESSAGE_FUNCTION, MESSAGE_STATUS,
     :  MESSAGE_CONTEXT, MESSAGE_NAME, MESSAGE_LENGTH, MESSAGE_VALUE,
     :  STATUS )
      CALL FAMS_SEND ( PATH, MESSAGE_FUNCTION, MESSAGE_STATUS,
     :  MESSAGE_CONTEXT, MESSAGE_NAME, MESSAGE_LENGTH, MESSAGE_VALUE,
     :  MESSID, STATUS )

      END



*+  MESSYS_UNPACK - convert a message from internal to external format
      SUBROUTINE MESSYS_UNPACK ( STUFF, MESSAGE_FUNCTION, 
     :  MESSAGE_STATUS, MESSAGE_CONTEXT, MESSAGE_NAME, MESSAGE_LENGTH, 
     :  MESSAGE_VALUE, STATUS )
*    Description :
*     Translate a message to be sent from external to internal format.
*    Invocation :
*     CALL MESSYS_UNPACK ( STUFF, MESSAGE_FUNCTION, MESSAGE_STATUS,
*    :  MESSAGE_CONTEXT, MESSAGE_NAME, MESSAGE_LENGTH, MESSAGE_VALUE,
*    :  STATUS )
*    Parameters :
*     STUFF=CHARACTER*(*) (given)
*           the message in external format
*     MESSAGE_FUNCTION=INTEGER (returned)
*           message function
*     MESSAGE_STATUS=INTEGER (returned)
*           message status
*     MESSAGE_CONTEXT=INTEGER (returned)
*           message context
*     MESSAGE_NAME=CHARACTER*(*) (returned)
*           message name
*     MESSAGE_LENGTH=INTEGER (returned)
*           length of value string
*     MESSAGE_VALUE=CHARACTER*(*) (returned)
*           value string
*     STATUS=INTEGER
*    Method :
*     Interpret the message.
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     B.D.Kelly (REVAD::BDK)
*    History :
*     12.04.1994:  original (REVAD::BDK)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'MESSYS_LEN'

*    Data structures :
      INCLUDE 'MESSYS_STRUC'

*    Import :
      INTEGER MESSAGE_FUNCTION         ! message function
      INTEGER MESSAGE_STATUS           ! message status
      INTEGER MESSAGE_CONTEXT          ! message context
      CHARACTER*(*) MESSAGE_NAME       ! message name
      INTEGER MESSAGE_LENGTH           ! length of value string
      CHARACTER*(*) MESSAGE_VALUE      ! value string

*    Export :
      CHARACTER*(*) STUFF       ! the message in external format

*    Status :
      INTEGER STATUS

*-

      IF ( STATUS .NE. SAI__OK ) RETURN

      MSG = STUFF

      MESSAGE_STATUS = MSG_STATUS
      MESSAGE_FUNCTION = MSG_FUNCTION 
      MESSAGE_CONTEXT = MSG_CONTEXT 
      MESSAGE_NAME = MSG_NAME 
      MESSAGE_LENGTH = MSG_LENGTH 
      MESSAGE_VALUE = MSG_VAL(1:MSG_LENGTH) 

      END
