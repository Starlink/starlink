*+  TASK_PUT_MESSINFO - set details of current action in subsidiary task
      SUBROUTINE TASK_PUT_MESSINFO ( PATH, CONTEXT, NAME, VALUE, MESSID,
     :  STATUS ) 
*    Description :
*     Sets details of the action in a subsidiary task that has given rise to
*     this ACT entry so that user code can call routines to retrieve the
*     information. This simply involves copying information to COMMON variables.
*    Invocation :
*     CALL TASK_PUT_MESSINFO ( PATH, CONTEXT, NAME, VALUE, MESSID, STATUS)
*    Parameters :
*     PATH=INTEGER (given)
*           The path identifying the subsidiary task
*     CONTEXT=INTEGER (given)
*           The subsidiary action context (OBEY or CANCEL)
*     NAME=CHARACTER*(*) (given)
*           The subsidiary action name
*     VALUE=CHARACTER*(*) (given)
*           The subsidiary action value string
*     MESSID=INTEGER (given)
*           The message id identifying the action in the subsidiary task
*     STATUS=INTEGER
*           Not checked on entry since it is the message status. Not altered.
*    Method :
*     Copy information to COMMON.
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     W.F.Lupton (AAOEPP::WFL)
*    History :
*     29.04.1989:  original (AAOEPP::WFL)
*     02.05.1989:  alter interface to be more consistent (AAOEPP::WFL)
*     23.04.1991:  rearrange INCLUDE files (REVAD::BDK)
*     06.05.1991:  remove ADAMDEFNS (REVAD::BDK)
*     24.05.1991:  use CURRACTVALIN for VALUE (REVAD::BDK)
*     12.06.1991:  use CURACTVALUE for VALUE (REVAD::BDK)
*      4.10.1992:  add PAR_PAR for porting (RLVAD::AJC)
*     24.08.1993:  Use SUBPAR_SYS not PAR_PAR (RLVAD::AJC)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'SUBPAR_SYS'
      INCLUDE 'MESSYS_LEN'
      INCLUDE 'TASK_PAR'

*    Import :
      INTEGER PATH          ! the path identifying the subsidiary task
      INTEGER CONTEXT       ! the subsidiary action context (OBEY or CANCEL)
      CHARACTER*(*) NAME    ! the subsidiary action name
      CHARACTER*(*) VALUE   ! the subsidiary action value string
      INTEGER MESSID        ! the message id id'ing the subsidiary action

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'TASK_CMN'
*-

*    Simply copy the values to COMMON.
*
      CURMESPATH = PATH
      CURMESCONTEXT = CONTEXT
      CURMESNAME = NAME
      CURACTVALUE = VALUE
      CURMESMESSID = MESSID
      CURMESSTATUS = STATUS

      END
