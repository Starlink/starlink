*+  TASK_GET_MESSINFO - get details of current action in subsidiary task
      SUBROUTINE TASK_GET_MESSINFO ( PATH, CONTEXT, NAME, VALUE, MESSID,
     :  EVENT, STATUS ) 
*    Description :
*     Gets details of the action in a subsidiary task that has given rise to
*     this ACT entry. This simply involves copying information from COMMON
*     variables.
*    Invocation :
*     CALL TASK_GET_MESSINFO ( PATH, CONTEXT, NAME, VALUE, MESSID, EVENT,
*     STATUS)
*    Parameters :
*     PATH=INTEGER (returned)
*           The path identifying the subsidiary task
*     CONTEXT=INTEGER (returned)
*           The subsidiary action context (OBEY or CANCEL)
*     NAME=INTEGER (returned)
*           The subsidiary action name
*     VALUE=CHARACTER*(*) (returned)
*           The subsidiary action value string
*     MESSID=INTEGER (returned)
*           The message id identifying the action in the subsidiary task
*     EVENT=INTEGER (returned)
*           The subsidiary action message status
*     STATUS=INTEGER
*           Normal inherited status
*    Method :
*     Copy information from COMMON.
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
*     17.09.1991:  separate EVENT and STATUS (RLVAD::AJC)
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

*    Export :
      INTEGER PATH          ! the path identifying the subsidiary task
      INTEGER CONTEXT       ! the subsidiary action context (OBEY or CANCEL)
      CHARACTER*(*) NAME    ! the subsidiary action name
      CHARACTER*(*) VALUE   ! the subsidiary action value string
      INTEGER MESSID        ! the message id id'ing the subsidiary action
      INTEGER EVENT         ! the subsidiary action message status

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'TASK_CMN'
*-
      IF ( STATUS .NE. SAI__OK ) RETURN
*
*    Simply copy the values from COMMON.
*
      PATH = CURMESPATH 
      CONTEXT = CURMESCONTEXT 
      NAME = CURMESNAME
      VALUE = CURACTVALUE
      MESSID = CURMESMESSID 
      EVENT = CURMESSTATUS 

      END
