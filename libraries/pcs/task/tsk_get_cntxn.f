*+  TASK_GET_CONTEXTNAME - get current action context name
      SUBROUTINE TASK_GET_CONTEXTNAME ( CONTEXTNAME, STATUS )
*    Description :
*     Gets current action context name. This simply involves copying the
*     context from COMMON and converting it to a character string.
*    Invocation :
*     CALL TASK_GET_CONTEXTNAME ( CONTEXTNAME, STATUS)
*    Parameters :
*     CONTEXTNAME=CHARACTER*(*) (returned)
*           The action context ('OBEY' or 'CANCEL'), truncated if not large
*           enough. If context is invalid, blank is returned.
*     STATUS=INTEGER
*    Method :
*     Copy and convert information from COMMON.
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     W.F.Lupton (AAOEPP::WFL)
*    History :
*     29.04.1989:  original (AAOEPP::WFL)
*     01.03.1990:  make DATA statements more obvious make more secure
*                  (AAOEPP::WFL)
*     23.04.1991:  rearrange INCLUDE files (REVAD::BDK)
*      4.10.1992:  add PAR_PAR for porting (RLVAD::AJC)
*      8.10.1992:  add CONTROL context (RLVAD::AJC)
*     24.08.1993:  Use SUBPAR_SYS not PAR_PAR (RLVAD::AJC)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'SUBPAR_SYS'
      INCLUDE 'ADAM_DEFNS'
      INCLUDE 'MESSYS_LEN'
      INCLUDE 'TASK_PAR'

*    Export :
      CHARACTER*(*) CONTEXTNAME ! the action context name ('OBEY' or 'CANCEL')

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'TASK_CMN'

*    Local variables :
      CHARACTER*7 CONTEXTNAMES(0:6) ! ADAM context names

*    Local data :
      DATA CONTEXTNAMES(0) / ' ' /
      DATA CONTEXTNAMES(SET) / 'SET' /
      DATA CONTEXTNAMES(GET) / 'GET' /
      DATA CONTEXTNAMES(OBEY) / 'OBEY' /
      DATA CONTEXTNAMES(CANCEL) / 'CANCEL' /
      DATA CONTEXTNAMES(CONTROL) / 'CONTROL' /
      DATA CONTEXTNAMES(6) / ' ' /
*-
      IF ( STATUS .NE. SAI__OK ) RETURN
*
*    Simply copy the value from COMMON.
*
      CONTEXTNAME = CONTEXTNAMES(MAX(0,MIN(6,CURACTCONTEXT)))

      END
