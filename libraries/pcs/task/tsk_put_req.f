*+  TASK_PUT_REQUEST - set REQUEST for action be the fixed-part
      SUBROUTINE TASK_PUT_REQUEST ( REQUEST, STATUS )
*    Description :
*     Sets the REQUEST for interpretation by the fixed-part.
*     This simply involves copying to COMMON.
*    Invocation :
*     CALL TASK_PUT_REQUEST ( REQUEST, STATUS )
*    Parameters :
*     REQUEST=INTEGER (given)
*           The REQUEST from the application for action by the 
*           fixed-part.
*     STATUS=INTEGER
*    Method :
*     Copy information to COMMON.
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     B.D.Kelly (REVAD::BDK)
*    History :
*     22.08.1991:  original (REVAD::BDK)
*      9.10.1992:  Add PAR_PAR (RLVAD::AJC)
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
      INTEGER REQUEST   ! the REQUEST for the fixed-part

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'TASK_CMN'
*-
      IF ( STATUS .NE. SAI__OK ) RETURN
*
*    Simply copy the REQUEST to COMMON.
*
      CURACTREQUEST = REQUEST

      END
