*+  DTASK_SETDUMP - Dummy for Unix: enable generation of stack dump on command
      SUBROUTINE DTASK_SETDUMP ( STATUS )
*    Description :
*     Enable or re-enable facility for generating a stack dump of a 
*     task.
*    Invocation :
*     CALL DTASK_SETDUMP ( STATUS )
*    Parameters :
*     parameter[(dimensions)]=type(access)
*           <description of parameter>
*     STATUS=INTEGER
*    Method :
*     The first call of this routine will be from mainline code. In this 
*     case, create a mailbox with a name based on the process name, and 
*     start a QIO to it declaring a completion AST handler. The AST 
*     handler generates a stack dump when some other task writes to the 
*     mailbox. The AST handler also calls this routine to re-enable the 
*     QIO.
*     Subsequent calls to this routine will be from the AST handler. In 
*     this case, restart the QIO.
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     B.D.Kelly (REVAD::BDK)
*    History :
*     13.08.1986:  Original (REVAD::BDK)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS

*    Local variables :
*-


      IF ( STATUS .NE. SAI__OK ) RETURN


      END
