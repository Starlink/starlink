      SUBROUTINE DTASK_SETDUMP ( STATUS )
*+
*  Name:
*     DTASK_SETDUMP

*  Purpose:
*     Dummy for Unix: enable generation of stack dump on command

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL DTASK_SETDUMP ( STATUS )

*  Description:
*     Enable or re-enable facility for generating a stack dump of a 
*     task.

*  Arguments:
*     parameter[(dimensions)]=type(access)
*           <description of parameter>
*     STATUS=INTEGER

*  Algorithm:
*     The first call of this routine will be from mainline code. In this 
*     case, create a mailbox with a name based on the process name, and 
*     start a QIO to it declaring a completion AST handler. The AST 
*     handler generates a stack dump when some other task writes to the 
*     mailbox. The AST handler also calls this routine to re-enable the 
*     QIO.
*     Subsequent calls to this routine will be from the AST handler. In 
*     this case, restart the QIO.

*  Authors:
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     13-AUG-1986 (REVAD::BDK):
*        Original
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'
*  Status:
      INTEGER STATUS

*  Local Variables:
*.


      IF ( STATUS .NE. SAI__OK ) RETURN


      END
