      SUBROUTINE ASTGETACTUNIT( STATUS )
*+
*  Name:
*     ASTGETACTUNIT

*  Purpose:
*     Get the value of the ActiveUnit flag for a rame.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASTGETACTUNIT( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application displays the current value of the ActiveUnit flag for
*     a Frame (see the description of the ASTSETACTUNIT command for a 
*     description of the ActiveUnit flag). The value of the flag is also 
*     written to an output parameter.

*  Usage:
*     astgetactunit this 

*  ADAM Parameters:
*     THIS = LITERAL (Read)
*        An NDF or text file holding the Frame. If an NDF is supplied, 
*        the WCS FrameSet will be used.
*     VALUE = _LOGICAL (Write)
*        On exit, this holds a boolean value indicating if the ActiveUnit
*        flag was set or not.

*  Notes:
*     - This application corresponds to the AST routine AST_GETACTIVEUNIT. 
*     The name has been abbreviated due to a limitation on the length of 
*     ADAM command names.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-SEP-2003 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER THIS
      LOGICAL VAL
*.

*  Check inherited status.      
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get a Frame.
      CALL ATL1_GTOBJ( 'THIS', 'Frame', AST_ISAFRAME, THIS, STATUS )

*  Get the flag.
      VAL = AST_GETACTIVEUNIT( THIS, STATUS )

*  Display it.
      CALL MSG_SETL( 'V', VAL )
      CALL MSG_OUT( 'ASTGETACTUNIT_MSG1', '^V', STATUS )

*  Store it in an output parameter.
      CALL PAR_PUT0L( 'VALUE', VAL, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ASTGETACTUNIT_ERR', 'Error getting the '//
     :                 'value of the ActiveUnit flag for an AST Frame.', 
     :                 STATUS )
      END IF

      END
