      SUBROUTINE ASTINVERT( STATUS )
*+
*  Name:
*     ASTINVERT

*  Purpose:
*     Invert a Mapping.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASTINVERT( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application inverts a Mapping. If a FrameSet is supplied, the
*     Base and Current Frames will be swapped.

*  Usage:
*     astinvert this result

*  ADAM Parameters:
*     RESULT = LITERAL (Read)
*        An text file to receive the inverted Mapping.
*     THIS = LITERAL (Read)
*        An NDF or text file holding the Mapping. If an NDF is supplied, 
*        the Mapping from the base Frame of the WCS FrameSet to the
*        current Frame will be used.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     9-OCT-2002 (DSB):
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
      INTEGER RESULT
*.

*  Check inherited status.      
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get the Mapping.
      CALL ATL1_GTOBJ( 'THIS', ' ', AST_NULL, THIS, STATUS )

*  Invert the Mapping.
      CALL AST_INVERT( THIS, STATUS )

*  Write this Mapping out to a text file.
      CALL ATL1_PTOBJ( 'RESULT', ' ', THIS, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ASTINVERT_ERR', 'Error inverting a '//
     :                 'Mapping.', STATUS )
      END IF

      END
