      SUBROUTINE ASTCOPY( STATUS )
*+
*  Name:
*     ASTCOPY

*  Purpose:
*     Copy an AST Object.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASTCOPY( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application reads an AST Object from a file and creates a copy
*     of it stored either in a new file or in an existing NDF. For example,
*     you can read a FrameSet from a set of FITS headers, and store it as
*     the WCS FrameSet in an NDF.

*  Usage:
*     astcopy this result

*  ADAM Parameters:
*     RESULT = LITERAL (Read)
*        A text file or NDF to receive the Object.
*     THIS = LITERAL (Read)
*        A text file or NDF containing the Object to be copied.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12-JAN-2001 (DSB):
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

*  External References:
      EXTERNAL AST_ISAOBJECT

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER RESULT
      INTEGER THIS
*.

*  Check inherited status.      
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get the object to copy.
      CALL ATL1_GTOBJ( 'THIS', 'Object', AST_ISAOBJECT, THIS, STATUS )

*  Write the FrameSet out to a text file.
      CALL ATL1_PTOBJ( 'RESULT', ' ', THIS, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ASTCOPY_ERR', 'Error copying an AST Object.',
     :                 STATUS )
      END IF

      END
