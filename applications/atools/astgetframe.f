      SUBROUTINE ASTGETFRAME( STATUS )
*+
*  Name:
*     ASTGETFRAME

*  Purpose:
*     Obtain a specified Frame in a FrameSet

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASTGETFRAME( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application returns a copy of a specified Frame in a FrameSet.

*  Usage:
*     astgetframe this iframe result

*  ADAM Parameters:
*     IFRAME = LITERAL (Read)
*        The integer index or Domain name of the required Frame within the
*        FrameSet (the strings AST__BASE and AST__CURRENT may also be
*        supplied).
*     RESULT = LITERAL (Read)
*        An text file to receive the Frame.
*     THIS = LITERAL (Read)
*        An NDF or text file holding the FrameSet. If an NDF is supplied, 
*        the WCS FrameSet will be used.

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

*  Status:
      INTEGER STATUS

*  External References:
      EXTERNAL AST_ISAFRAMESET

*  Local Variables:
      INTEGER IAST
      INTEGER IFRM
      INTEGER RESULT
*.

*  Check inherited status.      
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get an AST Object.
      CALL ATL1_GTOBJ( 'THIS', 'FrameSet', AST_ISAFRAMESET, IAST, 
     :                 STATUS )

*  Get the index of the required Frame.
      CALL ATL1_GTFRM( 'IFRAME', IAST, IFRM, STATUS )

*  Get the required Frame.
      RESULT = AST_GETFRAME( IAST, IFRM, STATUS )

*  Write the results out to a text file.
      CALL ATL1_PTOBJ( 'RESULT', ' ', RESULT, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ASTGETFRAME_ERR', 'Error extracting a Frame '//
     :                 'from a FrameSet.', STATUS )
      END IF

      END
