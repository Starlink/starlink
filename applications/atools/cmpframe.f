      SUBROUTINE CMPFRAME( STATUS )
*+
*  Name:
*     CMPFRAME

*  Purpose:
*     Create a CmpFrame.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL CMPFRAME( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application creates a new CmpFrame and optionally initialises
*     its attributes. A CmpFrame is a compound Frame which allows two 
*     component Frames (of any class) to be merged together to form a 
*     more complex Frame. The axes of the two component Frames then 
*     appear together in the resulting CmpFrame (those of the first Frame, 
*     followed by those of the second Frame). 
*
*     Since a CmpFrame is itself a Frame, it can be used as a component in
*     forming further CmpFrames. Frames of arbitrary complexity may be 
*     built from simple individual Frames in this way. 

*  Usage:
*     cmpframe frame1 frame2 options result

*  ADAM Parameters:
*     FRAME1 = LITERAL (Read) 
*        An NDF or text file holding the first component Frame. If an NDF 
*        is supplied, the current Frame in its WCS FrameSet will be used.
*     FRAME2 = LITERAL (Read) 
*        An NDF or text file holding the second component Frame. If an NDF 
*        is supplied, the current Frame in its WCS FrameSet will be used.
*     OPTIONS = LITERAL (Read)
*        A string containing an optional comma-separated list of attribute 
*        assignments to be used for initialising the new CmpFrame. 
*     RESULT = LITERAL (Read)
*        A text file to receive the new CmpFrame. 

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
      EXTERNAL AST_ISAFRAME

*  Local Variables:
      INTEGER RESULT
      INTEGER FRAME1
      INTEGER FRAME2
*.

*  Check inherited status.      
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get the first Frame.
      CALL ATL1_GTOBJ( 'FRAME1', 'Frame', AST_ISAFRAME, FRAME1, STATUS )

*  Get the second Frame.
      CALL ATL1_GTOBJ( 'FRAME2', 'Frame', AST_ISAFRAME, FRAME2, STATUS )

*  Create the required Frame.
      RESULT = AST_CMPFRAME( FRAME1, FRAME2, ' ', STATUS )

*  Store the required attribute values.
      CALL ATL1_SETOP( 'OPTIONS', RESULT, STATUS )

*  Write the results out to a text file.
      CALL ATL1_PTOBJ( 'RESULT', ' ', RESULT, STATUS )

 999  CONTINUE

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'CMPFRAME_ERR', 'Error creating a new CmpFrame.',
     :                 STATUS )
      END IF

      END
