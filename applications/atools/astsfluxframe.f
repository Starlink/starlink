      SUBROUTINE ASTSFLUXFRAME( STATUS )
*+
*  Name:
*     ASTSFLUXFRAME

*  Purpose:
*     Create a SpecFluxFrame.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASTSFLUXFRAME( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application creates a new SpecFluxFrame and optionally initialises
*     its attributes. 
*
*     A SpecFluxFrame combines a SpecFrame and a FluxFrame into a single 
*     2-dimensional compound Frame. Such a Frame can for instance be used
*     to describe a Plot of a spectrum in which the first axis represents
*     spectral position and the second axis represents flux.

*  Usage:
*     astspecfluxframe frame1 frame2 options result

*  ADAM Parameters:
*     FRAME1 = LITERAL (Read) 
*        An NDF or text file holding the SpecFrame. This will form the first 
*        axis in the new SpecFluxFrame. If an NDF is supplied, the current 
*        Frame in its WCS FrameSet will be used (which must be a SpecFrame).
*     FRAME2 = LITERAL (Read) 
*        An NDF or text file holding the FluxFrame. This will form the second 
*        axis in the new SpecFluxFrame. The "SpecVal" attribute of this 
*        FluxFrame is not used by the SpecFluxFrame class and so may be set 
*        null when the FluxFrame is created. If an NDF is supplied, the 
*        current Frame in its WCS FrameSet will be used (which must be a
*        FluxFrame).
*     OPTIONS = LITERAL (Read)
*        A string containing an optional comma-separated list of attribute 
*        assignments to be used for initialising the new SpecFluxFrame. 
*     RESULT = LITERAL (Read)
*        A text file to receive the new SpecFluxFrame. 

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     6-JAN-2005 (DSB):
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
      EXTERNAL AST_ISAFLUXFRAME
      EXTERNAL AST_ISASPECFRAME

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
      CALL ATL1_GTOBJ( 'FRAME1', 'SpecFrame', AST_ISASPECFRAME, FRAME1, 
     :                 STATUS )

*  Get the second Frame.
      CALL ATL1_GTOBJ( 'FRAME2', 'FluxFrame', AST_ISAFLUXFRAME, FRAME2, 
     :                 STATUS )

*  Create the required SpecFluxFrame.
      RESULT = AST_SPECFLUXFRAME( FRAME1, FRAME2, ' ', STATUS )

*  Store the required attribute values.
      CALL ATL1_SETOP( 'OPTIONS', RESULT, STATUS )

*  Write the results out to a text file.
      CALL ATL1_PTOBJ( 'RESULT', ' ', RESULT, STATUS )

 999  CONTINUE

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ASTSFLUXFRAME_ERR', 'Error creating a new '//
     :                 'SpecFluxFrame.', STATUS )
      END IF

      END
