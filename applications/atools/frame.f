      SUBROUTINE FRAME( STATUS )
*+
*  Name:
*     FRAME

*  Purpose:
*     Create a Frame.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL FRAME( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application creates a new Frame and optionally initialises its
*     attributes. A Frame is used to represent a coordinate system. It does
*     this in rather the same way that a frame around a graph describes the 
*     coordinate space in which data are plotted. Consequently, a Frame has 
*     a Title (string) attribute, which describes the coordinate space, and 
*     contains axes which in turn hold information such as Label and Units 
*     strings which are used for labelling (e.g.) graphical output. In
*     general, however, the number of axes is not restricted to two. 

*  Usage:
*     frame naxes options result

*  ADAM Parameters:
*     NAXES = _INTEGER (Read)
*        The number of Frame axes (i.e. the number of dimensions of the
*        coordinate space which the Frame describes). 
*     OPTIONS = LITERAL (Read)
*        A string containing an optional comma-separated list of attribute 
*        assignments to be used for initialising the new Frame. 
*     RESULT = LITERAL (Read)
*        A text file to receive the new Frame.

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
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER NAX
      INTEGER RESULT
*.

*  Check inherited status.      
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get the number of axes required.
      CALL PAR_GDR0I( 'NAXES', 2, 1, NDF__MXDIM, .FALSE., NAX, STATUS ) 

*  Create the required Frame.
      RESULT = AST_FRAME( NAX, ' ', STATUS )

*  Store the required attribute values.
      CALL ATL1_SETOP( 'OPTIONS', RESULT, STATUS )

*  Write the results out to a text file.
      CALL ATL1_PTOBJ( 'RESULT', ' ', RESULT, STATUS )

 999  CONTINUE

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'FRAME_ERR', 'Error creating a new Frame.',
     :                 STATUS )
      END IF

      END
