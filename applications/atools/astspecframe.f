      SUBROUTINE ASTSPECFRAME( STATUS )
*+
*  Name:
*     ASTSPECFRAME

*  Purpose:
*     Create a SpecFrame.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASTSPECFRAME( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application creates a new SpecFrame and optionally initialises
*     its attributes. A SpecFrame is a specialised form of one-dimensional 
*     Frame which represents various coordinate systems used to describe 
*     positions within an electro-magnetic spectrum. The particular 
*     coordinate system to be used is specified by setting the SpecFrame's 
*     System attribute (the default is wavelength) qualified, as necessary, 
*     by other attributes such as the rest frequency, the standard of rest, 
*     the epoch of observation, units, etc (see the description of the System 
*     attribute for details).

*  Usage:
*     astspecframe options result

*  ADAM Parameters:
*     OPTIONS = LITERAL (Read)
*        A string containing an optional comma-separated list of attribute 
*        assignments to be used for initialising the new SpecFrame. 
*     RESULT = LITERAL (Read)
*        A text file to receive the new SpecFrame.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-DEC-2002 (DSB):
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
      INTEGER RESULT
*.

*  Check inherited status.      
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Create the required SpecFrame.
      RESULT = AST_SPECFRAME( ' ', STATUS )

*  Store the required attribute values.
      CALL ATL1_SETOP( 'OPTIONS', RESULT, STATUS )

*  Write the results out to a text file.
      CALL ATL1_PTOBJ( 'RESULT', ' ', RESULT, STATUS )

 999  CONTINUE

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ASTSPECFRAME_ERR', 'Error creating a new '//
     :                 'SpecFrame.', STATUS )
      END IF

      END
