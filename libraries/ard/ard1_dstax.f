      SUBROUTINE ARD1_DSTAX( FRM, AXIS, STATUS )
*+
*  Name:
*     ARD1_DSTAX

*  Purpose:
*     Decide which Frame axis to use for measuring distances.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_DSTAX( FRM, AXIS, STATUS )

*  Description:
*     Distances supplied as arguments within keywords must be given as 
*     increments along one of the Frame axes (so that we can use 
*     AST_UNFORMAT to convert them to floating poiint). For normal Frames,
*     both axes are assumed to be equivalent and so either axis will do, 
*     and we arbitrarily choose axis 1. For SkyFrames, distances are
*     assumed to be arc-distances, and so are treated as an increment
*     along the latitude axis. The index of the selected axis is returned.

*  Arguments:
*     FRM = INTEGER (Given)
*        An AST pointer to a Frame describing user coordinates.
*     AXIS = INTEGER (Returned)
*        The axis index of the Axis within FRM which is to be used for
*        unformatting distance values.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     24-AUG-2001 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST_ constants

*  Arguments Given:
      INTEGER FRM

*  Arguments Returned:
      INTEGER AXIS

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      IF( AST_ISASKYFRAME( FRM, STATUS ) ) THEN
         AXIS = AST_GETI( FRM, 'LATAXIS', STATUS )
      ELSE
         AXIS = 1
      END IF

      END
