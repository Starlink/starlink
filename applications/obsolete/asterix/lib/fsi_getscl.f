      SUBROUTINE FSI_GETSCL( FID, IDX, SCALE, STATUS )
*+
*  Name:
*     FSI_GETSEL

*  Purpose:
*     Read scaling from a FileSet object

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL FSI_GETSCL( FID, IDX, SCALE, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     FID = INTEGER (given)
*        ADI identifier of FileSet object
*     IDX = INTEGER (given)
*        Index of file in file set whose selection is to be read
*     SCALE = REAL (returned)
*        Scaling factor.
*     STATUS = INTEGER (given and returned)
*        The global status.

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

*  Pitfalls:
*     {pitfall_description}...

*  Notes:
*     {routine_notes}...

*  Prior Requirements:
*     {routine_prior_requirements}...

*  Side Effects:
*     {routine_side_effects}...

*  Algorithm:
*     {algorithm_description}...

*  Accuracy:
*     {routine_accuracy}

*  Timing:
*     {routine_timing}

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     FSI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/fsi.html

*  Keywords:
*     package:fsi, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     RB: Richard Beard (University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     22 Jun 1998: (RB)
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ADI_PAR'

*  Arguments Given:
      INTEGER                   FID, IDX

*  Arguments Returned:
      REAL			SCALE

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			ARGS(3)			! Method arguments
      INTEGER			OARG			! Method return data
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Construct selection object
      ARGS(1) = FID
      CALL ADI_GETLINK( FID, ARGS(2), STATUS )
      CALL ADI_NEWV0I( IDX, ARGS(3), STATUS )

*  Invoke method
      CALL ADI_EXEC( 'ReadScl', 3, ARGS, OARG, STATUS )

*  Destroy argument
      CALL ADI_ERASE( ARGS(3), STATUS )

*  Extract selections
      IF ( OARG .NE. ADI__NULLID ) THEN
        CALL ADI_GET0R( OARG, SCALE, STATUS )
        CALL ADI_ERASE( OARG, STATUS )
      ELSE
        SCALE = 1.0
      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'FSI_GETSCL', STATUS )

      END
