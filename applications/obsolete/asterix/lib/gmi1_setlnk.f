      SUBROUTINE GMI1_SETLNK( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     GMI1_SETLNK

*  Purpose:
*     {routine_purpose}

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL GMI1_SETLNK( NARG, ARGS, OARG, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     NARG = INTEGER (given)
*        Number of method arguments
*     ARGS(*) = INTEGER (given)
*        ADI identifier of method arguments
*     OARG = INTEGER (returned)
*        Output data
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
*     GMI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/gmi.html

*  Keywords:
*     package:gmi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     9 Oct 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'
      INCLUDE 'ADI_PAR'
      INCLUDE 'ADI_ERR'

*  Arguments Given:
      INTEGER                   NARG, ARGS(*)

*  Arguments Returned:
      INTEGER                   OARG

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	LOC			! Dataset locator

      LOGICAL			OK			! Input is a m/g?
      LOGICAL			PRIM			! Input is primitive?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set return argument
      OARG = ADI__NULLID

*  Get locator
      CALL ADI1_GETLOC( ARGS(2), LOC, STATUS )

*  Check if primitive object
      OK = .FALSE.
      CALL DAT_PRIM( LOC, PRIM, STATUS )
      IF ( .NOT. PRIM ) THEN

*    Otherwise look for NDF component
        CALL DAT_THERE( LOC, 'NDF', OK, STATUS )

      END IF

*  It is a multi-graph
      IF ( OK ) THEN
        CALL ADI_SETLNK( ARGS(1), ARGS(2), STATUS )

      ELSE

*    Allow retry if not a multi-graph object
        STATUS = ADI__RETRY
        CALL ERR_REP( ' ', 'Input is not a multigraph dataset', STATUS )

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'GMI1_SETLNK', STATUS )

      END
