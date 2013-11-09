      SUBROUTINE MPC_END( ID, STATUS )
*+
*  Name:
*     MPC_END

*  Purpose:
*     End multi-processing

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL MPC_END( ID, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier of multi process control object
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
*     MPC Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/mpc.html

*  Keywords:
*     package:mpc, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1996

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     15 Apr 1996 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER			ID

*  Status:
      INTEGER 			STATUS             	! Global status
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Destroy ADI object
      CALL ADI_ERASE( ID, STATUS )

*  Free process resources
      CALL BSP_FINISH()

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'MPC_END', STATUS )

      END
