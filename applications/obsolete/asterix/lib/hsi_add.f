      SUBROUTINE HSI_ADD( IFID, NAME, STATUS )
*+
*  Name:
*     HSI_ADD

*  Purpose:
*     Create new history record

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL HSI_ADD( IFID, NAME, STATUS )

*  Description:
*     Create a new history record. The program creating the history is
*     named in the second argument.

*  Arguments:
*     IFID = INTEGER (given)
*        ADI identifier of the dataset
*     NAME = CHARACTER*(*) (given)
*        The name of the program writing the history
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

*  {machine}-specific features used:
*     {routine_machine_specifics}...

*  {DIY_prologue_heading}:
*     {DIY_prologue_text}

*  References:
*     {routine_references}...

*  Keywords:
*     {routine_keywords}...

*  Copyright:
*     {routine_copyright}

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     12 Jan 1995 (DJA):
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

*  Arguments Given:
      INTEGER			IFID			! Dataset identifier
      CHARACTER*(*)		NAME			! Program name

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	ILOC			! Dataset locator
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get locator and use HDS version
      CALL ADI_CGET0C( IFID, 'LOCATOR', ILOC, STATUS )
      CALL HIST_ADD( ILOC, NAME, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'HSI_ADD', STATUS )

      END
