      SUBROUTINE HSI_OK( IFID, OK, STATUS )
*+
*  Name:
*     HSI_OK

*  Purpose:
*     Check that history exists for the dataset

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL HSI_OK( IFID, OK, STATUS )

*  Description:
*     Checks that a history structure exists in the dataset supplied.

*  Arguments:
*     IFID = INTEGER (given)
*        ADI identifier of input dataset
*     OK = LOGICAL (returned)
*        History structure is ok?
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
      INTEGER			IFID			! Input dataset

*  Arguments Returned:
      LOGICAL			OK			! History is ok?

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	ILOC			! HDS dataset
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Simply invoke HDS routine for now
      CALL ADI_CGET0C( IFID, 'LOCATOR', ILOC, STATUS )
      CALL HIST_OK( ILOC, OK, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'HSI_OK', STATUS )

      END
