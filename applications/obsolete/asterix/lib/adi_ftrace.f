      SUBROUTINE ADI_FTRACE( ID, NLEV, PATH, FILE, STATUS )
*+
*  Name:
*     ADI_FTRACE

*  Purpose:
*     Return text description of file position of identifier

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ADI_FTRACE( ID, NLEV, PATH, FILE, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier of file object
*     NLEV = INTEGER (returned)
*        Number of hierarchy levels
*     PATH = CHARACTER*(*) (returned)
*        Sub-file path
*     FILE = CHARACTER*(*) (returned)
*        File specification
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
      INTEGER			ID			! File identifier

*  Arguments Returned:
      INTEGER			NLEV			! Number of levels
      CHARACTER*(*)		PATH, FILE		! Trace info

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	LOC			! File handle
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Extract locator
      CALL ADI_CGET0C( ID, '.LOCATOR', LOC, STATUS )

*  Do the trace
      CALL HDS_TRACE( LOC, NLEV, PATH, FILE, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI_FTRACE', STATUS )

      END
