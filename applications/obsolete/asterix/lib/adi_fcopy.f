      SUBROUTINE USI_FCOPY( IN, OUT, STATUS )
*+
*  Name:
*     USI_FCOPY

*  Purpose:
*     Copy a dataset

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL USI_FCOPY( IN, OUT, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     IN = INTEGER (given)
*        Input file object
*     OUT = INTEGER (given)
*        Output file object
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
      INTEGER			IN, OUT			! File identifiers

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	ILOC			! Input file
      CHARACTER*(DAT__SZLOC)	OLOC			! Output file
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Extract locators and copy
      CALL ADI_CGET0C( IN, '.LOCATOR', ILOC, STATUS )
      CALL ADI_CGET0C( OUT, '.LOCATOR', OLOC, STATUS )
      CALL HDX_COPY( ILOC, OLOC, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'USI_FCOPY', STATUS )

      END
