      SUBROUTINE HSI_COPY( IFID, OFID, STATUS )
*+
*  Name:
*     HSI_COPY

*  Purpose:
*     Copy history from first dataset to the second

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL HSI_COPY( IFID, OFID, STATUS )

*  Description:
*     Copy all history records from the first dataset to the second.
*     Although this routine is just using HDS at present, the interface
*     will not change when it is FITS capable.

*  Arguments:
*     IFID = INTEGER (given)
*        ADI identifier to input dataset
*     OFID = INTEGER (given)
*        ADI identifier to output dataset
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
      INTEGER			OFID			! Output dataset

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	ILOC			! Input locator
      CHARACTER*(DAT__SZLOC)	OLOC			! Output locator
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Simply extract locators and invoke HDS version for the momemnt
      CALL ADI_CGET0C( IFID, 'LOCATOR', ILOC, STATUS )
      CALL ADI_CGET0C( OFID, 'LOCATOR', OLOC, STATUS )
      CALL HIST_COPY( ILOC, OLOC, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'HSI_COPY', STATUS )

      END
