      SUBROUTINE ADI1_GETLOC( ID, LOC, STATUS )
*+
*  Name:
*     ADI1_GETLOC

*  Purpose:
*     Extract locator from dataset object

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI1_GETLOC( ID, LOC, STATUS )

*  Description:
*     Extract the locator from an object derived from HDSlocator.

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier of instance of object derived from HDSlocator
*     LOC = CHARACTER*(DAT__SZLOC) (returned)
*        The HDS locator to the object
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
*     ADI Subroutine Guide : http://www.sr.bham.ac.uk:8080/asterix-docs/Programmer/Guides/adi.html

*  Keywords:
*     package:adi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     14 Feb 1995 (DJA):
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
      INTEGER			ID			! Dataset object

*  Arguments Returned:
      CHARACTER*(DAT__SZLOC)	LOC			! Locator to object

*  Status:
      INTEGER 			STATUS             	! Global status
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Extract locator
      CALL ADI_CGET0C( ID, 'Locator', LOC, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI1_GETLOC', STATUS )

      END
