      SUBROUTINE ADI1_GRPLOC( ID, LOC, STATUS )
*+
*  Name:
*     ADI1_GRPLOC

*  Purpose:
*     Group an HDS locator as being associated with an ADI identifier

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI1_GRPLOC( ID, LOC, STATUS )

*  Description:
*     Put the locator supplied into the name group associated with a
*     particular ADI identifier. This makes cleaning up locators rather
*     easy.

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier
*     LOC = CHARACTER*(DAT__SZNAM) (given)
*        The HDS group name
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
*     ADI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/adi.html

*  Keywords:
*     package:adi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     6 Sep 1995 (DJA):
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
      INTEGER			ID
      CHARACTER*(DAT__SZLOC)	LOC

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*12		GRPNAM			! Group name
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the group name
      CALL ADI1_MKGRP( ID, GRPNAM, STATUS )

*  Group the locator
      CALL HDS_LINK( LOC, GRPNAM, STATUS )

      END
