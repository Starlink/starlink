      SUBROUTINE ADI1_PUTLOC( LOC, ID, STATUS )
*+
*  Name:
*     ADI1_PUTLOC

*  Purpose:
*     Wrap an HDS locator in an ADI object

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI1_PUTLOC( LOC, ID, STATUS )

*  Description:
*     Store an HDS locator in an ADI object. If the null HDS locator is
*     passed, then the output is the null ADI object, NOT an wrapped up
*     copy of the null locator.

*  Arguments:
*     LOC = CHARACTER*(DAT__SZLOC) (given)
*        HDS locator
*     ID = INTEGER (returned)
*        ADI identifier
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
      INCLUDE 'ADI_PAR'
      INCLUDE 'DAT_PAR'

*  Arguments Given:
      CHARACTER*(DAT__SZLOC)	LOC			! Locator to store

*  Arguments Returned:
      INTEGER			ID			! ADI identifier

*  Status:
      INTEGER 			STATUS             	! Global status
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Create ADI object to handle the locator
      IF ( LOC .EQ. DAT__NOLOC ) THEN
        ID = ADI__NULLID
      ELSE
        CALL ADI_NEW0( 'HDSfile', ID, STATUS )
        CALL ADI_CPUT0C( ID, 'Locator', LOC, STATUS )
      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI1_PUTLOC', STATUS )

      END
