      SUBROUTINE GMI1_NEWLNK( LHS, RHS, STATUS )
*+
*  Name:
*     GMI1_NEWLNK

*  Purpose:
*     Make link between MultiGraph object and new HDS file

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL GMI1_NEWLNK( LHS, RHS, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     LHS = INTEGER (given)
*        The high level object, in this case the MultiGraph object
*     RHS = INTEGER (given)
*        The low level object, in this case the HDS file
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

*  Arguments Given:
      INTEGER                   LHS, RHS

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	LOC			! Dataset locator
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Extract locator
      CALL ADI1_GETLOC( RHS, LOC, STATUS )

*  Change the type
      CALL DAT_RETYP( LOC, 'GRAFIX', STATUS )

*  Make the file link
      CALL ADI_SETLNK( LHS, RHS, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'GMI1_NEWLNK', STATUS )

      END
