      SUBROUTINE BDI0_ADJMCT( PSID, DELTA, STATUS )
*+
*  Name:
*     BDI0_ADJMCT

*  Purpose:
*     Adjust mapping count of a BDI item

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI0_ADJMCT( PSID, DELTA, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     PSID = INTEGER (given)
*        The ADI identifier of the private storage area
*     DELTA = INTEGER (given)
*        Amount to add to mapping count
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
*     BDI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/bdi.html

*  Keywords:
*     package:bdi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     30 Aug 1995 (DJA):
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
      INTEGER			PSID, DELTA

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			IVAL			! Map count value
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Read current map count
      CALL ADI_CGET0I( PSID, 'MapCount', IVAL, STATUS )
      IVAL = IVAL + DELTA

*  Store map count
      CALL ADI_CPUT0I( PSID, 'MapCount', IVAL, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'BDI0_ADJMCT', STATUS )

      END
