      SUBROUTINE BDI0_STOINV( ID, ITEM, ITID, STATUS )
*+
*  Name:
*     BDI0_STOINV

*  Purpose:
*     Store invented data for named item

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI0_STOINV( ID, ITEM, ITID, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier of top level BDI object
*     ITEM = CHARACTER*(*) (given)
*        The name of the item
*     ITID = INTEGER (given)
*        ADI identifier of the item data
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
*     5 Dec 1995 (DJA):
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
      INTEGER			ID, ITID
      CHARACTER*(*)		ITEM

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Constants:
      CHARACTER*11		IPROP
        PARAMETER 		( IPROP = '.Inventions' )

*  Local Variables:
      INTEGER			INVID			! Inventions store

      LOGICAL			THERE			! Component exists?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Locate the Inventions property
      CALL ADI_THERE( ID, IPROP, THERE, STATUS )
      IF ( .NOT. THERE ) THEN
        CALL ADI_CNEW0( ID, IPROP, 'STRUC', STATUS )
      END IF
      CALL ADI_FIND( ID, IPROP, INVID, STATUS )

*  Write item as component
      CALL ADI_CPUTID( INVID, ITEM, ITID, STATUS )

*  Release inventions store
      CALL ADI_ERASE( INVID, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'BDI0_STOINV', STATUS )

      END
