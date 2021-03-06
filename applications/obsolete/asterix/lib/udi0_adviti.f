      SUBROUTINE UDI0_ADVITI( ITEMS, START, STOP, IITEM, STATUS )
*+
*  Name:
*     UDI0_ADVITI

*  Purpose:
*     Advance one item down the item list

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL UDI0_ADVITI( ITEMS, START, STOP, IITEM, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     ITEMS = CHARACTER*(*) (given)
*        Item list to iterate over
*     START = INTEGER (given and returned)
*        Character index of start of first item in list
*     STOP = INTEGER (given and returned)
*        Character index of end of first item in list
*     IITEM = INTEGER (given and returned)
*        Item counter
*     STATUS = INTEGER (given)
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
*     UDI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/udi.html

*  Keywords:
*     package:udi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     9 Aug 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CHR_ERR'

*  Arguments Given:
      CHARACTER*(*)		ITEMS

*  Arguments Given and Returned:
      INTEGER			START, STOP, IITEM

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			CHR_FIWE
      EXTERNAL			CHR_FIWS
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  First start of first word
      START = STOP + 2
      CALL CHR_FIWS( ITEMS, START, STATUS )

*  Find end of this word
      STOP = START + 1
      CALL CHR_FIWE( ITEMS, STOP, STATUS )

*  Increment counter
      IITEM = IITEM + 1

*  If bad status then end of list is met
      IF ( STATUS .EQ. CHR__EOSNT ) THEN
        CALL ERR_ANNUL( STATUS )

      ELSE IF ( STATUS .NE. SAI__OK ) THEN
        START = 0
        STOP = 0
        CALL ERR_ANNUL( STATUS )
      END IF

      END
