      SUBROUTINE BDI0_CHKOP( ITEM, OP, STATUS )
*+
*  Name:
*     BDI0_CHKOP

*  Purpose:
*     Check operation is valid on an item

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI0_CHKOP( ITEM, OP, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     ITEM = CHARACTER*(*) (given)
*        Item operation is to be performed on
*     OP = CHARACTER*(*) (given)
*        Operation name
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

*  Arguments Given:
      CHARACTER*(*)		ITEM, OP

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			CHR_INSET
        LOGICAL			CHR_INSET

*  Local Variables:
      INTEGER			L			! Length of ITEM
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Length of item
      L = LEN(ITEM)

*  Toggle on operation
      IF ( OP .EQ. 'Map' ) THEN
        IF ( CHR_INSET( 'Units,Label,Title', ITEM(MAX(1,L-4):) ) .OR.
     :       (ITEM .EQ. 'QualityMask' ) .OR.
     :       (ITEM(MAX(1,L-9):) .EQ. 'Normalised' ) ) THEN
          STATUS = SAI__ERROR
          CALL MSG_SETC( 'OP', OP )
          CALL MSG_SETC( 'ITEM', ITEM )
          CALL ERR_REP( 'BDI0_CHKSCL_BAD', 'Operation "^OP" cannot '/
     :                  /'be applied to item ^ITEM', STATUS )

        END IF

      ELSE IF ( OP .EQ. 'Get' ) THEN

      ELSE IF ( OP .EQ. 'Put' ) THEN

      ELSE
        STATUS = SAI__ERROR
        CALL MSG_SETC( 'OP', OP )
        CALL ERR_REP( 'BDI0_CHKOP', 'Unknown operation name /^OP/',
     :                STATUS )

      END IF

      END
