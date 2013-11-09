      SUBROUTINE BDI0_MKISTR( ID, ITEM, ISTR, STATUS )
*+
*  Name:
*     BDI0_MKISTR

*  Purpose:
*     Make an ADI string given an item name

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI0_MKISTR( ID, ITEM, ISTR, STATUS )

*  Description:
*     Makes an ADI string from a BDI item name. If the name is invalid
*     then bad status is returned and ISTR is set to the null ADI object.

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier of object passed to BDI
*     ITEM = CHARACTER*(*) (given)
*        The type name to validate
*     ISTR = INTEGER (returned)
*        ADI string containing validated item name
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
*      4 Mar 1996 (DJA):
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

*  Arguments Given:
      INTEGER			ID
      CHARACTER*(*)		ITEM

*  Arguments Returned:
      INTEGER			ISTR

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*40		LITEM			! Local item name copy

      INTEGER			LITL			! Length of LITEM
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check item name is valid, and make a local copy. Removes any
*  special item names such as E_Axis_Label. If the item name is
*  not valid we don't have to execute a method
      CALL BDI0_CHKITM( ID, ITEM, LITEM, LITL, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_ANNUL( STATUS )
        ISTR = ADI__NULLID
      ELSE

*    Construct string for this item
        CALL ADI_NEWV0C( LITEM(:LITL), ISTR, STATUS )

      END IF

      END
