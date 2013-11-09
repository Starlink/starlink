      SUBROUTINE BDI1_DELET( MID, HID, ITEM, STATUS )
*+
*  Name:
*     BDI1_DELET

*  Purpose:
*     Simply invokes the HDS object finder in delete mode

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI1_DELET( MID, HID, ITEM, STATUS )

*  Description:
*     Locate and destroy the HDS component for a given item. It is not an
*     error for the object not to exist.

*  Arguments:
*     MID = INTEGER (given)
*        Model data object
*     HID = INTEGER (given)
*        HDSfile data object
*     ITEM = CHARACTER*(*) (given)
*        BDI data item
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
*      4 Mar 1995 (DJA):
*        Original version, adapted from BDI1_CFIND
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
      INTEGER			MID, HID
      CHARACTER*(*)		ITEM

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	CLOC

      INTEGER			CNDIM, CDIMS(DAT__MXDIM)
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Invoke object finder
      CALL BDI1_CFIND( MID, HID, ITEM, .FALSE., .TRUE., CLOC,
     :                 CNDIM, CDIMS, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_ANNUL( STATUS )
      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'BDI1_DELET', STATUS )

      END
