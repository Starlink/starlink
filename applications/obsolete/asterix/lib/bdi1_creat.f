      SUBROUTINE BDI1_CREAT( MID, HID, ITEM, CLOC,
     :                       CNDIM, CDIMS, STATUS )
*+
*  Name:
*     BDI1_CREAT

*  Purpose:
*     Simply invokes the HDS object finder in create mode

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI1_CREAT( MID, HID, ITEM, CLOC, CNDIM, CDIMS, STATUS )

*  Description:
*     Locate HDS component for a given item, creating if required. The routine
*     returns the shape of the object, whether or not it is created, which is
*     defined by the NDF data model.

*  Arguments:
*     MID = INTEGER (given)
*        Model data object
*     HID = INTEGER (given)
*        HDSfile data object
*     ITEM = CHARACTER*(*) (given)
*        BDI data item
*     CLOC = CHARACTER*(DAT__SZLOC) (returned)
*        Locator to object matching item. If the item does not exist
*        the CLOC is set to the symbolic value DAT__NOLOC
*     CNDIM = INTEGER (returned)
*        The dimensionality of the object according to the data model. Note
*        that this not necessarily the dimensionality of the actual HDS
*        component
*     CDIMS[] = INTEGER (returned)
*        The dimensions of the object according to the data model. Note
*        that these are not necessarily the dimensions of the actual HDS
*        component
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

*  Arguments Returned:
      CHARACTER*(DAT__SZLOC)	CLOC
      INTEGER			CNDIM, CDIMS(*)

*  Status:
      INTEGER 			STATUS             	! Global status
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Invoke object finder
      CALL BDI1_CFIND( MID, HID, ITEM, .TRUE., .FALSE., CLOC,
     :                 CNDIM, CDIMS, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'BDI1_CREAT', STATUS )

      END
