      SUBROUTINE BDI1_WBGEN( BDID, HFID, PSID, STATUS )
*+
*  Name:
*     BDI1_WBGEN

*  Purpose:
*     Write back data which has a direct counterpart in the HDS file

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI1_WBGEN( BDID, PSID, STATUS )

*  Description:
*     The data are written to the component named by the private storage
*     area, overwriting any existing data.

*  Arguments:
*     BDID = INTEGER (given)
*        The ADI identifier of the BinDS (or BinDS derived) object
*     HFID = INTEGER (given)
*        The ADI identifier of the HDS file object
*     PSID = INTEGER (given)
*        The ADI identifier of the item private storage
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
      INCLUDE 'DAT_PAR'

*  Arguments Given:
      INTEGER                   BDID, HFID, PSID

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*20		ITEM			! Item name
      CHARACTER*6		TYPE			! Mapping type
      CHARACTER*(DAT__SZLOC)	LOC			! Item locator

      INTEGER			NDIM, DIMS(DAT__MXDIM)	! Model object dims
      INTEGER			PTR			! Mapped data address
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get item name
      CALL ADI_NAME( PSID, ITEM, STATUS )

*  Extract the mapped type and data address
      CALL ADI_CGET0C( PSID, 'Type', TYPE, STATUS )
      CALL ADI_CGET0I( PSID, 'Ptr', PTR, STATUS )

*  Locate the file component
      CALL BDI1_CREAT( BDID, HFID, ITEM, LOC, NDIM, DIMS, STATUS )

*  Write array back to file
      CALL DAT_PUT( LOC, '_'//TYPE, NDIM, DIMS, %VAL(PTR), STATUS )

*  Release array
      CALL DAT_ANNUL( LOC, STATUS )

      END
