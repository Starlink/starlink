      SUBROUTINE BDI1_WBWID( BDID, HFID, PSID, STATUS )
*+
*  Name:
*     BDI1_WBWID

*  Purpose:
*     Write back invented axis widths to the file axis width component

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI1_WBWID( BDID, PSID, STATUS )

*  Description:
*     The data are written to the axis width component, overwriting any
*     existing data.

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
      CHARACTER*(DAT__SZLOC)	WLOC			! Widths locator

      INTEGER			N			! # mapped elements
      INTEGER			NDIM, DIMS(DAT__MXDIM)	! Model object dims
      INTEGER			PTR			! Mapped data address
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get item name
      CALL ADI_NAME( PSID, ITEM, STATUS )

*  Extract the mapped type, data address and number of elements
      CALL ADI_CGET0C( PSID, 'Type', TYPE, STATUS )
      CALL ADI_CGET0I( PSID, 'Ptr', PTR, STATUS )
      CALL ADI_CGET0I( PSID, 'Nelm', N, STATUS )

*  Locate the width component
      CALL BDI1_CFIND( BDID, HFID, ITEM, .TRUE., WLOC, NDIM, DIMS,
     :                 STATUS )

*  Write array back to file
      CALL DAT_PUT( WLOC, '_'//TYPE, NDIM, DIMS, %VAL(PTR), STATUS )

*  Release widths array
      CALL DAT_ANNUL( WLOC, STATUS )

      END
