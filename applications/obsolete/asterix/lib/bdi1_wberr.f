      SUBROUTINE BDI1_WBERR( BDID, HFID, PSID, STATUS )
*+
*  Name:
*     BDI1_WBERR

*  Purpose:
*     Write back invented errors to the file variance component

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI1_WBERR( BDID, PSID, STATUS )

*  Description:
*     The data are squared in situ and written directly to the VARIANCE
*     component of the dataset.

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
      CHARACTER*6		TYPE			! Mapping type
      CHARACTER*(DAT__SZLOC)	VLOC			! VARIANCE locator

      INTEGER			IERR, NERR		! VEC_ error info
      INTEGER			N			! # mapped elements
      INTEGER			NDIM, DIMS(DAT__MXDIM)	! VARIANCE shape
      INTEGER			PTR			! Mapped data address
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Extract the mapped type, data address and number of elements
      CALL ADI_CGET0C( PSID, 'Type', TYPE, STATUS )
      CALL ADI_CGET0I( PSID, 'Ptr', PTR, STATUS )
      CALL ADI_CGET0I( PSID, 'Nelm', N, STATUS )

*  Square the data
      IF ( TYPE .EQ. 'REAL' ) THEN
        CALL VEC_MULR( .FALSE., N, %VAL(PTR), %VAL(PTR), %VAL(PTR),
     :                 IERR, NERR, STATUS )
      ELSE IF ( TYPE .EQ. 'DOUBLE' ) THEN
        CALL VEC_MULD( .FALSE., N, %VAL(PTR), %VAL(PTR), %VAL(PTR),
     :                 IERR, NERR, STATUS )
      END IF

*  Locate the VARIANCE component
      CALL BDI1_CREAT( BDID, HFID, 'Variance', VLOC,
     :                 NDIM, DIMS, STATUS )

*  Write array back to VARIANCE
      CALL DAT_PUT( VLOC, '_'//TYPE, NDIM, DIMS, %VAL(PTR), STATUS )

*  Release VARIANCE array
      CALL DAT_ANNUL( VLOC, STATUS )

      END
