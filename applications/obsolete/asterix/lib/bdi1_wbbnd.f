      SUBROUTINE BDI1_WBBND( BDID, HFID, PSID, STATUS )
*+
*  Name:
*     BDI1_WBBND

*  Purpose:
*     Write back invented axis bounds to the file axis data & width components

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI1_WBBND( BDID, PSID, STATUS )

*  Description:
*     Axis bounds define both the bin centres and widths. This routine
*     writes back axis bounds to both the bin centre and width components
*     of an axis structure.

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
      CHARACTER*(DAT__SZLOC)	DLOC			! Widths locator
      CHARACTER*20		ITEM			! Item name
      CHARACTER*6		TYPE			! Mapping type
      CHARACTER*(DAT__SZLOC)	WLOC			! Widths locator

      INTEGER			DPTR,WPTR		! Data/width addresses
      INTEGER			N			! # mapped elements
      INTEGER			NDIM, DIMS(DAT__MXDIM)	! Model object dims
      INTEGER			PTR			! Mapped data address
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get item name and extract axis number
      CALL ADI_NAME( PSID, ITEM, STATUS )

*  Extract the mapped type, data address and number of elements
      CALL ADI_CGET0C( PSID, 'Type', TYPE, STATUS )
      CALL ADI_CGET0I( PSID, 'Ptr', PTR, STATUS )
      CALL ADI_CGET0I( PSID, 'Nelm', N, STATUS )

*  Locate the data and width components
      CALL BDI1_CFIND( BDID, HFID, ITEM(:7)//'Data', .TRUE., DLOC,
     :                 NDIM, DIMS, STATUS )
      CALL BDI1_CFIND( BDID, HFID, ITEM(:7)//'Width', .TRUE., WLOC,
     :                 NDIM, DIMS, STATUS )

*  Map the data and width for write access in the same type as the
*  invented data mapping type
      CALL DAT_MAP( DLOC, '_'//TYPE, NDIM, DIMS, DPTR, STATUS )
      CALL DAT_MAP( DLOC, '_'//TYPE, NDIM, DIMS, WPTR, STATUS )

*  Convert bounds to centres and widths
      IF ( TYPE .EQ. 'REAL' ) THEN
        CALL BDI1_WBBND_R( DIMS(1), %VAL(PTR), %VAL(DPTR), %VAL(WPTR),
     :                           STATUS )
      ELSE IF ( TYPE .EQ. 'DOUBLE' ) THEN
        CALL BDI1_WBBND_D( DIMS(1), %VAL(PTR), %VAL(DPTR), %VAL(WPTR),
     :                           STATUS )
      END IF

*  Release data and widths arrays
      CALL DAT_UNMAP( DLOC, STATUS )
      CALL DAT_ANNUL( DLOC, STATUS )
      CALL DAT_UNMAP( WLOC, STATUS )
      CALL DAT_ANNUL( WLOC, STATUS )

      END


      SUBROUTINE BDI1_WBBND_R( NBND, BNDS, CEN, WID, STATUS )
*+
*  Name:
*     BDI1_WBBND_R

*  Purpose:
*     Calculate REAL centres and widths from bounds

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI1_WBBND_R( NBND, BNDS, CEN, WID, STATUS )

*  Description:
*     The data are written to the axis width component, overwriting any
*     existing data.

*  Arguments:
*     NBND = INTEGER (given)
*        The number of axis bounds pairs
*     BNDS[2,N] = REAL (given)
*        The axis bounds
*     CEN[] = REAL (returned)
*        The axis centres
*     WID[] = REAL (returned)
*        The axis centres
*     STATUS = INTEGER (given and returned)
*        The global status.

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

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
      INTEGER                   NBND
      REAL			BNDS(2,*)

*  Arguments Given:
      REAL			CEN(*), WID(*)

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			I			! Loop over arrays
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  For each bound pair
      DO I = 1, NBND

*    Bin centre
        CEN(I) = (BNDS(1,I) + BNDS(2,I))/2.0

*    Bin width
        WID(I) = BNDS(2,I) - BNDS(1,I)

      END DO

      END


      SUBROUTINE BDI1_WBBND_D( NBND, BNDS, CEN, WID, STATUS )
*+
*  Name:
*     BDI1_WBBND_D

*  Purpose:
*     Calculate DOUBLE PRECISION centres and widths from bounds

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI1_WBBND_D( NBND, BNDS, CEN, WID, STATUS )

*  Description:
*     The data are written to the axis width component, overwriting any
*     existing data.

*  Arguments:
*     NBND = INTEGER (given)
*        The number of axis bounds
*     BNDS[2,NBND] = DOUBLE PRECISION (given)
*        The axis bounds
*     CEN[] = DOUBLE PRECISION (returned)
*        The axis centres
*     WID[] = DOUBLE PRECISION (returned)
*        The axis centres
*     STATUS = INTEGER (given and returned)
*        The global status.

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

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
      INTEGER                   NBND
      DOUBLE PRECISION		BNDS(2,*)

*  Arguments Given:
      DOUBLE PRECISION		CEN(*), WID(*)

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			I			! Loop over arrays
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  For each bound pair
      DO I = 1, NBND

*    Bin centre
        CEN(I) = (BNDS(1,I) + BNDS(2,I))/2.0

*    Bin width
        WID(I) = BNDS(2,I) - BNDS(1,I)

      END DO

      END
