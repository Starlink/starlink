      SUBROUTINE BDI1_WBLQ( BDID, HFID, PSID, STATUS )
*+
*  Name:
*     BDI1_WBLQ

*  Purpose:
*     Write back invented logical quality to the file quality component

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI1_WBLQ( BDID, HFID, PSID, STATUS )

*  Description:
*     Writes logical quality to a dataset. If no quality already exists
*     then QUAL__BAD is written to each pixel with bad (= false) logical
*     quality. If quality does already exist then only those pixels
*     which have become bad have their quality values changed.

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
      INCLUDE 'QUAL_PAR'

*  Arguments Given:
      INTEGER                   BDID, HFID, PSID

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	MLOC			! QualityMask locator
      CHARACTER*(DAT__SZLOC)	QLOC			! Quality locator

      INTEGER			N			! # mapped elements
      INTEGER			NDIM, DIMS(DAT__MXDIM)	! QUALITY shape
      INTEGER			PTR			! Mapped data address
      INTEGER			QPTR			! Mapped quality

      LOGICAL			EXISTS			! Quality exists?
      LOGICAL			OK			! Object is ok?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Extract the mapped type, data address and number of elements
      CALL ADI_CGET0I( PSID, 'Ptr', PTR, STATUS )
      CALL ADI_CGET0I( PSID, 'Nelm', N, STATUS )

*  Locate the Quality item. If it doesn't already exist create and
*  initialise to good quality.
      CALL BDI1_CFIND( BDID, HFID, 'Quality', .FALSE., QLOC,
     :                 NDIM, DIMS, STATUS )
      IF ( QLOC .EQ. DAT__NOLOC ) THEN

*    Create new quality array
        CALL BDI1_CFIND( BDID, HFID, 'Quality', .TRUE., QLOC,
     :                   NDIM, DIMS, STATUS )
        CALL DAT_MAP( QLOC, '_UBYTE', 'WRITE', NDIM, DIMS, QPTR,
     :                STATUS )
        EXISTS = .FALSE.

*  Already exists
      ELSE
        CALL DAT_MAP( QLOC, '_UBYTE', 'UPDATE', NDIM, DIMS, QPTR,
     :                STATUS )
        EXISTS = .TRUE.

      END IF

*  Get the quality mask, creating if not already there
      CALL BDI1_CFIND( BDID, HFID, 'QualityMask', .TRUE., MLOC,
     :                 NDIM, DIMS, STATUS )
      CALL DAT_STATE( MLOC, OK, STATUS )
      IF ( .NOT. OK ) THEN
        CALL DAT_PUT( MLOC, '_UBYTE', 0, 0, QUAL__MASK, STATUS )
      END IF

*  Convert logical quality to byte quality
      CALL BDI1_WBLQ_INT( N, %VAL(PTR), (.NOT.EXISTS), %VAL(QPTR),
     :                          STATUS )

*  Release QUALITY array
      CALL DAT_UNMAP( QLOC, STATUS )
      CALL DAT_ANNUL( QLOC, STATUS )
      CALL DAT_ANNUL( MLOC, STATUS )

      END



      SUBROUTINE BDI1_WBLQ_INT( N, LVAL, NEW, BVAL, STATUS )
*+
*  Name:
*     BDI1_WBLQ_INT

*  Purpose:
*     Convert logical quality to byte values

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI1_WBLQ_INT( N, LVAL, NEW, BVAL, STATUS )

*  Description:
*     Creates byte quality from logical quality. If the quality is new we
*     write directly, otherwise we bit-wise OR the existing quality with
*     QUAL__BAD.

*  Arguments:
*     N = INTEGER (given)
*        Number of values to copy
*     LVAL[] = LOGICAL (given)
*        Logical values, true if pixel is good
*     NEW = LOGICAL (given)
*        The byte values are new
*     BVAL[] = BYTE (given and returned)
*        Byte values
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
      INCLUDE 'QUAL_PAR'

*  Arguments Given:
      INTEGER                   N
      LOGICAL			NEW, LVAL(*)

*  Arguments Returned:
      BYTE			BVAL(*)

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			BIT_ORUB
        BYTE			BIT_ORUB

*  Local Variables:
      INTEGER			I			! Loop over values
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  New quality array?
      IF ( NEW ) THEN
        DO I = 1, N
          IF ( LVAL(I) ) THEN
            BVAL(I) = QUAL__GOOD
          ELSE
            BVAL(I) = QUAL__BAD
          END IF
        END DO
      ELSE
        DO I = 1, N
          IF ( LVAL(I) ) THEN
            BVAL(I) = QUAL__GOOD
          ELSE
            BVAL(I) = BIT_ORUB(BVAL(I),QUAL__BAD)
          END IF
        END DO
      END IF

      END
