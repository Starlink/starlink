      SUBROUTINE ADI2_STOMAP( PSID, CACHEID, MSYS, FPTR, PTR,
     :                        NDIM, DIMS,
     :                        FROW, NROW, WBPTR, TYPE, MODE, STATUS )
*+
*  Name:
*     ADI2_STOMAP

*  Purpose:
*     Store various things to do with a mapped FITS item

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_STOMAP( PSID, CACHEID, MSYS, FPTR, PTR, NDIM, DIMS, FROW,
*                       NROW, WBPTR, TYPE, MODE, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     PSID = INTEGER (given)
*        Item's private storage
*     CACHEID = INTEGER (given)
*        The cache object holding the data
*     MSYS = CHARACTER*(*) (given)
*        Mapping system, 'dyn' or 'inv'
*     FPTR = INTEGER (given)
*        Address of mapped file object, or identifier of invented object
*     PTR = INTEGER (given)
*        Address of item mapped memory
*     NDIM = INTEGER (given)
*        Dimensionality of mapped data
*     DIMS[] = INTEGER (given)
*        Dimensions of mapped data
*     FROW = INTEGER (given)
*        First row in a mapped column
*     NROW = INTEGER (given)
*        Number of rows mapped in a column
*     WBPTR = INTEGER (given)
*        WriteBack procedure address
*     TYPE = CHARACTER*(*) (given)
*        Data access type
*     MODE = CHARACTER*(*) (given)
*        Memory access mode
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
*     ADI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/adi.html

*  Keywords:
*     package:adi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     30 Aug 1995 (DJA):
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
      INTEGER			PSID, CACHEID, FPTR, PTR, NDIM, DIMS(*)
      INTEGER			WBPTR, FROW, NROW
      CHARACTER*(*)		TYPE, MODE, MSYS

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			CHR_LEN
        INTEGER			CHR_LEN

*  Local Variables:
      INTEGER			NELM			! # mapped elements
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Store everything
      CALL ADI_CNEWV0I( PSID, 'CacheID', CACHEID, STATUS )
      CALL ADI_CNEWV0I( PSID, 'Ptr', PTR, STATUS )

*  Store invented object
      IF ( MSYS .EQ. 'inv' ) THEN
        CALL ADI_CPUTREF( PSID, 'InvObj', FPTR, STATUS )
      END IF

*  Store the various items in the storage block
      IF ( WBPTR .NE. 0 ) THEN
        CALL ADI_CNEWV0I( PSID, 'WriteBack', WBPTR, STATUS )
      END IF
      CALL ADI_CNEWV0C( PSID, 'MapSystem', MSYS, STATUS )
      CALL ADI_CPUT0C( PSID, 'Type', TYPE(:CHR_LEN(TYPE)), STATUS )
      CALL ADI_CPUT0C( PSID, 'Mode', MODE(:CHR_LEN(MODE)), STATUS )

*  Write shape
      IF ( NDIM .GT. 0 ) THEN
        CALL ADI_CPUT1I( PSID, 'SHAPE', NDIM, DIMS, STATUS )
      END IF
      CALL ARR_SUMDIM( NDIM, DIMS, NELM )
      CALL ADI_CNEWV0I( PSID, 'Nelm', NELM, STATUS )

*  Store first row and number of rows
      CALL ADI_CNEWV0I( PSID, 'Frow', FROW, STATUS )
      CALL ADI_CNEWV0I( PSID, 'Nrow', NROW, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI2_STOMAP', STATUS )

      END
