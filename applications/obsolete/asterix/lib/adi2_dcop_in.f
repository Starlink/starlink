      SUBROUTINE ADI2_DCOP_IN( CACHEID, PTR, NELM, DTYPE, STATUS )
*+
*  Name:
*     ADI2_DCOP_IN

*  Purpose:
*     Copy data from FITS file into cache

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_DCOP_IN( CACHEID, PTR, NELM, DTYPE, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     CACHEID = INTEGER (given)
*        FITS data object to map
*     PTR = INTEGER (returned)
*        Mapped data pointer
*     NELM = INTEGER (returned)
*        Number of mapped elements
*     DTYPE = CHARACTER*(*) (given)
*        Required data type for final mapped data
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
*     Copyright (C) University of Birmingham, 1996

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     RB: Richard Beard (ROSAT, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*      5 Jun 1996 (DJA):
*        Original version.
*      27 Jan 1997 (RB)
*        Add image type conversion
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ADI_PAR'
      INCLUDE 'PRM_PAR'

*  Arguments Given:
      INTEGER			CACHEID
      CHARACTER*(*)		DTYPE

*  Arguments Returned:
      INTEGER			PTR, NELM

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*20		CTYPE			! Cache object type
      CHARACTER*8		TYPE			! Actual data type

      INTEGER			FSTAT			! FITSIO status code
      INTEGER			HDUTYPE			! HDU type
      INTEGER			IHDU			! HDU number
      INTEGER			LUN			! Logical unit number
      INTEGER			NDIM, DIMS(ADI__MXDIM)	! Actual dimensions

      LOGICAL			ANYF			! Any duff values
      LOGICAL			THERE			! Object exists?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get type and shape information
      CALL ADI_CGET0C( CACHEID, 'TYPE', TYPE, STATUS )
      CALL ADI_CGET1I( CACHEID, 'SHAPE', ADI__MXDIM, DIMS, NDIM,
     :                 STATUS )

*  Value exists?
      CALL ADI_THERE( CACHEID, 'Value', THERE, STATUS )
      IF ( .NOT. THERE ) THEN
        CALL ADI_CNEW( CACHEID, 'Value', TYPE, NDIM, DIMS, STATUS )
      END IF

*  Map cache object
      CALL ADI_CMAP( CACHEID, 'Value', TYPE, 'WRITE', PTR, STATUS )

*  Number of elements mapped
      CALL ARR_SUMDIM( NDIM, DIMS, NELM )

*  IMAGE extension?
      CALL ADI_TYPE( CACHEID, CTYPE, STATUS )
      IF ( CTYPE .EQ. 'FITSimgCache' ) THEN

*    Move to HDU
        CALL ADI_CGET0I( CACHEID, 'Lun', LUN, STATUS )
        CALL ADI_CGET0I( CACHEID, 'Hdu', IHDU, STATUS )
        FSTAT = 0
        CALL FTMAHD( LUN, IHDU, HDUTYPE, FSTAT )

*    Read data
        IF ( TYPE .EQ. 'DOUBLE' ) THEN
          CALL FTGPVD( LUN, 1, 1, NELM, VAL__BADD, %VAL(PTR), ANYF,
     :                 FSTAT )
        ELSE IF ( TYPE .EQ. 'REAL' ) THEN
          CALL FTGPVE( LUN, 1, 1, NELM, VAL__BADR, %VAL(PTR), ANYF,
     :                 FSTAT )
        ELSE IF ( TYPE .EQ. 'INTEGER' ) THEN
          CALL FTGPVJ( LUN, 1, 1, NELM, 0, %VAL(PTR), ANYF, FSTAT )
        ELSE IF ( TYPE .EQ. 'WORD' ) THEN
          CALL FTGPVI( LUN, 1, 1, NELM, 0, %VAL(PTR), ANYF, FSTAT )
        ELSE IF ( TYPE .EQ. 'BYTE' ) THEN
          CALL FTGPVB( LUN, 1, 1, NELM, 0, %VAL(PTR), ANYF, FSTAT )
        END IF

*    Convert the data to required final type
        CALL ADI2_IMGCNV( CACHEID, PTR, DTYPE, STATUS )
        CALL ADI_CPUT0C( CACHEID, 'TYPE', DTYPE, STATUS )

      ELSE
        print*, 'ADI2_DCOP_IN: not image'
      END IF

*  Report any errors
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'ADI2_DCOP_IN', STATUS )
      END IF

      END

