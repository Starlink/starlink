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
*     {enter_new_authors_here}

*  History:
*      5 Jun 1996 (DJA):
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


      SUBROUTINE ADI2_IMGCNV ( CACHEID, PTR, DTYPE, STATUS )

      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ADI_PAR'

      INTEGER			CACHEID
      INTEGER			PTR
      CHARACTER*(*)		DTYPE
      INTEGER			STATUS

      INTEGER			NDIM, DIMS(ADI__MXDIM)
      CHARACTER*8		TYPE
      INTEGER			NELM
      INTEGER			NID, NPTR

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Do we need to bother?
      CALL ADI_CGET0C( CACHEID, 'TYPE', TYPE, STATUS )
      IF ( TYPE .EQ. DTYPE ) GOTO 99

*  Create space for the final data type
      CALL ADI_CGET1I( CACHEID, 'SHAPE', ADI__MXDIM, DIMS, NDIM, STATUS )
      CALL ADI_NEW( DTYPE, NDIM, DIMS, NID, STATUS )
      CALL ADI_MAP( NID, DTYPE, 'WRITE', NPTR, STATUS )

*  Number of elements mapped
      CALL ARR_SUMDIM( NDIM, DIMS, NELM )

*  Switch on the final data type
      IF ( DTYPE .EQ. 'REAL' ) THEN
        CALL ADI2_IC2R( PTR, TYPE, %VAL(NPTR), NELM, STATUS )
      ELSE
        STATUS = SAI__ERROR
        CALL MSG_SETC( 'T', DTYPE )
        CALL ERR_REP( 'ADI2_IMGCNV', 'Can''t cope with final data type ^T', STATUS )
      END IF

*  Swap over the pointers
      PTR = NPTR

*  Remove the copied data
c     CALL ADI_ERASE( NID, STATUS )

*  Report any errors
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'ADI2_IMGCNV', STATUS )
      END IF

      END


      SUBROUTINE ADI2_IC2R( PTR, TYPE, DATA, NELM, STATUS )

      INCLUDE 'SAE_PAR'          ! Standard SAE constants

      INTEGER			PTR
      CHARACTER*(*)		TYPE
      REAL			DATA(*)
      INTEGER			NELM
      INTEGER			STATUS

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Switch on initial data type
      IF ( TYPE .EQ. 'WORD' ) THEN
        CALL ADI2_ICW2R( %VAL(PTR), DATA, NELM, STATUS )
      ELSE
        STATUS = SAI__ERROR
        CALL MSG_SETC( 'T', TYPE )
        CALL ERR_REP( 'ADI2_IC2R', 'Can''t cope with initial data type ^T', STATUS )
      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'ADI2_IC2R', STATUS )
      END IF

      END


      SUBROUTINE ADI2_ICW2R( DIN, DOUT, NELM, STATUS )

      INCLUDE 'SAE_PAR'          ! Standard SAE constants

      INTEGER*2			DIN(*)
      REAL			DOUT(*)
      INTEGER			NELM
      INTEGER			STATUS
      INTEGER			I

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Convert all the data
      DO I = 1, NELM
        DOUT(I) = REAL(DIN(I))
      END DO

      END
