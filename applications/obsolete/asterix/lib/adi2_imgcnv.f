      SUBROUTINE ADI2_IMGCNV ( CACHEID, PTR, DTYPE, STATUS )
*+
*  Name:
*     ADI2_IMGCNV

*  Purpose:
*     Convert FITS file cache data to the correct type

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_IMGCNV ( CACHEID, PTR, DTYPE, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     CACHEID = INTEGER (given)
*        FITS data object to map
*     PTR = INTEGER (given and returned)
*        Mapped data pointer
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
*     Copyright (C) University of Birmingham, 1997

*  Authors:
*     RB: Richard Beard (ROSAT, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*      27 Jan 1997 (RB)
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

*  Arguments Given:
      INTEGER			CACHEID
      CHARACTER*(*)		DTYPE

*  Arguments Returned:
      INTEGER			PTR

*  Status:
      INTEGER			STATUS

*  Local Variables:
      INTEGER			NDIM, DIMS(ADI__MXDIM)	! Data dimensions
      CHARACTER*8		TYPE			! Initial data type
      INTEGER			NELM			! Number of elements
      INTEGER			NID, NPTR		! Holder for final data type
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Do we need to bother?
      CALL ADI_CGET0C( CACHEID, 'TYPE', TYPE, STATUS )
      IF ( TYPE .EQ. DTYPE ) GOTO 99

*  Create space for the final data type
      CALL ADI_CGET1I( CACHEID, 'SHAPE', ADI__MXDIM, DIMS, NDIM,
     :                 STATUS )
      CALL ADI_NEW( DTYPE, NDIM, DIMS, NID, STATUS )
      CALL ADI_MAP( NID, DTYPE, 'WRITE', NPTR, STATUS )

*  Number of elements mapped
      CALL ARR_SUMDIM( NDIM, DIMS, NELM )

*  Switch on the final data type
      IF ( DTYPE .EQ. 'REAL' ) THEN
        CALL ADI2_IC2R( PTR, TYPE, %VAL(NPTR), NELM, STATUS )
      ELSE IF ( DTYPE .EQ. 'DOUBLE' ) THEN
        CALL ADI2_IC2D( PTR, TYPE, %VAL(NPTR), NELM, STATUS )
      ELSE IF ( DTYPE .EQ. 'INTEGER' ) THEN
        CALL ADI2_IC2I( PTR, TYPE, %VAL(NPTR), NELM, STATUS )
      ELSE IF ( DTYPE .EQ. 'BYTE' .OR. DTYPE .EQ. 'UBYTE' ) THEN
        CALL ADI2_IC2B( PTR, TYPE, %VAL(NPTR), NELM, STATUS )
      ELSE IF ( DTYPE .EQ. 'LOGICAL' ) THEN
c       CALL ADI2_IC2L( PTR, TYPE, %VAL(NPTR), NELM, STATUS )
      ELSE
        STATUS = SAI__ERROR
        CALL MSG_SETC( 'T', DTYPE )
        CALL ERR_REP( 'ADI2_IMGCNV',
     :                'Can''t cope with final data type ^T', STATUS )
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


      SUBROUTINE ADI2_IMGCNV_NOTL( DATA, N, STATUS )
*+
*  Description:
*    Asterix Quality definition => 0 = GOOD and >0 = BAD
*    This is the reverse of most architecture bit logic
*    and thus if a LOGICAL dataset is required we have to
*    .NOT. all the converted values.

*  Authors:
*    RB: Richard Beard (ROSAT, University of Birmingham)

*  History:
*    17 May 1997: Original version (RB)
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Import/export:
      LOGICAL			DATA(*)
      INTEGER			N, STATUS

*  Local variables:
      INTEGER			I
*.

*  Check global status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Flip all the logical value
      DO I = 1, N
        DATA(I) = .NOT. DATA(I)
      END DO

      END
