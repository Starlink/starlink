      SUBROUTINE BDI2_SPMAP( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     BDI2_SPMAP

*  Purpose:
*     Service FileItemMap(Spectrum,FITSfile,item,type,mode) requests

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI2_SPMAP( NARG, ARGS, OARG, STATUS )

*  Description:
*     Service FileItemGet(Spectrum,FITSfile,item) requests from the BDI system

*  Arguments:
*     NARG = INTEGER (given)
*        Number of method arguments
*     ARGS(*) = INTEGER (given)
*        ADI identifier of method arguments
*     OARG = INTEGER (returned)
*        Output data
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
      INCLUDE 'ADI_PAR'

*  Arguments Given:
      INTEGER                   NARG, ARGS(*)

*  Arguments Returned:
      INTEGER                   OARG

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*20		ITEM
      CHARACTER*6		MODE			! Mapping mode
      CHARACTER*8		TYPE			! Mapping type
      CHARACTER*72		CMNT			! (rb)

      INTEGER			BCOL			! Binary table column
      INTEGER			DIM			! Size of spectrum
      INTEGER			PTR			! Mapped data
      INTEGER			PSID			! Private storage
      INTEGER			SPHDU			! SPECTRUM hdu id
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Extract the arguments
      CALL ADI_GET0C( ARGS(3), ITEM, STATUS )
      CALL ADI_GET0C( ARGS(4), TYPE, STATUS )
      CALL ADI_GET0C( ARGS(5), MODE, STATUS )

*  Locate the SPECTRUM hdu
      CALL ADI2_FNDHDU( ARGS(2), 'SPECTRUM', .FALSE., SPHDU, STATUS )

*  Switch on the various items
      IF ( ITEM .EQ. 'Data' ) THEN

*    Locate column holding data
        CALL ADI2_FNDBTC( SPHDU, 'COUNTS', BCOL, STATUS )
        IF ( BCOL .EQ. 0 ) THEN
          CALL ADI2_FNDBTC( SPHDU, 'RATE', BCOL, STATUS )
        END IF

*  Axis data
      ELSE IF ( ITEM .EQ. 'Axis_1_Data' ) THEN

*    Map column data
        CALL ADI2_FNDBTC( SPHDU, 'CHANNEL', BCOL, STATUS )

*  Data error (this okay? - rb)
      ELSE IF ( ITEM .EQ. 'Error' .OR. ITEM .EQ. 'Variance' ) THEN

*    Map column data
        CALL ADI2_FNDBTC( SPHDU, 'STAT_ERR', BCOL, STATUS )

*  Data quality
      ELSE IF ( ITEM .EQ. 'SystematicError' ) THEN

*    Map column data
        CALL ADI2_FNDBTC( SPHDU, 'SYS_ERR', BCOL, STATUS )

*  Data quality
      ELSE IF ( ITEM .EQ. 'Quality' ) THEN

*    Map column data
        CALL ADI2_FNDBTC( SPHDU, 'QUALITY', BCOL, STATUS )

      END IF

*  Column defined?
      IF ( BCOL .GT. 0 ) THEN

*    Locate the BDI private storage for the item, creating if required
        CALL BDI0_LOCPST( ARGS(1), ITEM, .TRUE., PSID, STATUS )

*    Number of elements to map
        CALL ADI2_HGKYI( SPHDU, 'NAXIS2', DIM, CMNT, STATUS )

*    Map the column (sphdu -> args(1) - rb)
        CALL ADI2_MAPCOL( ARGS(1), SPHDU, BCOL, 1, DIM, TYPE, MODE,
     :                    PSID, PTR, STATUS )

*   Fudge! (rb)
        if (item.eq.'Variance') then
          call fudge_it(%val(ptr), dim)
        end if

*    Release storage
        CALL ADI_ERASE( PSID, STATUS )

      END IF

*  Release the SPECTRUM hdu
      CALL ADI_ERASE( SPHDU, STATUS )

*  Store the pointer
      CALL ADI_NEWV0I( PTR, OARG, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'BDI2_SPMAP', STATUS )

      END


	subroutine fudge_it(arr, dim)
	real arr(*)
	integer dim, i
	do i = 1, dim
		arr(i) = arr(i) * arr(i)
	end do
	end
