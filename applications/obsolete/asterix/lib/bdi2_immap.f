      SUBROUTINE BDI2_IMMAP( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     BDI2_IMMAP

*  Purpose:
*     Service FileItemMap(Spectrum,FITSfile,item,type,mode) requests

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI2_IMMAP( NARG, ARGS, OARG, STATUS )

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
      INTEGER                   NARG                    ! # arguments
      INTEGER                   ARGS(*)                 ! Method arguments

*  Arguments Returned:
      INTEGER                   OARG                    ! Returned data

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*20		ITEM
      CHARACTER*6		MODE			! Mapping mode
      CHARACTER*8		TYPE			! Mapping type

      DOUBLE PRECISION		BASE			! SGP/38 style base
      DOUBLE PRECISION 		CDELT, CRPIX		! Axis keywords

      INTEGER			DIM			! Size of spectrum
      INTEGER			IAX			! An axis number
      INTEGER			PTR			! Mapped data
      INTEGER			PSID			! Private storage
      INTEGER			IMHDU			! SPECTRUM hdu id
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Extract the arguments
      CALL ADI_GET0C( ARGS(3), ITEM, STATUS )
      CALL ADI_GET0C( ARGS(4), TYPE, STATUS )
      CALL ADI_GET0C( ARGS(5), MODE, STATUS )

*  Locate the IMAGE hdu
      CALL ADI2_FNDHDU( ARGS(2), ' ', IMHDU, STATUS )

*  Locate the BDI private storage for the item, creating if required
      CALL BDI0_LOCPST( ARGS(1), ITEM, .TRUE., PSID, STATUS )

*  Switch on the various items
      IF ( ITEM .EQ. 'Data' ) THEN

*    Simply map primary data
        CALL ADI2_MAPIMG( IMHDU, TYPE, MODE, PSID, PTR, STATUS )

*  Axis data
      ELSE IF ( (ITEM(:5) .EQ. 'Axis_') .AND.
     :          (ITEM(8:).EQ.'Data') ) THEN

*    Get axis number
        CALL CHR_CTOI( ITEM(6:6), IAX, STATUS )

*    Do standard keywords exist?
        CALL ADI2_HGKYID( IMHDU, 'CRPIX', IAX, CRPIX, STATUS )
        CALL ADI2_HGKYID( IMHDU, 'CDELT', IAX, CDELT, STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_ANNUL( STATUS )
          CRPIX = DBLE(DIM)/2.0D0 + 0.5D0
          CDELT = 1.0D0
        END IF

*    Convert CRPIX to SGP/38 style base
        BASE = (1.0D0-CRPIX) * CDELT

*    Get axis size
        CALL ADI2_HGKYII( IMHDU, 'NAXIS', IAX, DIM, STATUS )

*    Create dynamic array
        CALL DYN_MAPT( 1, DIM, TYPE, PTR, STATUS )
        IF ( TYPE .EQ. 'REAL' ) THEN
          CALL ARR_REG1R( REAL(BASE), REAL(CDELT), DIM,
     :                    %VAL(PTR), STATUS )
        ELSE IF ( TYPE .EQ. 'DOUBLE' ) THEN
          CALL ARR_REG1D( BASE, CDELT, DIM, %VAL(PTR), STATUS )
        END IF

*  Axis widths
      ELSE IF ( (ITEM(:5) .EQ. 'Axis_') .AND.
     :          (ITEM(8:).EQ.'Width') ) THEN

*    Get axis number
        CALL CHR_CTOI( ITEM(6:6), IAX, STATUS )

*    Does standard keywords exist?
        CALL ADI2_HGKYID( IMHDU, 'CDELT', IAX, CDELT, STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_ANNUL( STATUS )
          CDELT = 1.0D0
        END IF

*    Get axis size
        CALL ADI2_HGKYII( IMHDU, 'NAXIS', IAX, DIM, STATUS )

*    Create dynamic array
        CALL DYN_MAPT( 1, DIM, TYPE, PTR, STATUS )
        IF ( TYPE .EQ. 'REAL' ) THEN
          CALL ARR_INIT1R( REAL(CDELT), DIM, %VAL(PTR), STATUS )
        ELSE IF ( TYPE .EQ. 'DOUBLE' ) THEN
          CALL ARR_INIT1D( CDELT, DIM, %VAL(PTR), STATUS )
        END IF

      END IF

*  Store details of mapping
      CALL ADI2_STOMAP( PSID, IMHDU, 'I', PTR, TYPE, MODE, STATUS )

*  Release storage
      CALL ADI_ERASE( PSID, STATUS )

*  Release the IMAGE hdu
      CALL ADI_ERASE( IMHDU, STATUS )

*  Store the pointer
      CALL ADI_NEWV0I( PTR, OARG, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'BDI2_IMMAP', STATUS )

      END
