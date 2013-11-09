      SUBROUTINE BDI2_SPGET( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     BDI2_SPGET

*  Purpose:
*     Service FileItemGet(Spectrum,FITSfile,item) requests from the BDI system

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI2_SPGET( NARG, ARGS, OARG, STATUS )

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
      INCLUDE 'QUAL_PAR'

*  Arguments Given:
      INTEGER                   NARG                    ! # arguments
      INTEGER                   ARGS(*)                 ! Method arguments

*  Arguments Returned:
      INTEGER                   OARG                    ! Returned data

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			CHR_LEN
        INTEGER			CHR_LEN

*  Local Variables:
      CHARACTER*20		ITEM
      CHARACTER*40		UNIT			! Unit string
      CHARACTER*72		CMNT

      INTEGER			BCOL			! Binary table column
      INTEGER			SPHDU			! SPECTRUM hdu id

      LOGICAL			ISRATE			! Corrected spectrum?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise
      OARG = ADI__NULLID

*  Extract the arguments
      CALL ADI_GET0C( ARGS(3), ITEM, STATUS )

*  Locate the SPECTRUM hdu
      CALL ADI2_FNDHDU( ARGS(2), 'SPECTRUM', .FALSE., SPHDU, STATUS )

*  Switch on the various items
*  Channel axis label
      IF ( ITEM .EQ. 'Axis_1_Label' ) THEN
        CALL ADI_NEWV0C( 'Energy', OARG, STATUS )

*  Channel axis units
      ELSE IF ( ITEM .EQ. 'Axis_1_Units' ) THEN
        CALL ADI_NEWV0C( 'channels', OARG, STATUS )

*  Whole axis
      ELSE IF ( ITEM .EQ. 'Axis_1' ) THEN

*    Create ADI axis description object
        CALL ADI_NEW0( 'BinDSAxis', OARG, STATUS )

*    Copy each component of the axis structure
        CALL ADI_CPUT0C( OARG, 'Label', 'Energy', STATUS )
        CALL ADI_CPUT0C( OARG, 'Units', 'channels', STATUS )

*  Intensity label
      ELSE IF ( ITEM .EQ. 'Label' ) THEN
        CALL ADI_NEWV0C( 'Intensity', OARG, STATUS )

*  Intensity units
      ELSE IF ( ITEM .EQ. 'Units' ) THEN

*    Identify the intensity column
        CALL ADI2_FNDBTC( SPHDU, 'COUNTS', BCOL, STATUS )
        IF ( STATUS .EQ. SAI__OK ) THEN
          CALL ADI2_FNDBTC( SPHDU, 'RATE', BCOL, STATUS )
          ISRATE = .TRUE.
        ELSE
          ISRATE = .FALSE.
        END IF

*    Generate intensity unit keyword name
        CALL ADI2_HGKYIC( SPHDU, 'TUNIT', BCOL, UNIT, CMNT, STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_ANNUL( STATUS )
          IF ( ISRATE ) THEN
            UNIT = 'count/s'
          ELSE
            UNIT = 'count'
          END IF
        END IF

        CALL ADI_NEWV0C( UNIT(:CHR_LEN(UNIT)), OARG, STATUS )

      END IF

*  Release the SPECTRUM hdu
      CALL ADI_ERASE( SPHDU, STATUS )

*  Report if unable to get data
      IF ( OARG .EQ. ADI__NULLID ) STATUS = SAI__ERROR

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'BDI2_SPGET', STATUS )

      END
