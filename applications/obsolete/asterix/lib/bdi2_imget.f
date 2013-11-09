      SUBROUTINE BDI2_IMGET( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     BDI2_IMGET

*  Purpose:
*     Service FileItemGet(XYimage,FITSfile,item) requests from the BDI system

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI2_IMGET( NARG, ARGS, OARG, STATUS )

*  Description:
*     Service FileItemGet(XYimage,FITSfile,item) requests from the BDI system

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
      INTEGER                   NARG, ARGS(*)

*  Arguments Returned:
      INTEGER                   OARG

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*20		ITEM
      CHARACTER*40		UNIT			! Unit string
      CHARACTER*72		CMNT			! (rb)

      INTEGER			IMHDU			! IMAGE hdu id
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise
      OARG = ADI__NULLID

*  Extract the arguments
      CALL ADI_GET0C( ARGS(3), ITEM, STATUS )

*  Locate the IMAGE hdu
      CALL ADI2_FNDHDU( ARGS(2), ' ', .FALSE., IMHDU, STATUS )

*  Switch on the various items
*  X axis label
      IF ( ITEM .EQ. 'Axis_1_Label' ) THEN

        CALL ADI_NEWV0C( 'X axis offset', OARG, STATUS )

*  Y axis label
      ELSE IF ( ITEM .EQ. 'Axis_2_Label' ) THEN

        CALL ADI_NEWV0C( 'Y axis offset', OARG, STATUS )

*  X axis units
      ELSE IF ( ITEM .EQ. 'Axis_1_Units' ) THEN
        CALL ADI2_HGKYC( IMHDU, 'CUNIT1', UNIT, CMNT, STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_ANNUL( STATUS )
          UNIT = 'degrees'
        END IF
        CALL ADI_NEWV0C( UNIT, OARG, STATUS )

*  Y axis units
      ELSE IF ( ITEM .EQ. 'Axis_2_Units' ) THEN
        CALL ADI2_HGKYC( IMHDU, 'CUNIT2', UNIT, CMNT, STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_ANNUL( STATUS )
          UNIT = 'degrees'
        END IF
        CALL ADI_NEWV0C( UNIT, OARG, STATUS )

      END IF

*  Release the IMAGE hdu
      CALL ADI_ERASE( IMHDU, STATUS )

*  Report if unable to get data
      IF ( OARG .EQ. ADI__NULLID ) STATUS = SAI__ERROR

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'BDI2_IMGET', STATUS )

      END
