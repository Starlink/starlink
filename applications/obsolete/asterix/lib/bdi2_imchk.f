      SUBROUTINE BDI2_IMCHK( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     BDI2_IMCHK

*  Purpose:
*     Service FileItemChk(XYimage,FITSfile,item) requests from the BDI system

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI2_IMCHK( NARG, ARGS, OARG, STATUS )

*  Description:
*     Service FileItemChk(XYimage,FITSfile,item) requests from the BDI system

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
*     7 Nov 96: argument missing from adi2_fndhdu (rjv)
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

*  External References:
      EXTERNAL			CHR_INSET
        LOGICAL			CHR_INSET

*  Local Variables:
      CHARACTER*20		ITEM

      INTEGER			IMHDU			! IMAGE hdu id

      LOGICAL			OK			! Object exists/ok?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Extract the arguments
      CALL ADI_GET0C( ARGS(3), ITEM, STATUS )


*  Locate the IMAGE hdu
      CALL ADI2_FNDHDU( ARGS(2), ' ', .FALSE.,IMHDU, STATUS )


*  Switch on the various items
*  Primary data
      IF ( CHR_INSET( 'Data,Axes,Axis_1_Data,Axis_1_Label,'/
     :                /'Axis_1_Units,Axis_2_Data,Axis_2_Label,'/
     :                /'Axis_2_Units', ITEM ) ) THEN
        OK = .TRUE.

*  Nothing else is supported for OGIP spectra
      ELSE
        OK = .FALSE.

      END IF

*  Release the IMAGE hdu
      CALL ADI_ERASE( IMHDU, STATUS )

*  Create return value
      CALL ADI_NEWV0L( OK, OARG, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'BDI2_IMCHK', STATUS )

      END
