      SUBROUTINE BDI1_CHK( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     BDI1_CHK

*  Purpose:
*     Service FileItemChk requests from the BDI system for HDS files

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI1_CHK( NARG, ARGS, OARG, STATUS )

*  Description:
*     Services BDI chk requests for HDS files.

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
      INCLUDE 'DAT_PAR'

*  Arguments Given:
      INTEGER                   NARG                    ! # arguments
      INTEGER                   ARGS(*)                 ! Method arguments

*  Arguments Returned:
      INTEGER                   OARG                    ! Returned data

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	CLOC			! New component

      CHARACTER*20		ITEM

      LOGICAL			PRIM			! Object is primitive?
      LOGICAL			OK			! Object exists/ok?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Extract the arguments
      CALL ADI_GET0C( ARGS(3), ITEM, STATUS )

*  Ensure objects satisfy mapping requirement
      CALL BDI1_CFIND( ARGS(1), ARGS(2), ITEM, .FALSE., CLOC, STATUS )

*  Everything ok?
      IF ( (STATUS .EQ. SAI__OK) .AND. (CLOC.NE.DAT__NOLOC) ) THEN

*    Data is valid
        CALL DAT_PRIM( CLOC, PRIM, STATUS )
        IF ( PRIM ) THEN
          CALL DAT_STATE( CLOC, OK, STATUS )
        ELSE
          OK = .TRUE.
        END IF

*    Release object
        CALL DAT_ANNUL( CLOC, STATUS )

      ELSE
        OK = .FALSE.

      END IF

*  Create return valuie
      CALL ADI_NEWV0L( OK, OARG, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'BDI1_CHK', STATUS )

      END
