      SUBROUTINE MASK( STATUS )
*+
*  Name:
*     MASK

*  Purpose:
*     Set the quality mask

*  Language:
*     Starlink Fortran

*  Type of Module:
*     ASTERIX task

*  Invocation:
*     CALL MASK( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     {routine_description}

*  Usage:
*     mask {parameter_usage}

*  Environment Parameters:
*     INP = CHAR (read)
*        {parameter_description}

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

*  Implementation Status:
*     {routine_implementation_status}

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     {task_references}...

*  Keywords:
*     mask, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     RJV: Bob Vallance (ROSAT, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     ?? ??? 199? V1.7-0 (RJV):
*        Original version.
*     24 Nov 1994 V1.8-0 (DJA):
*        Now use USI for user interface
*     12 Sep 1995 V2.0-0 (DJA):
*        Full ADI port
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Status:
      INTEGER			STATUS             	! Global status

*  Local Constants:
      CHARACTER*30		VERSION
        PARAMETER		( VERSION = 'MASK Version 2.1-0b' )

*  Local Variables:
      CHARACTER*8 		MSTR                    ! Char value of MASK

      INTEGER			OFID			! File identifier

      BYTE 			BMASK                   ! The mask value
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Version id
      CALL MSG_PRNT( VERSION )

*  Initialise ASTERIX
      CALL AST_INIT()

*  Associate input object
      CALL USI_ASSOC( 'INP', 'BinDS', 'UPDATE', OFID, STATUS )

*  Get current mask and use as default
      CALL BDI_GET0UB( OFID, 'QualityMask', BMASK, STATUS )
      CALL STR_BTOC( BMASK, MSTR, STATUS )
      CALL USI_DEF0C( 'MASK', MSTR, STATUS )

*  Get new mask
      CALL USI_GET0C( 'MASK', MSTR, STATUS )

*  Set the new mask
      CALL STR_CTOB( MSTR, BMASK, STATUS )
      CALL BDI_PUT0UB( OFID, 'QualityMask', BMASK, STATUS )

*  Tidy up
      CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END
