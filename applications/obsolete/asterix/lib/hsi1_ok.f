      SUBROUTINE HSI1_OK( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     HSI1_OK

*  Purpose:
*     Does file contain good history?

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL HSI1_OK( NARG, ARGS, OARG, STATUS )

*  Description:
*     Tests whether specified HDS file contains good history.

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

*  Algorithm:
*     {algorithm_description}...

*  Accuracy:
*     {routine_accuracy}

*  Timing:
*     {routine_timing}

*  External Routines Used:
*     DAT:
*        DAT_ANNUL	- Release an HDS locator
*        DAT_THERE	- Does an HDS component exist?

*  References:
*     HSI Subroutine Guide : http://www.sr.bham.ac.uk:8080/asterix-docs/Programmer/Guides/hsi.html

*  Keywords:
*     package:hsi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     14 Feb 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          			! SAE constants
      INCLUDE 'ADI_PAR'					! ADI constants
      INCLUDE 'DAT_PAR'					! HDS constants

*  Arguments Given:
      INTEGER			NARG			! # arguments
      INTEGER			ARGS(*)			! Method arguments

*  Arguments Returned:
      INTEGER			OARG			! Returned data

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	HLOC			! Input HISTORY object
      CHARACTER*(DAT__SZLOC)	LOC			! Output HDS object

      LOGICAL			OK, OK1, OK2, OK3	! Structures present
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Default output from this method
      OARG = ADI__NULLID

*  Extract locator
      CALL ADI1_GETLOC( ARGS(1), LOC, STATUS )

*  History exists?
      CALL DAT_THERE( LOC, 'HISTORY', OK, STATUS )
      IF ( OK ) THEN

*    Locate top-level structure
        CALL DAT_FIND( LOC, 'HISTORY', HLOC, STATUS )

*    Check top-level structures
        CALL DAT_THERE( HLOC, 'EXTEND_SIZE', OK1, STATUS )
        CALL DAT_THERE( HLOC, 'CURRENT_RECORD', OK2, STATUS )
        CALL DAT_THERE( HLOC, 'RECORDS', OK3, STATUS )

        OK = (OK1.AND.OK2.AND.OK3)

*    Free top-level structure
        CALL DAT_ANNUL( HLOC, STATUS )

      END IF

*  Set return value
      CALL ADI_NEWV0L( OK, OARG, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'HSI1_OK', STATUS )
      END IF

      END
B
