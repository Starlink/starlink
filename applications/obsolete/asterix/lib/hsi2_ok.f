      SUBROUTINE HSI2_OK( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     HSI2_OK

*  Purpose:
*     Does FITS file contain good history?

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL HSI2_OK( NARG, ARGS, OARG, STATUS )

*  Description:
*     Tests whether specified FITS file contains good history.

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
*     HSI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/hsi.html

*  Keywords:
*     package:hsi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     Richard Beard (ROSAT, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     4 Feb 1997 (RB):
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
      INTEGER			HDUID			! HDU identifier
      INTEGER			COUNT			! Number of history cards

      LOGICAL			OK, THERE		! Structures present
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Default output from this method
      OARG = ADI__NULLID

*  Locate primary HDU
      CALL ADI2_FNDHDU( ARGS(1), ' ', .FALSE., HDUID, STATUS )

*  History exists?
      CALL ADI_THERE( HDUID, 'HistoryCount', THERE, STATUS )
      IF ( THERE ) THEN
        CALL ADI_CGET0I( HDUID, 'HistoryCount', COUNT, STATUS )
        IF ( COUNT .GT. 0 ) THEN
          OK = .TRUE.
        ELSE
          OK = .FALSE.
        END IF
      ELSE
        OK = .FALSE.
      END IF

*  Go for it!?!
      OK = .TRUE.

*  Set return value
      CALL ADI_NEWV0L( OK, OARG, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'HSI2_OK', STATUS )
      END IF

      END
