      SUBROUTINE FSI1_PUTSCL( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     FSI1_PUTSCL

*  Purpose:
*     Write scaling to an HDS object attached to a FileSet

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL FSI1_PUTSCL( NARG, ARGS, OARG, STATUS )

*  Description:
*     {routine_description}

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
*     FSI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/fsi.html

*  Keywords:
*     package:fsi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     RB: Richard Beard (University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     22 Jun 1998 (RB):
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
      INTEGER                   NARG, ARGS(*)

*  Arguments Returned:
      INTEGER                   OARG                    ! Returned data

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*6		CNAM			! Component name
      CHARACTER*(DAT__SZLOC)	TLOC			! HDS top level
      CHARACTER*(DAT__SZLOC)	RLOC			! Reference locator

      REAL			SCALE			! Scaling factor
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  No return value
      OARG = ADI__NULLID

*  Extract locator
      CALL ADI1_GETLOC( ARGS(2), TLOC, STATUS )

*  Create reference name
      CALL FSI1_NAME( ARGS(3), 'REF', CNAM, STATUS )

*  Extract scaling value
      CALL ADI_GET0R( ARGS(4), SCALE, STATUS )

*  Goto reference and add value
      CALL DAT_FIND( TLOC, CNAM, RLOC, STATUS )
      CALL DAT_NEW0R( RLOC, 'SCALING', STATUS )
      CALL CMP_PUT0R( RLOC, 'SCALING', SCALE, STATUS )

*  Report any errors
 99   IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'FSI1_PUTSCL', STATUS )

      END
