      SUBROUTINE FSI1_PUTREF( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     FSI1_PUTREF

*  Purpose:
*     Write file reference to an HDS object attached to a FileSet

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL FSI1_PUTREF( NARG, ARGS, OARG, STATUS )

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
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     27 Jul 1995 (DJA):
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
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  No return value
      OARG = ADI__NULLID

*  Extract locator
      CALL ADI1_GETLOC( ARGS(1), TLOC, STATUS )

*  Create reference name
      CALL FRI1_NAME( ARGS(2), 'REF', CNAM, STATUS )

*  Create reference
      CALL FRI1_PUT1( TLOC, CNAM, ARGS(3), STATUS )

*  Report any errors
 99   IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'FSI1_PUTREF', STATUS )

      END
