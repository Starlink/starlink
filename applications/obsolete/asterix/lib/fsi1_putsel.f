      SUBROUTINE FSI1_PUTSEL( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     FSI1_PUTSEL

*  Purpose:
*     Write a selection array to an HDS file attached to a FileSet

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL FSI1_PUTSEL( NARG, ARGS, OARG, STATUS )

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
*     29 Nov 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'

*  Arguments Given:
      INTEGER                   NARG, ARGS(*)

*  Arguments Returned:
      INTEGER                   OARG

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	TLOC			! HDS file locator
      CHARACTER*6		CNAM			! Component  name
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get top-level locator
      CALL ADI1_GETLOC( ARGS(1), TLOC, STATUS )

*  Construct the component name
      CALL FSI1_NAME( ARGS(2), 'SEL', CNAM, STATUS )

*  Copy the data
      CALL ADI1_CCA2HI( ARGS(3), ' ', TLOC, CNAM, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'FSI1_PUTSEL', STATUS )

      END
