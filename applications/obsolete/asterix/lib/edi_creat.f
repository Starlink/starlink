      SUBROUTINE EDI_CREAT( ID, LID, STATUS )
*+
*  Name:
*     EDI_CREAT

*  Purpose:
*     Create list in file, replacing any existing list of the same name

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL EDI_CREAT( ID, LID, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier of EventDS or derived object
*     LID = INTEGER (given)
*        ADI identifier of EventList object describing details of new list
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
*     EDI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/edi.html

*  Keywords:
*     package:edi, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     18 Aug 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER			ID, LID

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			ARGS(3)			! Method arguments
      INTEGER			OARG			! Method return value
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check derivation of ID
      CALL EDI0_CHKDER( ID, STATUS )

*  Construct argument list
      ARGS(1) = ID
      CALL ADI_GETLINK( ID, ARGS(2), STATUS )
      ARGS(3) = LID

*  Invoke the method
      CALL ADI_FEXEC( 'ListCreate', 3, ARGS, OARG, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'EDI_CREAT', STATUS )

      END
