      SUBROUTINE EDI_ALTLEN( ID, NEVENT, STATUS )
*+
*  Name:
*     EDI_ALTLEN

*  Purpose:
*     Adjust length of the lists in an event dataset

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL EDI_ALTLEN( ID, NEVENT, STATUS )

*  Description:
*     Adjust length of the lists in an event dataset

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier of EventDS or derived object
*     NEVENT = INTEGER (given)
*        The new length of the lists
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

*  Arguments Given:
      INTEGER			ID, NEVENT

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			ARGS(3)			! Function args
      INTEGER			OARG			! Return value
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check correct type
      CALL EDI0_CHKDER( ID, STATUS )

*  First function argument is the identifier
      ARGS(1) = ID

*  Second is the linked file object
      CALL ADI_GETLINK( ID, ARGS(2), STATUS )

*  Third argument is the new list length
      CALL ADI_NEWV0I( NEVENT, ARGS(3), STATUS )

*  Invoke the list altering method
      CALL ADI_FEXEC( 'ListAlterLength', 3, ARGS, OARG, STATUS )

*  Release the list length
      CALL ADI_ERASE( ARGS(3), STATUS )

*  Store list length
      CALL ADI_CPUT0I( ID, 'NEVENT', NEVENT, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'EDI_ALTLEN', STATUS )

      END
