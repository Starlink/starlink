      SUBROUTINE EDI_LUPDT( ID, LID, ATTRS, STATUS )
*+
*  Name:
*     EDI_LUPDT

*  Purpose:
*     Update list description in file, replacing specified list attributes

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL EDI_LUPDT( ID, LID, ATTRS, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier of EventDS or derived object
*     LID = INTEGER (given)
*        ADI identifier of EventList object describing details of list
*     ATTRS = CHARACTER*(*) (given)
*        List attributes to modify
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
      CHARACTER*(*)		ATTRS

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			ARGS(4)			! Method arguments
      INTEGER			C1, C2			! Character pointers
      INTEGER			IATTR			! Iteration counter
      INTEGER			OARG			! Method return value
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check derivation of ID
      CALL EDI0_CHKDER( ID, STATUS )

*  Construct argument list
      ARGS(1) = ID
      CALL ADI_GETLINK( ID, ARGS(2), STATUS )
      ARGS(4) = LID

*  Loop over items to update
      CALL UDI0_CREITI( ATTRS, C1, C2, IATTR, STATUS )
      DO WHILE ( (C1.NE.0) .AND. (STATUS.EQ.SAI__OK) )

*    Construct string for this item
        CALL ADI_NEWV0C( ATTRS(C1:C2), ARGS(3), STATUS )

*    Invoke the method
        CALL ADI_FEXEC( 'ListModify', 4, ARGS, OARG, STATUS )

*    Release the list name string
        CALL ERR_BEGIN( STATUS )
        CALL ADI_ERASE( ARGS(3), STATUS )
        CALL ERR_END( STATUS )

*    Advance iterator to next attribute
        CALL UDI0_ADVITI( ATTRS, C1, C2, IATTR, STATUS )

      END DO

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'EDI_LUPDT', STATUS )

      END
