      SUBROUTINE EDI_IDX( ID, NUMBER, LID, STATUS )
*+
*  Name:
*     EDI_IDX

*  Purpose:
*     Index a list by its number within a datset

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL EDI_IDX( ID, NUMBER, LID, STATUS )

*  Description:
*     Display formatted list of lists available in an event dataset

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier of EventDS or derived object
*     NUMBER = INTEGER (given)
*        The consecutive list number
*     LID = INTEGER (returned)
*        The ADI identifier of the EventList object
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
*     15 Aug 1995 (DJA):
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
      INTEGER			ID, NUMBER

*  Arguments Returned:
      INTEGER			LID

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			EDI0_PRPIL
        CHARACTER*8		EDI0_PRPIL

*  Local Variables:
      CHARACTER*20		LNAME			! List name
      CHARACTER*8		PNAME			! Property name

      INTEGER			LCID			! List container
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check correct object
      CALL EDI0_CHKDER( ID, STATUS )

*  Locate list container
      CALL ADI_FIND( ID, 'Lists', LCID, STATUS )

*  Construct property name
      PNAME = EDI0_PRPIL( NUMBER )

*  Get name of this list
      CALL ADI_CGET0C( LCID, PNAME, LNAME, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*    Locate list
        CALL ADI_FIND( LCID, LNAME, LID, STATUS )

      ELSE
        CALL ERR_ANNUL( STATUS )
        LID = ADI__NULLID

      END IF

*  Free list container
      CALL ADI_ERASE( LCID, STATUS )

*  Null identifier means invalid list number
      IF ( LID .EQ. ADI__NULLID ) THEN
        CALL MSG_SETI( 'N', NUMBER )
        STATUS = SAI__ERROR
        CALL ERR_REP( 'EDI_IDX', 'List number ^N is invalid', STATUS )
      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'EDI_IDX', STATUS )

      END
