      SUBROUTINE EDI_SELCTN( ID, PAR, LIST, STATUS )
*+
*  Name:
*     EDI_SELCTN

*  Purpose:
*     Select a list by name

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL EDI_SELCTN( ID, PAR, LIST, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier of EventDS or derived object
*     PAR = CHARACTER*(*) (given)
*        Name of environment parameter to use to get name of list
*     LIST = CHARACTER*(*) (returned)
*        The name of the selected list
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
*     21 Aug 1995 (DJA):
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
      INTEGER			ID
      CHARACTER*(*)		PAR

*  Arguments Returned:
      CHARACTER*(*)		LIST

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      LOGICAL			OK			! List exists?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check object is ok
      CALL EDI0_CHKDER( ID, STATUS )

*  Get value of parameter
      CALL USI_GET0C( PAR, LIST, STATUS )

*  Check list is ok
      CALL EDI_CHK( ID, LIST, OK, STATUS )

*  If not present, report error
      IF ( .NOT. OK ) THEN
        STATUS = SAI__ERROR
        CALL MSG_SETC( 'L', LIST )
        CALL ERR_REP( 'EDI_SELCTN_1', 'List ^L does not exist in file',
     :                STATUS )
      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'EDI_SELCTN', STATUS )

      END
