      SUBROUTINE EDI_CHK( ID, LIST, OK, STATUS )
*+
*  Name:
*     EDI_CHK

*  Purpose:
*     Check to see if a list is present

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL EDI_DISP( ID, LIST, OK, STATUS )

*  Description:
*     Test whether the named list exists in an event dataset

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier of EventDS or derived object
*     LIST = CHARACTER*(*) (given)
*        Name of list whose presence is to be checked
*     OK = LOGICAL (returned)
*        List is present?
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

*  Arguments Given:
      INTEGER			ID
      CHARACTER*(*)		LIST

*  Arguments Returned:
      LOGICAL			OK

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			LCID			! List container
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check correct object
      CALL EDI0_CHKDER( ID, STATUS )

*  Locate list container
      CALL ADI_FIND( ID, 'Lists', LCID, STATUS )

*  Does named component exist?
      CALL ADI_THERE( LCID, LIST, OK, STATUS )

*  Free list container
      CALL ADI_ERASE( LCID, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'EDI_CHK', STATUS )

      END
