      SUBROUTINE ADI1_LOCHEAD( ID, HLOC, STATUS )
*+
*  Name:
*     ADI1_LOCHEAD

*  Purpose:
*     Locate ASTERIX header structure given HDS object

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI1_LOCHEAD( ID, HLOC, STATUS )

*  Description:
*     Locate ASTERIX header structure given HDS object. The routine first
*     checks that the object has not already been found.

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier referencing HDS object
*     HLOC = CHARACTER*(DAT__SZLOC) (returned)
*        Locate to HEADER object
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
*     ADI Subroutine Guide : http://www.sr.bham.ac.uk:8080/asterix-docs/Programmer/Guides/adi.html

*  Keywords:
*     package:adi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     24 Feb 1995 (DJA):
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
      INTEGER			ID			! ADI identifier

*  Arguments Returned:
      CHARACTER*(DAT__SZLOC)	HLOC			! HEADER locator

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Constants:
      CHARACTER*10		HPROPN			! Name of property
        PARAMETER		( HPROPN = 'HEADER_LOC'

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	LOC			! Object locator

      LOGICAL			THERE			! Property exists?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Located already?
      CALL ADI_THERE( ID, HPROPN, THERE, STATUS )
      IF ( THERE ) THEN
        CALL ADI_CGET0C( ID, HPROPN, HLOC, STATUS )
      ELSE
        CALL ADI1_GETLOC( ID, LOC, STATUS )
        CALL ADI1_FIND( LOC, 'MORE.ASTERIX.HEADER', HLOC, STATUS )
        CALL ADI_CPUT0C( ID, HPROPN, HLOC, STATUS )
      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'ADI1_LOCHEAD', STATUS )
      END IF

      END
