      SUBROUTINE ADI1_MKFILE( LOC, ACCESS, ID, STATUS )
*+
*  Name:
*     ADI1_MKFILE

*  Purpose:
*     Make a bona fide HDSfile object given a locator

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI1_MKFILE( LOC, ACCESS, ID, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     LOC = CHARACTER*(DAT__SZLOC) (given)
*        Locator to HDS file object
*     ACCESS = CHARACTER*(*) (given)
*        The access required with the locator
*     ID = INTEGER (returned)
*        The HDSfile object
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
*     ADI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/adi.html

*  Keywords:
*     package:adi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     9 Oct 1995 (DJA):
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
      CHARACTER*(DAT__SZLOC)	LOC
      CHARACTER*(*)		ACCESS

*  Arguments Returned:
      INTEGER			ID

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			HREPID			! HDS file rep'n id
        SAVE			HREPID

      LOGICAL			FIRST			! First time through?
        SAVE			FIRST

*  Local Data:
      DATA 			FIRST/.TRUE./
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If first time through, locate the HDS representation
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL ADI_LOCREP( 'HDS', HREPID, STATUS )
      END IF

*  Create HDSfile object
      CALL ADI_NEW0( 'HDSfile', ID, STATUS )
      CALL ADI_CPUT0C( ID, 'MODE', ACCESS, STATUS )
      CALL ADI_CPUT0I( ID, 'REP', HREPID, STATUS )
      CALL ADI_CPUT0C( ID, 'Locator', LOC, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI1_MKFILE', STATUS )

      END
