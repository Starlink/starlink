      SUBROUTINE ADI1_LOCSORT( ID, CREATE, SLOC, STATUS )
*+
*  Name:
*     ADI1_LOCSORT

*  Purpose:
*     Locate SORT structure given HDS object

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI1_LOCSORT( ID, CREATE, SLOC, STATUS )

*  Description:
*     Locate ASTERIX SORT structure given HDS object. The routine first
*     checks that the object has not already been found, but only creates
*     if CREATE is specified true.

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier referencing HDS object
*     CREATE = LOGICAL (given)
*        Create component if it doesn't exist?
*     SLOC = CHARACTER*(DAT__SZLOC) (returned)
*        Locator to SORT object
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
      LOGICAL 			CREATE			! Create if not there?

*  Arguments Returned:
      CHARACTER*(DAT__SZLOC)	SLOC			! INSTRUMENT locator

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Constants:
      CHARACTER*13		SPROPN			! Name of property
        PARAMETER		( SPROPN = '.SORTlocator' )

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	ILOC			! Object locator

      LOGICAL			THERE			! Property exists?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Located already?
      CALL ADI_THERE( ID, SPROPN, THERE, STATUS )
      IF ( THERE ) THEN
        CALL ADI_CGET0C( ID, SPROPN, SLOC, STATUS )
      ELSE
        CALL ADI1_LOCINSTR( ID, CREATE, ILOC, STATUS )
        CALL DAT_THERE( ILOC, 'SORT', THERE, STATUS )
        IF ( CREATE .AND. .NOT. THERE ) THEN
          CALL DAT_NEW( ILOC, 'SORT', 'EXT', 1, 1, STATUS )
        END IF
        CALL DAT_FIND( ILOC, 'SORT', SLOC, STATUS )
        CALL ADI1_GRPLOC( ID, SLOC, STATUS )
        CALL ADI_CPUT0C( ID, SPROPN, SLOC, STATUS )
      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'ADI1_LOCSORT', STATUS )
      END IF

      END
