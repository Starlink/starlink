      SUBROUTINE ADI1_LOCGRAF( ID, CREATE, GLOC, STATUS )
*+
*  Name:
*     ADI1_LOCGRAF

*  Purpose:
*     Locate ASTERIX.GRAFIX structure given HDS object

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI1_LOCGRAF( ID, CREATE, GLOC, STATUS )

*  Description:
*     Locate ASTERIX grafix structure given HDS object. The routine first
*     checks that the object has not already been found.

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier referencing HDS object
*     CREATE = LOGICAL (given)
*        Create component if it doesn't exist?
*     GLOC = CHARACTER*(DAT__SZLOC) (returned)
*        Locate to GRAFIX object
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
      INTEGER			ID
      LOGICAL			CREATE

*  Arguments Returned:
      CHARACTER*(DAT__SZLOC)	GLOC

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Constants:
      CHARACTER*14		GPROPN			! Name of property
        PARAMETER		( GPROPN = '.GRAFIXlocator' )

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	ALOC			! Object locator

      LOGICAL			THERE			! Property exists?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Located already?
      CALL ADI_THERE( ID, GPROPN, THERE, STATUS )
      IF ( THERE ) THEN
        CALL ADI_CGET0C( ID, GPROPN, GLOC, STATUS )
      ELSE
        CALL ADI1_LOCAST( ID, CREATE, ALOC, STATUS )
        CALL DAT_THERE( ALOC, 'GRAFIX', THERE, STATUS )
        IF ( CREATE .AND. .NOT. THERE ) THEN
          CALL DAT_NEW( ALOC, 'GRAFIX', 'EXT', 0, 0, STATUS )
        END IF
        CALL DAT_FIND( ALOC, 'GRAFIX', GLOC, STATUS )
        CALL ADI_CPUT0C( ID, GPROPN, GLOC, STATUS )
      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'ADI1_LOCGRAF', STATUS )
      END IF

      END
