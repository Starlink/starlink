      SUBROUTINE PRF1_GET( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     PRF1_GET

*  Purpose:
*     Get value of processing flag from HDS file

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL PRF1_GET( NARG, ARGS, OARG, STATUS )

*  Description:
*     Returns the value of a named processing flag. The name is given as
*     the HDS structure after the MORE.ASTERIX.PROCESSING object, eg.
*     CORRECTED.EXPOSURE or BGND_SUBTRACTED.

*  Arguments:
*     NARG = INTEGER (given)
*        Number of method arguments
*     ARGS(*) = INTEGER (given)
*        ADI identifier of method arguments
*     OARG = INTEGER (returned)
*        Output data
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
*     PRF Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/prf.html

*  Keywords:
*     package:prf, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     23 Jan 1996 (DJA):
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
      INTEGER                   NARG, ARGS(*)

*  Arguments Returned:
      INTEGER                   OARG

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	FLOC			! Flag object
      CHARACTER*(DAT__SZLOC)	LOC			! File object
      CHARACTER*40		NAME			! Flag name
      CHARACTER*(DAT__SZLOC)	PLOC			! PROCESSING object

      LOGICAL			VALUE			! Return value
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Default is flag not set
      VALUE = .FALSE.

*  Get file locator
      CALL ADI1_GETLOC( IARG(2), LOC, STATUS )

*  Does PROCESSING component exist?
      CALL ADI1_FIND( LOC, 'MORE.ASTERIX.PROCESSING', PLOC, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*    Extract processing flag name
        CALL ADI_GET0C( IARG(3), NAME, STATUS )

*    Does named flag exist?
        CALL ADI1_FIND( PLOC, NAME, FLOC, STATUS )
        IF ( STATUS .EQ. SAI__OK ) THEN

*      Get flag value
          CALL DAT_GET0L( FLOC, VALUE, STATUS )
          IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
            VALUE = .FALSE.
          END IF

*      Free flag structure
          CALL DAT_ANNUL( FLOC, STATUS )

        ELSE
          CALL ERR_ANNUL( STATUS )
        END IF

*    Free PROCESSING structure
        CALL DAT_ANNUL( PLOC, STATUS )

      ELSE
        CALL ERR_ANNUL( STATUS )
      END IF

*  Set return value
      CALL ADI_NEWV0L( VALUE, OARG, STATUS )

*  Report any outstanding errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'PRF1_GET', STATUS )

      END
