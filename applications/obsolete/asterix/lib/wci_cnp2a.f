      SUBROUTINE WCI_CNP2A( PPOS, PIXID, APOS, STATUS )
*+
*  Name:
*     WCI_CNP2A

*  Purpose:
*     Convert position in pixels to axis units

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL WCI_CNP2A( PPOS, PIXID, APOS, STATUS )

*  Description:
*     Converts a position APOS in continuous pixel coordinates into a
*     position in axis units using the pixellation data in PIXID.

*  Arguments:
*     PPOS[2] = REAL (given)
*        Position in fractional pixels
*     PIXID = INTEGER (given)
*        ADI identifier of pixellation object
*     APOS[2] = REAL (returned)
*        Position in axis units
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
*     WCI Subroutine Guide : http://www.sr.bham.ac.uk:8080/asterix-docs/Programmer/Guides/wci.html

*  Keywords:
*     package:wci, usage:private

*  Copyright:
*     {routine_copyright}

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     6 Jan 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Global Variables:
      INCLUDE 'WCI_CMN'                 ! ASTERIX WCI common block
*       WCS_INIT = LOGICAL (given)
*         WCI class definitions loaded?

*  Arguments Given:
      REAL			APOS(2)			! Axis position
      INTEGER			PIXID			! Pixellation

*  Arguments Returned:
      REAL			PPOS(2)			! Pixel position

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      REAL			ABASE(2)		! Axis val at pix 1
      REAL			ASCALE(2)		! Bin width

      INTEGER			DUM			! Array size
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check initialised
      IF ( .NOT. WCI_INIT ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'WCI has not been initialised', STATUS )
      END IF

*  Get axis scalings
      CALL ADI_CGET1I( PIXID, 'BASE', 2, ABASE, DUM, STATUS )
      CALL ADI_CGET1I( PIXID, 'SCALE', 2, ASCALE, DUM, STATUS )

*  Calculate axis values
      APOS(1) = ABASE(1) + (PPOS(1)-1.0)*ASCALE(1)
      APOS(2) = ABASE(2) + (PPOS(2)-1.0)*ASCALE(2)

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'WCI_CNP2A', STATUS )

      END
