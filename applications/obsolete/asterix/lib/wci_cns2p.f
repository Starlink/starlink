      SUBROUTINE WCI_CNS2P( SPOS, PIXID, PRJID, PPOS, STATUS )
*+
*  Name:
*     WCI_CNS2P

*  Purpose:
*     Convert position in pixels to standard system

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL WCI_CNS2P( SPOS, PIXID, PRJID, PPOS, STATUS )

*  Description:
*     Converts a position in the WCI standard system (FK5, equinox 2000)
*     to a position in continuous pixel coordinates.

*  Arguments:
*     SPOS[2] = DOUBLE (given)
*        Celestial position in radians
*     PIXID = INTEGER (given)
*        Pixellation object
*     PRJID = INTEGER (given)
*        Projection object
*     PPOS[2] = REAL (returned)
*        Position in fractional pixels. 1.0 is the centre of the first pixel
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
*     package:wci, usage:public

*  Copyright:
*     {routine_copyright}

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     5 Jan 1995 (DJA):
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
      DOUBLE PRECISION		SPOS(2)			! Celestial position
      INTEGER			PIXID			! Pixellation
      INTEGER			PRJID			! Projection details

*  Arguments Returned:
      REAL			PPOS(2)			! Pixel position

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      REAL			APOS(2)			! Position in axis units
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Convert standard system coordinates to axis values
      CALL WCI_CNS2A( SPOS, PIXID, PRJID, APOS, STATUS )

*  And then axis units to pixels
      CALL WCI_CNA2P( APOS, PIXID, PPOS, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'WCI_CNS2P', STATUS )

      END
