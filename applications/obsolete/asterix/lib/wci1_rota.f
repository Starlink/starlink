      SUBROUTINE WCI1_ROTA( IPOS, PIXID, FORW, OPOS, STATUS )
*+
*  Name:
*     WCI1_ROTA

*  Purpose:
*     Rotate or derotate a position given a position angle

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL WCI1_ROTA( IPOS, PIXID, FORW, OPOS, STATUS )

*  Description:
*     The position IPOS is rotated about the origin by an amount
*     determined by the ROTATION data member of PIXID. The sense
*     of the rotation is positive if FORW is true, otherwise the
*     sense is negative.

*  Arguments:
*     IPOS[2] = DOUBLE (given)
*        Input position
*     PIXID = INTEGER (given)
*        ADI identifier of object containing rotation angle
*     FORW = LOGICAL (given)
*        Rotate if specified true, otherwise derotate
*     OPOS[2] = DOUBLE (returned)
*        The output position
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
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     31 Jan 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          		! Standard SAE constants

*  Global Variables:
      INCLUDE 'WCI_CMN'				! ASTERIX WCI common block
*       WCS_INIT = LOGICAL (given)
*         WCI class definitions loaded?

*  Arguments Given:
      DOUBLE PRECISION		IPOS(2)			! Input position
      INTEGER			PIXID			! Object defining rot'n
      LOGICAL			FORW			! Rotate forward?

*  Arguments Returned:
      DOUBLE PRECISION		OPOS(2)			! Output position

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local variables:
      DOUBLE PRECISION		ROTA			! Rotation angle
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check initialised
      IF ( .NOT. WCI_INIT ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'WCI has not been initialised', STATUS )
      END IF

*  Extract rotation
      CALL ADI_CGET0D( PIXID, 'ROTATION', ROTA, STATUS )

*  Null rotation?
      IF ( ROTA .NE. 0D0 ) THEN
        IF ( FORW ) THEN
          OPOS(1) = COSD(ROTA)*IPOS(1) + SIND(ROTA)*IPOS(2)
          OPOS(2) = COSD(ROTA)*IPOS(2) - SIND(ROTA)*IPOS(1)

        ELSE
          OPOS(1) = COSD(ROTA)*IPOS(1) - SIND(ROTA)*IPOS(2)
          OPOS(2) = COSD(ROTA)*IPOS(2) + SIND(ROTA)*IPOS(1)

        END IF

      ELSE
        OPOS(1) = IPOS(1)
        OPOS(2) = IPOS(2)

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'WCI1_ROTA', STATUS )

      END
