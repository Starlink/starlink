      SUBROUTINE WCI1_LOCPRJ( NAME, PTR, STATUS )
*+
*  Name:
*     WCI1_LOCPRJ

*  Purpose:
*     Locate projector routine for a named projection

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL WCI1_LOCPRJ( NAME, PTR, STATUS )

*  Description:
*     Each projection is serviced by a projector routine. This subroutine
*     returns the address of the projector routine given the name of the
*     projection.

*  Arguments:
*     NAME = CHARACTER*(*) (given)
*        The name of the required projection
*     RPTR = INTEGER (returned)
*        The address of the projector routine
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

*  {machine}-specific features used:
*     {routine_machine_specifics}...

*  {DIY_prologue_heading}:
*     {DIY_prologue_text}

*  References:
*     {routine_references}...

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

*  Arguments Given:
      CHARACTER*(*)		NAME			! Projection name

*  Arguments Returned:
      INTEGER			PTR			! Projector address

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			WCI1_XPARC		! Zenithal equidistant
      EXTERNAL			WCI1_XPAZP		! Zenithal perspective
      EXTERNAL			WCI1_XPCAR		! Cartesian
      EXTERNAL			WCI1_XPSIN		! Orthographic
      EXTERNAL			WCI1_XPSTG		! Stereoraphic
      EXTERNAL			WCI1_XPTAN		! Tan/Gnomonic

      EXTERNAL			UTIL_PLOC		! Portable %LOC
        INTEGER			UTIL_PLOC
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Switch on projection name
      IF ( NAME .EQ. 'CAR' ) THEN
        PTR = UTIL_PLOC( WCI1_XPCAR )

      ELSE IF ( NAME .EQ. 'TAN' ) THEN
        PTR = UTIL_PLOC( WCI1_XPTAN )

      ELSE IF ( NAME .EQ. 'AZP' ) THEN
        PTR = UTIL_PLOC( WCI1_XPAZP )

      ELSE IF ( NAME .EQ. 'SIN' ) THEN
        PTR = UTIL_PLOC( WCI1_XPSIN )

      ELSE IF ( NAME .EQ. 'STG' ) THEN
        PTR = UTIL_PLOC( WCI1_XPSTG )

      ELSE IF ( NAME .EQ. 'ARC' ) THEN
        PTR = UTIL_PLOC( WCI1_XPARC )

*  Not implemented yet...
      ELSE
        STATUS = SAI__ERROR
        CALL MSG_SETC( 'PROJ', NAME )
        CALL ERR_REP( ' ', 'Projection /^PROJ/ has not yet been'/
     :                /' implemented', STATUS )
      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'WCI1_LOCPRJ', STATUS )

      END
