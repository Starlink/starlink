      SUBROUTINE WCI1_XPTAN( OP, UNPROJ, NPAR, PARAM, PROJ, STATUS )
*+
*  Name:
*     WCI1_XPTAN

*  Purpose:
*     Perform projection operations for Tan/Gnomonic (TAN) projection

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL WCI1_XPTAN( OP, UNPROJ, NPAR, PARAM, PROJ, STATUS )

*  Description:
*     Performs conversions for the TAN projection. This is a perspective
*     zenithal projection where the plane of projection is tangent to the
*     sphere at the special point, and the source of the projection is
*     the centre of the sphere.
*
*     UNPROJ are the native spherical coordinates, PROJ are the projected
*     linearised x,y coordinates.

*  Arguments:
*     OP = INTEGER (given)
*        Operation code
*     UNPROJ[2] = DOUBLE (given and returned depending on OP)
*        Unprojected coordinates
*     NPAR = INTEGER (given)
*        Number of projection parameters
*     PARAM(*) = REAL (given)
*        Projection parameters
*     PROJ[2] = DOUBLE (given and returned depending on OP)
*        Projected coordinates
*     STATUS = INTEGER (given)
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
*     WCI Subroutine Guide : http://www.star.sr.bham.ac.uk:8080/asterix-docs/prog/wci.html
*     Representation of Celestial Coordinates in FITS : http://.../

*  Keywords:
*     {routine_keywords}...

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
      INCLUDE 'WCI_PAR'			! ASTERIX WCI constants

*  Arguments Given:
      INTEGER			OP			! Operation code
      INTEGER			NPAR			! # projection pars
      REAL			PARAM(*)		! Projection pars

*  Arguments Given and Returned:
      DOUBLE PRECISION          UNPROJ(2)		! Unprojected coords
      DOUBLE PRECISION          PROJ(2)			! Projected coords

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local variables:
      DOUBLE PRECISION		RTHETA			! Off-axis angle
*.

*  Check inherited status on entry
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Switch on code
      IF ( OP .EQ. WCI__OPN2S ) THEN

*    Distance from special point
        RTHETA = 1D0 / TAN( UNPROJ(2) )

*    Convert native sphericals to linear x,y
        PROJ(1) = RTHETA * SIN( UNPROJ(1) )
        PROJ(2) = - RTHETA * COS( UNPROJ(1) )

      ELSE IF ( OP .EQ. WCI__OPS2N ) THEN

*    Distance from special point
        RTHETA = SQRT( PROJ(1)**2 + PROJ(2)**2 )

*    Convert linear x,y to native sphericals
        UNPROJ(1) = ATAN2( PROJ(1), - PROJ(2) )
        UNPROJ(2) = ATAN( 1D0 / RTHETA )

      END IF

      END
