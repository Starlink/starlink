      SUBROUTINE WCI1_XPCAR( OP, UNPROJ, NPAR, PARAM, PROJ, STATUS )
*+
*  Name:
*     WCI1_XPCAR

*  Purpose:
*     Perform projection operations for Cartesian (CAR) projection

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL WCI1_XPCAR( OP, UNPROJ, NPAR, PARAM, PROJ, STATUS )

*  Description:
*     Performs conversions for the CAR projection. This is the simplest
*     possible (sensible) projection.

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
      INTEGER STATUS             ! Global status
*.

*  Check inherited status on entry
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Switch on code
      IF ( OP .EQ. WCI__OPN2S ) THEN
        PROJ(1) = UNPROJ(1)
        PROJ(2) = UNPROJ(2)

      ELSE IF ( OP .EQ. WCI__OPS2N ) THEN
        UNPROJ(1) = PROJ(1)
        UNPROJ(2) = PROJ(2)

      END IF

      END
