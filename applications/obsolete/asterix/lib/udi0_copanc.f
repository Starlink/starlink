      SUBROUTINE UDI0_COPANC( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     UDI0_COPANC

*  Purpose:
*     File form independent copy of ancillary data

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL UDI0_COPANC( NARG, ARGS, OARG, STATUS )

*  Description:
*     {routine_description}

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
*     UDI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/udi.html

*  Keywords:
*     package:udi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     19 Sep 1995 (DJA):
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
      INTEGER                   NARG, ARGS(*)

*  Arguments Returned:
      INTEGER                   OARG

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			PIXID, PRJID, SYSID	! WCS data
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  World coordinates
      CALL WCI_GETIDS( ARGS(1), PIXID, PRJID, SYSID, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
        CALL WCI_PUTIDS( ARGS(4), PIXID, PRJID, SYSID, STATUS )
      ELSE
        CALL ERR_ANNUL( STATUS )
      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'UDI0_COPANC', STATUS )
      END IF

      END
