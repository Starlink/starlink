      SUBROUTINE WCI1_INIT( STATUS )
*+
*  Name:
*     WCI1_INIT

*  Purpose:
*     Load ADI definitions required for WCI operation

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL WCI1_INIT( STATUS )

*  Description:
*     Loads those class definitions required by the WCI subroutine group.
*     Results in the following classes being defined,
*
*       Pixellation    - Describes pixel grid w.r.t. some fiducial point
*       Projection     - Describes a map projection
*       CoordSystem    - Describes an astronomical coordinate system

*  Arguments:
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
*     ADI:
*        ADI_REQPKG - Load a package from the load path

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
*     9 Jan 1995 (DJA):
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
      INCLUDE 'WCI_CMN'					! WCI globals
*        WCI_INIT = LOGICAL (given and returned)
*           WCI definitions load attempted?

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			ADI_REQPKG
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check not already initialised?
      IF ( .NOT. WCI_INIT ) THEN

*    Load the ADI classes
        CALL ADI_REQPKG( 'wcs', STATUS )

*    Now initialised
	WCI_INIT = .TRUE.

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'WCI1_INIT', STATUS )

      END
