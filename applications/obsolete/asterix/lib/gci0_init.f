      SUBROUTINE GCI0_INIT( STATUS )
*+
*  Name:
*     GCI0_INIT

*  Purpose:
*     Load ADI definitions required for GCI operation

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL GCI0_INIT( STATUS )

*  Description:
*     Loads those class definitions required by the GCI subroutine group.
*     Results in the following classes being defined,
*
*       MissionStrings - Describes hardware configuration use for observation
*
*     Methods are defined to read and write GCI information from HDS and
*     FITS files.

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

*  References:
*     GCI Subroutine Guide : http://www.sr.bham.ac.uk:8080/asterix-docs/Programmer/Guides/gci.html

*  Keywords:
*     package:gci, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

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
      INCLUDE 'GCI_CMN'					! GCI globals
*        GCI_INIT = LOGICAL (given and returned)
*           GCI definitions load attempted?

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			ADI_REQPKG

*  Local variables:
      INTEGER			DID			! Ignored identifier
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check not already initialised?
      IF ( .NOT. GCI_INIT ) THEN

*    Load the ADI classes
        CALL ADI_REQPKG( 'gencal', STATUS )

*    Now initialised
	GCI_INIT = .TRUE.

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'GCI0_INIT', STATUS )

      END
