      SUBROUTINE FCI0_INIT( STATUS )
*+
*  Name:
*     FCI0_INIT

*  Purpose:
*     {routine_purpose}

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL FCI0_INIT( STATUS )

*  Description:
*     {routine_description}

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
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     FCI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/fci.html

*  Keywords:
*     package:fci, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     7 Mar 1996 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PKG'

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      LOGICAL			AST_QPKGI
        EXTERNAL		AST_QPKGI
      EXTERNAL			FCI1_SHOW

*  Local Variables:
      INTEGER			DID			! Method id
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check initialised
      IF ( .NOT. AST_QPKGI( FCI__PKG ) ) THEN

*    Require the fitting package
        CALL ADI_REQPKG( 'fitting', STATUS )

*    Define methods
        CALL ADI_DEFMTH( 'ShowStatus(_INTEGER,_CurfitControl)',
     :                   FCI1_SHOW, DID, STATUS )

*    Mark initialised
        CALL AST_SPKGI( FCI__PKG )

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'FCI0_INIT', STATUS )

      END
