      SUBROUTINE BDI1_UNLNK( LHS, RHS, STATUS )
*+
*  Name:
*     BDI1_UNLNK

*  Purpose:
*     Service UnLink method for various class to HDSfile links

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI1_UNLNK( LHS, RHS, STATUS )

*  Description:
*     Establishes ADI file link between high level objects Scalar, Array
*     and BinDS and the HDSfile.

*  Arguments:
*     LHS = INTEGER (given)
*        ADI identifier of high level object
*     RHS = INTEGER (given)
*        ADI identifier of low level object
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
*     BDI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/bdi.html

*  Keywords:
*     package:bdi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     9 Aug 1995 (DJA):
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
      INTEGER                   LHS, RHS

*  Status:
      INTEGER 			STATUS             	! Global status
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Retype if write access
      CALL ADI1_LRETYP( LHS, 'BINDS', RHS, STATUS )

*  Unmap any loose items
      CALL ADI0_UNLNK( LHS, RHS, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'BDI1_UNLNK', STATUS )

*  Invoke base method to perform unlinkage
      CALL ADI_CALNXT( STATUS )

      END
