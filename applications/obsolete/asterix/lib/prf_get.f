      SUBROUTINE PRF_GET( ID, FLAG, VALUE, STATUS )
*+
*  Name:
*     PRF_GET

*  Purpose:
*     Get the value of a logical processing flag

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL PRF_GET( ID, FLAG, VALUE, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier of dataset
*     FLAG = CHARACTER*(*) (given)
*        The name of the flag
*     VALUE = LOGICAL (returned)
*        The value of the flag
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
*     PRF Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/prf.html

*  Keywords:
*     package:prf, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     15 Feb 1995 (DJA):
*        Original version.
*     23 Jan 1996 (DJA)
*        Proper ADI version
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          			! SAE constants
      INCLUDE 'ADI_PAR'

*  Arguments Given:
      INTEGER			ID
      CHARACTER*(*)		FLAG

*  Arguments Returned:
      LOGICAL			VALUE

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			IARG(3)			! Method inputs
      INTEGER			OARG			! Method return value
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get linked file object
      IARG(1) = ID
      CALL ADI_GETLINK( ID, IARG(2), STATUS )

*  Store flag name
      CALL ADI_NEWV0C( FLAG, IARG(3), STATUS )

*  Execute the method
      CALL ADI_EXEC( 'GetProFlag', 3, IARG, OARG, STATUS )

*  Extract return value
      CALL ADI_GET0L( OARG, VALUE, STATUS )
      CALL ADI_ERASE( OARG, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'PRF_GET', STATUS )

      END
