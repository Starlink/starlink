      SUBROUTINE PRF_SET( ID, FLAG, VALUE, STATUS )
*+
*  Name:
*     PRF_SET

*  Purpose:
*     Set the value of a logical processing flag

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL PRF_SET( ID, FLAG, VALUE, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier of dataset
*     FLAG = CHARACTER*(*) (given)
*        The name of the flag
*     VALUE = LOGICAL (given)
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
*     PRF Subroutine Guide : http://www.sr.bham.ac.uk:8080/asterix-docs/Programmer/Guides/prf.html

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
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          			! SAE constants
      INCLUDE 'DAT_PAR'					! HDS constants

*  Arguments Given:
      INTEGER			ID			! Dataset id
      CHARACTER*(*)		FLAG			! Flag name
      LOGICAL			VALUE			! Flag value

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	LOC			! Temporary !
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get locator and invoke HDS version
      CALL ADI1_GETLOC( ID, LOC, STATUS )
      CALL PRO_SET( LOC, FLAG, VALUE, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'PRF_SET', STATUS )

      END
