      SUBROUTINE GMI_SETLAYOUT( ID, SETX, NX, SETY, NY, STATUS )
*+
*  Name:
*     GMI_SETLAYOUT

*  Purpose:
*     Set LAYOUT attributes for multiple dataset

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL GMI_SETLAYOUT( ID, SETX, NX, SETY, NY, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier of MultiGraph object
*     SETX = LOGICAL (given)
*        Set number of X plots?
*     NX = INTEGER (given)
*        Number of X plots
*     SETY = LOGICAL (given)
*        Set number of Y plots?
*     NY = INTEGER (given)
*        Number of Y plots
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
*     GMI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/gmi.html

*  Keywords:
*     package:gmi, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     5 Oct 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'

*  Arguments Given:
      INTEGER			ID, NX, NY
      LOGICAL			SETX, SETY

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*(DAT__SZLOC)    LOC                     ! Dataset locator
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Just invoke HDS version
      CALL ADI1_GETLOC( ID, LOC, STATUS )
      CALL GMD_SETPLOTS( LOC, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'GMI_SETLAYOUT', STATUS )
      END IF

      END
