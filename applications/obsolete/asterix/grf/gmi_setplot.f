      SUBROUTINE GMI_SETPLOT( ID, PLOT, BASE, OVLY, STATUS )
*+
*  Name:
*     GMI_SETPLOT

*  Purpose:
*     Set design of given plot

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL GMI_SETPLOT( ID, PLOT, BASE, OVLY, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier of multi-graph dataset
*     PLOT = INTEGER (given)
*        Plot number
*     BASE = INTEGER (given)
*        Base graph in plot
*     OVLY = CHARACTER*(*) (given)
*        Graphs to overlay
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
*     GMI Subroutine Guide : http://www.sr.bham.ac.uk:8080/asterix-docs/Programmer/Guides/gmi.html

*  Keywords:
*     package:gmi, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     24 Apr 1995 (DJA):
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

*  Global Variables:
      INCLUDE 'GMI_CMN'                                 ! GMI common block
*       GMI_INIT = LOGICAL (given)
*         GMI class definitions loaded?

*  Arguments Given:
      INTEGER			ID			! See above
      INTEGER			PLOT
      INTEGER			BASE
      CHARACTER*(*)		OVLY

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
c      EXTERNAL			GMI0_BLK		! Ensures inclusion

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	LOC			! Dataset locator
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check initialised
c      IF ( .NOT. GMI_INIT ) CALL GMI0_INIT( STATUS )

*  Just call HDS version for the mo
      CALL ADI1_GETLOC( ID, LOC, STATUS )
      CALL GMD_SETPLOT( LOC, PLOT, BASE, OVLY, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'GMI_SETPLOT', STATUS )

      END
