      SUBROUTINE GMI_GETPLOT( ID, PLOT, BASE, OVLY, STATUS )
*+
*  Name:
*     GMI_GETPLOT

*  Purpose:
*     Get details of a paricular plot in a multi-graph dataset

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL GMI_GETPLOT( ID, PLOT, BASE, OVLY, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier of multi-graph dataset
*     PLOT = INTEGER (given)
*        Plot number in multi-graph
*     BASE = INTEGER (returned)
*        Base graph in plot
*     OVLY = CHARACTER*(*) (returned)
*        Graphs to overlay in this plot
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
*     12 Sep 1995 (DJA):
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
      INTEGER			ID, PLOT

*  Arguments Returned:
      INTEGER			BASE
      CHARACTER*(*)		OVLY

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	LOC
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Just invoke HDS version
      CALL ADI1_GETLOC( ID, LOC, STATUS )
      CALL GMD_GETPLOT( LOC, PLOT, BASE, OVLY, STATUS )

*  Check initialised
      IF ( .NOT. GMI_INIT ) CALL GMI0_INIT( STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'GMI_GETPLOT', STATUS )

      END
