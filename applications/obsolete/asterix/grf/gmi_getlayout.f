      SUBROUTINE GMI_GETLAYOUT( ID, XSET, NX, YSET, NY, STATUS )
*+
*  Name:
*     GMI_GETLAYOUT

*  Purpose:
*     Get plot layout for multi-graph dataset

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL GMI_GETLAYOUT( ID, XSET, NX, YSET, NY, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier of multi-graph dataset
*     XSET = LOGICAL (returned)
*        Number of plots in X direction defined?
*     NX = INTEGER (returned)
*        Number of plots in X direction
*     YSET = LOGICAL (returned)
*        Number of plots in Y direction defined?
*     NY = INTEGER (returned)
*        Number of plots in Y direction
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
      INTEGER			ID

*  Arguments Returned:
      INTEGER			NX, NY
      LOGICAL			XSET, YSET

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	LOC
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Just invoke HDS for the moment
      CALL ADI1_GETLOC( ID, LOC, STATUS )
      CALL GMD_GETLAYOUT( LOC, XSET, NX, YSET, NY, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'GMI_GETLAYOUT', STATUS )
      END IF

      END
