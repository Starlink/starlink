      SUBROUTINE GMI_QNDF( ID, N, STATUS )
*+
*  Name:
*     GMI_QNDF

*  Purpose:
*     Returns number of NDFs in multiple dataset

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL GMI_QNDF( ID, N, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier of dataset
*     N = INTEGER (returned)
*        Number of graphs in multi-graph dataset
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
*     28 Apr 1995 (DJA):
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
c      INCLUDE 'GMI_CMN'                                 ! GMI common block
*       GMI_INIT = LOGICAL (given)
*         GMI class definitions loaded?

*  Arguments Given:
      INTEGER			ID			! Dataset

*  Arguments Returned:
      INTEGER			N			! Number of graphs

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

*  Extract locator and call HDS version
       CALL ADI1_GETLOC( ID, LOC, STATUS )
       CALL GMD_QNDF( LOC, N, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'GMI_QNDF', STATUS )

      END
