      SUBROUTINE GMI_QMULT( ID, MULT, STATUS )
*+
*  Name:
*     GMI_QMULT

*  Purpose:
*     Determines if dataset is multiple type

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL GMI_QMULT( ID, MULT, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier of dataset
*     MULT = LOGICAL (returned)
*        Is dataset specified by ID a multi-graph dataset?
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
      INCLUDE '{global_constants_file}' ! [global_constants_description]

*  Global Variables:
c      INCLUDE 'GMI_CMN'                                 ! GMI common block
*       GMI_INIT = LOGICAL (given)
*         GMI class definitions loaded?

*  Arguments Given:
      INTEGER			ID			! Dataset identifier

*  Arguments Returned:
      LOGICAL			MULT			! ID is multiple?

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
c      EXTERNAL			GMI0_BLK		! Ensures inclusion

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	LOC			!
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check initialised
C      IF ( .NOT. GMI_INIT ) CALL GMI0_INIT( STATUS )

*  Extract locator and call HDS version
      CALL ADI1_GETLOC( ID, LOC, STATUS )
      CALL GMD_QMULT( LOC, MULT, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'GMI_QMULT', STATUS )

      END
