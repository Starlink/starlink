      SUBROUTINE GCB_FLOAD( FID, STATUS )
*+
*  Name:
*     GCB_FLOAD

*  Purpose:
*     Loads Grafix Control Block from file object

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL GCB_FLOAD( FID, STATUS )

*  Description:
*     Loads Grafix Control Block from file object. Currently just calls the
*     HDS version.

*  Arguments:
*     FID = INTEGER (given)
*        ADI identifier of file object
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
*     GCB Subroutine Guide : http://www.sr.bham.ac.uk:8080/asterix-docs/Programmer/Guides/gcb.html

*  Keywords:
*     package:gcb, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     12 Jan 1995 (DJA):
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
      INTEGER			FID			! File object id

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	LOC			! HDS file handle
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Extract locator and call HDS routine
      CALL ADI1_GETLOC( FID, LOC, STATUS )
      CALL GCB_LOAD( LOC, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'GCB_FLOAD', STATUS )

      END
