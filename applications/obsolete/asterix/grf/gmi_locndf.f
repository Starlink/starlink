      SUBROUTINE GMI_LOCNDF( FID, N, CLASS, GID, STATUS )
*+
*  Name:
*     GMI_LOCNDF

*  Purpose:
*     Locate the specified element of a multi-graph dataset

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL GMI_LOCNDF( FID, N, CLASS, GID, STATUS )

*  Description:
*     Simply call HDS version for the moment

*  Arguments:
*     FID = INTEGER (given)
*        ADI identifier of the top-level file object
*     N = INTEGER (given)
*        The graph to locate
*     CLASS = CHARACTER*(*) (given)
*        Class list used to link the multi-graph component to the dat
*        system. Supply '*' if no linkage required.
*     GID = INTEGER (returned)
*        The identifier of the graph component
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
*     21 Apr 1995 (DJA):
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
      INTEGER			FID, N
      CHARACTER*(*)		CLASS

*  Arguments Returned:
      INTEGER			GID

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	FLOC			!
      CHARACTER*(DAT__SZLOC)	GLOC			!

      INTEGER			CID			! Component id
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Extract top-level locator, call HDS version and give locator to ADI
      CALL ADI1_GETLOC( FID, FLOC, STATUS )
      CALL GMD_LOCNDF( FLOC, N, GLOC, STATUS )
      CALL ADI1_MKFILE( GLOC, 'READ', CID, STATUS )

*  Perform linkage
      CALL ADI_FLINK( CID, CLASS, GID, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'GMI_LOCNDF', STATUS )

      END
