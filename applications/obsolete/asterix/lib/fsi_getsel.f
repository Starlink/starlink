      SUBROUTINE FSI_GETSEL( FID, IDX, MXSEL, SEL, NSEL, STATUS )
*+
*  Name:
*     FSI_GETSEL

*  Purpose:
*     Read selection array from a FileSet object

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL FSI_GETSEL( FID, IDX, MXSEL, SEL, NSEL, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     FID = INTEGER (given)
*        ADI identifier of FileSet object
*     IDX = INTEGER (given)
*        Index of file in file set whose selection is to be read
*     MXSEL = INTEGER (given)
*        Maximum number of selected items
*     SEL[] = INTEGER (returned)
*        Selected item numbers from the IDX'th file
*     NSEL = INTEGER (returned)
*        Number of selected items from the IDX'th file
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
*     FSI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/fsi.html

*  Keywords:
*     package:fsi, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     29 Nov 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ADI_PAR'

*  Arguments Given:
      INTEGER			FID, IDX, MXSEL

*  Arguments Returned:
      INTEGER			NSEL, SEL(*)

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			ARGS(2)			! Method arguments
      INTEGER			OARG			! Method return data
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Construct selection object
      ARGS(1) = FID
      CALL ADI_NEWV0I( IDX, ARGS(2), STATUS )

*  Invoke method
      CALL ADI_EXEC( 'ReadSel', 2, ARGS, OARG, STATUS )

*  Destroy argument
      CALL ADI_ERASE( ARGS(2), STATUS )

*  Extract selections
      IF ( OARG .NE. ADI__NULLID ) THEN
        CALL ADI_GET1I( OARG, MXSEL, SEL, NSEL, STATUS )
        CALL ADI_ERASE( OARG, STATUS )
      ELSE
        NSEL = 0
        STATUS = SAI__ERROR
        CALL MSG_SETI( 'N', IDX )
        CALL ERR_REP( ' ', 'No selections for reference ^N', STATUS )
      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'FSI_GETSEL', STATUS )

      END
