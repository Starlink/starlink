      SUBROUTINE ADI2_HPCMT( HDUID, CMNT, STATUS )
*+
*  Name:
*     ADI2_HPCMT

*  Purpose:
*     Write a comment to a specified HDU

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_HPCMT( HDUID, CMNT, STATUS )

*  Description:
*     Write value of keyword to specified HDU. Any existing keyword value
*     is overwritten.

*  Arguments:
*     HDUID = INTEGER (given)
*        ADI identifier of HDU object
*     CMNT = CHARACTER*(*)
*        The comment text
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
*     ADI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/adi.html

*  Keywords:
*     package:adi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     11 Sep 1995 (DJA):
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
      INTEGER			HDUID
      CHARACTER*(*)		CMNT

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      LOGICAL			SCAND			! HDU has been scanned?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Have keywords been scanned for this HDU
      CALL ADI_CGET0L( HDUID, 'Scanned', SCAND, STATUS )
      IF ( .NOT. SCAND ) THEN
        CALL ADI2_SCAN( HDUID, STATUS )
      END IF

*  Add keyword definition
      CALL ADI2_ADDCMT( HDUID, CMNT, .TRUE., STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI2_HPCMT', STATUS )

      END
