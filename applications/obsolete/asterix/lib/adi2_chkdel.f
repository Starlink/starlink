      SUBROUTINE ADI2_CHKDEL( ID, STATUS )
*+
*  Name:
*     ADI2_CHKDEL

*  Purpose:
*     Is object marked for delete? If so, issue warning

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_CHKDEL( ID, STATUS )

*  Description:
*     Issues an error if the specified object is marked for deletion

*  Arguments:
*     ID = INTEGER (given)
*        The FITS cache object to check
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
*     Copyright (C) University of Birmingham, 1996

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     10 Aug 1995 (DJA):
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

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      LOGICAL			MRKDEL
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get mark for delete flag
      CALL ADI_CGET0L( ID, 'MarkedForDelete', MRKDEL, STATUS )
      IF ( MRKDEL ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Object is marked for delete - '/
     :                         /'access forbidden', STATUS )
      END IF

*  Report name
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'ADI2_CHKDEL', STATUS )
      END IF

      END
