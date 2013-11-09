      SUBROUTINE SLN_PUTREC( ID, SID, STATUS )
*+
*  Name:
*     SLN_PUTREC

*  Purpose:
*     Write a selection record to a file

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL SLN_PUTREC( ID, SID, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier of dataset containing selection info
*     SID = INTEGER (given)
*        ADI identifier of selection record
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
*     SLN Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/sln.html

*  Keywords:
*     package:sln, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     4 Sep 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PKG'

*  Arguments Given:
      INTEGER			ID, SID

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      LOGICAL			AST_QPKGI
        EXTERNAL		AST_QPKGI

*  Local Variables:
      INTEGER			IARG(2)			! Method inputs
      INTEGER			OARG			! Method result
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check initialised
      IF ( .NOT. AST_QPKGI( SLN__PKG ) ) CALL SLN0_INIT( STATUS )

*  Locate file identifier
      CALL ADI_GETFILE( ID, IARG(1), STATUS )

*  Store selection record id
      IARG(2) = SID

*  Invoke method
      CALL ADI_EXEC( 'PutSelRec', 2, IARG, OARG, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'SLN_PUTREC', STATUS )

      END
