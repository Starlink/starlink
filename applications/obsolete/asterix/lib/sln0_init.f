      SUBROUTINE SLN0_INIT( STATUS )
*+
*  Name:
*     SLN0_INIT

*  Purpose:
*     Initialise SLN routines

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL SLN0_INIT( STATUS )

*  Description:
*     {routine_description}

*  Arguments:
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
*     package:sln, usage:private

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

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			SLN1_GETREC
      EXTERNAL			SLN1_NREC
      EXTERNAL			SLN1_PUTREC

*  Local Variables:
      INTEGER			DID			! Method identifier
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Load the selections package
      CALL ADI_REQPKG( 'select', STATUS )

*  Define the methods
      CALL ADI_DEFMTH( 'GetSelNrec(_HDSfile)',
     :                 SLN1_NREC, DID, STATUS )
      CALL ADI_DEFMTH( 'GetSelRec(_HDSfile,_CHAR,_INTEGER)',
     :                 SLN1_GETREC, DID, STATUS )
      CALL ADI_DEFMTH( 'PutSelRec(_HDSfile,_SelectionRecord)',
     :                 SLN1_PUTREC, DID, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'SLN0_INIT', STATUS )

      END
