      SUBROUTINE FCI_SHOMC( CHAN, CTRLID, STATUS )
*+
*  Name:
*     FCI_SHOMC

*  Purpose:
*     Report status of a minimisation

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL FCI_SHOMC( CHAN, CTRLID, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     CHAN = INTEGER (given)
*        Output AIO channel id
*     CTRLID = INTEGER (given)
*        Minimisation control object
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
*     FCI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/fci.html

*  Keywords:
*     package:fci, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     7 Mar 1996 (DJA):
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
      INTEGER			CHAN, CTRLID

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      LOGICAL			AST_QPKGI
        EXTERNAL		AST_QPKGI

*  Local Variables:
      INTEGER			ICHAN			! ADI copy of CHAN
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check initialised
      IF ( .NOT. AST_QPKGI( FCI__PKG ) ) CALL FCI0_INIT( STATUS )

*  Create temporary to store channel
      CALL ADI_NEWV0I( CHAN, ICHAN, STATUS )

*  Execute method
      CALL ADI_EXEC2( 'ShowStatus', ICHAN, CTRLID, STATUS )

*  Release temporary
      CALL ADI_ERASE( ICHAN, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'FCI_SHOMC', STATUS )

      END
