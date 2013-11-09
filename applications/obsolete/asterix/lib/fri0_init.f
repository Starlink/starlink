      SUBROUTINE FRI0_INIT( STATUS )
*+
*  Name:
*     FRI0_INIT

*  Purpose:
*     Make ADI definitions required for FRI package

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL FRI0_INIT( STATUS )

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
*     FRI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/fri.html

*  Keywords:
*     package:fri, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     27 Jul 1995 (DJA):
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

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      LOGICAL			AST_QPKGI
        EXTERNAL		AST_QPKGI
      EXTERNAL			FRI1_CHK
      EXTERNAL			FRI1_GET
      EXTERNAL			FRI1_PUT

*  Local Variables:
      INTEGER			DID			! Dummy return value
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Not already loaded?
      IF ( .NOT. AST_QPKGI( FRI__PKG ) ) THEN

*    Load methods for reading and writing references
        CALL ADI_DEFMTH( 'ChkRef(_HDSfile,_CHAR)', FRI1_CHK,
     :                                       DID, STATUS )
        CALL ADI_DEFMTH( 'GetRef(_HDSfile,_CHAR)', FRI1_GET,
     :                                       DID, STATUS )
        CALL ADI_DEFMTH( 'PutRef(_HDSfile,_CHAR,_)', FRI1_PUT,
     :                                       DID, STATUS )

*  Mark as initialised
        CALL AST_SPKGI( FRI__PKG )

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'FRI0_INIT', STATUS )

      END
