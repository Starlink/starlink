      SUBROUTINE SSI_FINDDS( SFID, ICOMP, FFID, STATUS )
*+
*  Name:
*     SSI_FINDDS

*  Purpose:
*     Returns identifier to dataset searched to find ICOMP'th source in SFID

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL SSI_FINDDS( SFID, ICOMP, FFID, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     SFID = INTEGER (given)
*        SSDS identifier
*     ICOMP = INTEGER (given)
*        File number in SSDS
*     FFID = INTEGER (given and returned)
*        The file searched
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
*     SSI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/ssi.html

*  Keywords:
*     package:ssi, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     9 May 1995 (DJA):
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
      INTEGER			SFID			! SSDS identifier
      INTEGER			ICOMP			! File number

*  Arguments Returned:
      INTEGER			FFID			! File searched

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*132		SFILE
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise
      FFID = ADI__NULLID

*  Locate the SEARCHED parameter
      CALL SSI_GETPAR0C( SFID, ICOMP, 'SEARCHED', SFILE, STATUS )

*  Got filename ok?
      IF ( STATUS .EQ. SAI__OK ) THEN

*    Try to open it
        CALL ADI_FOPEN( SFILE, '*', 'READ', FFID, STATUS )

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'SSI_FINDDS', STATUS )

      END
