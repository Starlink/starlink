      SUBROUTINE ADI2_STDCMT( KEY, COMNT, STATUS )
*+
*  Name:
*     ADI2_STDCMT

*  Purpose:
*     Return a standard comment for a keyword

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_STDCMT( KEY, COMNT, STATUS )

*  Description:
*     Return a standard comment for a standard FITS keyword

*  Arguments:
*     KEY = CHARACTER*(*) (given)
*        The name of the keyword
*     COMNT = CHARACTER*(*) (returned)
*        The keyword comment
*     STATUS = INTEGER (given)
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
*     ADI Subroutine Guide : http://www.sr.bham.ac.uk:8080/asterix-docs/Programmer/Guides/adi.html

*  Keywords:
*     package:adi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     27 Feb 1995 (DJA):
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
      CHARACTER*(*)		KEY			! Keyword name

*  Arguments Returned:
      CHARACTER*(*)		COMNT			! The keyword comment

*  Status:
      INTEGER 			STATUS             	! Global status
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Simply return blank for the moment
      COMNT = ' '

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI2_STDCMT', STATUS )

      END
