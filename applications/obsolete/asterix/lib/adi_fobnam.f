      SUBROUTINE ADI_FOBNAM( ID, NLEV, PATH, FILE, STATUS )
*+
*  Name:
*     ADI_FOBNAM

*  Purpose:
*     Return one line text description of file position of identifier

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI_FOBNAM( ID, NAME, NLEN, STATUS )

*  Description:
*     Returns a description of the full specification of a file based
*     data object.

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier of file object
*     NAME = CHARACTER*(*) (returned)
*        Description of object name
*     NLEN = INTEGER (returned)
*        Number of characters used in NAME
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
*     package:adi, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     12 Jan 1995 (DJA):
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

*  Arguments Returned:
      CHARACTER*(*)		NAME
      INTEGER			NLEN

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			FID			! File object
      INTEGER			OARG			! Method output
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the file object
      CALL ADI_GETFILE( ID, FID, STATUS )

*  Invoke the method
      CALL ADI_EXEC( 'FileObjName', 1, FID, OARG, STATUS )

*  Extract data from the returned object
      CALL ADI_CGET0C( OARG, 'File', NAME, STATUS )
      L = CHR_LEN(NAME)

*  And destroy the result
      CALL ADI_ERASE( OARG, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI_FOBNAM', STATUS )

      END
