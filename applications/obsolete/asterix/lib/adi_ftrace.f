      SUBROUTINE ADI_FTRACE( ID, NLEV, PATH, FILE, STATUS )
*+
*  Name:
*     ADI_FTRACE

*  Purpose:
*     Return text description of file position of identifier

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI_FTRACE( ID, NLEV, PATH, FILE, STATUS )

*  Description:
*     Returns a description of the full specification of a file based
*     data object. This is split into 2 parts. The FILE part gives the
*     operating system filename containing the data object. The PATH
*     contains any sub-file specification required. NLEV contains a
*     count of the number of levels of sub-structure within the PATH
*     value.

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier of file object
*     NLEV = INTEGER (returned)
*        Number of hierarchy levels
*     PATH = CHARACTER*(*) (returned)
*        Sub-file path
*     FILE = CHARACTER*(*) (returned)
*        File specification
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
*     ADI Subroutine Guide : http://www.sr.bham.ac.uk:8080/asterix-docs/Programmer/Guides/adi.html

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
      INTEGER			ID			! File identifier

*  Arguments Returned:
      INTEGER			NLEV			! Number of levels
      CHARACTER*(*)		PATH, FILE		! Trace info

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			OARG			! Method output
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Invoke the method
      CALL ADI_EXEC( 'FileTrace', 1, ID, OARG, STATUS )

*  Extract data from the returned object
      CALL ADI_CGET0C( OARG, 'File', FILE, STATUS )
      CALL ADI_CGET0C( OARG, 'Path', PATH, STATUS )
      CALL ADI_CGET0I( OARG, 'Nlev', NLEV, STATUS )

*  And destroy the result
      CALL ADI_ERASE( OARG, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI_FTRACE', STATUS )

      END
