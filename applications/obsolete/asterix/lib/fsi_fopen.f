      SUBROUTINE FSI_FOPEN( FID, IDX, CLASS, RFID, STATUS )
*+
*  Name:
*     FSI_FOPEN

*  Purpose:
*     Read a reference to a FileSet object and open with specified class

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL FSI_FOPEN( FID, IDX, CLASS, RFID, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     FID = INTEGER (given)
*        ADI identifier of FileSet object
*     IDX = INTEGER (given)
*        Index of file in file set whose reference is to be written
*     RFID = INTEGER (given)
*        ADI identifier of referenced object
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
*     FSI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/fsi.html

*  Keywords:
*     package:fsi, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     29 Nov 1995 (DJA):
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
      INTEGER			FID, IDX
      CHARACTER*(*)		CLASS

*  Arguments Given:
      INTEGER			RFID

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			ARGS(4)			! Method arguments
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Construct selection object
      ARGS(1) = FID
      CALL ADI_GETLINK( FID, ARGS(2), STATUS )
      CALL ADI_NEWV0I( IDX, ARGS(3), STATUS )
      CALL ADI_NEWV0C( CLASS, ARGS(4), STATUS )

*  Invoke method
      CALL ADI_EXEC( 'ReadRef', 4, ARGS, RFID, STATUS )

*  Destroy temporary arguments
      CALL ADI_ERASE( ARGS(3), STATUS )
      CALL ADI_ERASE( ARGS(4), STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'FSI_FOPEN', STATUS )

      END
