      SUBROUTINE FRI_GETC( FID, RNAME, RFILE, STATUS )
*+
*  Name:
*     FRI_GETC

*  Purpose:
*     Read a file reference into a character string

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL FRI_GETC( FID, RNAME, RFILE, STATUS )

*  Description:
*     Read a file reference into a character string. No checking is
*     performed to see if the named object exists.

*  Arguments:
*     FID = INTEGER (given)
*        ADI identifier of file to read reference from
*     RNAME = CHARACTER*(*) (given)
*        Logical name of reference to read
*     RFILE = CHARACTER*(*) (returned)
*        Name of object link points to
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
*     package:fri, usage:public

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

*  Arguments Given:
      INTEGER			FID
      CHARACTER*(*)		RNAME

*  Arguments Returned:
      CHARACTER*(*)		RFILE

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      LOGICAL			AST_QPKGI
        EXTERNAL		AST_QPKGI

*  Local Variables:
      INTEGER			FILID			! Base file object
      INTEGER			RID			! ADI string of RNAME
      INTEGER			OARG			! Method output
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check initialised
      IF ( .NOT. AST_QPKGI( FRI__PKG ) ) CALL FRI0_INIT( STATUS )

*  Construct arguments
      CALL ADI_NEWV0C( RNAME, RID, STATUS )

*  Get base file object
      CALL ADI_GETFILE( FID, FILID, STATUS )

*  Invoke method
      CALL ADI_EXEC2( 'GetRef', FILID, RID, OARG, STATUS )

*  Extract return value
      CALL ADI_GET0C( OARG, RFILE, STATUS )

*  Erase argument and return value
      CALL ADI_ERASE( RID, STATUS )
      CALL ADI_ERASE( OARG, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'FRI_GETC', STATUS )

      END
