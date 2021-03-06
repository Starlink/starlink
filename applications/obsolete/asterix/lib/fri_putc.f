      SUBROUTINE FRI_PUTC( FID, RNAME, RFILE, STATUS )
*+
*  Name:
*     FRI_PUTC

*  Purpose:
*     Write a file reference by character string

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL FRI_PUTC( FID, RNAME, RFILE, STATUS )

*  Description:
*     Write a file reference by character string. No checking is
*     performed to see if the named object exists, as this routine
*     may be used to write two mutually self-referencing files
*     which are not yet committed to disk (depending on the behaviour
*     of the underlying file interface).

*  Arguments:
*     FID = INTEGER (given)
*        ADI identifier of file to write reference to
*     RNAME = CHARACTER*(*) (given)
*        Logical name of reference to write
*     RFILE = CHARACTER*(*) (given)
*        Name of object to link to
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
      CHARACTER*(*)		RNAME, RFILE

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      LOGICAL			AST_QPKGI
        EXTERNAL		AST_QPKGI

*  Local Variables:
      INTEGER			ARGS(3)			! Method inputs
      INTEGER			OARG			! Method output
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check initialised
      IF ( .NOT. AST_QPKGI( FRI__PKG ) ) CALL FRI0_INIT( STATUS )

*  Construct arguments
      CALL ADI_GETFILE( FID, ARGS(1), STATUS )
      CALL ADI_NEWV0C( RNAME, ARGS(2), STATUS )
      CALL ADI_NEWV0C( RFILE, ARGS(3), STATUS )

*  Invoke method
      CALL ADI_EXEC( 'PutRef', 3, ARGS, OARG, STATUS )

*  Erase arguments
      CALL ADI_ERASE( ARGS(2), STATUS )
      CALL ADI_ERASE( ARGS(3), STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'FRI_PUTC', STATUS )

      END
