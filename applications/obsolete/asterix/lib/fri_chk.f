      SUBROUTINE FRI_CHK( FID, RNAME, EXIST, STATUS )
*+
*  Name:
*     FRI_CHK

*  Purpose:
*     Check existance of a file reference

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL FRI_CHK( FID, RNAME, EXIST, STATUS )

*  Description:
*     Check to see whether the named file reference exists in the file
*     specified. This routine does not check to see if the link is valid.

*  Arguments:
*     FID = INTEGER (given)
*        ADI identifier of file to write reference to
*     RNAME = CHARACTER*(*) (given)
*        Logical name of reference to write
*     EXIST = LOGICAL (returned)
*        Does link exist?
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

*  Global Variables:
      INCLUDE 'FRI_CMN'                                 ! FRI common block
*       FRI_INIT = LOGICAL (given)
*         FRI class definitions loaded?

*  Arguments Given:
      INTEGER			FID
      CHARACTER*(*)		RNAME

*  Arguments Returned:
      LOGICAL                   EXIST

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			FRI0_BLK		! Ensures inclusion

*  Local Variables:
      INTEGER			OARG			! Method output
      INTEGER			RID			! ADI string of RNAME
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check initialised
      IF ( .NOT. FRI_INIT ) CALL FRI0_INIT( STATUS )

*  Construct arguments
      CALL ADI_NEWV0C( RNAME, RID, STATUS )

*  Invoke method
      CALL ADI_EXEC2( 'ChkRef', FID, RID, OARG, STATUS )

*  Extract return value
      CALL ADI_GET0L( OARG, EXIST, STATUS )

*  Erase argument and return value
      CALL ADI_ERASE( RID, STATUS )
      CALL ADI_ERASE( OARG, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'FRI_CHK', STATUS )

      END
