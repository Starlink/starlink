      SUBROUTINE FRI_PUTI( FID, RNAME, RID, STATUS )
*+
*  Name:
*     FRI_PUTI

*  Purpose:
*     Write a file reference by ADI identifier

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL FRI_PUTI( FID, RNAME, RFILE, STATUS )

*  Description:
*     Write a file reference to the file object defined by the RID
*     argument.

*  Arguments:
*     FID = INTEGER (given)
*        ADI identifier of file to write reference to
*     RNAME = CHARACTER*(*) (given)
*        Logical name of reference to write
*     RID = INTEGER (given)
*        ADI identifier of the file object to be linked
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
      INTEGER			RID

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			FRI0_BLK		! Ensures inclusion

*  Local Variables:
      INTEGER			ARGS(3)			! Method inputs
      INTEGER			OARG			! Method output
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check initialised
      IF ( .NOT. FRI_INIT ) CALL FRI0_INIT( STATUS )

*  Construct arguments
      ARGS(1) = FID
      CALL ADI_NEWV0C( RNAME, ARGS(2), STATUS )
      ARGS(3) = RID

*  Invoke method
      CALL ADI_EXEC( 'PutRef', 3, ARGS, OARG, STATUS )

*  Erase arguments
      CALL ADI_ERASE( ARGS(2), STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'FRI_PUTI', STATUS )

      END
