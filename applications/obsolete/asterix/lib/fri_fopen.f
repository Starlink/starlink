      SUBROUTINE FRI_FOPEN( ID, NAME, CLASS, MODE, LID, STATUS )
*+
*  Name:
*     FRI_FOPEN

*  Purpose:
*     Open a file specified by the named logical link

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL FRI_FOPEN( ID, NAME, CLASS, MODE, LID, STATUS )

*  Description:
*     Wraps up the extraction of the value of a logical file link and
*     a subsequent call to ADI_FOPEN to open the file.

*  Arguments:
*     ID = INTEGER (given)
*        The ADI identifier of the dataset containing the link
*     NAME = CHARACTER*(*) (given)
*        The logical file link
*     CLASS = CHARACTER*(*) (given)
*        The class to supply to ADI_FOPEN to open the file
*     MODE = CHARACTER*(*) (given)
*        The file access mode
*     LID = INTEGER (returned)
*        The identifier to the opened file
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
*     1 Aug 1995 (DJA):
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
      CHARACTER*(*)		NAME, CLASS, MODE

*  Arguments Returned:
      INTEGER			LID

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*200		FILE			! Path data
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Extract the link data
      CALL FRI_GET( ID, NAME, FILE, STATUS )

*  Open the file
      CALL ADI_FOPEN( FILE, CLASS, MODE, LID, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'FRI_FOPEN', STATUS )

      END
