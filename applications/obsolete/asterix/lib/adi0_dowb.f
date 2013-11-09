      SUBROUTINE ADI0_DOWB( WRITEB, MODID, FILEID, PSID, STATUS )
*+
*  Name:
*     ADI0_DOWB

*  Purpose:
*     Invoke a WriteBack function

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI0_DOWB( WRITEB, MODID, FILEID, PSID, STATUS )

*  Description:
*     Invokes a WriteBack function to change memory data back to the file
*     representation immediately prior to it being written to file.

*  Arguments:
*     WRITEB = EXTERNAL (given)
*        The WriteBack procedure
*     MODID = INTEGER (given)
*        ADI identifier to top level model object
*     FILEID = INTEGER (given)
*        The ADI identifier of the file object
*     PSID = INTEGER (given)
*        ADI identifier private storage
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
*     package:adi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*      9 Aug 1995 (DJA):
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
      EXTERNAL			WRITEB
      INTEGER                   MODID, FILEID, PSID

*  Status:
      INTEGER 			STATUS             	! Global status
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Invoke write back
      CALL WRITEB( MODID, FILEID, PSID, STATUS )

      END
