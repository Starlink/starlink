      SUBROUTINE ADI2_HGKY( HDUID, KEY, VID, STATUS )
*+
*  Name:
*     ADI2_HGKY

*  Purpose:
*     Get value of keyword from specified HDU

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_HGKY( HDUID, KEY, VID, STATUS )

*  Description:
*     Get value of keyword from specified HDU. It is an error for the
*     keyword not to exist.

*  Arguments:
*     HDUID = INTEGER (given)
*        ADI identifier of HDU object
*     KEY = CHARACTER*(*) (given)
*        The name of the keyword to be extracted
*     VID = INTEGER (returned)
*        The keyword value object
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
*     11 Sep 1995 (DJA):
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
      INTEGER			HDUID
      CHARACTER*(*)		KEY

*  Arguments Returned:
      INTEGER			VID

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			KID			! Keywords list

      LOGICAL			SCAND			! HDU has been scanned?
      LOGICAL			THERE			! Keyword exists?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Have keywords been scanned for this HDU
      CALL ADI_CGET0L( HDUID, 'Scanned', SCAND, STATUS )
      IF ( .NOT. SCAND ) THEN
        CALL ADI2_SCAN( HDUID, STATUS )
      END IF

*  Locate keywords container
      CALL ADI_FIND( HDUID, 'Keys', KID, STATUS )

*  Does our keyword exist?
      CALL ADI_THERE( KID, KEY, THERE, STATUS )
      IF ( THERE ) THEN

*    Extract its value
        CALL ADI_FIND( KID, KEY, VID, STATUS )

      ELSE
        STATUS = SAI__ERROR
        CALL MSG_SETC( 'KEY', KEY )
        CALL ERR_REP( ' ', 'Keyword /^KEY/ does not exist in HDU',
     :                STATUS )
      END IF

*  Release keyword container
      CALL ERR_BEGIN( STATUS )
      CALL ADI_ERASE( KID, STATUS )
      CALL ERR_END( STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI2_HGKY', STATUS )

      END
