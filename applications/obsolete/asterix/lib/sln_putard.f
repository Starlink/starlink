      SUBROUTINE SLN_PUTARD( SID, QNAM, GRPID, STATUS )
*+
*  Name:
*     SLN_PUTARD

*  Purpose:
*     Write selector consisting of an area description

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL SLN_PUTARD( SID, QNAM, GRPID, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     SID = INTEGER (given)
*        ADI identifier of selection record
*     QNAM = CHARACTER*(*) (given)
*        Name of selection quantity
*     GRPID = INTEGER (given)
*        GRP identifier holding text of area description
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
*     SLN Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/sln.html

*  Keywords:
*     package:sln, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     4 Sep 1995 (DJA):
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
      INCLUDE 'SLN_CMN'                                 ! SLN common block
*       SLN_INIT = LOGICAL (given)
*         SLN class definitions loaded?

*  Arguments Given:
      INTEGER			SID, GRPID
      CHARACTER*(*)		QNAM

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			SLN0_BLK		! Ensures inclusion

*  Local Variables:
      INTEGER			QID			! Quantity structure
      INTEGER			SSID			! Selectors structure
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check initialised
      IF ( .NOT. SLN_INIT ) CALL SLN0_INIT( STATUS )

*  Locate Selectors structure
      CALL ADI_FIND( SID, 'Selectors', SSID, STATUS )

*  Create new structure, and locate it
      CALL ADI_CNEW0( SSID, QNAM, 'STRUC', STATUS )
      CALL ADI_FIND( SSID, QNAM, QID, STATUS )

*  Write variant
      CALL ADI_CNEWV0C( QID, 'Variant', 'AREA_DESCRIPTION', STATUS )

*  Write ranges
      CALL ADI_CPUT0I( QID, 'GRPID', GRPID, STATUS )

*  Release identifiers
      CALL ADI_ERASE( QID, STATUS )
      CALL ADI_ERASE( SSID, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'SLN_PUTARD', STATUS )

      END
