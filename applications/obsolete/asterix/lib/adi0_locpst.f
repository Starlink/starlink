      SUBROUTINE ADI0_LOCPST( ID, ITEM, CREATE, PSID, STATUS )
*+
*  Name:
*     ADI0_LOCPST

*  Purpose:
*     Locate private storage structure for a named item, creating if needed

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI0_LOCPST( ID, ITEM, CREATE, PSID, STATUS )

*  Description:
*     Private storage is used by high level ADI packages to store things
*     like mapping details and writeback functions on particular components
*     of the abstract data model. The information is indexed by the name
*     of the abstract data item.

*  Arguments:
*     ID = INTEGER (given)
*        The ADI identifier of the object being managed by ADI
*     ITEM = CHARACTER*(*) (given)
*        The item being accessed
*     CREATE = LOGICAL (given)
*        Create private store if not present
*     PSID = INTEGER (returned)
*        The ADI identifier of the private storage area
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
*     30 Aug 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ADI_PAR'

*  Arguments Given:
      INTEGER			ID
      CHARACTER*(*)		ITEM
      LOGICAL			CREATE

*  Arguments Returned:
      INTEGER			PSID

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Constants:
      CHARACTER*4		ADICMP
        PARAMETER 		( ADICMP = '.BDI' )

*  Local Variables:
      INTEGER			ADIID			! ADICMP identifier

      LOGICAL			CREATED			! Created container?
      LOGICAL			THERE			! Object exists?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise
      PSID = ADI__NULLID
      CREATED = .FALSE.

*  Locate top-level ADI container, creating if required
      CALL ADI_THERE( ID, ADICMP, THERE, STATUS )
      IF ( CREATE .AND. .NOT. THERE ) THEN
        CALL ADI_CNEW0( ID, ADICMP, 'STRUC', STATUS )
      ELSE IF ( .NOT. THERE ) THEN
        GOTO 99
      END IF
      CALL ADI_FIND( ID, ADICMP, ADIID, STATUS )

*  Now locate item container, creating if necessary
      CALL ADI_THERE( ADIID, ITEM, THERE, STATUS )
      IF ( CREATE .AND. .NOT. THERE ) THEN
        CALL ADI_CNEW0( ADIID, ITEM, 'STRUC', STATUS )
        CREATED = .TRUE.
      ELSE IF ( .NOT. THERE ) THEN
        GOTO 89
      END IF
      CALL ADI_FIND( ADIID, ITEM, PSID, STATUS )

*  If we created the container, initialise it
      IF ( CREATED ) THEN
        CALL ADI_CPUT0I( PSID, 'MapCount', 0, STATUS )
      END IF

*  Continue after failure to find item storage
 89   CONTINUE

*  Release ADI container
      CALL ADI_ERASE( ADIID, STATUS )

*  Continue after failure to find ADICMP
 99   CONTINUE

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'ADI0_LOCPST', STATUS )
      END IF

      END
