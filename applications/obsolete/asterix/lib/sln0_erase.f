      SUBROUTINE SLN0_ERASE( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     SLN0_ERASE

*  Purpose:
*     Destroy a SelectionRecord object

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL SLN0_ERASE( NARG, ARGS, OARG, STATUS )

*  Description:
*     This destructor searches the selection record for any GRP identifiers
*     and deletes them.

*  Arguments:
*     NARG = INTEGER (given)
*        Number of method arguments
*     ARGS(*) = INTEGER (given)
*        ADI identifier of method arguments
*     OARG = INTEGER (returned)
*        Output data
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
*     package:sln, usage:private

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

*  Arguments Given:
      INTEGER                   NARG                    ! # arguments
      INTEGER                   ARGS(*)                 ! Method arguments

*  Arguments Returned:
      INTEGER                   OARG                    ! Returned data

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			GRPID			! GRP identifier
      INTEGER			ICMP			! Loop over components
      INTEGER			NCMP			! # components
      INTEGER			SELID			! Selectors component
      INTEGER			SID			! Selectors structure

      LOGICAL			THERE			! GRP identifier there?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Locate the Selectors structure
      CALL ADI_FIND( ARGS(1), SID, STATUS )

*  Loop over each component
      CALL ADI_NCMP( SID, NCMP, STATUS )
      DO ICMP = 1, NCMP
        CALL ADI_INDCMP( SID, ICMP, SELID, STATUS )
        CALL ADI_THERE( SELID, 'GRPID', THERE, STATUS )
        IF ( THERE ) THEN
          CALL ADI_CGET0I( SELID, 'GRPID', GRPID, STATUS )
          CALL GRP_DELET( GRPID, STATUS )
        END IF
        CALL ADI_ERASE( SELID, STATUS )
      END DO

*  Release Selectors
      CALL ADI_ERASE( SID, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'SLN0_ERASE', STATUS )

      END
