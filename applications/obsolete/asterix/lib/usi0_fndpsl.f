      SUBROUTINE USI0_FNDPSL( PAR, CREATE, PSID, STATUS )
*+
*  Name:
*     USI0_FNDPSL

*  Purpose:
*     Locate a parameter's private storage area, creating if required

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL USI0_FNDPSL( PAR, CREATE, PSID, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     PAR = CHARACTER*(*) (given)
*        Parameter name
*     CREATE = LOGICAL (given)
*        Create if not present?
*     PSID = INTEGER (returned)
*        ADI identifier to parameter storage area
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
*     USI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/usi.html

*  Keywords:
*     package:usi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     6 Mar 1996 (DJA):
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

*  Global Variables:
      INCLUDE 'USI_CMN'                                 ! USI common block
*       USI_INIT = LOGICAL (given)
*         USI class definitions loaded?

*  Arguments Given:
      CHARACTER*(*)		PAR
      LOGICAL			CREATE

*  Arguments Returned:
      INTEGER			PSID

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			USI0_BLK		! Ensures inclusion

*  Local Variables:
      LOGICAL			THERE			! Object exists?
*.

*  Default return value
      PSID = ADI__NULLID

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Object already exists?
      CALL ADI_THERE( CTX_PST(USI_ICTX), PAR, THERE, STATUS )
      IF ( CREATE .AND. .NOT. THERE ) THEN
        CALL ADI_CNEW0( CTX_PST(USI_ICTX), PAR, 'STRUC', STATUS )
        THERE = (STATUS.EQ.SAI__OK)
      END IF

*  Locate it
      IF ( THERE ) THEN
        CALL ADI_FIND(  CTX_PST(USI_ICTX), PAR, PSID, STATUS )
      ELSE IF ( .NOT. CREATE ) THEN
        STATUS = SAI__ERROR
        CALL MSG_SETC( 'P', PAR )
        CALL ERR_REP( ' ', 'Parameter ^P is not known to USI', STATUS )
      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'USI0_FNDPSL', STATUS )

      END
