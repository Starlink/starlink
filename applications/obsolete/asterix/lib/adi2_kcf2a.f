      SUBROUTINE ADI2_KCF2A( HDUID, KEY, OBJ, MEMBER, OK, STATUS )
*+
*  Name:
*     ADI2_KCF2A

*  Purpose:
*     Copy a FITS keyword value to an ADI object component

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_KCF2A( HDUID, KEY, OBJ, MEMBER, OK, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     HDUID = INTEGER (given)
*        ADI identifier of FITShdu object
*     KEY = CHARACTER*(*) (given)
*        Name of keyword whose value is to be copied
*     OBJ = INTEGER (given)
*        ADI identifier of structure or class instance whose component is
*        to be written
*     MEMBER = CHARACTER*(*) (given)
*        Name of member to write if keyword data is defined
*     OK = LOGICAL (returned)
*        True if keyword data copied successfully
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
*     17 Oct 1995 (DJA):
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
      INTEGER			HDUID, OBJ
      CHARACTER*(*)		KEY, MEMBER

*  Arguments Returned:
      LOGICAL			OK

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER                   KID                     ! Keywords cache object
      INTEGER                   KVID                    ! Keywords value object
      INTEGER			MVID			! Member value object
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise
      OK = .FALSE.

*  Look for keyword
      CALL ADI2_CFIND_KEY( HDUID, KEY, .FALSE., KID, .FALSE., STATUS )

*  Does our keyword exist?
      IF ( KVID .NE. ADI__NULLID ) THEN

*    Locate the value
        CALL ADI_FIND( KID, 'Value', KVID, STATUS )

*    Make a copy
        CALL ADI_COPY( KVID, MVID, STATUS )

*    Release keyword value
        CALL ADI_ERASE( KID, STATUS )

*    Write object component
        IF ( MEMBER .GT. ' ' ) THEN
          CALL ADI_CPUTID( OBJ, MEMBER, MVID, STATUS )
        ELSE
          OBJ = MVID
        END IF

*    Set return flag
        IF ( STATUS .EQ. SAI__OK ) THEN
          OK = .TRUE.
        ELSE
          CALL ERR_ANNUL( STATUS )
        END IF

*    Release keyword cache object
        CALL ADI_ERASE( KID, STATUS )

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI2_KCF2A', STATUS )

      END
