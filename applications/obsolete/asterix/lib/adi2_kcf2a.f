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

*  Arguments Given:
      INTEGER			HDUID, OBJ
      CHARACTER*(*)		KEY, MEMBER

*  Arguments Returned:
      LOGICAL			OK

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER                   KID                     ! Keywords list
      INTEGER                   KVID                    ! Keywords value object
      INTEGER			MVID			! Member value object

      LOGICAL                   SCAND                   ! HDU has been scanned?
      LOGICAL                   THERE                   ! Keyword exists?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise
      OK = .FALSE.

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

*    Locate the value
        CALL ADI_FIND( KID, KEY, KVID, STATUS )

*    Make a copy
        CALL ADI_COPY( KVID, MVID, STATUS )

*    Release keyword value
        CALL ADI_ERASE( KVID, STATUS )

*    Write object component
        CALL ADI_CPUTID( OBJ, MEMBER, MVID, STATUS )

*    Set return flag
        IF ( STATUS .EQ. SAI__OK ) THEN
          OK = .TRUE.
        ELSE
          CALL ERR_ANNUL( STATUS )
        END IF

      END IF

*  Release keyword container
      CALL ERR_BEGIN( STATUS )
      CALL ADI_ERASE( KID, STATUS )
      CALL ERR_END( STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI2_KCF2A', STATUS )

      END
