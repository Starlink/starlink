      SUBROUTINE ADI2_ADDCRC( HDUID, FORM, OBJ, NCARD, STATUS )
*+
*  Name:
*     ADI2_ADDCRC

*  Purpose:
*     Allocate a new HDU card number for the specified object

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_ADDCRC( HDUID, FORM, OBJ, NCARD, STATUS )

*  Description:
*     Write value of keyword to specified HDU. Any existing keyword value
*     is overwritten.

*  Arguments:
*     HDUID = INTEGER (given)
*        ADI identifier of HDU object
*     FORM = CHARACTER*(*) (given)
*        The form of the object, [K]eyword, [C]omment or [H]istory
*     OBJ = INTEGER (given)
*        ADI identifier of object holding card info
*     NCARD = INTEGER (returned)
*        Card number of OBJ
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
      INTEGER			HDUID, OBJ
      CHARACTER*(*)		FORM

*  Arguments Returned:
      INTEGER			NCARD

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*4		CINAME			! Card index entry name
      CHARACTER*5		CIVAL			! Card index value

      INTEGER			CIID			! Card index
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get existing card count
      CALL ADI_CGET0I( HDUID, 'Ncard', NCARD, STATUS )

*  Increment and write back
      NCARD = NCARD + 1
      CALL ADI_CPUT0I( HDUID, 'Ncard', NCARD, STATUS )

*  Write property to card object
      CALL ADI_CPUT0I( OBJ, '.Icard', NCARD, STATUS )

*  Locate card index
      CALL ADI_FIND( HDUID, 'CardIndex', CIID, STATUS )

*  Write card index entry
      WRITE( CINAME, '(A1,I3.3)' ) 'I', NCARD
      IF ( FORM(1:1) .EQ. 'K' ) THEN
        CALL ADI_CPUT0C( CIID, CINAME, FORM(2:), STATUS )
      ELSE
        WRITE( CIVAL, '(2A1,I3.3)' ) '_', FORM, NCARD
        CALL ADI_CPUT0C( CIID, CINAME, CIVAL, STATUS )
      END IF

*  Release the card index
      CALL ADI_ERASE( CIID, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI2_ADDCRC', STATUS )

      END
