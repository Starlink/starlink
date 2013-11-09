      SUBROUTINE ADI2_GETCIE( HDUID, CIID, ICARD, FORM, OBJID, STATUS )
*+
*  Name:
*     ADI2_GETCIE

*  Purpose:
*     Get a card index form and the object it points to

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_GETCIE( HDUID, CIID, ICARD, FORM, OBJID, STATUS )

*  Description:
*     Write value of keyword to specified HDU. Any existing keyword value
*     is overwritten.

*  Arguments:
*     HDUID = INTEGER (given)
*        ADI identifier of HDU object
*     CIID = INTEGER (given)
*        The CardIndex structure for this HDU
*     ICARD = INTEGER (given)
*        The card number we're interested in
*     FORM = CHARACTER*(*) (returned)
*        The form of the object, [K]eyword, [C]omment or [H]istory
*     OBJID = INTEGER (returned)
*        ADI identifier of object holding card info
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
      INTEGER			HDUID, CIID, ICARD

*  Arguments Returned:
      CHARACTER*(*)		FORM
      INTEGER			OBJID

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*4		CINAME			! Card index entry name
      CHARACTER*8		CIVAL			! Card index value

      INTEGER			CID			! Card container
      INTEGER			IDX			! Card index number
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Write card index entry name
      WRITE( CINAME, '(A1,I3.3)' ) 'I', ICARD
      CALL ADI_CGET0C( CIID, CINAME, CIVAL, STATUS )

*  Comment or history
      IF ( (CIVAL(1:2) .EQ. '_C') .OR. (CIVAL(1:2) .EQ. '_H') ) THEN
        READ( CIVAL(3:), '(I3.3)' ) IDX

        FORM = CIVAL(2:2)
        IF ( FORM .EQ. 'C' ) THEN
          CALL ADI_FIND( HDUID, 'Comments', CID, STATUS )
          CALL ADI_FIND( CID, CIVAL(2:), OBJID, STATUS )
          CALL ADI_ERASE( CID, STATUS )
        ELSE
          CALL ADI_FIND( HDUID, 'History', CID, STATUS )
          CALL ADI_FIND( CID, CIVAL(2:), OBJID, STATUS )
          CALL ADI_ERASE( CID, STATUS )
        END IF

*  Otherwise keyword
      ELSE

*    Locate key container and object within it
        CALL ADI_FIND( HDUID, 'Keys', CID, STATUS )
        CALL ADI_FIND( CID, CIVAL, OBJID, STATUS )
        CALL ADI_ERASE( CID, STATUS )
        FORM = 'K'

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI2_GETCIE', STATUS )

      END
