      SUBROUTINE BDI_DELETE( ID, ITEMS, STATUS )
*+
*  Name:
*     BDI_DELETE

*  Purpose:
*     Delete the named items from a the specified object

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI_DELETE( ID, ITEMS, STATUS )

*  Description:
*     Deletes the named items if present

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier of BinDS, Array or Scalar object, or derivatives
*        thereof
*     ITEMS = CHARACTER*(*) (given)
*        List of items to be deleted
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
*     BDI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/bdi.html

*  Keywords:
*     package:bdi, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     9 Aug 1995 (DJA):
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
      INCLUDE 'BDI_CMN'                                 ! BDI common block
*       BDI_INIT = LOGICAL (given)
*         BDI class definitions loaded?

*  Arguments Given:
      INTEGER			ID
      CHARACTER*(*)		ITEMS

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			BDI0_BLK		! Ensures inclusion

*  Local Variables:
      INTEGER			ARGS(3)			! Function args
      INTEGER			C1, C2			! Character pointers
      INTEGER			IITEM			! Item counter
      INTEGER			OARG			! Return value
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check initialised
      IF ( .NOT. BDI_INIT ) CALL BDI0_INIT( STATUS )

*  First function argument is the identifier
      ARGS(1) = ID

*  Second is the linked file object
      CALL ADI_GETLINK( ID, ARGS(2), STATUS )

*  Loop over items while more of them and status is ok
      CALL UDI0_CREITI( ITEMS, C1, C2, IITEM, STATUS )
      DO WHILE ( (C1.NE.0) .AND. (STATUS.EQ.SAI__OK) )

*    Import the item name
        CALL BDI0_MKISTR( ID, ITEMS(C1:C2), ARGS(3), STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_ANNUL( STATUS )

        ELSE

*      Invoke the function
          CALL ADI_FEXEC( 'FileItemDel', 3, ARGS, OARG, STATUS )

*      Release the item string
          CALL ADI_ERASE( ARGS(3), STATUS )

*      Error?
          IF ( STATUS .NE. SAI__OK ) THEN
            CALL MSG_SETC( 'IT', ITEMS(C1:C2) )
            CALL ERR_REP( ' ', 'Error deleting item /^IT/',
     :                    STATUS )
          END IF

*    End of switch on valid item
        END IF

*    Advance iterator to next item
        CALL UDI0_ADVITI( ITEMS, C1, C2, IITEM, STATUS )

      END DO

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'BDI_DELETE', STATUS )

      END
