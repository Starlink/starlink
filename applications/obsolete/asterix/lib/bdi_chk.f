      SUBROUTINE BDI_CHK( ID, ITEMS, OKS, STATUS )
*+
*  Name:
*     BDI_CHK

*  Purpose:
*     Check that the named items exist and have valid data

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI_CHK( ID, ITEMS, OKS, STATUS )

*  Description:
*     Checks the existance and integrity of the named items.

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier of BinDS, Array or Scalar object, or derivatives
*        thereof
*     ITEMS = CHARACTER*(*) (given)
*        List of items to be checked
*     OKS[] = LOGICAL (returned)
*        Are named items ok?
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
      INCLUDE 'AST_PKG'

*  Arguments Given:
      INTEGER			ID
      CHARACTER*(*)		ITEMS

*  Arguments Returned:
      LOGICAL			OKS(*)

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      LOGICAL			AST_QPKGI
        EXTERNAL		AST_QPKGI

*  Local Variables:
      INTEGER			ARGS(3)			! Function args
      INTEGER			C1, C2			! Character pointers
      INTEGER			IITEM			! Item counter
      INTEGER			OARG			! Return value
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check initialised
      IF ( .NOT. AST_QPKGI( BDI__PKG ) ) CALL BDI0_INIT( STATUS )

*  First function argument is the identifier
      ARGS(1) = ID

*  Second is the linked file object
      CALL ADI_GETLINK( ID, ARGS(2), STATUS )

*  Loop over items while more of them and status is ok
      CALL UDI0_CREITI( ITEMS, C1, C2, IITEM, STATUS )
      DO WHILE ( (C1.NE.0) .AND. (STATUS.EQ.SAI__OK) )

*    Import item name
        CALL BDI0_MKISTR( ID, ITEMS(C1:C2), ARGS(3), STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_ANNUL( STATUS )
          OKS(IITEM) = .FALSE.

        ELSE

*      Invoke the function
          CALL ADI_FEXEC( 'FileItemChk', 3, ARGS, OARG, STATUS )

*      Extract flag from return value
          IF ( (STATUS .EQ. SAI__OK) .AND. (OARG.NE.ADI__NULLID) ) THEN
            CALL ADI_GET0L( OARG, OKS(IITEM), STATUS )
            CALL ADI_ERASE( OARG, STATUS )
          ELSE
            OKS(IITEM) = .FALSE.
          END IF

*      Release the item string
          CALL ERR_BEGIN( STATUS )
          CALL ADI_ERASE( ARGS(3), STATUS )
          CALL ERR_END( STATUS )

*      Error?
          IF ( STATUS .NE. SAI__OK ) THEN
            CALL MSG_SETC( 'IT', ITEMS(C1:C2) )
            CALL ERR_REP( ' ', 'Error checking presence of item /^IT/',
     :                    STATUS )
          END IF

*    End of switch on valid item
        END IF

*    Advance iterator to next item
        CALL UDI0_ADVITI( ITEMS, C1, C2, IITEM, STATUS )

      END DO

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'BDI_CHK', STATUS )

      END
