      SUBROUTINE EDI_QUNMAP( ID, LISTS, STATUS )
*+
*  Name:
*     EDI_QUNMAP

*  Purpose:
*     Unmap the named lists vector quanta

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL EDI_QUNMAP( ID, LISTS, STATUS )

*  Description:
*     Unmaps the lists specified by the LISTS string

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier of BinDS, Array or Scalar object, or derivatives
*        thereof
*     LISTS = CHARACTER*(*) (given)
*        List of lists to have their vector quanta mapped
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
*     EDI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/edi.html

*  Keywords:
*     package:edi, usage:public

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

*  Arguments Given:
      INTEGER			ID
      CHARACTER*(*)		LISTS

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			ARGS(3)			! Function args
      INTEGER			C1, C2			! Character pointers
      INTEGER			IITEM			! Iteration counter
      INTEGER			OARG			! Return value
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check correct type
      CALL EDI0_CHKDER( ID, STATUS )

*  First function argument is the identifier
      ARGS(1) = ID

*  Second is the linked file object
      CALL ADI_GETLINK( ID, ARGS(2), STATUS )

*  Loop over lists while more of them and status is ok
      CALL UDI0_CREITI( LISTS, C1, C2, IITEM, STATUS )
      DO WHILE ( (C1.NE.0) .AND. (STATUS.EQ.SAI__OK) )

*    Construct string for this item
        CALL ADI_NEWV0C( LISTS(C1:C2), ARGS(3), STATUS )

*    Invoke the function
        CALL ADI_FEXEC( 'ListUnmapQuantum', 3, ARGS, OARG, STATUS )

*    Release the list name string
        CALL ERR_BEGIN( STATUS )
        CALL ADI_ERASE( ARGS(3), STATUS )
        CALL ERR_END( STATUS )

*    Advance iterator to next list
        CALL UDI0_ADVITI( LISTS, C1, C2, IITEM, STATUS )

      END DO

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'EDI_QUNMAP', STATUS )

      END
