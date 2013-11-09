      SUBROUTINE BDI_AXPUT1<T>( ID, IAX, ITEMS, DIM, DATA, STATUS )
*+
*  Name:
*     BDI_AXPUT1<T>

*  Purpose:
*     Put the named axis items as <COMM> vectors

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI_AXPUT1<T>( ID, IAX, ITEMS, DIM, DATA, STATUS )

*  Description:
*     Writes the axis items specified by the ITEMS string as <COMM>
*     vector values.

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier of BinDS, Array or Scalar object, or derivatives
*        thereof
*     IAX = INTEGER (given)
*        Axis number of the items to be checked
*     ITEMS = CHARACTER*(*) (given)
*        List of axis items to be read
*     DIM = INTEGER (given)
*        Size of user buffer
*     DATA[] = <TYPE> (given)
*        The data to write
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

*  Arguments Given:
      INTEGER			ID, IAX, DIM
      CHARACTER*(*)		ITEMS

*  Arguments Returned:
      <TYPE>			DATA(*)

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*20		LITEM			! Local item name

      INTEGER                   C1, C2                  ! Character pointers
      INTEGER                   IITEM                   ! Item counter
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop over items while more of them and status is ok
      CALL UDI0_CREITI( ITEMS, C1, C2, IITEM, STATUS )
      DO WHILE ( (C1.NE.0) .AND. (STATUS.EQ.SAI__OK) )

*    Construct the item name
 10     FORMAT( 'Axis_', I1, '_', A )
        WRITE( LITEM, 10 ) IAX, ITEMS(C1:C2)

*    Check the axis item
        CALL BDI_PUT<T>( ID, LITEM, 1, DIM, DATA(IITEM), STATUS )

*    Advance iterator to next item
        CALL UDI0_ADVITI( ITEMS, C1, C2, IITEM, STATUS )

      END DO

      END
