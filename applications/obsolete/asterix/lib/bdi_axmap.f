      SUBROUTINE BDI_AXMAP( ID, IAX, ITEMS, TYPE, MODE, PTRS, STATUS )
*+
*  Name:
*     BDI_AXMAP

*  Purpose:
*     Map the named axis items with the specified type and mode

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI_AXMAP( ID, IAX, ITEMS, TYPE, MODE, PTRS, STATUS )

*  Description:
*     Maps the axis items specified by the ITEMS string with a type and mode
*     specified by TYPE and MODE. The pointers to the resulting areas
*     of memory are returned in PTRS.

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier of BinDS, Array or Scalar object, or derivatives
*        thereof
*     IAX = INTEGER (given)
*        Axis number of the items to be mapped
*     ITEMS = CHARACTER*(*) (given)
*        List of items to be mapped
*     TYPE = CHARACTER*(*) (given)
*        The type with whichthe mapping will be performed
*     MODE = CHARACTER*(*) (given)
*        The access mode for the items
*     PTRS[] = INTEGER (returned)
*        The pointers to the mapped items
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
      INTEGER			ID, IAX
      CHARACTER*(*)		ITEMS, TYPE, MODE

*  Arguments Returned:
      INTEGER			PTRS(*)

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*2               ASTR                    ! Axis number string

      INTEGER                   C1, C2                  ! Character pointers
      INTEGER                   IITEM                   ! Item counter
      INTEGER                   NDIG                    ! # digits used in ASTR
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Construct axis number string
      CALL CHR_ITOC( IAX, ASTR, NDIG )

*  Loop over items while more of them and status is ok
      CALL UDI0_CREITI( ITEMS, C1, C2, IITEM, STATUS )
      DO WHILE ( (C1.NE.0) .AND. (STATUS.EQ.SAI__OK) )

*    Map the axis item
        CALL BDI_MAP( ID, 'Axis_'//ASTR(:NDIG)//'_'//ITEMS(C1:C2),
     :                TYPE, MODE, PTRS(IITEM), STATUS )

*    Advance iterator to next item
        CALL UDI0_ADVITI( ITEMS, C1, C2, IITEM, STATUS )

      END DO

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'BDI_AXMAP', STATUS )

      END
