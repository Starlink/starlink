      SUBROUTINE EDI_MAP( ID, LISTS, TYPE, MODE, LINDEX, HINDEX,
     :                    PTRS, STATUS )
*+
*  Name:
*     EDI_MAP

*  Purpose:
*     Map the named lists with the specified type and mode

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL EDI_MAP( ID, LISTS, TYPE, MODE, LINDEX, HINDEX, PTRS, STATUS )

*  Description:
*     Maps the lists specified by the LISTS string with a type and mode
*     specified by TYPE and MODE. The pointers to the resulting areas
*     of memory are returned in PTRS.

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier of BinDS, Array or Scalar object, or derivatives
*        thereof
*     LISTS = CHARACTER*(*) (given)
*        List of lists to be mapped
*     TYPE = CHARACTER*(*) (given)
*        The type with whichthe mapping will be performed
*     MODE = CHARACTER*(*) (given)
*        The access mode for the lists' data
*     LINDEX = INTEGER (given)
*        The first list element to access
*     HINDEX = INTEGER (given)
*        The last list element to access
*     PTRS[] = INTEGER (returned)
*        The pointers to the mapped lists
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
      INTEGER			ID, LINDEX, HINDEX
      CHARACTER*(*)		LISTS, TYPE, MODE

*  Arguments Returned:
      INTEGER			PTRS(*)

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*15		LTYPE			! Local type copy

      INTEGER			ARGS(7)			! Function args
      INTEGER			C1, C2			! Character pointers
      INTEGER			IITEM			! Item counter
      INTEGER			OARG			! Return value
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check correct type
      CALL EDI0_CHKDER( ID, STATSU )

*  First function argument is the identifier
      ARGS(1) = ID

*  Second is the linked file object
      CALL ADI_GETLINK( ID, ARGS(2), STATUS )

*  Fourth is the mapping type
      CALL UDI0_CHKTYP( TYPE, LTYPE, STATUS )
      CALL ADI_NEWV0C( LTYPE, ARGS(4), STATUS )

*  Fifth is the mapping mode
      CALL ADI_NEWV0C( MODE, ARGS(5), STATUS )

*  Sixth is mapping bounds
      CALL ADI_NEWV0I( LINDEX, ARGS(6), STATUS )
      CALL ADI_NEWV0I( HINDEX, ARGS(7), STATUS )

*  Loop over lists while more of them and status is ok
      CALL UDI0_CREITI( LISTS, C1, C2, IITEM, STATUS )
      DO WHILE ( (C1.NE.0) .AND. (STATUS.EQ.SAI__OK) )

*    Construct string for this item
        CALL ADI_NEWV0C( LISTS(C1:C2), ARGS(3), STATUS )

*    Invoke the function
        CALL ADI_FEXEC( 'ListMap', 7, ARGS, OARG, STATUS )

*    Extract pointer from return value
        IF ( (STATUS .EQ. SAI__OK) .AND. (OARG.NE.ADI__NULLID) ) THEN
          CALL ADI_GET0I( OARG, PTRS(IITEM), STATUS )
          CALL ADI_ERASE( OARG, STATUS )
        ELSE IF ( STATUS .NE. SAI__OK ) THEN
          CALL MSG_SETC( 'ITEM', LISTS(C1:C2) )
          CALL ERR_REP( 'EDI_MAP_1', 'Unable to map item ^ITEM',
     :                    STATUS )
        ELSE
          PTRS(IITEM) = 0
        END IF

*    Release the list name string
        CALL ERR_BEGIN( STATUS )
        CALL ADI_ERASE( ARGS(3), STATUS )
        CALL ERR_END( STATUS )

*    Advance iterator to next list
        CALL UDI0_ADVITI( LISTS, C1, C2, IITEM, STATUS )

      END DO

*  Scrub the temporary strings holding type and mode if they've been created
      CALL ERR_BEGIN( STATUS )
      CALL ADI_ERASE( ARGS(4), STATUS )
      CALL ADI_ERASE( ARGS(5), STATUS )
      CALL ADI_ERASE( ARGS(6), STATUS )
      CALL ADI_ERASE( ARGS(7), STATUS )
      CALL ERR_END( STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'EDI_MAP', STATUS )

      END
