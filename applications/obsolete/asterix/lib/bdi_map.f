      SUBROUTINE BDI_MAP( ID, ITEMS, TYPE, MODE, PTRS, STATUS )
*+
*  Name:
*     BDI_MAP

*  Purpose:
*     Map the named items with the specified type and mode

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI_MAP( ID, ITEMS, TYPE, MODE, PTRS, STATUS )

*  Description:
*     Maps the items specified by the ITEMS string with a type and mode
*     specified by TYPE and MODE. The pointers to the resulting areas
*     of memory are returned in PTRS.

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier of BinDS, Array or Scalar object, or derivatives
*        thereof
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
      INCLUDE 'ADI_PAR'

*  Global Variables:
      INCLUDE 'BDI_CMN'                                 ! BDI common block
*       BDI_INIT = LOGICAL (given)
*         BDI class definitions loaded?

*  Arguments Given:
      INTEGER			ID
      CHARACTER*(*)		ITEMS, TYPE, MODE

*  Arguments Returned:
      INTEGER			PTRS(*)

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			BDI0_BLK		! Ensures inclusion

*  Local Variables:
      CHARACTER*20		LITEM			! Local item name
      CHARACTER*6               LMODE			! Local copy of mode
      CHARACTER*7 		LTYPE			! Local copy of type

      INTEGER			ARGS(5)			! Function args
      INTEGER			C1, C2			! Character pointers
      INTEGER			IITEM			! Item counter
      INTEGER			LITL			! Used length of LITEM
      INTEGER			PSID			! Private item storage
      INTEGER			MCOUNT			! Object map count
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

*  Fourth is the mapping type
      CALL UDI0_CHKTYP( TYPE, LTYPE, STATUS )
      CALL ADI_NEWV0C( LTYPE, ARGS(4), STATUS )

*  Fifth is the mapping mode
      LMODE = MODE
      CALL CHR_UCASE( LMODE )
      CALL ADI_NEWV0C( LMODE, ARGS(5), STATUS )

*  Loop over items while more of them and status is ok
      CALL UDI0_CREITI( ITEMS, C1, C2, IITEM, STATUS )
      DO WHILE ( (C1.NE.0) .AND. (STATUS.EQ.SAI__OK) )

*    Check item name is valid, and make a local copy. Removes any
*    special item names such as E_Axis_Label.
        CALL BDI0_CHKITM( ID, ITEMS(C1:C2), LITEM, LITL, STATUS )

*    Check that item can be mapped
        CALL BDI0_CHKOP( LITEM(:LITL), 'Map', STATUS )

*    Locate private storage for this item
        CALL BDI0_LOCPST( ID, LITEM(:LITL), .TRUE., PSID, STATUS )

*    Get map count
        CALL ADI_CGET0I( PSID, 'MapCount', MCOUNT, STATUS )

*    Already mapped?
        IF ( MCOUNT .GT. 0 ) THEN

*      Extract pointer. Should check that mode and type match...
          CALL ADI_CGET0I( PSID, 'Ptr', PTRS(IITEM), STATUS )

        ELSE

*      Construct string for this item
          CALL ADI_NEWV0C( LITEM(:LITL), ARGS(3), STATUS )

*      Invoke the function
          CALL ADI_FEXEC( 'FileItemMap', 5, ARGS, OARG, STATUS )

*      Extract pointer from return value
          IF ( (STATUS .EQ. SAI__OK) .AND. (OARG.NE.ADI__NULLID) ) THEN
            CALL ADI_GET0I( OARG, PTRS(IITEM), STATUS )
            CALL ADI_ERASE( OARG, STATUS )
          ELSE IF ( STATUS .NE. SAI__OK ) THEN
            CALL MSG_SETC( 'ITEM', LITEM(:LITL) )
            CALL ERR_REP( 'BDI_MAP_1', 'Unable to map item ^ITEM',
     :                    STATUS )
          ELSE
            PTRS(IITEM) = 0
          END IF

        END IF

*    Adjust map count
        CALL BDI0_ADJMCT( PSID, 1, STATUS )

*    Release private storage
        CALL ADI_ERASE( PSID, STATUS )

*    Release the item string
        CALL ERR_BEGIN( STATUS )
        CALL ADI_ERASE( ARGS(3), STATUS )
        CALL ERR_END( STATUS )

*    Advance iterator to next item
        CALL UDI0_ADVITI( ITEMS, C1, C2, IITEM, STATUS )

      END DO

*  Scrub the temporary strings holding type and mode if they've been created
      CALL ERR_BEGIN( STATUS )
      CALL ADI_ERASE( ARGS(4), STATUS )
      CALL ADI_ERASE( ARGS(5), STATUS )
      CALL ERR_END( STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'BDI_MAP', STATUS )

      END
