      SUBROUTINE BDI_UNMAP( ID, ITEMS, PTRS, STATUS )
*+
*  Name:
*     BDI_UNMAP

*  Purpose:
*     Unmap the named items

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI_UNMAP( ID, ITEMS, PTRS, STATUS )

*  Description:
*     Unmaps the items specified by the ITEMS string. The value of the
*     pointer is required for checking purposes and in the case of
*     memory based objects where mapping for multiple types can be in
*     force.

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier of BinDS, Array or Scalar object, or derivatives
*        thereof
*     ITEMS = CHARACTER*(*) (given)
*        List of items to be unmapped
*     PTRS[] = INTEGER (given)
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
      INCLUDE 'AST_PKG'

*  Arguments Given:
      INTEGER			ID, PTRS(*)
      CHARACTER*(*)		ITEMS

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      LOGICAL			AST_QPKGI
        EXTERNAL		AST_QPKGI

*  Local Variables:
      CHARACTER*20              LITEM                   ! Local item name

      INTEGER			ARGS(4)			! Function args
      INTEGER			C1, C2			! Character pointers
      INTEGER			IITEM			! Item counter
      INTEGER                   LITL                    ! Used length of LITEM
      INTEGER			MCOUNT			! Mapping count
      INTEGER			OARG			! Return value
      INTEGER                   PSID                    ! Private item storage
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

*    Check item name is valid, and make a local copy. Removes any
*    special item names such as E_Axis_Label.
        CALL BDI0_CHKITM( ID, ITEMS(C1:C2), LITEM, LITL, STATUS )

*    Locate private item store
        CALL BDI0_LOCPST( ID, LITEM(:LITL), .FALSE., PSID, STATUS )

*    If null then the item is not mapped
        IF ( PSID .EQ. ADI__NULLID ) THEN
          STATUS = SAI__ERROR
          CALL MSG_SETC( 'IT', LITEM(:LITL) )
          CALL ERR_REP( 'BDI_UNMAP_1', 'Item ^IT is not mapped by BDI',
     :                  STATUS )
        ELSE

*      Get map count
          CALL ADI_CGET0I( PSID, 'MapCount', MCOUNT, STATUS )

*      If one, physically unmap
          IF ( MCOUNT .EQ. 1 ) THEN

*        Construct string for this item
            CALL ADI_NEWV0C( ITEMS(C1:C2), ARGS(3), STATUS )

*        Store pointer
            CALL ADI_NEWV0I( PTRS(IITEM), ARGS(4), STATUS )

*        Invoke the function
            CALL ADI_FEXEC( 'FileItemUnmap', 4, ARGS, OARG, STATUS )

*        Release the item string and pointer value
            CALL ERR_BEGIN( STATUS )
            CALL ADI_ERASE( ARGS(3), STATUS )
            CALL ADI_ERASE( ARGS(4), STATUS )
            CALL ERR_END( STATUS )

          END IF

*      Decrement map count
          CALL BDI0_ADJMCT( PSID, -1, STATUS )

*      Release private storage
          CALL ADI_ERASE( PSID, STATUS )

        END IF

*    Advance iterator to next item
        CALL UDI0_ADVITI( ITEMS, C1, C2, IITEM, STATUS )

      END DO

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'BDI_UNMAP', STATUS )

      END
