      SUBROUTINE BDI1_MAP( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     BDI1_MAP

*  Purpose:
*     Service FileItemMap requests from the BDI system for HDS files

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI1_MAP( NARG, ARGS, OARG, STATUS )

*  Description:
*     Services BDI map requests for HDS files. The BDI system ensures that
*     this routine is not called more than once for a given object. So, all
*     the routine does is translate map requests supplying the name of the
*     abstract model quantity, type and mode into calls to map HDS
*     components. The arguments supplied are,
*
*       ModelObject, HDSfile, Item, Type, Mode
*
*     Mode can be read, write or update. For read and update the object
*     must exist, and for read the data must be valid. In write mode the
*     item need not exist as all valid item dimensions and types can be
*     defaulted using information in the ModelObject.

*  Arguments:
*     NARG = INTEGER (given)
*        Number of method arguments
*     ARGS(*) = INTEGER (given)
*        ADI identifier of method arguments
*     OARG = INTEGER (returned)
*        Output data
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
*     package:bdi, usage:private

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
      INCLUDE 'DAT_PAR'

*  Arguments Given:
      INTEGER                   NARG                    ! # arguments
      INTEGER                   ARGS(*)                 ! Method arguments

*  Arguments Returned:
      INTEGER                   OARG                    ! Returned data

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
C      [external_declaration]
C      {data_type} {external_name} ! [external_description]

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	CLOC			! New component

      CHARACTER*20		ITEM
      CHARACTER*6		MODE
      CHARACTER*7		TYPE

      INTEGER			FPTR			! Mapped file data
      INTEGER			NELM			! # mapped items
      INTEGER			PSID			! Private item storage
      INTEGER			PTR			! Mapped data address

      LOGICAL			ISDYN			! Ptr is DYN object?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Default return value
      OARG = ADI__NULLID

*  Extract the arguments
      CALL ADI_GET0C( ARGS(3), ITEM, STATUS )
      CALL ADI_GET0C( ARGS(4), TYPE, STATUS )
      CALL ADI_GET0C( ARGS(5), MODE, STATUS )

*  Ensure objects satisfy mapping requirement
      CALL BDI1_CFIND( ARGS(1), ARGS(2), ITEM, (MODE.EQ.'WRITE'), CLOC,
     :                 STATUS )

*  Everything ok?
      IF ( (STATUS .EQ. SAI__OK) .AND. (CLOC.NE.DAT__NOLOC) ) THEN

*    Locate the BDI private storage for the item, creating if required
        CALL BDI0_LOCPST( ARGS(1), ITEM, .TRUE., PSID, STATUS )

*    Write mode?
        IF ( MODE .EQ. 'WRITE' ) THEN

          CALL BDI1_ARYMAP( CLOC, TYPE, MODE, .FALSE., FPTR, PTR,
     :                      ISDYN, NELM, STATUS )

        ELSE

          CALL BDI1_ARYMAP( CLOC, TYPE, MODE, .FALSE., FPTR, PTR,
     :                      ISDYN, NELM, STATUS )

        END IF

*    Store the pointer and the locator in the private storage
        CALL BDI1_STOMAP( PSID, ISDYN, CLOC, FPTR, PTR, TYPE, MODE,
     :                    STATUS )

*    Release storage
        CALL ADI_ERASE( PSID, STATUS )

*    If mapping went ok, store the pointer in the return argument
        CALL ADI_NEWV0I( PTR, OARG, STATUS )

*  Objects doesn't exist?
      ELSE IF ( STATUS .EQ. SAI__OK ) THEN

*     Report error
        STATUS = SAI__ERROR
        CALL MSG_SETC( 'IT', ITEM )
        CALL ERR_REP( 'BDI1_MAP_1', 'Item ^IT does not exist', STATUS )

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'BDI1_MAP', STATUS )

      END
