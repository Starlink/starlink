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
      INTEGER                   NARG, ARGS(*)

*  Arguments Returned:
      INTEGER                   OARG

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	CLOC			! New component

      CHARACTER*20		ITEM			! Item to map
      CHARACTER*6		MODE			! Mapping mode
      CHARACTER*7		TYPE			! Mapping type

      INTEGER			ITID			! Invented data item
      INTEGER			NELM			! # mapped items
      INTEGER			PSID			! Private item storage
      INTEGER			PTR			! Mapped data address
      INTEGER			WBPTR			! WriteBack procedure
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
      CALL BDI1_CFIND( ARGS(1), ARGS(2), ITEM, (MODE(1:1).EQ.'W'),
     :                 CLOC, STATUS )

*  Everything ok?
      IF ( (STATUS .EQ. SAI__OK) .AND. (CLOC.NE.DAT__NOLOC) ) THEN

*    Locate the BDI private storage for the item, creating if required
        CALL BDI0_LOCPST( ARGS(1), ITEM, .TRUE., PSID, STATUS )

*    Map the array
        CALL BDI1_ARYMAP( CLOC, TYPE, MODE, .FALSE., PSID, PTR,
     :                    NELM, STATUS )

*  Object doesn't exist?
      ELSE

*    Cancel bad status
        IF ( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )

*    Try to invent the object
        CALL BDI1_INVNT( ARGS(1), ARGS(2), ITEM, TYPE, MODE,
     :                   ITID, NELM, WBPTR, STATUS )

*    Successful?
        IF ( STATUS .EQ. SAI__OK ) THEN

*      Store the object as a component of the BinDS object
          CALL BDI0_STOINV( ARGS(1), ITEM, ITID, STATUS )

*      Locate the BDI private storage for the item, creating if required
          CALL BDI0_LOCPST( ARGS(1), ITEM, .TRUE., PSID, STATUS )

*      Map the invented object
          CALL ADI_MAP( ITID, TYPE, MODE, PTR, STATUS )

*      Store mapping details
          CALL BDI1_STOMAP( PSID, 'inv', DAT__NOLOC, ITID, PTR,
     :                      NELM, WBPTR, TYPE, MODE, STATUS )

        END IF

      END IF

*  Everything went ok?
      IF ( STATUS .EQ. SAI__OK ) THEN

*    Release storage
        CALL ADI_ERASE( PSID, STATUS )

*    If mapping went ok, store the pointer in the return argument
        CALL ADI_NEWV0I( PTR, OARG, STATUS )

*    Release the object if defined
        IF ( CLOC .NE. DAT__NOLOC ) THEN
          CALL DAT_ANNUL( CLOC, STATUS )
        END IF

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'BDI1_MAP', STATUS )

      END
