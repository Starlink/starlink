      SUBROUTINE BDI2_MAP( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     BDI2_MAP

*  Purpose:
*     Service FileItemMap requests from the BDI system for FITS files

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI2_MAP( NARG, ARGS, OARG, STATUS )

*  Description:
*     Services BDI map requests for FITS files. The BDI system ensures that
*     this routine is not called more than once for a given object. So, all
*     the routine does is translate map requests supplying the name of the
*     abstract model quantity, type and mode into calls to map HDS
*     components. The arguments supplied are,
*
*       ModelObject, FITSfile, Item, Type, Mode
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
*      9 Aug 1995 (DJA):
*        Original version.
*     13 Mar 1996 (DJA):
*        Added number of elements to return value
*      5 Jun 1996 (DJA):
*        FITS version taken directly from BDI1_MAP
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
      INTEGER                   NARG, ARGS(*)

*  Arguments Returned:
      INTEGER                   OARG

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*20		ITEM			! Item to map
      CHARACTER*6		MODE			! Mapping mode
      CHARACTER*7		TYPE			! Mapping type

      INTEGER			CACHEID			! Cache object id
      INTEGER			ITID			! Invented data item
      INTEGER			NDIM, DIMS(ADI__MXDIM)	! Object model dims
      INTEGER			NELM			! # mapped items
      INTEGER			PSID			! Private item storage
      INTEGER			PTR			! Mapped data address
      INTEGER			RVAL(2)			! Returned data
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

*  Locate the data item, creating if in write mode
      CALL BDI2_CFIND( ARGS(1), ARGS(2), ITEM, (MODE(1:1).EQ.'W'),
     :                 .FALSE., CACHEID, NDIM, DIMS, STATUS )

*  Everything ok?
      IF ( (STATUS .EQ. SAI__OK) .AND. (CACHEID.NE.ADI__NULLID) ) THEN

*    Locate the private storage for the item, creating if required
        CALL ADI0_LOCPST( ARGS(1), ITEM, .TRUE., PSID, STATUS )

*    Map the array
        CALL ADI2_DMAP( ARGS(1), ARGS(2), CACHEID, TYPE, MODE, NDIM, DIMS,
     :                  PSID, PTR, NELM, STATUS )

*  Object doesn't exist?
      ELSE

*    Cancel bad status
        IF ( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )

*    Try to invent the object
        CALL BDI2_INVNT( ARGS(1), ARGS(2), ITEM, TYPE, MODE,
     :                   ITID, NDIM, DIMS, WBPTR, STATUS )

*    Successful?
        IF ( STATUS .EQ. SAI__OK ) THEN

*      Store the object as a component of the BinDS object
          CALL BDI0_STOINV( ARGS(1), ITEM, ITID, STATUS )

*      Locate the private storage for the item, creating if required
          CALL ADI0_LOCPST( ARGS(1), ITEM, .TRUE., PSID, STATUS )

*      Map the invented object
          CALL ADI_MAP( ITID, TYPE, MODE, PTR, STATUS )

*      Store mapping details
          CALL ADI2_STOMAP( PSID, ADI__NULLID, 'inv', ITID, PTR,
     :             NDIM, DIMS, 0, 0, WBPTR, TYPE, MODE, STATUS )

        END IF

      END IF

*  Everything went ok?
      IF ( STATUS .EQ. SAI__OK ) THEN

*    Release storage
        CALL ADI_ERASE( PSID, STATUS )

*    If mapping went ok, store the pointer & number of elements in
*    the return argument
        RVAL(1) = PTR
        RVAL(2) = NELM
        CALL ADI_NEWV1I( 2, RVAL, OARG, STATUS )

*    Release the object if defined
        IF ( CACHEID .NE. ADI__NULLID ) THEN
          CALL ADI_ERASE( CACHEID, STATUS )
        END IF

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'BDI2_MAP', STATUS )

      END
