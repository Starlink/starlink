      SUBROUTINE BDI_PUT( ID, ITEMS, TYPE, NDIM, DIMS, DATA, STATUS )
*+
*  Name:
*     BDI_PUT

*  Purpose:
*     Put the named items with the specified type and dimensions

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI_PUT( ID, ITEMS, TYPE, NDIM, DIMS, DATA, STATUS )

*  Description:
*     Writes the items specified by the ITEMS string with the specified
*     TYPE. Should only be used for numeric items.

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier of BinDS, Array or Scalar object, or derivatives
*        thereof
*     ITEMS = CHARACTER*(*) (given)
*        List of items to be mapped
*     TYPE = CHARACTER*(*) (given)
*        The type with which access mapping will be performed
*     NDIM = INTEGER (given)
*        The dimensionality of the users data buffer
*     DIMS[NDIM] = INTEGER (given)
*        The dimensions of the users buffer
*     DATA[] = varies (given)
*        The returned data
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
      INTEGER			ID, NDIM, DIMS(*)
      CHARACTER*(*)		ITEMS, TYPE
      BYTE			DATA(*)

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      LOGICAL			AST_QPKGI
        EXTERNAL		AST_QPKGI

*  Local Variables:
      INTEGER			C1, C2			! ITEMS indices
      INTEGER			CURELM			! Current o/p element
      INTEGER			DOBJ			! Temp data object
      INTEGER			ESIZE			! Element size
      INTEGER			FC			! 1st TYPE character
      INTEGER			IITEM			! Item counter
      INTEGER			LID			! Linked object id
      INTEGER			NELM			! Max elements per item
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check initialised
      IF ( .NOT. AST_QPKGI( BDI__PKG ) ) CALL BDI0_INIT( STATUS )

*  Second is the linked file object
      CALL ADI_GETLINK( ID, LID, STATUS )

*  Current element in output buffer
      CURELM = 1

*  Element size in bytes
      FC = 1
      IF ( TYPE(1:1) .EQ. '_' ) FC = 2
      CALL UDI0_TYPSIZ( TYPE(FC:), ESIZE, STATUS )

*  Number of output data elements user has supplied per item
      CALL ARR_SUMDIM( NDIM, DIMS, NELM )

*  Loop over items while more of them and status is ok
      CALL UDI0_CREITI( ITEMS, C1, C2, IITEM, STATUS )
      DO WHILE ( (C1.NE.0) .AND. (STATUS.EQ.SAI__OK) )

*    Construct ADI object holding value
        CALL ADI_NEW( TYPE(FC:), NDIM, DIMS, DOBJ, STATUS )
        CALL ADI_PUT( DOBJ, TYPE(FC:), NDIM, DIMS, DATA, STATUS )

*    Write the data
        CALL BDI_PUT1( ID, LID, ITEMS(C1:C2), DOBJ, STATUS )

*    Release our temporary data object
        CALL ERR_BEGIN( STATUS )
        CALL ADI_ERASE( DOBJ, STATUS )
        CALL ERR_END( STATUS )

*    Advance pointer into output buffer
        CURELM = CURELM + NELM * ESIZE

*    Advance iterator to next item
        CALL UDI0_ADVITI( ITEMS, C1, C2, IITEM, STATUS )

      END DO

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'BDI_PUT', STATUS )

      END



      SUBROUTINE BDI_PUT1( ID, LID, ITEM, DATA, STATUS )
*+
*  Name:
*     BDI_PUT1

*  Purpose:
*     Put the ADI data to the named item in the file object

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI_PUT1( ID, LID, ITEM, DATA, STATUS )

*  Description:
*     Writes the items specified by the ITEMS string. Supports the following
*     high level abstractions in addition to simple items.
*
*       Axis_<n>_SpacedData  - Converts to expanded array
*       Axis_<n>_ScalarWidth - Ditto

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier of BinDS, Array or Scalar object, or derivatives
*        thereof
*     LID = INTEGER (given)
*        The object linked to ID
*     ITEM = CHARACTER*(*) (given)
*        Item to be written
*     DATA = INTEGER (given)
*        ADI object containing data to be written
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

*  Arguments Given:
      INTEGER			ID, LID, DATA
      CHARACTER*(*)		ITEM

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*20		LITEM			! Local item name
      CHARACTER*15              LTYPE                   ! Local type

      DOUBLE PRECISION		SPARR(2)		! Spaced array info

      INTEGER			ARGS(4)			! Function args
      INTEGER			DIMS(ADI__MXDIM)	! Dataset dimensions
      INTEGER			IAX			! Axis number
      INTEGER			LDATA			! Local data copy
      INTEGER			LITL			! Used length of LITEM
      INTEGER			NDIM			! Dimensionality
      INTEGER			OARG			! Return value
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check item name is valid, and make a local copy. Removes any
*  special item names such as E_Axis_Label.
      CALL BDI0_CHKITM( ID, ITEM, LITEM, LITL, STATUS )

*  Check specials
      IF ( (LITEM(1:5) .EQ. 'Axis_') .AND.
     :          (LITEM(8:17).EQ.'SpacedData') ) THEN

*    Decode axis number
        CALL CHR_CTOI( LITEM(6:6), IAX, STATUS )

*    Get shape of data
        CALL BDI_GETSHP( ID, ADI__MXDIM, DIMS, NDIM, STATUS )

*    Create new object in same type as supplied spaced data
        CALL ADI_TYPE( DATA, LTYPE, STATUS )
        CALL ADI_NEW1( LTYPE, DIMS(IAX), LDATA, STATUS )

*    Extract spaced parameters
        CALL ADI_GET1D( DATA, 2, SPARR, NDAT, STATUS )

*    Map temporary object
        CALL ADI_MAPD( LDATA, 'WRITE', DPTR, STATUS )
        CALL ARR_REG1D( SPARR(1), SPARR(2), DIMS(IAX), %VAL(DPTR),
     :                  STATUS )
        CALL ADI_UNMAP( LDATA, DPTR, STATUS )

*    Change local item
        LITEM = LITEM(:7)//'Data'

      ELSE
        CALL ADI_CLONE( DATA, LDATA, STATUS )

      END IF

*  First function argument is the identifier
      ARGS(1) = ID
      ARGS(2) = LID
      ARGS(4) = LDATA

*  Check that item can be written
      CALL BDI0_CHKOP( LITEM(:LITL), 'Put', STATUS )

*  Construct string for this item
      CALL ADI_NEWV0C( LITEM(:LITL), ARGS(3), STATUS )

*  Invoke the function
      CALL ADI_FEXEC( 'FileItemPut', 4, ARGS, OARG, STATUS )

*  Extract data from return value
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL MSG_SETC( 'ITEM', LITEM(:LITL) )
        CALL ERR_REP( 'BDI_PUT1_1', 'Unable to put item ^ITEM',
     :                  STATUS )
      END IF

*  Release the item string and our temporary data objects
      CALL ERR_BEGIN( STATUS )
      CALL ADI_ERASE( ARGS(3), STATUS )
      CALL ADI_ERASE( LDATA, STATUS )
      CALL ERR_END( STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'BDI_PUT1', STATUS )

      END
