      SUBROUTINE BDI_GET( ID, ITEMS, TYPE, NDIM, DIMX, DATA,
     :                    DIMS, STATUS )
*+
*  Name:
*     BDI_GET

*  Purpose:
*     Get the named items with the specified type and dimensions

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI_GET( ID, ITEMS, TYPE, NDIM, DIMX, DATA, DIMS, STATUS )

*  Description:
*     Retrieves the items specified by the ITEMS string with the specified
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
*     DIMX[NDIM] = INTEGER (given)
*        The dimensions of the users buffer
*     DATA[] = varies (returned)
*        The returned data
*     DIMS[] = INTEGER (returned)
*        The actual sizes of the data written the users buffer
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
      INTEGER			ID, NDIM, DIMX(*)
      CHARACTER*(*)		ITEMS, TYPE

*  Arguments Returned:
      BYTE			DATA(*)
      INTEGER			DIMS(*)

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      LOGICAL			AST_QPKGI
        EXTERNAL		AST_QPKGI

*  Local Variables:
      INTEGER			C1, C2			! Character pointers
      INTEGER			CURELM			! Current o/p element
      INTEGER			ESIZE			! Element size
      INTEGER			FC			! 1st non _ in TYPE
      INTEGER			IITEM			! Item counter
      INTEGER			LID			! Linked data object
      INTEGER			NELM			! Max elements per item
      INTEGER			OARG			! Return value
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check initialised
      IF ( .NOT. AST_QPKGI( BDI__PKG ) ) CALL BDI0_INIT( STATUS )

*  Protect error environment
      CALL ERR_BEGIN( STATUS )

*  Second is the linked file object
      CALL ADI_GETLINK( ID, LID, STATUS )

*  Current element in output buffer
      CURELM = 1

*  Element size in bytes
      CALL UDI0_TYPSIZ( TYPE, ESIZE, STATUS )

*  Number of output data elements user has supplied per item
      CALL ARR_SUMDIM( NDIM, DIMX, NELM )

*  First significant character of TYPE
      FC = 1
      IF ( TYPE(1:1) .EQ. '_' ) FC = 2

*  Loop over items while more of them and status is ok
      CALL UDI0_CREITI( ITEMS, C1, C2, IITEM, STATUS )
      DO WHILE ( (C1.NE.0) .AND. (STATUS.EQ.SAI__OK) )

*    Get an ADI object holding the object data using mode 0 (invent if
*    not present)
        CALL BDI_GET1( ID, LID, ITEMS(C1:C2), 0, OARG, STATUS )

*    Extract data from return value
        IF ( (STATUS .EQ. SAI__OK) .AND. (OARG.NE.ADI__NULLID) ) THEN

          CALL ADI_GET( OARG, TYPE(FC:), NDIM, DIMX, DATA(CURELM),
     :                    DIMS, STATUS )
          CALL ADI_ERASE( OARG, STATUS )

        END IF

*    Advance pointer into output buffer
        CURELM = CURELM + NELM * ESIZE

*    Advance iterator to next item
        CALL UDI0_ADVITI( ITEMS, C1, C2, IITEM, STATUS )

      END DO

*  Restore error environment
      CALL ERR_END( STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'BDI_GET', STATUS )

      END



      SUBROUTINE BDI_GET1( ID, LID, ITEM, IMODE, DID, STATUS )
*+
*  Name:
*     BDI_GET1

*  Purpose:
*     Get the named item as an ADI object

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI_GET1( ID, LID, ITEM, IMODE, DID, STATUS )

*  Description:
*     Retrieves the item specified by the ITEM string as an ADI object.
*     The behaviour when the item does not exist is controlled by IMODE.
*     With a value of zero BDI_GET1 will try to invent replacement data
*     and if it cannot will report an error. If IMODE is one then a
*     missing item results in an error. If IMODE is two then no error
*     is reported, but DID is set to ADI__NULLID.

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier of BinDS, Array or Scalar object, or derivatives
*        thereof
*     LID = INTEGER (given)
*        Object ID is linked to
*     ITEM = CHARACTER*(*) (given)
*        Single item to be retrieved
*     IMODE = INTEGER (given)
*        How to handle missing items (see above)
*     DID = INTEGER (returned)
*        The ADI object containing the item data
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
      INCLUDE 'QUAL_PAR'
      INCLUDE 'ADI_PAR'

*  Arguments Given:
      INTEGER		        ID, LID, IMODE
      CHARACTER*(*)		ITEM

*  Arguments Returned:
      INTEGER			DID

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			CHR_INSET
        LOGICAL			CHR_INSET

*  Local Variables:
      CHARACTER*20		LITEM			! Local item name

      DOUBLE PRECISION		SPARR(2)		! Regular spaced data

      INTEGER			ARGS(3)			! Function args
      INTEGER			AXNO			! Axis number
      INTEGER			AXPTR			! Ptr to axis data
      INTEGER			NDIM, DIMS(ADI__MXDIM)	! Dataset shape
      INTEGER			LITL			! Used length of LITEM
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  First function argument is the identifier, second is linked object
      ARGS(1) = ID
      ARGS(2) = LID

*  Check item name is valid, and make a local copy. Removes any
*  special item names such as E_Axis_Label.
      CALL BDI0_CHKITM( ID, ITEM, LITEM, LITL, STATUS )

*  Check that the GET operation is allowed on this item
      CALL BDI0_CHKOP( LITEM(:LITL), 'Get', STATUS )

*  Construct string for this item
      CALL ADI_NEWV0C( LITEM(:LITL), ARGS(3), STATUS )

*  Start new error context to protect caller from errors we cancel
      CALL ERR_BEGIN( STATUS )

*  Invoke the function
      CALL ADI_FEXEC( 'FileItemGet', 3, ARGS, DID, STATUS )

*  If we failed to get the data, see if its one of the items which we
*  can default
      IF ( STATUS .NE. SAI__OK ) THEN

*    Caller doesn't want to know about missing items
        IF ( IMODE .EQ. 2 ) THEN
          CALL ERR_ANNUL( STATUS )
          DID = ADI__NULLID

*    Caller will accept errors about missing items
        ELSE IF ( IMODE .EQ. 1 ) THEN

*    All text items just get defaulted to blank
        ELSE IF ( CHR_INSET( 'Title,Units,Label', LITEM(:LITL) ) .OR.
     :       CHR_INSET( 'Units,Label', LITEM(MAX(1,LITL-4):LITL) )
     :     ) THEN

          CALL ERR_ANNUL( STATUS )
          CALL ADI_NEWV0C( ' ', DID, STATUS )

*    Axis normalisations default to false
        ELSE IF ( LITEM(MAX(1,LITL-9):LITL) .EQ. 'Normalised' ) THEN

          CALL ERR_ANNUL( STATUS )
          CALL ADI_NEWV0L( .FALSE., DID, STATUS )

*    Quality mask defaults to all bits set
        ELSE IF ( LITEM(:LITL) .EQ. 'QualityMask' ) THEN

          CALL ERR_ANNUL( STATUS )
          CALL ADI_NEWV0I( 255, DID, STATUS )

*    Regular spaced data
        ELSE IF ( (LITEM(1:5) .EQ. 'Axis_') .AND.
     :            (LITEM(8:LITL) .EQ. 'SpacedData') ) THEN

          CALL ERR_ANNUL( STATUS )

*      Decode the axis number
          CALL CHR_CTOI( LITEM(6:6), AXNO, STATUS )

*      Map the axis data
          CALL BDI_AXMAPD( ID, AXNO, 'Data', 'READ', AXPTR, STATUS )

*      Extract extrema and convert to base and scale
          CALL BDI_GETSHP( ID, ADI__MXDIM, DIMS, NDIM, STATUS )
          CALL ARR_ELEM1D( AXPTR, DIMS(AXNO), 1, SPARR(1), STATUS )
          IF ( DIMS(AXNO) .GT. 1 ) THEN
            CALL ARR_ELEM1D( AXPTR, DIMS(AXNO), DIMS(AXNO), SPARR(2),
     :                       STATUS )
            SPARR(2) = (SPARR(2) - SPARR(1))/REAL(DIMS(AXNO)-1)
          ELSE
            SPARR(2) = 1.0D0
          END IF

*      Unmap the axis
          CALL BDI_AXUNMAP( ID, AXNO, 'Data', AXPTR, STATUS )

*      Create the data
          CALL ADI_NEWV1D( 2, SPARR, DID, STATUS )

*    Otherwise report error
        ELSE
          CALL MSG_SETC( 'ITEM', LITEM(:LITL) )
          CALL ERR_REP( 'BDI_GET_1', 'Unable to get item ^ITEM',
     :                    STATUS )

        END IF

      END IF

*  Release the error context
      CALL ERR_END( STATUS )

*  Release the item string
      CALL ADI_ERASE( ARGS(3), STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'BDI_GET1', STATUS )

      END
