      SUBROUTINE BDI1_INVNT( BDID, HFID, ITEM, TYPE, MODE,
     :                       ITID, NELM, WBPTR, STATUS )
*+
*  Name:
*     BDI1_INVNT

*  Purpose:
*     Invent BinDS data and store in the appropriate member

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI1_INVNT( BDID, HFID, ITEM, TYPE, MODE, ITID, NELM, WBPTR, STATUS )

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
*     BDID = INTEGER (given)
*        The ADI identifier of the BinDS (or BinDS derived) object
*     HFID = INTEGER (given)
*        The ADI identifier of the HDS file
*     ITEM = CHARACTER*(*) (given)
*        The item to be invented
*     TYPE = CHARACTER*(*) (given)
*        The data type access is required in
*     MODE = CHARACTER*(*) (given)
*        The access mode
*     ITID = INTEGER (returned)
*        Identifier to the invented item
*     NELM = INTEGER (returned)
*        Number of data elements invented
*     WBPTR = INTEGER (returned)
*        Address of WriteBack function
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
      INCLUDE 'QUAL_PAR'
      INCLUDE 'DAT_PAR'

*  Arguments Given:
      INTEGER                   BDID,HFID
      CHARACTER*(*)		ITEM,MODE,TYPE

*  Arguments Returned:
      INTEGER                   ITID, NELM, WBPTR

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			BDI1_INVNT_WBER
      EXTERNAL			BDI1_INVNT_WBLQ
      EXTERNAL			BDI1_INVNT_WBWID
      EXTERNAL			UTIL_PLOC
        INTEGER			UTIL_PLOC

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	CLOC			! New component
      CHARACTER*(DAT__SZLOC)	QLOC			! Quality array
      CHARACTER*(DAT__SZLOC)	MLOC			! Quality mask

      INTEGER			NDIM, DIMS(ADI__MXDIM)	! Dataset shape
      INTEGER			PSID			! Private item storage
      INTEGER			PTR			! Mapped data address
      INTEGER			WPTR			! Workspace

      BYTE			MASK			! Quality mask

      LOGICAL			RMODE			! READ mode?
      LOGICAL			WMODE			! WRITE mode?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise
      ITID = ADI__NULLID
      WBPTR = 0

*  Get mode toggles
      RMODE = (MODE(1:1).EQ.'R')
      WMODE = (MODE(1:1).EQ.'W')

*  Get dimensions of BinDS
      CALL BDI_GETSHP( BDID, ADI__MXDIM, DIMS, NDIM, STATUS )

*  Axis widths?
      IF ( (ITEM(1:5).EQ.'Axis_') .AND. (ITEM(8:).EQ.'Width') ) THEN

*    Locate the data
        CALL BDI1_CFIND( BDID, HFID, ITEM(1:7)//'Data',
     :                   .FALSE., CLOC, STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 59

*    Private storage for axis data
        CALL BDI0_LOCPST( BDID, ITEM(1:7)//'Data', .TRUE., PSID,
     :                    STATUS )

*    Map it
        CALL BDI1_ARYMAP( CLOC, 'REAL', 'READ', PSID, PTR,
     :                    NELM, STATUS )

*    Create invented object and map
        CALL ADI_NEW1( TYPE, NELM, ITID, STATUS )
        CALL ADI_MAPR( ITID, 'WRITE', WPTR, STATUS )

*    Convert to widths
        CALL BDI1_INVNT_V2W( NELM, %VAL(PTR), %VAL(WPTR), STATUS )

*    Free mapped file data and mapped item
        CALL BDI1_UNMAP_INT( BDID, HFID, PSID, STATUS )
        CALL ADI_UNMAP( ITID, WPTR, STATUS )

*    Set the WriteBack function
        IF ( .NOT. RMODE ) THEN
          WBPTR = UTIL_PLOC( BDI1_INVNT_WBWID )
        END IF

*  Logical quality
      ELSE IF ( ITEM .EQ. 'LogicalQuality' ) THEN

*    Create invented object
        CALL ADI_NEW( 'LOGICAL', NDIM, DIMS, ITID, STATUS )

*    The size of the invented data
        CALL ARR_SUMDIM( NDIM, DIMS, NELM )

*    Locate quality object
        CALL BDI1_CFIND( BDID, HFID, 'Quality', .FALSE., QLOC, STATUS )

*    And the mask
        CALL BDI1_CFIND( BDID, HFID, 'QualityMask', .FALSE.,
     :                   MLOC, STATUS )

*    Map logical quality array
        CALL ADI_MAPL( ITID, 'WRITE', WPTR, STATUS )

*    If no quality array, build a dynamic array to mark everything as good
        IF ( QLOC .EQ. DAT__NOLOC ) THEN

*      Fill it with 'good' value
          CALL ARR_INIT1L( .TRUE., NELM, %VAL(WPTR), STATUS )

        ELSE

*      Default mask if not present
          IF ( MLOC .EQ. DAT__NOLOC ) THEN
            MASK = QUAL__MASK
          ELSE
            CALL DAT_GET( MLOC, '_UBYTE', 0, 0, MASK, STATUS )
          END IF

*      Map the QUALITY array
          CALL DAT_GET( QLOC, '_UBYTE', NDIM, DIMS, %VAL(WPTR), STATUS )

*      Logical AND with the mask
          CALL BIT_AND1UB( NELM, %VAL(WPTR), MASK, STATUS )

*      Copy mask bytes to logical values
          CALL BDI1_INVNT_BCOP( NELM, %VAL(WPTR), %VAL(WPTR), STATUS )

*      Release the QUALITY array
          CALL DAT_ANNUL( QLOC, STATUS )

        END IF

*    Unmap logical quality array
        CALL ADI_UNMAP( ITID, WPTR, STATUS )

*    Release locators
        IF ( MLOC .NE. DAT__NOLOC ) THEN
          CALL DAT_ANNUL( MLOC, STATUS )
        END IF

*    Set the WriteBack function
        IF ( .NOT. RMODE ) THEN
          WBPTR = UTIL_PLOC( BDI1_INVNT_WBLQ )
        END IF

*  Masked quality
      ELSE IF ( ITEM .EQ. 'MaskedQuality' ) THEN

*    Create invented object
        CALL ADI_NEW( 'UBYTE', NDIM, DIMS, ITID, STATUS )

*    The size of the invented data
        CALL ARR_SUMDIM( NDIM, DIMS, NELM )

*    Locate quality object
        CALL BDI1_CFIND( BDID, HFID, 'Quality', .FALSE., QLOC, STATUS )

*    And the mask
        CALL BDI1_CFIND( BDID, HFID, 'QualityMask', .FALSE.,
     :                   MLOC, STATUS )

*    Map logical quality array
        CALL ADI_MAP( ITID, 'UBYTE', 'WRITE', WPTR, STATUS )

*    If no quality array, build a dynamic array to mark everything as good
        IF ( QLOC .EQ. DAT__NOLOC ) THEN

*      Fill it with 'good' value
          CALL ARR_INIT1B( QUAL__GOOD, NELM, %VAL(WPTR), STATUS )

        ELSE

*      Default mask if not present
          IF ( MLOC .EQ. DAT__NOLOC ) THEN
            MASK = QUAL__MASK
          ELSE
            CALL DAT_GET( MLOC, '_UBYTE', 0, 0, MASK, STATUS )
          END IF

*      Map the quality array
          CALL DAT_GET( QLOC, '_UBYTE', NDIM, DIMS, %VAL(WPTR), STATUS )

*      Logical AND with the mask
          CALL BIT_AND1UB( NELM, %VAL(WPTR), MASK, STATUS )

*      Release the quality array
          CALL DAT_ANNUL( QLOC, STATUS )

        END IF

*    Unmap logical quality array
        CALL ADI_UNMAP( ITID, WPTR, STATUS )

*    Release locators
        IF ( MLOC .NE. DAT__NOLOC ) THEN
          CALL DAT_ANNUL( MLOC, STATUS )
        END IF

*  Data error?
      ELSE IF ( ITEM .EQ. 'Error' ) THEN

*    Locate variance, creating if necessary
        CALL BDI1_CFIND( BDID, HFID, 'Variance', WMODE, CLOC, STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 59

*    Create invented object
        CALL ADI_NEW( TYPE, NDIM, DIMS, ITID, STATUS )

*    Copy file data to invented object if appropriate
        IF ( .NOT. WMODE ) THEN

*      Locate the BDI private storage for the variance, creating if required
          CALL BDI0_LOCPST( BDID, 'Variance', .TRUE., PSID, STATUS )

*      Map it
          CALL BDI1_ARYMAP( CLOC, TYPE, 'READ', PSID, PTR,
     :                        NELM, STATUS )

*      Map the invented object
          CALL ADI_MAP( ITID, TYPE, 'WRITE', WPTR, STATUS )

*      Convert to error
          IF ( TYPE .EQ. 'REAL' ) THEN
            CALL BDI1_INVNT_V2ER( NELM, %VAL(PTR), %VAL(WPTR), STATUS )
          ELSE IF ( TYPE .EQ. 'DOUBLE' ) THEN
            CALL BDI1_INVNT_V2ED( NELM, %VAL(PTR), %VAL(WPTR), STATUS )
          ELSE
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'T', TYPE )
            CALL ERR_REP( ' ', 'Error converting variance to error '/
     :                    /'for type ^T', STATUS )
          END IF

*      Unmap the invented object and the file data
          CALL ADI_UNMAP( ITID, WPTR, STATUS )
          CALL BDI1_UNMAP_INT( BDID, HFID, PSID, STATUS )

*      Set the WriteBack function
          WBPTR = UTIL_PLOC( BDI1_INVNT_WBER )

*      Release storage
          CALL ADI_ERASE( PSID, STATUS )

        END IF

*  Axis low or high widths?
      ELSE IF ( (ITEM(1:5).EQ.'Axis_') .AND.
     :            ((ITEM(8:).EQ.'LoWidth').OR.
     :             (ITEM(8:).EQ.'HiWidth')) ) THEN

*       Real widths present?
          CALL BDI1_CFIND( BDID, HFID, ITEM(1:7)//'Width',
     :                     .FALSE., CLOC, STATUS )
          IF ( CLOC .NE. DAT__NOLOC ) THEN

*        Locate the BDI private storage for the item, creating if required
            CALL BDI0_LOCPST( BDID, ITEM, .TRUE., PSID, STATUS )

*        Map it
            CALL BDI1_ARYMAP( CLOC, TYPE, 'READ', PSID, PTR,
     :                        NELM, STATUS )

*        Create dynamic array
            CALL DYN_MAPR( 1, NELM, WPTR, STATUS )

*        Convert to widths to half-widths
            CALL BDI1_INVNT_W2HW( NELM, %VAL(PTR), %VAL(WPTR), STATUS )

*        Free mapped data
            CALL BDI1_UNMAP_INT( BDID, HFID, PSID, STATUS )

*         Return widths
            PTR = WPTR

*       Store dynamic mapped widths
            CALL BDI1_STOMAP( PSID, .TRUE., DAT__NOLOC, 0, PTR, 'REAL',
     :                        'READ', STATUS )

          ELSE

*        Clear any bad status
            IF ( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )

*        Locate the main data
            CALL BDI1_CFIND( BDID, HFID, ITEM(1:7)//'Data',
     :                       .FALSE., CLOC, STATUS )
            IF ( CLOC .NE. DAT__NOLOC ) THEN

*          Locate the BDI private storage for the item, creating if required
              CALL BDI0_LOCPST( BDID, ITEM, .TRUE., PSID, STATUS )

*          Map it
              CALL BDI1_ARYMAP( CLOC, TYPE, 'READ', PSID, PTR,
     :                          NELM, STATUS )

*          Create dynamic array
              CALL DYN_MAPR( 1, NELM, WPTR, STATUS )

*          Convert to values to half-widths
              CALL BDI1_INVNT_V2HW( NELM, %VAL(PTR), %VAL(WPTR),
     :                              STATUS )

*          Free mapped data
              CALL BDI1_UNMAP_INT( BDID, HFID, PSID, STATUS )

*          Return widths
              PTR = WPTR

*          Store dynamic mapped widths
              CALL BDI1_STOMAP( PSID, .TRUE., DAT__NOLOC, 0, PTR,
     :                          'REAL', 'READ', STATUS )

            END IF

          END IF

      ELSE

*    Report error
        STATUS = SAI__ERROR

      END IF

*  Everything went ok?
 59   IF ( STATUS .NE. SAI__OK ) THEN

*    Report error
        STATUS = SAI__ERROR
        CALL MSG_SETC( 'IT', ITEM )
        CALL ERR_REP( 'BDI1_INVNT_1', 'Don''t know how to invent '/
     :                /'data for Item ^IT', STATUS )

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'BDI1_INVNT', STATUS )

      END



      SUBROUTINE BDI1_INVNT_V2W( NVAL, AXVAL, WIDTH, STATUS )
*+
*  Name:
*     BDI1_INVNT_V2W

*  Purpose:
*     Invent axis widths from axis values

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI1_INVNT_V2W( NVAL, AXVAL, WIDTH, STATUS )

*  Description:

*  Arguments:
*     NVAL = INTEGER (given)
*        Number of axis widths to invent
*     AXVAL(*) = REAL (given)
*        Axis values
*     WIDTH(*) = REAL (returned)
*        Axis widths
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

*  Arguments Given:
      INTEGER                   NVAL
      REAL			AXVAL(*)

*  Arguments Given and Returned:
      REAL			WIDTH(*)

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			I			! Loop over values
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check for single axis value
      IF ( NVAL .EQ. 1 ) THEN
        WIDTH(1) = 0.0

      ELSE
        DO I = 2, NVAL - 1
          WIDTH(I) = ABS((AXVAL(I+1) - AXVAL(I-1))/2.0)
        END DO
        WIDTH(1) = ABS(AXVAL(2) - AXVAL(1))
        WIDTH(NVAL) = ABS(AXVAL(NVAL) - AXVAL(NVAL-1))

      END IF

      END



      SUBROUTINE BDI1_INVNT_W2HW( NVAL, WIDTH, HWIDTH, STATUS )
*+
*  Name:
*     BDI1_INVNT_W2HW

*  Purpose:
*     Invent axis half-widths from axis widths

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI1_INVNT_W2HW( NVAL, WIDTH, HWIDTH, STATUS )

*  Description:

*  Arguments:
*     NVAL = INTEGER (given)
*        Number of axis widths to invent
*     WIDTH(*) = REAL (given)
*        Axis widths
*     HWIDTH(*) = REAL (returned)
*        Axis widths
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

*  Arguments Given:
      INTEGER                   NVAL
      REAL			WIDTH(*)

*  Arguments Given and Returned:
      REAL			HWIDTH(*)

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			I			! Loop over values
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Convert widths to half-widths
      DO I = 1, NVAL
        HWIDTH(I) = WIDTH(I) / 2.0
      END DO

      END



      SUBROUTINE BDI1_INVNT_V2HW( NVAL, VALUE, HWIDTH, STATUS )
*+
*  Name:
*     BDI1_INVNT_V2HW

*  Purpose:
*     Invent axis half-widths from axis values

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI1_INVNT_V2HW( NVAL, VALUE, HWIDTH, STATUS )

*  Description:

*  Arguments:
*     NVAL = INTEGER (given)
*        Number of axis widths to invent
*     VALUE(*) = REAL (given)
*        Axis values
*     HWIDTH(*) = REAL (returned)
*        Axis half widths
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

*  Arguments Given:
      INTEGER                   NVAL
      REAL			VALUE(*)

*  Arguments Given and Returned:
      REAL			HWIDTH(*)

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			I			! Loop over values
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Convert values to half-widths
      DO I = 1, NVAL-1
        HWIDTH(I) = ABS(VALUE(I+1)-VALUE(I))/2.0
      END DO
      HWIDTH(NVAL) = HWIDTH(NVAL-1)

      END



      SUBROUTINE BDI1_INVNT_V2ER( NVAL, VAR, ERR, STATUS )
*+
*  Name:
*     BDI1_INVNT_V2ER

*  Purpose:
*     Invent errors from REAL variances

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI1_INVNT_V2ER( NVAL, VAR, ERR, STATUS )

*  Description:

*  Arguments:
*     NVAL = INTEGER (given)
*        Number of axis widths to invent
*     VAR(*) = REAL (given)
*        Variance values
*     ERR(*) = REAL (returned)
*        Error values
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

*  Arguments Given:
      INTEGER                   NVAL
      REAL			VAR(*)

*  Arguments Returned:
      REAL			ERR(*)

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			I			! Loop over values
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Convert values to half-widths
      DO I = 1, NVAL
        IF ( VAR(I) .GT. 0.0 ) THEN
          ERR(I) = SQRT( VAR(I) )
        ELSE
          ERR(I) = 0.0
        END IF
      END DO

      END



      SUBROUTINE BDI1_INVNT_V2ED( NVAL, VAR, ERR, STATUS )
*+
*  Name:
*     BDI1_INVNT_V2ED

*  Purpose:
*     Invent errors from DOUBLE variances

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI1_INVNT_V2ER( NVAL, VAR, ERR, STATUS )

*  Description:

*  Arguments:
*     NVAL = INTEGER (given)
*        Number of axis widths to invent
*     VAR(*) = DOUBLE PRECISION (given)
*        Variance values
*     ERR(*) = DOUBLE PRECISION (returned)
*        Error values
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

*  Arguments Given:
      INTEGER                   NVAL
      DOUBLE PRECISION		VAR(*)

*  Arguments Returned:
      DOUBLE PRECISION		ERR(*)

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			I			! Loop over values
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Convert values to half-widths
      DO I = 1, NVAL
        IF ( VAR(I) .GT. 0.0D0 ) THEN
          ERR(I) = SQRT( VAR(I) )
        ELSE
          ERR(I) = 0.0D0
        END IF
      END DO

      END



      SUBROUTINE BDI1_INVNT_WBER( BDID, HFID, PSID, STATUS )
*+
*  Name:
*     BDI1_INVNT_WBER

*  Purpose:
*     Write back invented errors to the file variance component

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI1_INVNT_WBER( BDID, PSID, STATUS )

*  Description:
*     The data are squared in situ and written directly to the VARIANCE
*     component of the dataset.

*  Arguments:
*     BDID = INTEGER (given)
*        The ADI identifier of the BinDS (or BinDS derived) object
*     HFID = INTEGER (given)
*        The ADI identifier of the HDS file object
*     PSID = INTEGER (given)
*        The ADI identifier of the item private storage
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
      INCLUDE 'DAT_PAR'

*  Arguments Given:
      INTEGER                   BDID, HFID, PSID

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*6		TYPE			! Mapping type
      CHARACTER*(DAT__SZLOC)	VLOC			! VARIANCE locator

      INTEGER			IERR, NERR		! VEC_ error info
      INTEGER			N			! # mapped elements
      INTEGER			NDIM, DIMS(DAT__MXDIM)	! VARIANCE shape
      INTEGER			PTR			! Mapped data address
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Extract the mapped type, data address and number of elements
      CALL ADI_CGET0C( PSID, 'Type', TYPE, STATUS )
      CALL ADI_CGET0I( PSID, 'Ptr', PTR, STATUS )
      CALL ADI_CGET0I( PSID, 'Nelm', N, STATUS )

*  Square the data
      IF ( TYPE .EQ. 'REAL' ) THEN
        CALL VEC_MULR( .FALSE., N, %VAL(PTR), %VAL(PTR), %VAL(PTR),
     :                 IERR, NERR, STATUS )
      ELSE IF ( TYPE .EQ. 'DOUBLE' ) THEN
        CALL VEC_MULD( .FALSE., N, %VAL(PTR), %VAL(PTR), %VAL(PTR),
     :                 IERR, NERR, STATUS )
      END IF

*  Locate the VARIANCE component
      CALL BDI1_CFIND( BDID, HFID, 'Variance', .TRUE., VLOC, STATUS )

*  Get dimensions of VARIANCE
      CALL BDI_GETSHP( BDID, DAT__MXDIM, DIMS, NDIM, STATUS )

*  Write array back to VARIANCE
      CALL DAT_PUT( VLOC, '_'//TYPE, NDIM, DIMS, %VAL(PTR), STATUS )

*  Release VARIANCE array
      CALL DAT_ANNUL( VLOC, STATUS )

      END



      SUBROUTINE BDI1_INVNT_WBWID( BDID, HFID, PSID, STATUS )
*+
*  Name:
*     BDI1_INVNT_WBWID

*  Purpose:
*     Write back invented axis widths to the file axis width component

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI1_INVNT_WBWID( BDID, PSID, STATUS )

*  Description:
*     The data are written to the axis width component, overwriting any
*     existing data.

*  Arguments:
*     BDID = INTEGER (given)
*        The ADI identifier of the BinDS (or BinDS derived) object
*     HFID = INTEGER (given)
*        The ADI identifier of the HDS file object
*     PSID = INTEGER (given)
*        The ADI identifier of the item private storage
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
      INCLUDE 'DAT_PAR'

*  Arguments Given:
      INTEGER                   BDID, HFID, PSID

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*20		ITEM			! Item name
      CHARACTER*6		TYPE			! Mapping type
      CHARACTER*(DAT__SZLOC)	WLOC			! Widths locator

      INTEGER			N			! # mapped elements
      INTEGER			PTR			! Mapped data address
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get item name
      CALL ADI_NAME( PSID, ITEM, STATUS )

*  Extract the mapped type, data address and number of elements
      CALL ADI_CGET0C( PSID, 'Type', TYPE, STATUS )
      CALL ADI_CGET0I( PSID, 'Ptr', PTR, STATUS )
      CALL ADI_CGET0I( PSID, 'Nelm', N, STATUS )

*  Locate the width component
      CALL BDI1_CFIND( BDID, HFID, ITEM, .TRUE., WLOC, STATUS )

*  Write array back to file
      CALL DAT_PUT( WLOC, '_'//TYPE, 1, N, %VAL(PTR), STATUS )

*  Release widths array
      CALL DAT_ANNUL( WLOC, STATUS )

      END



      SUBROUTINE BDI1_INVNT_WBLQ( BDID, HFID, PSID, STATUS )
*+
*  Name:
*     BDI1_INVNT_WBLQ

*  Purpose:
*     Write back invented logical quality to the file quality component

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI1_INVNT_WBLQ( BDID, HFID, PSID, STATUS )

*  Description:
*     Writes logical quality to a dataset. If no quality already exists
*     then QUAL__BAD is written to each pixel with bad (= false) logical
*     quality. If quality does already exist then only those pixels
*     which have become bad have their quality values changed.

*  Arguments:
*     BDID = INTEGER (given)
*        The ADI identifier of the BinDS (or BinDS derived) object
*     HFID = INTEGER (given)
*        The ADI identifier of the HDS file object
*     PSID = INTEGER (given)
*        The ADI identifier of the item private storage
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
      INCLUDE 'DAT_PAR'
      INCLUDE 'QUAL_PAR'

*  Arguments Given:
      INTEGER                   BDID, HFID, PSID

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	MLOC			! QualityMask locator
      CHARACTER*(DAT__SZLOC)	QLOC			! Quality locator

      INTEGER			N			! # mapped elements
      INTEGER			NDIM, DIMS(DAT__MXDIM)	! QUALITY shape
      INTEGER			PTR			! Mapped data address
      INTEGER			QPTR			! Mapped quality

      LOGICAL			EXISTS			! Quality exists?
      LOGICAL			OK			! Object is ok?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Extract the mapped type, data address and number of elements
      CALL ADI_CGET0I( PSID, 'Ptr', PTR, STATUS )
      CALL ADI_CGET0I( PSID, 'Nelm', N, STATUS )

*  Get dimensions of QUALITY
      CALL BDI_GETSHP( BDID, DAT__MXDIM, DIMS, NDIM, STATUS )

*  Locate the Quality item. If it doesn't already exist create and
*  initialise to good quality.
      CALL BDI1_CFIND( BDID, HFID, 'Quality', .FALSE., QLOC, STATUS )
      IF ( QLOC .EQ. DAT__NOLOC ) THEN

*    Create new quality array
        CALL BDI1_CFIND( BDID, HFID, 'Quality', .TRUE., QLOC, STATUS )
        CALL DAT_MAP( QLOC, '_UBYTE', 'WRITE', NDIM, DIMS, QPTR,
     :                STATUS )
        EXISTS = .FALSE.

*  Already exists
      ELSE
        CALL DAT_MAP( QLOC, '_UBYTE', 'UPDATE', NDIM, DIMS, QPTR,
     :                STATUS )
        EXISTS = .TRUE.

      END IF

*  Get the quality mask, creating if not already there
      CALL BDI1_CFIND( BDID, HFID, 'QualityMask', .TRUE., MLOC, STATUS )
      CALL DAT_STATE( MLOC, OK, STATUS )
      IF ( .NOT. OK ) THEN
        CALL DAT_PUT( MLOC, '_UBYTE', 0, 0, QUAL__MASK, STATUS )
      END IF

*  Convert logical quality to byte quality
      CALL BDI1_INVNT_WBLQ_INT( N, %VAL(PTR), (.NOT.EXISTS), %VAL(QPTR),
     :                          STATUS )

*  Release QUALITY array
      CALL DAT_UNMAP( QLOC, STATUS )
      CALL DAT_ANNUL( QLOC, STATUS )
      CALL DAT_ANNUL( MLOC, STATUS )

      END



      SUBROUTINE BDI1_INVNT_WBLQ_INT( N, LVAL, NEW, BVAL, STATUS )
*+
*  Name:
*     BDI1_INVNT_WBLQ_INT

*  Purpose:
*     Convert logical quality to byte values

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI1_INVNT_WBLQ_INT( N, LVAL, NEW, BVAL, STATUS )

*  Description:
*     Creates byte quality from logical quality

*  Arguments:
*     N = INTEGER (given)
*        Number of values to copy
*     LVAL[] = LOGICAL (given)
*        Logical values, true if pixel is good
*     NEW = LOGICAL (given)
*        The byte values are new
*     BVAL[] = BYTE (given and returned)
*        Byte values
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
      INCLUDE 'QUAL_PAR'

*  Arguments Given:
      INTEGER                   N
      LOGICAL			NEW, LVAL(*)

*  Arguments Returned:
      BYTE			BVAL(*)

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			BIT_ORUB
        BYTE			BIT_ORUB

*  Local Variables:
      INTEGER			I			! Loop over values
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  New quality array?
      IF ( NEW ) THEN
        DO I = 1, N
          IF ( LVAL(I) ) THEN
            BVAL(I) = QUAL__GOOD
          ELSE
            BVAL(I) = QUAL__BAD
          END IF
        END DO
      ELSE
        DO I = 1, N
          IF ( LVAL(I) ) THEN
            BVAL(I) = QUAL__GOOD
          ELSE
            BVAL(I) = BIT_ORUB(BVAL(I),QUAL__BAD)
          END IF
        END DO
      END IF

      END



      SUBROUTINE BDI1_INVNT_BCOP( N, BVAL, LVAL, STATUS )
*+
*  Name:
*     BDI1_INVNT_BCOP

*  Purpose:
*     Convert masked BYTE values to LOGICAL in situ

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI1_INVNT_BCOP( N, BVAL, LVAL, STATUS )

*  Description:
*     Provides mapping for the 'Error' class member of BinDS derived
*     objects in HDS files. This member is derived from the VARIANCE
*     file object.

*  Arguments:
*     N = INTEGER (given)
*        Number of values to copy
*     BVAL[] = BYTE (given)
*        Byte values
*     LVAL[] = LOGICAL (returned)
*        Logical values, true if BVAL is zero, false otherwise
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

*  Arguments Given:
      INTEGER                   N
      BYTE			BVAL(*)

*  Arguments Returned:
      LOGICAL			LVAL(*)

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			I			! Loop over values
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop over array back to front, as its really the same array
      DO I = N, 1, -1
        LVAL(I) = (BVAL(I).EQ.0)
      END DO

      END
