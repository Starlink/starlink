      SUBROUTINE BDI2_INVNT( BDID, FITID, ITEM, TYPE, MODE,
     :                       ITID, NDIM, DIMS, WBPTR, STATUS )
*+
*  Name:
*     BDI2_INVNT

*  Purpose:
*     Invent BinDS data and store in an ADI object

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI2_INVNT( BDID, FITID, ITEM, TYPE, MODE, ITID, NDIM, DIMS,
*                      WBPTR, STATUS )

*  Description:
*     Services BDI invent requests for FITS files.

*  Arguments:
*     BDID = INTEGER (given)
*        The ADI identifier of the BinDS (or BinDS derived) object
*     FITID = INTEGER (given)
*        The ADI identifier of the FITS file
*     ITEM = CHARACTER*(*) (given)
*        The item to be invented
*     TYPE = CHARACTER*(*) (given)
*        The data type access is required in
*     MODE = CHARACTER*(*) (given)
*        The access mode
*     ITID = INTEGER (returned)
*        Identifier to the invented item
*     NDIM = INTEGER (returned)
*        Dimensionality of invented data
*     DIMS[] = INTEGER (returned)
*        Dimensions of invented data
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
*     RB: Richard Beard (ROSAT, University of Birmnigham)
*     {enter_new_authors_here}

*  History:
*      9 Aug 1995 (DJA):
*        Original version.
*     23 May 1996 (DJA):
*        Added LoError and HiError invention
*     31 May 1996 (DJA):
*        Had forgotten to divide ASCALE by two for LoWidth and HiWidth
*     14 Feb 1997 (RB):
*        Add code for Axis_n_Bounds
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

*  Arguments Given:
      INTEGER                   BDID,FITID
      CHARACTER*(*)		ITEM,MODE,TYPE

*  Arguments Returned:
      INTEGER                   ITID, NDIM, DIMS(*), WBPTR

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			BDI2_AXWB
C      EXTERNAL			BDI2_WBBND
C      EXTERNAL			BDI2_WBERR
C      EXTERNAL			BDI2_WBGEN
C      EXTERNAL			BDI2_WBLQ
C      EXTERNAL			BDI2_WBWID
      EXTERNAL			UTIL_PLOC
        INTEGER			UTIL_PLOC

*  Local Variables:
      CHARACTER*8		AKEY			! Axis keyword
      CHARACTER*1		CAX			! Axis code char
      CHARACTER*72		CMNT			! Keyword comment

      REAL			AXVAL			! Axis scale
      REAL			RPIX, PIXW		! Axis keyword values

      INTEGER			I			! Loop variable
      INTEGER			IAX			! Axis number
      INTEGER			PHDU			! Primary HDU id
      INTEGER			PSID			! Private item storage
      INTEGER			WPTR			! Workspace
      INTEGER			BDIMS(2)		! Axis bounds dimension
      INTEGER			BPTR			! Axis bounds values
      INTEGER			BITID			! Axis bounds item ID

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

*  Axis data?
      IF ( ITEM(1:5) .EQ. 'Axis_' .AND.
     :     ( ITEM(8:) .EQ. 'Data' .OR. ITEM(8:) .EQ. 'Bounds' ) ) THEN

*    Get dimensions of BinDS
        CALL BDI_GETSHP( BDID, ADI__MXDIM, DIMS, NDIM, STATUS )

*    Extract axis number
        CAX = ITEM(6:6)
        CALL CHR_CTOI( CAX, IAX, STATUS )

*    Private storage for axis data
        CALL ADI0_LOCPST( BDID, ITEM, .TRUE., PSID, STATUS )

*    Create invented object and attempt to fill values unless write mode
        CALL ADI_NEW1R( DIMS(IAX), ITID, STATUS )
        IF ( .NOT. WMODE ) THEN
          CALL ADI_MAPR( ITID, 'WRITE', WPTR, STATUS )

*      Locate primary HDU
          CALL ADI2_FNDHDU( FITID, 'PRIMARY', .FALSE., PHDU, STATUS )

*      Enumerated axis values?
          CALL ADI2_HGKYR( PHDU, 'AX'//CAX//'C0001', AXVAL, CMNT,
     :                     STATUS )
          IF ( STATUS .EQ. SAI__OK ) THEN

*      Read them one by one
            DO I = 1, DIMS(IAX)
              WRITE( AKEY, '(A,I4.4)' ) 'AX'//CAX//'C', I
              CALL ADI2_HGKYR( PHDU, AKEY, AXVAL, CMNT, STATUS )
              CALL ARR_SELEM1R( WPTR, DIMS(IAX), I, AXVAL, STATUS )
            END DO

*      Test for standard keywords
          ELSE
            CALL ERR_ANNUL( STATUS )
            CALL ADI2_HGKYR( PHDU, 'CRPIX'//CAX, RPIX, CMNT, STATUS )
c           CALL ADI2_HGKYR( PHDU, 'CRVAL'//CAX, RVAL, CMNT, STATUS )
            CALL ADI2_HGKYR( PHDU, 'CDELT'//CAX, PIXW, CMNT, STATUS )

*        Standard keywords there?
            IF ( STATUS .EQ. SAI__OK ) THEN
              CALL ARR_REG1R( PIXW*(1.0 - RPIX), PIXW, DIMS(IAX),
     :                        %VAL(WPTR), STATUS )

*        Otherwise regular values
            ELSE
              CALL ERR_ANNUL( STATUS )
              CALL ARR_REG1R( 1.0, 1.0, DIMS(IAX), %VAL(WPTR), STATUS )

            END IF

          END IF

*      Now check the item is catered for
          IF ( ITEM(8:) .EQ. 'Data' ) THEN
            CALL ADI_UNMAP( ITID, WPTR, STATUS )

          ELSE IF ( ITEM(8:) .EQ. 'Bounds' ) THEN
            BDIMS(1) = 2
            BDIMS(2) = DIMS(IAX)
            CALL ADI_NEW( TYPE, 2, BDIMS, BITID, STATUS )
            CALL ADI_MAPR( BITID, 'WRITE', BPTR, STATUS )

*        Construct the bounds
            CALL BDI2_INVNT_VW2B( BDIMS(2), %VAL(WPTR), .FALSE., 0.0,
     :                       %VAL(BPTR), STATUS )

*        Swap the item IDs over
            CALL ADI_UNMAP( ITID, WPTR, STATUS )
            ITID = BITID
            CALL ADI_UNMAP( ITID, BPTR, STATUS )
          END IF

*      Release HDU
          CALL ADI_ERASE( PHDU, STATUS )
        END IF

*    Set dimensions
        IF ( ITEM(8:) .EQ. 'Bounds' ) THEN
          DIMS(1) = 2
          DIMS(2) = BDIMS(2)
          NDIM = 2
        ELSE
          DIMS(1) = DIMS(IAX)
          NDIM = 1
        END IF

*    Set the WriteBack function
        IF ( .NOT. RMODE ) THEN
          WBPTR = UTIL_PLOC( BDI2_AXWB )
        END IF

      ELSE IF ( ITEM .EQ. 'Title' ) THEN
        CALL ADI_NEWV0C( ' ', ITID, STATUS )

      ELSE

*    Report error
        STATUS = SAI__ERROR

      END IF

*  Everything went ok?
 59   IF ( STATUS .NE. SAI__OK ) THEN

*    Report error
        STATUS = SAI__ERROR
        CALL MSG_SETC( 'IT', ITEM )
        CALL ERR_REP( 'BDI2_INVNT_1', 'Don''t know how to invent '/
     :                /'data for Item ^IT', STATUS )

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'BDI2_INVNT', STATUS )

      END



      SUBROUTINE BDI2_INVNT_VW2B( N, CEN, WOK, WID, BNDS, STATUS )
*+
*  Name:
*     BDI2_INVNT_VW2B

*  Purpose:
*     Invent axis bounds from centres and optionally widths

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI2_INVNT_VW2B( N, CEN, WOK, WID, BNDS, STATUS )

*  Description:

*  Arguments:
*     N = INTEGER (given)
*        Number of axis centres/widths/bound pairs
*     CEN[*] = REAL (given)
*        Axis values
*     WOK = LOGICAL (given)
*        Widths present?
*     WID[*] = REAL (given)
*        Axis widths
*     BNDS[2,N] = REAL (returned)
*        Axis bounds
*     STATUS = INTEGER (given and returned)
*        The global status.

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

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
      LOGICAL			WOK
      REAL			CEN(*), WID(*)

*  Arguments Returned:
      REAL			BNDS(2,*)

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      REAL			DIR			! Sign of axis increase
      REAL			HWID			! Bin half width

      INTEGER			I			! Loop over values
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Widths present?
      IF ( WOK ) THEN

*    Establish direction
        IF ( N .EQ. 1 ) THEN
          DIR = 1.0
        ELSE
          DIR = (CEN(N)-CEN(1))/ABS(CEN(N)-CEN(1))
        END IF

        DO I = 1, N
          HWID = WID(I)*DIR/2.0
          BNDS(1,I) = CEN(I) - HWID
          BNDS(2,I) = CEN(I) + HWID
        END DO

      ELSE

*    First bound pair
        BNDS(1,1) = CEN(1) - (CEN(2)-CEN(1))/2.0
        BNDS(2,1) = (CEN(1)+CEN(2))/2.0

*    Intermediate ones
        DO I = 2, N-1
          BNDS(1,I) = BNDS(2,I-1)
          BNDS(2,I) = (CEN(I)+CEN(I+1))/2.0
        ENDDO

*    Last bound pair
        BNDS(1,N) = BNDS(2,N-1)
        BNDS(2,N) = CEN(N) + (CEN(N)-CEN(N-1))/2.0

      END IF

      END



      SUBROUTINE BDI2_INVNT_V2W( NVAL, AXVAL, WIDTH, STATUS )
*+
*  Name:
*     BDI2_INVNT_V2W

*  Purpose:
*     Invent axis widths from axis values

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI2_INVNT_V2W( NVAL, AXVAL, WIDTH, STATUS )

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



      SUBROUTINE BDI2_INVNT_W2HW( NVAL, WIDTH, HWIDTH, STATUS )
*+
*  Name:
*     BDI2_INVNT_W2HW

*  Purpose:
*     Invent axis half-widths from axis widths

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI2_INVNT_W2HW( NVAL, WIDTH, HWIDTH, STATUS )

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



      SUBROUTINE BDI2_INVNT_V2HW( NVAL, VALUE, HWIDTH, STATUS )
*+
*  Name:
*     BDI2_INVNT_V2HW

*  Purpose:
*     Invent axis half-widths from axis values

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI2_INVNT_V2HW( NVAL, VALUE, HWIDTH, STATUS )

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
      IF ( NVAL .EQ. 1 ) THEN
        DO I = 1, NVAL-1
          HWIDTH(I) = ABS(VALUE(I+1)-VALUE(I))/2.0
        END DO
        HWIDTH(NVAL) = HWIDTH(NVAL-1)
      ELSE
        HWIDTH(NVAL) = 0.0
      END IF

      END



      SUBROUTINE BDI2_INVNT_V2ER( NVAL, VAR, ERR, STATUS )
*+
*  Name:
*     BDI2_INVNT_V2ER

*  Purpose:
*     Invent errors from REAL variances

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI2_INVNT_V2ER( NVAL, VAR, ERR, STATUS )

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



      SUBROUTINE BDI2_INVNT_V2ED( NVAL, VAR, ERR, STATUS )
*+
*  Name:
*     BDI2_INVNT_V2ED

*  Purpose:
*     Invent errors from DOUBLE variances

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI2_INVNT_V2ER( NVAL, VAR, ERR, STATUS )

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



      SUBROUTINE BDI2_INVNT_BCOP( N, BVAL, LVAL, STATUS )
*+
*  Name:
*     BDI2_INVNT_BCOP

*  Purpose:
*     Convert masked BYTE values to LOGICAL in situ

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI2_INVNT_BCOP( N, BVAL, LVAL, STATUS )

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
