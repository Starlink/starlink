      SUBROUTINE AST2QDP( STATUS )
*+
*  Name:
*     AST2QDP

*  Purpose:
*     Converts a 1D binned dataset to QDP format

*  Language:
*     Starlink Fortran

*  Type of Module:
*     ASTERIX task

*  Invocation:
*     CALL AST2QDP( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     {routine_description}

*  Usage:
*     ast2qdp {parameter_usage}

*  Environment Parameters:
*     INP = UNIV (read)
*       The input binned dataset
*     OUT = CHAR (read)
*       Output text file name

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

*  Implementation Status:
*     {routine_implementation_status}

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     {task_references}...

*  Keywords:
*     astqdp, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*      8 Apr 1991 V1.4-0 (DJA):
*        Original version.
*     14 Oct 1992 V1.4-1 (DJA):
*        NCMD explicity zeroed
*     22 Sep 1993 V1.7-0 (DJA):
*        Treatment of axis widths corrected (DJA)
*      8 Nov 1994 V1.8-0 (DJA):
*        Updated to use new graphics
*     24 Nov 1994 V1.8-1 (DJA):
*        Now use USI for user interface (DJA)
*     22 Feb 1995 V1.8-2 (DJA):
*        Write to file rather than invoking PLT directly.
*        Use BDI for data interface, and AIO for output.
*      1 Aug 1995 V1.8-3 (DJA):
*        New prologues.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ADI_PAR'
      INCLUDE 'PRM_PAR'

*  Status:
      INTEGER			STATUS             	! Global status

*  Local Constants:
      CHARACTER*30		VERSION
        PARAMETER		( VERSION = 'AST2QDP Version 1.8-3' )

*  Local Variables:
      CHARACTER*80                     LABEL              !
      CHARACTER*80                     TITLE              !
      CHARACTER*40                     UNITS              !

      REAL                             LO, HI             ! Range bounds
      REAL                             SIZE               ! Character size

      INTEGER                   APTR, AWPTR        	! Axis data/width
      INTEGER                   DIMS(ADI__MXDIM)   	! Dimensions
      INTEGER                   DPTR               	! Data values
      INTEGER                   EPTR               	! Error values
      INTEGER			IFID			! Input dataset
      INTEGER                   NDIM               	! Dimensionality
      INTEGER                   NELM               	! # of data items
      INTEGER			OID			! Output text file
      INTEGER                   QPTR               	! Quality pointer
      INTEGER                   STYLE              	! Line style
      INTEGER                   SYM                	! Symbol type
      INTEGER			WIDTH			! Width of output file

      LOGICAL                   AOK                	! Axis data valid?
      LOGICAL                   ANYBAD             	! Any bad points?
      LOGICAL                   AWOK               	! Axis widths valid?
      LOGICAL                   DOK                	! Data valid?
      LOGICAL                   GOK                	! GRAFIX there?
      LOGICAL                   LSET, HSET         	! Range bounds set?
      LOGICAL                   OK                 	! General validity?
      LOGICAL                   QOK                	! Quality ok?
      LOGICAL                   REG                	! Axis regular?
      LOGICAL                   REGWID             	! Axis widths regular?
      LOGICAL                   VOK                	! Variance ok?
      LOGICAL                   PRIM               	! Input primitive?
      LOGICAL                   SET,SIZ_SET            	! Size set?
      LOGICAL                   STY_SET            	! Style set?
      LOGICAL                   SYM_SET            	! Symbol set?
      LOGICAL                   XLOG, YLOG         	! Log axes?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Version id
      CALL MSG_PRNT( VERSION )

*  Initialise ASTERIX
      CALL AST_INIT()

*  Associate input file
      CALL USI_TASSOCI( 'INP', '*', 'READ', IFID, STATUS )
      CALL BDI_PRIM( IFID, PRIM, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Get output text file
      CALL AIO_ASSOCO( 'OUT', 'LIST', OID, WIDTH, STATUS )

*  Check data
      CALL BDI_CHKDATA( IFID, DOK, NDIM, DIMS, STATUS )
      IF ( DOK ) THEN
        IF ( NDIM .GT. 1 ) THEN
          STATUS = SAI__ERROR
          CALL ERR_REP( ' ', 'Data must be 1-dimensional', STATUS )
          GOTO 99
        ELSE
          NELM = DIMS(1)
        END IF
      ELSE
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Invalid data', STATUS )
        GOTO 99
      END IF

*  Warning if primitive
      IF ( PRIM ) THEN
        CALL MSG_PRNT( 'Primitive input - pixel numbers will be'/
     :                                        /' used for axis' )
      END IF

*  Quality and variance present?
      CALL BDI_CHKQUAL( IFID, QOK, NDIM, DIMS, STATUS )
      CALL BDI_CHKVAR( IFID, VOK, NDIM, DIMS, STATUS )

*  Check axis data and widths
      IF ( .NOT. PRIM ) THEN
        CALL BDI_CHKAXVAL( IFID, 1, AOK, REG, DIMS, STATUS )
        CALL BDI_CHKAXWID( IFID, 1, AWOK, REGWID, DIMS, STATUS )
        IF ( .NOT. AOK ) THEN
          CALL MSG_PRNT( 'No axis values present in input, will use'/
     :                   /' pixel number instead' )
          AWOK = .FALSE.
        END IF
      ELSE
        AOK = .FALSE.
        AWOK = .FALSE.
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Axis data
      IF ( AOK ) THEN
        CALL BDI_MAPAXVAL( IFID, 'READ', 1, APTR, STATUS )
      END IF

*  Widths
      IF ( AWOK ) THEN
        CALL BDI_MAPAXWID( IFID, 'READ', 1, AWPTR, STATUS )
      END IF

*  Map quality
      IF ( QOK ) THEN
        CALL BDI_MAPLQUAL( IFID, 'READ', ANYBAD, QPTR, STATUS )
        IF ( .NOT. ANYBAD ) THEN
          CALL BDI_UNMAPQUAL( IFID, STATUS )
          QOK = .FALSE.
        END IF
      END IF

*  Copy data array
      CALL BDI_MAPDATA( IFID, 'READ', DPTR, STATUS )
      IF ( VOK ) CALL BDI_MAPERR( IFID, 'READ', EPTR, STATUS )

*  Write a command telling QDP how to read the errors
      IF ( AWOK .AND. VOK ) THEN
        CALL AIO_WRITE( OID, 'READ SERR 1 2', STATUS )
      ELSE IF ( AWOK ) THEN
        CALL AIO_WRITE( OID, 'READ SERR 1', STATUS )
      ELSE IF ( VOK ) THEN
        CALL AIO_WRITE( OID, 'READ SERR 2', STATUS )
      END IF

*  Dataset title
      CALL BDI_GETTITLE( IFID, TITLE, STATUS )
      IF ( TITLE .GT. ' ' ) THEN
        CALL MSG_SETC( 'LAB', TITLE )
        CALL AIO_WRITE( OID, 'LABEL T "^LAB"', STATUS )
      END IF

*  X axis label and units
      CALL BDI_GETAXLABEL( IFID, 1, LABEL, STATUS )
      IF ( LABEL .GT. ' ' ) THEN
        CALL MSG_SETC( 'LAB', LABEL )
        CALL BDI_GETAXUNITS( IFID, 1, UNITS, STATUS )
        IF ( UNITS .GT. ' ' ) THEN
          CALL MSG_SETC( 'UNIT', UNITS )
          CALL AIO_WRITE( OID, 'LAB X "^LAB (^UNIT)"', STATUS )
        ELSE
          CALL AIO_WRITE( OID, 'LAB X "^LAB"', STATUS )
        END IF
      END IF

*  The data (=Y) axis label and units
      CALL BDI_GETLABEL( IFID, LABEL, STATUS )
      IF ( LABEL .GT. ' ' ) THEN
        CALL MSG_SETC( 'LAB', LABEL )
        CALL BDI_GETUNITS( IFID, UNITS, STATUS )
        IF ( UNITS .GT. ' ' ) THEN
          CALL MSG_SETC( 'UNIT', UNITS )
          CALL AIO_WRITE( OID, 'LABEL Y "^LAB (^UNIT)"', STATUS )
        ELSE
          CALL AIO_WRITE( OID, 'LABEL Y "^LAB"', STATUS )
        END IF
      END IF

*  Test ASTERIX plotting attributes if present
      CALL BDI_CHKGRAF( IFID, GOK, STATUS )
      IF ( GOK ) THEN

*    Connect to graphics system
        CALL GCB_LCONNECT( STATUS )
        CALL GCB_FLOAD( IFID, STATUS )

*    X range?
        CALL GCB_GETR( 'XAXIS_LO', LSET, LO, STATUS )
        CALL GCB_GETR( 'XAXIS_HI', HSET, HI, STATUS )
        IF ( LSET .AND. HSET ) THEN
          CALL MSG_SETR( 'LO', LO )
          CALL MSG_SETR( 'HI', HI )
          CALL AIO_WRITE( OID, 'RESCALE X ^LO ^HI', STATUS )
        END IF

*    Log X axis?
        CALL GCB_GETL( 'XAXIS_LOG', OK, XLOG, STATUS )
        IF ( OK .AND. XLOG ) THEN
          CALL AIO_WRITE( OID, 'LOG X', STATUS )
        END IF

*    Y range?
        CALL GCB_GETR( 'YAXIS_LO', LSET, LO, STATUS )
        CALL GCB_GETR( 'YAXIS_HI', HSET, HI, STATUS )
        IF ( LSET .AND. HSET ) THEN
          CALL MSG_SETR( 'LO', LO )
          CALL MSG_SETR( 'HI', HI )
          CALL AIO_WRITE( OID, 'RESCALE Y ^LO ^HI', STATUS )
        END IF

*    Log Y axis?
        CALL GCB_GETL( 'XAXIS_LOG', OK, YLOG, STATUS )
        IF ( OK .AND. YLOG ) THEN
          CALL AIO_WRITE( OID, 'LOG Y', STATUS )
        END IF

*    Markers? Only size and symbol available in PLT command
        CALL GCB_GETL( 'POINT_FLAG', OK, SET, STATUS )
        IF ( OK .AND. SET ) THEN
          CALL GCB_GETI( 'POINT_SYMBOL', SYM_SET, SYM, STATUS )
          CALL GCB_GETR( 'POINT_SIZE', SIZ_SET, SIZE, STATUS )
          IF ( .NOT. SYM_SET ) SYM = 2
          IF ( .NOT. SIZ_SET ) SIZE = 1.0
          CALL MSG_SETI( 'SYM', SYM )
          CALL MSG_SETR( 'SIZ', SIZE )
          CALL AIO_WRITE( OID, 'MARK ^SYM ON SIZE ^SIZ', STATUS )
        END IF

*    Polyline?
        CALL GCB_GETL( 'POLY_FLAG', OK, SET, STATUS )
        IF ( OK .AND. SET ) THEN
          CALL GCB_GETI( 'POLY_STYLE', STY_SET, STYLE, STATUS )
          IF ( STY_SET ) THEN
            CALL MSG_SETI( 'STY', STYLE )
            CALL AIO_WRITE( OID, 'LSTYLE ^STY ON', STATUS )
          END IF
          CALL AIO_WRITE( OID, 'LINE ON', STATUS )
        END IF

*    Stepline?
        CALL GCB_GETL( 'STEP_FLAG', OK, SET, STATUS )
        IF ( OK .AND. SET ) THEN
          CALL GCB_GETI( 'STEP_STYLE', STY_SET, STYLE, STATUS )
          IF ( STY_SET ) THEN
            CALL MSG_SETI( 'STY', STYLE )
            CALL AIO_WRITE( OID, 'LSTYLE ^STY ON', STATUS )
          END IF
          CALL AIO_WRITE( OID, 'LINE STEPPED ON', STATUS )
        ELSE
          CALL AIO_WRITE( OID, 'LINE OFF', STATUS )
        END IF

*    Detach from graphics
        CALL GCB_DETACH( STATUS )

      END IF

*  Write the data
      CALL AST2QDP_WDATA( OID, NELM, %VAL(DPTR), QOK, %VAL(QPTR),
     :                    AOK, %VAL(APTR), AWOK, %VAL(AWPTR),
     :                    VOK, %VAL(EPTR), STATUS )

*  Close output file
      CALL AIO_CANCL( 'OUT', STATUS )

*  Tidy up
 99   CALL AST_CLOSE( STATUS )
      CALL AST_ERR( STATUS )

      END



      SUBROUTINE AST2QDP_WDATA( OID, N, DATA, QOK, QUAL, AOK, AXIS,
     :                          AWOK, AXWID, EOK, ERRS, STATUS )
*+
*  Name:
*     AST2QDP_WDATA

*  Purpose:
*     Write data from arrays to AIO file in QDP format

*  Language:
*     Starlink Fortran

*  Type of Module:
*     ASTERIX task

*  Invocation:
*     CALL AST2QDP_WDATA( OID, N, DATA, QOK, QUAL, AOK, AXIS,
*                         AWOK, AXWID, EOK, ERRS, STATUS )

*  Arguments:
*     OID = INTEGER (given)
*        Output file identifier
*     N = INTEGER (given)
*        Number of data values
*     DATA[] = REAL (given)
*        Data values
*     QOK = LOGICAL (given)
*        Quality values present?
*     QUAL[] = LOGICAL (given)
*        Quality values
*     AOK = LOGICAL (given)
*        Axis values present?
*     AXIS[] = REAL (given)
*        Axis data values
*     AWOK = LOGICAL (given)
*        Axis width values present?
*     AXWID[] = REAL (given)
*        Axis width data values
*     EOK = LOGICAL (given)
*        Error values present?
*     ERRS[] = REAL (given)
*        Error values
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     {routine_description}

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

*  Implementation Status:
*     {routine_implementation_status}

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     {task_references}...

*  Keywords:
*     astqdp, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*      8 Apr 1991 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Import:
      INTEGER			OID, N
      REAL			DATA(*), AXIS(*), AXWID(*), ERRS(*)
      LOGICAL			QOK, AOK, AWOK, EOK
      LOGICAL                   QUAL(*)

*  Status:
      INTEGER			STATUS             	! Global status

*  Local Constants:
      CHARACTER*10		RFMT
        PARAMETER		( RFMT = '(1PG14.6)' )
      CHARACTER*10		IFMT
        PARAMETER		( IFMT = '(I6)' )

*  Local Variables:
      CHARACTER*132		BUF			! Output buffer

      INTEGER			ACOL			! Axis column
      INTEGER			AWCOL			! Axis width column
      INTEGER			DCOL			! Data column
      INTEGER			ECOL			! Errors column
      INTEGER			FSTAT			! i/o status
      INTEGER                   I                   	! Loop over data
      INTEGER			LCOL			! Last column in use

      LOGICAL			OK			! Point is ok?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Good by default
      OK = .TRUE.

*  Choose columns for output
      ACOL = 1
      IF ( AOK ) THEN
        IF ( AWOK ) THEN
          AWCOL = ACOL + 15
        ELSE
          AWCOL = ACOL
        END IF
        DCOL = AWCOL + 15
      ELSE
        DCOL = ACOL + 6
      END IF
      IF ( EOK ) THEN
        ECOL = DCOL + 15
      ELSE
        ECOL = DCOL
      END IF
      LCOL = ECOL + 16

*  Loop over data
      DO I = 1, N

*    Point is good?
        IF ( QOK ) OK = QUAL(I)
        IF ( OK ) THEN

*      Clear buffer
          BUF(:LCOL) = ' '

*      Write axis value, or pixel if not present
          IF ( AOK ) THEN
            WRITE( BUF(ACOL:ACOL+13), RFMT, IOSTAT=FSTAT ) AXIS(I)
            IF ( AWOK ) THEN
              WRITE( BUF(AWCOL:AWCOL+13), RFMT, IOSTAT=FSTAT )
     :                                        AXWID(I)*0.5
            END IF
          ELSE
            WRITE( BUF(ACOL:ACOL+5), IFMT, IOSTAT=FSTAT ) I
          END IF

*      Write the data value
          WRITE( BUF(DCOL:DCOL+13), RFMT, IOSTAT=FSTAT ) DATA(I)

*      Write error value
          IF ( EOK ) THEN
            WRITE( BUF(ECOL:ECOL+13), RFMT, IOSTAT=FSTAT ) ERRS(I)
          END IF

*      Write buffer
          CALL AIO_WRITE( OID, BUF(:LCOL), STATUS )

*    End of good point test
        END IF

*  End of loop over data points
      END DO

      END
