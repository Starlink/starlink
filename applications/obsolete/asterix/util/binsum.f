      SUBROUTINE BINSUM( STATUS )
*+
*  Name:
*     BINSUM

*  Purpose:
*     Integrate values in dataset

*  Language:
*     Starlink Fortran

*  Type of Module:
*     ASTERIX task

*  Invocation:
*     CALL BINSUM( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Integrates ( ie. sums ) data in either a primitive or an NDF. If
*     input is not 1D then user is warned and the array is treated as a
*     sequence of 1D strips.

*  Usage:
*     binsum {parameter_usage}

*  Environment Parameters:
*     INP = CHAR (read)
*        Input dataset
*     REVERSE = LOGICAL (read)
*        Sum from end of array backwards
*     OUT = CHAR (read)
*        Output dataset

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
*     Obvious. If bad quality is present then output data values reflect
*     the integrated value being constant until good quality is met.

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
*     binsum, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*      8 Dec 1989 V1.0-0 (DJA):
*        Original version.
*      5 Oct 1990 V1.3-0 (DJA):
*        REVERSE option added
*      3 Nov 1994 V1.8-0 (DJA):
*        Upgraded to new graphics
*     24 Nov 1994 V1.8-1 (DJA):
*        Now use USI for user interface
*     12 Jan 1995 V1.8-2 (DJA):
*        HDS removed, using new interfaces
*     16 Nov 1995 V2.0-0 (DJA):
*        Full ADI port
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ADI_PAR'

*  Status:
      INTEGER			STATUS             	! Global status

*  Local Constants:
      INTEGER                   MAXLINES         ! Maximum amount of hist text
         PARAMETER                (MAXLINES = 5)

      CHARACTER*30		VERSION
        PARAMETER		( VERSION = 'BINSUM Version 2.1-0' )

*  Local Variables:
      CHARACTER*132             TEXT(MAXLINES)   	! Hostory text

      INTEGER                   DIMS(ADI__MXDIM) 	! Input dimensions

      INTEGER                   IDPTR        		! Input data
      INTEGER			IFID			! I/p file identifier
      INTEGER                   IQPTR        		! Input quality data
      INTEGER                   NDIM             	! Input dimensionality
      INTEGER                   NELM             	! Total number of data items
      INTEGER                   NREC             	! Amount of TEXT used
      INTEGER                   ODPTR        		! Output data
      INTEGER			OFID			! O/p file identifier

      LOGICAL                   DATA_OK          	! Input data ok?
      LOGICAL                   REVERSE          	! In reverse mode?
      LOGICAL                   QUAL_OK          	! Input has quality?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Version id
      CALL MSG_PRNT( VERSION )

*  Initialise ASTERIX
      CALL AST_INIT()

*  Associate input dataset - can be primitive
      CALL USI_ASSOC( 'INP', 'BinDS|Array', 'READ', IFID, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Check data
      CALL BDI_CHK( IFID, 'Data', DATA_OK, STATUS )
      CALL BDI_GETSHP( IFID, ADI__MXDIM, DIMS, NDIM, STATUS )
      IF ( DATA_OK ) THEN
        CALL BDI_MAPR( IFID, 'Data', 'READ', IDPTR, STATUS )
      ELSE
        CALL ERR_REP( ' ', 'Invalid data', STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Warn if NDIM > 1
      IF ( NDIM .GT. 1 ) THEN
         CALL MSG_PRNT( 'Dimensionality > 1, will process input '/
     :                               /'as if array of 1D strips' )
      END IF

*  Work in reverse mode?
      CALL USI_GET0L( 'REVERSE', REVERSE, STATUS )

*  Find total number of elements in dataset
      CALL ARR_SUMDIM( NDIM, DIMS, NELM )

*  Check quality
      CALL BDI_CHK( IFID, 'Quality', QUAL_OK, STATUS )

*  Map if quality there
      IF ( QUAL_OK ) THEN
        CALL BDI_MAPL( IFID, 'LogicalQuality', 'READ', IQPTR, STATUS )
      END IF

*  Associate output dataset
      CALL USI_CLONE( 'INP', 'OUT', 'BinDS', OFID, STATUS )

*  Map output data
      CALL BDI_MAPR( OFID, 'Data', 'WRITE', ODPTR, STATUS )

*  Perform integration
      CALL BINSUM_INT( DIMS(1), NELM / DIMS(1), REVERSE,%VAL(IDPTR),
     :          QUAL_OK, %VAL(IQPTR), %VAL(ODPTR), STATUS )

*  Set up histogram style
      CALL GCB_LCONNECT( STATUS )
      CALL GCB_SETL( 'STEP_FLAG', .TRUE., STATUS )
      CALL GCB_FSAVE( OFID, STATUS )
      CALL GCB_DETACH( STATUS )

*  Put name of input dataset into history
      TEXT(1) = 'Input dataset {INP}'
      NREC = MAXLINES
      CALL USI_TEXT( 1, TEXT, NREC, STATUS )

*  Copy and update HISTORY
      CALL HSI_ADD( OFID, VERSION, STATUS )
      CALL HSI_PTXT( OFID, NREC, TEXT, STATUS )

*  Tidy up
 99   CALL AST_CLOSE
      CALL AST_ERR( STATUS )

      END


*+  BINSUM_INT - Performs integration of input data
      SUBROUTINE BINSUM_INT( NX, NY, REVERSE, IN, QOK, QIN, OUT,
     :                                                  STATUS )
*    Description :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     08 Dec 89 : Original (DJA)
*
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*
*    Import :
*
      INTEGER        NX, NY
      REAL           IN(NX,NY)
      LOGICAL        REVERSE
      LOGICAL        QOK
      LOGICAL        QIN(NX,NY)
*
*    Export :
*
      REAL           OUT(NX,NY)
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      REAL           SUM

      INTEGER        I,J,START,END,DELTA
*-

*  Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Choose base and delta depending on direction
      IF ( REVERSE ) THEN
        START = NX
        END = 1
        DELTA = -1
      ELSE
        START = 1
        END = NX
        DELTA = 1
      END IF

*  Switch depending on presence of quality
      IF ( QOK ) THEN
        DO J = 1, NY
          SUM = 0.0
          DO I = START, END, DELTA
            IF ( QIN(I,J) ) THEN
              SUM = SUM + IN(I,J)
            END IF
            OUT(I,J) = SUM
          END DO
        END DO
      ELSE
        DO J = 1, NY
          SUM = 0.0
          DO I = START, END, DELTA
            SUM = SUM + IN(I,J)
            OUT(I,J) = SUM
          END DO
        END DO
      END IF

      END
