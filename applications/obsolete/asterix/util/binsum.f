*+  BINSUM - Integrate values in dataset
      SUBROUTINE BINSUM(STATUS)
*
*    Description :
*
*     Integrates ( ie. sums ) data in either a primitive or an NDF. If
*     input is not 1D then user is warned and the array is treated as a
*     sequence of 1D strips.
*
*    Environment parameters :
*
*     INP = UNIV(R)
*           Input dataset
*     REVERSE = LOGICAL(R)
*           Sum from end of array backwards
*     OUT = UNIV(W)
*           Output dataset
*
*    Method :
*
*     Obvious. If bad quality is present then output data values reflect
*     the integrated value being constant until good quality is met.
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*      8 Dec 89 : V1.0-0  Original (DJA)
*      5 Oct 90 : V1.3-0  REVERSE option added (DJA)
*      3 Nov 94 : V1.8-0  Upgraded to new graphics (DJA)
*     24 Nov 94 : V1.8-1  Now use USI for user interface (DJA)
*     12 Jan 95 : V1.8-2  HDS removed, using new interfaces (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'
*
*    Status :
*
      INTEGER STATUS
*
*    Local constants :
*
      INTEGER                   MAXLINES         ! Maximum amount of hist text
         PARAMETER                (MAXLINES = 5)
*
*    Local variables :
*
      CHARACTER*132             TEXT(MAXLINES)   	! Hostory text

      INTEGER                   DIMS(ADI__MXDIM) 	! Input dimensions
      INTEGER                   QDIMS(ADI__MXDIM)	! Input quality dimensions

      INTEGER                   IDPTR        		! Input data
      INTEGER			IFID			! I/p file identifier
      INTEGER                   IQPTR        		! Input quality data
      INTEGER                   NDIM             	! Input dimensionality
      INTEGER                   NELM             	! Total number of data items
      INTEGER                   NREC             	! Amount of TEXT used
      INTEGER                   ODPTR        		! Output data
      INTEGER			OFID			! O/p file identifier
      INTEGER                   QNDIM            	! Input quality dimensionality

      LOGICAL                   ANYBAD     		! Any bad quality points?
      LOGICAL                   DATA_OK          	! Input data ok?
      LOGICAL			IN_PRIM			! Input is primitive?
      LOGICAL                   REVERSE          	! In reverse mode?
      LOGICAL                   QUAL_OK          	! Input has quality?
*
*    Version :
*
      CHARACTER*30 VERSION
         PARAMETER (VERSION = 'BINSUM Version 1.8-2')
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Version announcement
      CALL MSG_PRNT( VERSION )

*    Initialise
      CALL AST_INIT()

*    Associate input dataset - can be primitive
      CALL USI_TASSOCI( 'INP', 'BinDS,Primitive', 'READ', IFID, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Check data
      CALL BDI_CHKDATA( IFID, DATA_OK, NDIM, DIMS, STATUS )
      IF ( DATA_OK ) THEN
        CALL BDI_MAPDATA( IFID, 'READ', IDPTR, STATUS )
      ELSE
        CALL ERR_REP( ' ', 'Invalid data', STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Warn if NDIM > 1
      IF ( NDIM .GT. 1 ) THEN
         CALL MSG_PRNT( 'Dimensionality > 1, will process input '/
     :                               /'as if array of 1D strips' )
      END IF

*    Work in reverse mode?
      CALL USI_GET0L( 'REVERSE', REVERSE, STATUS )

*    Find total number of elements in dataset
      CALL ARR_SUMDIM( NDIM, DIMS, NELM )

*    Check quality
      CALL BDI_CHKQUAL( IFID, QUAL_OK, QNDIM, QDIMS, STATUS )

*    If quality there
      IF ( QUAL_OK ) THEN

*      Map it
        CALL BDI_MAPLQUAL( IFID, 'READ', ANYBAD, IQPTR, STATUS )

*      Check for all good quality though
        IF ( .NOT. ANYBAD ) THEN
          CALL BDI_UNMAPLQUAL( IFID, STATUS )
          QUAL_OK = .FALSE.
        END IF

      END IF

*    Associate output dataset
      CALL USI_TASSOCO( 'OUT', 'CUM_DISTRIB', OFID, STATUS )

*    Create components
      CALL BDI_CREDATA( OFID, NDIM, DIMS, STATUS )
      IF ( QUAL_OK ) THEN
         CALL BDI_CREQUAL( OFID, NDIM, DIMS, STATUS )
      END IF

*    Map output data
      CALL BDI_MAPDATA( OFID, 'WRITE', ODPTR, STATUS )

*    Perform integration
      CALL BINSUM_INT( DIMS(1), NELM / DIMS(1), REVERSE,%VAL(IDPTR),
     :          QUAL_OK, %VAL(IQPTR), %VAL(ODPTR), STATUS )

*    Copy axis data from dataset to the other
      CALL USI_PRIM( IFID, IN_PRIM, STATUS )
      IF ( .NOT. IN_PRIM ) THEN

*       Copy axis bin info
         CALL BDI_COPAXES( IFID, OFID, STATUS )

      END IF

*    Copy data label
      CALL BDI_COPTEXT( IFID, OFID, STATUS )

*    Copy over MORE box and QUALITY if present
      CALL BDI_COPMORE( IFID, OFID, STATUS )
      IF ( QUAL_OK ) THEN
        CALL BDI_COPQUAL( IFID, OFID, STATUS )
      END IF

*    Set up histogram style
      CALL GCB_LCONNECT(STATUS)
      CALL GCB_SETL('STEP_FLAG',.TRUE.,STATUS)
      CALL GCB_FSAVE(OFID,STATUS)
      CALL GCB_DETACH(STATUS)

*    Put name of input dataset into history
      TEXT(1) = 'Input dataset {INP}'
      NREC = MAXLINES
      CALL USI_TEXT( 1, TEXT, NREC, STATUS )

*    Write this into history structure
      CALL HSI_PTXT( OFID, NREC, TEXT, STATUS )

*    Copy and update HISTORY
      CALL HSI_COPY( IFID, OFID, STATUS )
      CALL HSI_ADD( OFID, VERSION, STATUS )

*    Tidy up
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

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Choose base and delta depending on direction
      IF ( REVERSE ) THEN
         START = NX
         END = 1
         DELTA = -1
      ELSE
         START = 1
         END = NX
         DELTA = 1
      END IF

*    Switch depending on presence of quality
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
