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
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*
*    Status :
*
      INTEGER STATUS
*
*    Local constants :
*
      INTEGER                     MAXLINES         ! Maximum amount of hist text
         PARAMETER                (MAXLINES = 5)
*
*    Local variables :
*
      CHARACTER*(DAT__SZLOC)      ILOC             ! Input dataset
      CHARACTER*(DAT__SZLOC)      OLOC             ! Output dataset
      CHARACTER*132               TEXT(MAXLINES)   ! Hostory text

      INTEGER                     DIMS(DAT__MXDIM) ! Input dimensions
      INTEGER                     QDIMS(DAT__MXDIM)! Input quality dimensions

      INTEGER                     IDATA_PTR        ! Input data
      INTEGER                     IQUAL_PTR        ! Input quality data
      INTEGER                     NDIM             ! Input dimensionality
      INTEGER                     NELM             ! Total number of data items
      INTEGER                     NREC             ! Amount of TEXT used
      INTEGER                     ODATA_PTR        ! Output data
      INTEGER                     QNDIM            ! Input quality dimensionality

      LOGICAL                     ANY_BAD_QUAL     ! Any bad quality points?
      LOGICAL                     IN_PRIM          ! Input primitive?
      LOGICAL                     DATA_OK          ! Input data ok?
      LOGICAL                     REVERSE          ! In reverse mode?
      LOGICAL                     QUAL_OK          ! Input has quality?
*
*    Version :
*
      CHARACTER*30 VERSION
         PARAMETER (VERSION = 'BINSUM Version 1.8-1')
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Version announcement
      CALL MSG_PRNT( VERSION )

*    Initialise
      CALL AST_INIT( STATUS )

*    Associate input dataset - can be primitive
      CALL USI_ASSOCI( 'INP', 'READ', ILOC, IN_PRIM, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Check data
      CALL BDA_CHKDATA( ILOC, DATA_OK, NDIM, DIMS, STATUS )
      IF ( DATA_OK ) THEN
         CALL BDA_MAPDATA( ILOC, 'READ', IDATA_PTR, STATUS )
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

*    Look for quality and axis values if not primitive
      IF ( .NOT. IN_PRIM ) THEN

*       Check quality
         CALL BDA_CHKQUAL( ILOC, QUAL_OK, QNDIM, QDIMS, STATUS )

*       If quality there
         IF ( QUAL_OK ) THEN

*          Map it
            CALL BDA_MAPLQUAL( ILOC, 'READ', ANY_BAD_QUAL, IQUAL_PTR,
     :                                                       STATUS )

*          Check for all good quality though
            IF ( .NOT. ANY_BAD_QUAL ) THEN
               CALL BDA_UNMAPLQUAL( ILOC, STATUS )
               QUAL_OK = .FALSE.
            END IF

         END IF

      ELSE
         QUAL_OK = .FALSE.

      END IF

*    Associate output dataset
      CALL USI_ASSOCO( 'OUT', 'CUM_DISTRIB', OLOC, STATUS )

*    Create components
      CALL BDA_CREDATA( OLOC, NDIM, DIMS, STATUS )
      IF ( QUAL_OK ) THEN
         CALL BDA_CREQUAL( OLOC, NDIM, DIMS, STATUS )
      END IF

*    Map output data
      CALL BDA_MAPDATA( OLOC, 'WRITE', ODATA_PTR, STATUS )

*    Perform integration
      CALL BINSUM_INT( DIMS(1), NELM / DIMS(1), REVERSE,%VAL(IDATA_PTR),
     :          QUAL_OK, %VAL(IQUAL_PTR), %VAL(ODATA_PTR), STATUS )

*    Copy axis data from dataset to the other
      IF ( .NOT. IN_PRIM ) THEN

*       Copy axis bin info
         CALL BDA_COPAXES( ILOC, OLOC, STATUS )

      END IF

*    Copy data label
      CALL BDA_COPTEXT( ILOC, OLOC, STATUS )

*    Copy over MORE box and QUALITY if present
      CALL BDA_COPMORE( ILOC, OLOC, STATUS )
      IF ( QUAL_OK ) THEN
         CALL BDA_COPQUAL( ILOC, OLOC, STATUS )
      END IF

*    Set up histogram style
      CALL GCB_LCONNECT(STATUS)
      CALL GCB_SETL('STEP_FLAG',.TRUE.,STATUS)
      CALL GCB_SAVE(OLOC,STATUS)
      CALL GCB_DETACH(STATUS)

*    Put name of input dataset into history
      TEXT(1) = 'Input dataset {INP}'
      NREC = MAXLINES
      CALL USI_TEXT( 1, TEXT, NREC, STATUS )

*    Write this into history structure
      CALL HIST_PTXT( OLOC, NREC, TEXT, STATUS )

*    Copy and update HISTORY
      CALL HIST_COPY( ILOC, OLOC, STATUS )
      CALL HIST_ADD( OLOC, VERSION, STATUS )

*    Tidy up
 99   CONTINUE

      CALL AST_CLOSE

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( ' ', '...from BINSUM', STATUS )
      END IF

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
      INCLUDE 'DAT_PAR'
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
