*+  SCATTERGRAM - 1D plot of one data object against another
      SUBROUTINE SCATTERGRAM(STATUS)
*
*    Description :
*
*     This program produces a scattergram plot of one data object against
*     another.
*     Either or both of the two data objects can be DATA_ARRAY subclass
*     structures or primitives.
*     If the data object is DATA_ARRAY subclass then the DATA_ARRAY
*     component will be used.
*     Note that both data objects must be the same size but need not
*     be vectors (as they will be vectorised by the program).
*     The user is required to select a subset of the items he
*     wants displayed. The pair of numbers entered in response to the
*     "SUBSET" prompt is the range, in items, of the subset to be displayed.
*     The output is directed to a user specified display dataset. This dataset
*     can then be displayed via the usual Graphics packages.
*
*    Parameters :
*
*     INP1=UNIV(R)
*           Input data object (DATA_ARRAY or vector primitive)
*     INP2=UNIV(R)
*           Input data object (DATA_ARRAY or vector primitive)
*     SUBSET(3)=INTEGER(R)
*           Subset and spacing of items to be selected
*     MARKER=LOGICAL(R)
*           Set output style to markers
*     OUT=UNIV(W)
*           Output (display) dataset
*    Method :
*    Deficiencies :
*
*    Bugs :
*    Authors :
*     Jim Peden (BHVAD::JCMP)
*    History :
*     17 Sep 85 : V0.3-1 Original (BHVAD::JCMP)
*      8 Nov 85 : V0.4-1 Bug fix for taking subsets (BHVAD::JCMP)
*     17 Dec 85 : V0.4-2 ADAM version (BHVAD::JCMP)
*     28 Apr 86 : V0.4-3 Data object version (BHVAD::JCMP)
*     10 Jun 86 : V0.5-1 Additional spacing parameter (BHVAD::JCMP)
*     10 Sep 86 : V0.5-2 Extra comments, variable N which was used in several
*                        contexts redefined in places as NSUB and NITEM to
*                        reduce ambiguity. Scatter_move renamed Scattergram_move
*                        (BHVAD::JKD)
*     18 Aug 87 : V0.6-1 Changed to create output suitable for new GRAFIX
*     25 May 88 : V1.0-0 Changed to new data formats  (BHVAD::RJV)
*      9 Jan 89 : V1.0-1 Now uses new history routines and USI package. (DJA)
*      2 Oct 90 : V1.3-0 Output style changed to markers, better checking
*                        of subset parameters . MARKER keyword added (DJA)
*     28 Apr 93 : V1.7-0 Handles input quality correctly (DJA)
*     25 Jun 93 : V1.7-1 Removed superfluous STATUS argument to MSG_MAKE (DJA)
*     24 Nov 94 : V1.8-0 Now use USI for user interface (DJA)
*     14 Aug 95 : V1.8-1 Start of ADI updates (DJA)
*
*    Type Definitions :
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
*    Local Constants :
*
        INTEGER        MAXLINES                 ! max amount of history text
           PARAMETER   ( MAXLINES = 10 )

        INTEGER MARKER
           PARAMETER (MARKER=2)			! PGPLOT marker number
      BYTE	BIT_ORUB

*
*    Local variables :
*
	CHARACTER*(132) LINES(MAXLINES)		! history text
      CHARACTER*80	LABEL,UNITS
      CHARACTER*132	FILE		!

        INTEGER IQPTR1, IQPTR2                  ! Input quality
        INTEGER NLINES                          ! Lines of history text used
	INTEGER SIZE1,SIZE2			! sizes of objects
	INTEGER SUBSET(3)			! required subset, spacing
        INTEGER FROM,TO,INCR			!            "       "
	INTEGER XPTR,YPTR			! pointers to input data
	INTEGER DPTR,APTR			! display dataset pointers
        INTEGER VPTR,WPTR			! pointers to VAR and WID
        INTEGER N                               ! No of Data points
        INTEGER NSUB                            ! No of values read from SUBSET
        INTEGER NITEM                           ! Variable used in item count
        INTEGER OQPTR                           ! Output quality
        INTEGER TLEN                            ! Length of a message string
      INTEGER			IFID1, IFID2, DFID, NLEV

        BYTE    MASK1, MASK2                    ! Input quality masks
        BYTE    OMASK                           ! Output quality mask

        LOGICAL DOMARKER
	LOGICAL PRIM1,PRIM2			! Object primitive
	LOGICAL DEX,DEY			        ! Error data present
        LOGICAL QOK1,QOK2                       ! Input quality present?
	LOGICAL OK				! Data item ok?
        LOGICAL OUTQ                            ! Output quality?
*
*    Version id :
*
      CHARACTER*(30) VERSION
	 PARAMETER   (VERSION = 'SCATTERGRAM Version 2.0-0')
*-

*    Version announcement
      CALL MSG_PRNT( VERSION )

      CALL AST_INIT()

*    Associate input objects
      CALL USI_ASSOC( 'INP1', 'BinDS|Array', 'READ', IFID1, STATUS )
      CALL USI_ASSOC( 'INP2', 'BinDS|Array', 'READ', IFID2, STATUS )
      CALL ADI_DERVD( IFID1, 'Array', PRIM1, STATUS )
      CALL ADI_DERVD( IFID2, 'Array', PRIM2, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Check out input objects
      CALL BDI_CHK( IFID1, 'Data', OK, STATUS )
      CALL BDI_GETNEL( IFID1, SIZE1, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99
      IF ( .NOT. OK ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Object invalid - numeric data required',
     :                                                      STATUS )
        GOTO 99
      END IF

      CALL BDI_CHK( IFID2, 'Data', OK, STATUS )
      CALL BDI_GETNEL( IFID2, SIZE2, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99
      IF ( .NOT. OK ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Object invalid - numeric data required',
     :                                                      STATUS )
        GOTO 99
      END IF

*  Errors present in inputs?
      CALL BDI_CHK( IFID1, 'Variance', DEX, STATUS )
      CALL BDI_CHK( IFID2, 'Variance', DEY, STATUS )

*  Quality present in inputs?
      CALL BDI_CHK( IFID1, 'Quality', QOK1, STATUS )
      CALL BDI_CHK( IFID2, 'Quality', QOK2, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Check sizes
      IF ( SIZE1 .NE. SIZE2 ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Input objects have different size', STATUS )
        GOTO 99
      ELSE
        N = SIZE1
      END IF

*  Obtain subset
      CALL MSG_SETI('N',N)
      CALL MSG_PRNT( 'There are ^N items' )
      SUBSET(1)=1
      SUBSET(2)=N
      SUBSET(3)=1
      CALL USI_DEF1I( 'SUBSET', 3, SUBSET, STATUS )
      NSUB=0
      DO WHILE (NSUB.EQ.0.AND.STATUS.EQ.SAI__OK)
         CALL USI_GET1I('SUBSET',3,SUBSET,NSUB,STATUS)
         IF(NSUB.EQ.2) THEN
            SUBSET(3)=1
         END IF
         IF(NSUB.EQ.1) THEN
            CALL MSG_PRNT( 'At least 2 values required' )
            CALL USI_CANCL('SUBSET',STATUS)
            NSUB=0
         ELSE IF(SUBSET(2).LT.SUBSET(1).OR.SUBSET(1).LT.1
     :                                    .OR.SUBSET(3).LT.1) THEN
            CALL MSG_PRNT( 'Illegal value(s)' )
            CALL USI_CANCL('SUBSET',STATUS)
            NSUB=0
         END IF
      END DO
      IF ( STATUS.NE. SAI__OK) GOTO 99

*  Deduce item count of output dataset
      FROM = SUBSET(1)
      TO = SUBSET(2)
      INCR = SUBSET(3)
      NITEM = (TO-FROM+1)/INCR

*  Set output style to markers?
      CALL USI_GET0L( 'MARKER', DOMARKER, STATUS )

*  Create display object
      CALL USI_CREAT( 'OUT', ADI__NULLID, DFID, STATUS )
      CALL BDI_LINK( 'BinDS', 1, NITEM, 'REAL', DFID, STATUS )

*  Create output quality?
      OUTQ = (QOK1 .OR. QOK2)
      IF ( OUTQ ) THEN

*    Read input quality arrays
        CALL BDI_MAPUB( IFID1, 'Quality', 'READ', IQPTR1, STATUS )
        CALL BDI_MAPUB( IFID2, 'Quality', 'READ', IQPTR2, STATUS )
        CALL BDI_GET0UB( IFID1, 'QualityMask', MASK1, STATUS )
        CALL BDI_GET0UB( IFID2, 'QualityMask', MASK2, STATUS )

*    Output mask is the bit-wise OR of the inputs
        OMASK = BIT_ORUB(MASK1,MASK2)

*    Create o/p quality
        CALL BDI_MAPUB( DFID, 'Quality', 'WRITE', OQPTR, STATUS )
        CALL BDI_PUT0UB( DFID, 'QualityMask', OMASK, STATUS )

*    Copy it
        CALL SCATTERGRAM_COPQUAL( QOK1, %VAL(IQPTR1),
     :                            QOK2, %VAL(IQPTR2),
     :                            FROM, TO, INCR,
     :                            %VAL(OQPTR), STATUS )

       END IF

*  Map and copy data
      CALL BDI_MAPR( IFID1, 'Data', 'READ', XPTR, STATUS )
      CALL BDI_MAPR( IFID2, 'Data', 'READ', YPTR, STATUS )

      CALL BDI_MAPR( DFID, 'Data', 'WRITE', DPTR, STATUS )
      CALL BDI_AXMAPR( DFID, 1, 'Data', 'WRITE', APTR, STATUS )

      CALL SCATTERGRAM_COPDATR(%VAL(XPTR),FROM,TO,INCR,%VAL(APTR),
     :                                                   STATUS )
      CALL SCATTERGRAM_COPDATR(%VAL(YPTR),FROM,TO,INCR,%VAL(DPTR),
     :                                                   STATUS )

*  Map and copy error data
      IF ( DEX ) THEN
        CALL BDI_MAPR( IFID1, 'Variance', 'READ', VPTR, STATUS )
        CALL BDI_AXMAPR( DFID, 1, 'Width', 'WRITE', WPTR, STATUS )
        CALL SCATTERGRAM_COPWID(%VAL(VPTR),FROM,TO,INCR,%VAL(WPTR),
     :                                                      STATUS )
      END IF
      IF ( DEY ) THEN
        CALL BDI_COPY( IFID2, 'Variance', DFID, ' ', STATUS )
      END IF

*  Fill in labels and units
      IF ( PRIM1 ) THEN
        CALL ADI_FTRACE( IFID1, NLEV, LABEL, FILE, STATUS )
        UNITS = ' '
      ELSE
        CALL BDI_AXGET0C( IFID1, 1, 'Label', LABEL, STATUS )
        CALL BDI_AXGET0C( IFID1, 1, 'Units', UNITS, STATUS )
      END IF
      CALL BDI_AXPUT0C( DFID, 1, 'Label', LABEL, STATUS )
      CALL BDI_AXPUT0C( DFID, 1, 'Units', UNITS, STATUS )

      IF ( PRIM2 ) THEN
        CALL ADI_FTRACE( IFID2, NLEV, LABEL, FILE, STATUS )
        CALL BDI_PUT0C( DFID, 'Label', LABEL, STATUS )
      ELSE
        CALL BDI_COPY( IFID1, 'Title,Label,Units', DFID, ' ', STATUS )
      END IF

*    GCB control (format output as markers)
      IF( DOMARKER .AND. .NOT.(DEX.OR.DEY)) THEN

*      Set up markers
        CALL GCB_LCONNECT(STATUS)
        CALL GCB_SETL('POINT_FLAG',.TRUE.,STATUS)
        CALL GCB_SETI('POINT_SYMBOL',MARKER,STATUS)
        CALL GCB_FSAVE(DFID,STATUS)
        CALL GCB_DETACH(STATUS)

      END IF

*  History
      CALL HSI_ADD( DFID, VERSION, STATUS )

      IF ( STATUS .EQ. SAI__OK ) THEN
        LINES(1) = 'X data {INP1}'
        LINES(2) = 'Y data {INP2}'
        CALL MSG_SETI( 'FROM', SUBSET(1) )
        CALL MSG_SETI( 'TO', SUBSET(2) )
        CALL MSG_SETI( 'STEPS', SUBSET(3) )
        CALL MSG_MAKE( 'Subset from ^FROM to ^TO in steps of ^STEPS',
     :                                                LINES(3), TLEN )
        NLINES = MAXLINES
        CALL USI_TEXT( 3, LINES, NLINES, STATUS )
        CALL HSI_PTXT( DFID, NLINES, LINES, STATUS )
      END IF

*  Tidy up
 99   CALL AST_CLOSE
      CALL AST_ERR( STATUS )

      END



*+  SCATTERGRAM_COPDATR - copies data in SCATTERGRAM application
      SUBROUTINE SCATTERGRAM_COPDATR(X1,I1,I2,INC,X2,STATUS)
*    Description :
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
      REAL X1(*)            ! input values
      INTEGER I1,I2		! indices
      INTEGER INC		! spacing
*    Import-Export :
*    Export :
      REAL X2(*)		! output values
*    Status :
      INTEGER STATUS
*    External references :
*    Global variables :
*    Local Constants :
*    Local variables :
      INTEGER I,J			! index variables
*-
      IF (STATUS.EQ.SAI__OK) THEN
        J=0
        DO I=I1,I2,INC
          J=J+1
          X2(J)=X1(I)
        END DO
      END IF
      END



*+  SCATTERGRAM_COPQUAL - Copies quality data in SCATTERGRAM application
      SUBROUTINE SCATTERGRAM_COPQUAL(QOK1,Q1,QOK2,Q2,I1,I2,INC,OQ,
     :                               STATUS)
*    Description :
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      LOGICAL       QOK1, QOK2         ! Quality ok?
      BYTE          Q1(*),Q2(*)        ! Input values
      INTEGER I1,I2		! indices
      INTEGER INC		! spacing
*    Export :
      BYTE          OQ(*)		! output values
*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER I,J			! index variables
*-

*    Check status
      IF (STATUS.NE.SAI__OK) RETURN

*    Only one good quality array?
      IF ( .NOT. (QOK1.AND.QOK2) ) THEN
        IF ( QOK1 ) THEN
          J=0
          DO I=I1,I2,INC
            J=J+1
            OQ(J)=Q1(I)
          END DO
        ELSE
          J=0
          DO I=I1,I2,INC
            J=J+1
            OQ(J)=Q2(I)
          END DO
        END IF
      ELSE
        J=0
        DO I=I1,I2,INC
          J=J+1
          OQ(J)=(Q1(I).OR.Q2(I))
        END DO
      END IF

      END



*+  SCATTERGRAM_COPWID - copies VARIANCE into WIDTH
      SUBROUTINE SCATTERGRAM_COPWID(X1,I1,I2,INC,X2,STATUS)
*    Description :
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
      REAL X1(*)		! input values
      INTEGER I1,I2		! indices
      INTEGER INC		! spacing
*    Import-Export :
*    Export :
      REAL X2(*)		! output values
*    Status :
      INTEGER STATUS
*    External references :
*    Global variables :
*    Local Constants :
*    Local variables :
      INTEGER I,J			! index variables
*-
      IF (STATUS.EQ.SAI__OK) THEN
        J=0
        DO I=I1,I2,INC
          J=J+1
          X2(J)=SQRT(X1(I))*2.0
        END DO
      END IF
      END
