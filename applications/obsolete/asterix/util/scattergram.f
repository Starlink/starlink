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
*
*    Type Definitions :
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
*    Local Constants :
*
        INTEGER        MAXLINES                 ! max amount of history text
           PARAMETER   ( MAXLINES = 10 )

        INTEGER MARKER
           PARAMETER (MARKER=2)			! PGPLOT marker number
*
*    Local variables :
*
	CHARACTER*(DAT__SZLOC) LOC1		! 1st input locator
	CHARACTER*(DAT__SZLOC) LOC2		! 2nd input locator
	CHARACTER*(DAT__SZLOC) DLOC		! output object locator
        CHARACTER*(DAT__SZLOC) VLOC		! locator to VARIANCE
	CHARACTER*(DAT__SZNAM) XNAME,YNAME	! data object names
	CHARACTER*(80)         YUNIT		! data object units
	CHARACTER*(132) LINES(MAXLINES)		! history text

        INTEGER IQPTR1, IQPTR2                  ! Input quality
        INTEGER NLINES                          ! Lines of history text used
	INTEGER SIZE1,SIZE2			! sizes of objects
        INTEGER NDIM,DIMS(DAT__MXDIM)		! dimensions of data
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
	 PARAMETER   (VERSION = 'SCATTERGRAM Version 1.8-0')
*-

*    Version announcement
      CALL MSG_PRNT( VERSION )

      CALL AST_INIT()

*    Associate input objects
      CALL USI_ASSOCI( 'INP1', 'READ', LOC1, PRIM1, STATUS )
      CALL USI_ASSOCI( 'INP2', 'READ', LOC2, PRIM2, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Check out input objects
      CALL BDA_CHKDATA( LOC1, OK, NDIM, DIMS, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99
      IF ( .NOT. OK ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Object invalid - numeric data required',
     :                                                      STATUS )
        GOTO 99
      END IF
      CALL ARR_SUMDIM( NDIM, DIMS, SIZE1 )

      CALL BDA_CHKDATA( LOC2, OK, NDIM, DIMS, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99
      IF ( .NOT. OK ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Object invalid - numeric data required',
     :                                                      STATUS )
        GOTO 99
      END IF
      CALL ARR_SUMDIM( NDIM, DIMS, SIZE2 )

*    Errors present in inputs?
      CALL BDA_CHKVAR( LOC1, DEX, NDIM, DIMS, STATUS )
      CALL BDA_CHKVAR( LOC2, DEY, NDIM, DIMS, STATUS )

*    Quality present in inputs?
      CALL BDA_CHKQUAL( LOC1, QOK1, NDIM, DIMS, STATUS )
      CALL BDA_CHKQUAL( LOC2, QOK2, NDIM, DIMS, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Check sizes
      IF ( SIZE1 .NE. SIZE2 ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Input objects have different size', STATUS )
        GOTO 99
      ELSE
        N = SIZE1
      END IF

*    Obtain subset
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

*    Deduce item count of output dataset
      FROM = SUBSET(1)
      TO = SUBSET(2)
      INCR = SUBSET(3)
      NITEM = (TO-FROM+1)/INCR

*    Set output style to markers?
      CALL USI_GET0L( 'MARKER', DOMARKER, STATUS )

*    Create display object
      CALL USI_ASSOCO( 'OUT', 'SCATTERGRAM', DLOC, STATUS )

*    Create components
      CALL BDA_CREDATA(DLOC,1,NITEM,STATUS)
      CALL BDA_CREAXVAL(DLOC,1,.FALSE.,NITEM,STATUS)

*    Create output quality?
      OUTQ = (QOK1 .OR. QOK2)
      IF ( OUTQ ) THEN

*      Read input quality arrays
        CALL BDA_MAPQUAL( LOC1, 'READ', IQPTR1, STATUS )
        CALL BDA_MAPQUAL( LOC2, 'READ', IQPTR2, STATUS )
        CALL BDA_GETMASK( LOC1, MASK1, STATUS )
        CALL BDA_GETMASK( LOC2, MASK2, STATUS )

*      Output mask is the bit-wise OR of the inputs
        OMASK = (MASK1.OR.MASK2)

*      Create o/p quality
        CALL BDA_CREQUAL( DLOC, 1, NITEM, STATUS )
        CALL BDA_MAPQUAL( DLOC, 'WRITE', OQPTR, STATUS )
        CALL BDA_PUTMASK( DLOC, OMASK, STATUS )

*      Copy it
        CALL SCATTERGRAM_COPQUAL( QOK1, %VAL(IQPTR1),
     :                            QOK2, %VAL(IQPTR2),
     :                            FROM, TO, INCR,
     :                            %VAL(OQPTR), STATUS )

       END IF

*    Map and copy data
      CALL BDA_MAPDATA( LOC1, 'READ', XPTR, STATUS )
      CALL BDA_MAPDATA( LOC2, 'READ', YPTR, STATUS )

      CALL BDA_MAPDATA(DLOC,'WRITE',DPTR,STATUS)
      CALL BDA_MAPAXVAL(DLOC,'WRITE',1,APTR,STATUS)

      CALL SCATTERGRAM_COPDATR(%VAL(XPTR),FROM,TO,INCR,%VAL(APTR),
     :                                                   STATUS )
      CALL SCATTERGRAM_COPDATR(%VAL(YPTR),FROM,TO,INCR,%VAL(DPTR),
     :                                                   STATUS )

*    Map and copy error data
      IF ( DEX ) THEN
         CALL BDA_LOCVAR(LOC1,VLOC,STATUS)
         CALL DAT_MAPV(VLOC,'_REAL','READ',VPTR,N,STATUS)
         CALL BDA_CREAXWID(DLOC,1,.FALSE.,NITEM,STATUS)
         CALL BDA_MAPAXWID(DLOC,'W',1,WPTR,STATUS)
         CALL SCATTERGRAM_COPWID(%VAL(VPTR),FROM,TO,INCR,%VAL(WPTR),
     :                                                      STATUS )
         CALL DAT_UNMAP(VLOC,STATUS)
      END IF
      IF ( DEY ) THEN
         CALL BDA_COPVAR(LOC2,DLOC,STATUS)
      END IF

*    Fill in labels and units
      IF ( PRIM1 ) THEN
        CALL DAT_NAME(LOC1,XNAME,STATUS)
      ELSE
        CALL BDA_GETLABEL(LOC1,XNAME,STATUS)
        CALL BDA_GETUNITS(LOC1,YUNIT,STATUS)
        CALL BDA_PUTAXUNITS(DLOC,1,YUNIT,STATUS)
      END IF
      CALL BDA_PUTAXLABEL(DLOC,1,XNAME,STATUS)

      IF ( PRIM2 ) THEN
        CALL DAT_NAME(LOC2,YNAME,STATUS)
        CALL BDA_PUTLABEL(DLOC,YNAME,STATUS)
      ELSE
        CALL BDA_COPTEXT( LOC1, DLOC, STATUS )
      END IF

*    GCB control (format output as markers)
      IF( DOMARKER .AND. .NOT.(DEX.OR.DEY)) THEN

*      Set up markers
        CALL GCB_LCONNECT(STATUS)
        CALL GCB_SETL('POINT_FLAG',.TRUE.,STATUS)
        CALL GCB_SETI('POINT_SYMBOL',MARKER,STATUS)
        CALL GCB_SAVE(DLOC,STATUS)
        CALL GCB_DETACH(STATUS)

      END IF

*    History
      CALL HIST_ADD( DLOC, VERSION, STATUS )

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
         CALL HIST_PTXT( DLOC, NLINES, LINES, STATUS )
      END IF

*    Release files
      CALL BDA_RELEASE( LOC1, STATUS )
      CALL BDA_RELEASE( LOC2, STATUS )
      CALL BDA_RELEASE( DLOC, STATUS )

*    Tidy up
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
