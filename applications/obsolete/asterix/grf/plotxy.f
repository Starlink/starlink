*+  PLOTXY - plot any two arrays against each other
      SUBROUTINE PLOTXY(STATUS)

*    Description :
*    Parameters :
*    Method :
*
*    Deficiencies :
*    Bugs :
*    Authors :
*             (BHVAD::RJV)
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'
*    Export :
*    Status :
      INTEGER STATUS
*    Local Constants :
      CHARACTER*30 VERSION
      PARAMETER (VERSION='PLOTXY Version 2.0-0')
*    Local variables :
      CHARACTER*30 		DEV			! Plot device
      CHARACTER*80 		XLBL,YLBL,TITLE

      REAL 			X1,X2,Y1,Y2
      REAL 			XLO,XHI,YLO,YHI

      INTEGER 			NVAL			! # points to plot
      INTEGER 			SYMBOL			! Plot symbol
      INTEGER 			XFID,YFID		! Input datasets
      INTEGER 			XLEN,YLEN		! # elements in inputs
      INTEGER 			XPTR,YPTR		! Mapped inputs

      LOGICAL 			POLY,STEP
      LOGICAL 			ACTIVE			! Already active?
*-
      CALL MSG_PRNT(VERSION)

      CALL AST_INIT()

*  Get input objects
      CALL USI_ASSOC('X','Array','READ',XFID,STATUS)
      CALL USI_ASSOC('Y','BinDS|Array','READ',YFID,STATUS)
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Get dimensions
      CALL BDI_GETNEL( XFID, XLEN, STATUS )
      CALL BDI_GETNEL( YFID, YLEN, STATUS )
      NVAL=MIN(XLEN,YLEN)

*  Map data
      CALL BDI_MAPR( XFID, 'Data', 'READ', XPTR, STATUS )
      CALL BDI_MAPR( YFID, 'Data', 'READ', YPTR, STATUS )

*  Get plot style
      POLY=.FALSE.
      STEP=.FALSE.
      SYMBOL=0
      CALL USI_GET0L('POLY',POLY,STATUS)
      CALL USI_GET0L('STEP',STEP,STATUS)
      CALL USI_GET0I('SYMBOL',SYMBOL,STATUS)
      IF (.NOT.(POLY.OR.STEP).AND.SYMBOL.EQ.0) THEN
        POLY=.TRUE.
      END IF

*  Get axis ranges
      CALL ARR_RANG1R(NVAL,%VAL(XPTR),X1,X2,STATUS)
      CALL USI_DEF0R('XLO',X1,STATUS)
      CALL USI_GET0R('XLO',XLO,STATUS)
      CALL USI_DEF0R('XHI',X2,STATUS)
      CALL USI_GET0R('XHI',XHI,STATUS)
      CALL ARR_RANG1R(NVAL,%VAL(YPTR),Y1,Y2,STATUS)
      CALL USI_DEF0R('YLO',Y1,STATUS)
      CALL USI_GET0R('YLO',YLO,STATUS)
      CALL USI_DEF0R('YHI',Y2,STATUS)
      CALL USI_GET0R('YHI',YHI,STATUS)

*  Get labels
      CALL USI_GET0C('XLBL',XLBL,STATUS)
      CALL USI_GET0C('YLBL',YLBL,STATUS)
      CALL USI_GET0C('TITLE',TITLE,STATUS)

      IF (STATUS.EQ.SAI__OK) THEN

*    Get device if not already open
        CALL GDV_STATUS(ACTIVE,STATUS)
        IF (.NOT.ACTIVE) THEN
          CALL USI_GET0C('DEV',DEV,STATUS)
          CALL GDV_OPEN(DEV,1,1,STATUS)
          IF (STATUS.NE.SAI__OK) THEN
            CALL MSG_PRNT('AST_ERR: unable to open device')
          ENDIF
        ELSE
          CALL GDV_CLEAR(STATUS)
        ENDIF

        IF (STATUS.EQ.SAI__OK) THEN

*      Set transformations and draw axes
          CALL PGVSTAND()
          CALL PGWINDOW(XLO,XHI,YLO,YHI)
          CALL PGBOX('BCNTS',0.0,0,'BCNTS',0.0,0)
          CALL PGLABEL(XLBL,YLBL,TITLE)

*      Plot data
          CALL PLOTXY_DOIT(NVAL,%VAL(XPTR),%VAL(YPTR),POLY,STEP,SYMBOL,
     :                                                         STATUS)

        ENDIF

      ENDIF

*  Close device if we opened it
      IF ( .NOT. ACTIVE ) THEN
        CALL GDV_CLOSE( STATUS )
      END IF

*  Close inputs
      CALL USI_ANNUL( 'X',STATUS )
      CALL USI_ANNUL( 'Y',STATUS )

*  Tidy up
 99   CALL AST_CLOSE()
      CALL AST_ERR(STATUS)

      END



      SUBROUTINE PLOTXY_DOIT(N,X,Y,POLY,STEP,SYMBOL,STATUS)
*    Description :
*    Authors :
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Global variables :
*    Import :
      INTEGER N
      REAL X(*),Y(*)
      LOGICAL POLY,STEP
      INTEGER SYMBOL
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
*-

      IF (STATUS.EQ.SAI__OK) THEN

*  plot data
        IF (POLY) THEN
          CALL PGLINE(N,X,Y)
        ENDIF
        IF (STEP) THEN
          CALL PGBIN(N,X,Y,.TRUE.)
        ENDIF
        IF (SYMBOL.GT.0) THEN
          CALL PGPOINT(N,X,Y,SYMBOL)
        ENDIF

      ENDIF

      END
