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
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*    Import :
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Local Constants :
      CHARACTER*30 VERSION
      PARAMETER (VERSION='PLOTXY Version 1.7-0')
*    Local variables :
      CHARACTER*(DAT__SZLOC) XLOC	! locator to x-array
      CHARACTER*(DAT__SZLOC) YLOC	! locator to y-array
      CHARACTER*30 DEV
      CHARACTER*80 XLBL,YLBL,TITLE
      REAL X1,X2,Y1,Y2
      REAL XLO,XHI,YLO,YHI
      INTEGER XPTR,YPTR
      INTEGER XLEN,YLEN
      INTEGER NVAL
      INTEGER SYMBOL
      LOGICAL XPRIM,YPRIM
      LOGICAL POLY,STEP
      LOGICAL ACTIVE
*    Global variables :
*    Functions :
*-
      CALL MSG_PRNT(VERSION)

      CALL AST_INIT()


      CALL USI_ASSOCI('X','READ',XLOC,XPRIM,STATUS)
      CALL USI_ASSOCI('Y','READ',YLOC,YPRIM,STATUS)

*  check both are primitive arrays
      IF (.NOT.(XPRIM.AND.YPRIM)) THEN
        CALL MSG_PRNT('AST_ERR: input not primitive')
        STATUS=SAI__ERROR
      ENDIF

      IF (STATUS.EQ.SAI__OK) THEN

        CALL DAT_MAPV(XLOC,'_REAL','READ',XPTR,XLEN,STATUS)
        CALL DAT_MAPV(YLOC,'_REAL','READ',YPTR,YLEN,STATUS)
        NVAL=MIN(XLEN,YLEN)

*  get plot style
        POLY=.FALSE.
        STEP=.FALSE.
        SYMBOL=0
        CALL PAR_GET0L('POLY',POLY,STATUS)
        CALL PAR_GET0L('STEP',STEP,STATUS)
        CALL PAR_GET0I('SYMBOL',SYMBOL,STATUS)
        IF (.NOT.(POLY.OR.STEP).AND.SYMBOL.EQ.0) THEN
          POLY=.TRUE.
        ENDIF

*  get axis ranges
        CALL ARR_RANG1R(NVAL,%VAL(XPTR),X1,X2,STATUS)
        CALL PAR_DEF0R('XLO',X1,STATUS)
        CALL PAR_GET0R('XLO',XLO,STATUS)
        CALL PAR_DEF0R('XHI',X2,STATUS)
        CALL PAR_GET0R('XHI',XHI,STATUS)
        CALL ARR_RANG1R(NVAL,%VAL(YPTR),Y1,Y2,STATUS)
        CALL PAR_DEF0R('YLO',Y1,STATUS)
        CALL PAR_GET0R('YLO',YLO,STATUS)
        CALL PAR_DEF0R('YHI',Y2,STATUS)
        CALL PAR_GET0R('YHI',YHI,STATUS)

*  get labels
        CALL PAR_GET0C('XLBL',XLBL,STATUS)
        CALL PAR_GET0C('YLBL',YLBL,STATUS)
        CALL PAR_GET0C('TITLE',TITLE,STATUS)


      ENDIF

      IF (STATUS.EQ.SAI__OK) THEN

*  get device if not already open
        CALL GDV_STATUS(ACTIVE,STATUS)
        IF (.NOT.ACTIVE) THEN
          CALL PAR_GET0C('DEV',DEV,STATUS)
          CALL GDV_OPEN(DEV,1,1,STATUS)
          IF (STATUS.NE.SAI__OK) THEN
            CALL MSG_PRNT('AST_ERR: unable to open device')
          ENDIF
        ELSE
          CALL GDV_CLEAR(STATUS)
        ENDIF



        IF (STATUS.EQ.SAI__OK) THEN

*  set transformations and draw axes
          CALL PGVSTAND()
          CALL PGWINDOW(XLO,XHI,YLO,YHI)
          CALL PGBOX('BCNTS',0.0,0,'BCNTS',0.0,0)
          CALL PGLABEL(XLBL,YLBL,TITLE)

*  plot data
          CALL PLOTXY_DOIT(NVAL,%VAL(XPTR),%VAL(YPTR),POLY,STEP,SYMBOL,
     :                                                         STATUS)

        ENDIF


      ENDIF

      IF (.NOT.ACTIVE) THEN
        CALL GDV_CLOSE(STATUS)
      ENDIF

      CALL USI_ANNUL(XLOC,STATUS)
      CALL USI_ANNUL(YLOC,STATUS)

      CALL AST_CLOSE()
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
