*+  GDRAW - draws dataset or current GRAFIX data
      SUBROUTINE GDRAW(STATUS)

*    Description :
*    Parameters :
*    Method :
*
*    Deficiencies :
*    Bugs :
*    Authors :
*             (BHVAD::RJV)
*    History :
*     24 Jan 89 : rearrangement of common blocks and structures (RJV)
*     18 May 89 : V1.0-3  now handles irregular images
*      2 Jun 89 : V1.0-4  saves transformation details in common block
*      6 Jul 89 : V1.0-5  bug fixes in log plots (RJV)
*     14 Sep 89 : V1.0-6  some changes to positioning and colour bar added (RJV)
*     14 DEC 89 : V1.0-7  puts grid on 2D image - dynamic PGPLOT defaults
*     26 Jan 90 : V1.0-8  fixed log axis problem with tick marks (RJV)
*      1 Feb 90 : V1.0-9  autoscaling for 2D takes account of magic values
*      5 Feb 90 : V1.0-10 ID parameter added to put date and user-id on plot
*      7 Feb 90 : V1.0-11 handles 1D sets stored in 2D array (RJV)
*      7 Jun 90 : V1.0-12 improvement to GRID plotting (RJV)
*      5 Jul 90 : V1.0-13 Shape plotting added (DJA)
*      6 Aug 90 : V1.0-14 Plot stamping added (RJV)
*     19 Feb 91 : V1.0-15 checks if device open if spooling selected (RJV)
*     11 Apr 91 : V1.0-16 better 3D plotting
*     24 Apr 91 : V1.0-17 generalised 1D plotting subroutines used
*                         better auto-ranging for log plots
*                         fixed problem of dotted contours  (RJV)
*     30 Apr 91 : V1.0-18 takes account of QUALITY and magic values
*                         when choosing axis ranges (RJV)
*     13 Sep 91 : V1.0-19 sets colour table before plotting (RJV)
*     22 Jan 92 : V1.0-20 Added PASSALL and FORM parameters to cope with
*                         postscript devices (DJA)
*     20 Aug 92 : V1.0-21 Changes to grid plotting
*      2 Sep 92 : V1.7-0  GDV and GCB added
*     26 Jan 93 : V1.7-1  Autospooling removed for portability (RJV)
*     12 May 93 : V1.7-2  Log axes propagated from base plot to overlays (RJV)
*     13 May 93 : V1.7-3  Bad quality pixels not plotted for overlays (RJV)
*     17 Jun 93 : V1.7-4  GTR used (RJV)
*      4 Feb 94 : V1.7-5  Local GCB used when not in interactive mode (RJV)
*      1 Jul 94 : V1.7-6  More intelligent screen handling (RJV)
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
*    Functions :
*    Local Constants :
      CHARACTER*30 VERSION
      PARAMETER (VERSION='GDRAW Version 1.7-5')
*    Local variables :
      CHARACTER*(DAT__SZLOC) LOC	! locator to input object
      CHARACTER*132 NAME
      CHARACTER*20 DEV
      INTEGER L
      LOGICAL CLEAR
      LOGICAL FRESH
      LOGICAL PRIM
      LOGICAL ID
      LOGICAL ACTIVE
*    Global variables :
      INCLUDE 'GFX_CMN'
      INCLUDE 'GMD_CMN'
*-
      CALL MSG_PRNT(VERSION)
      CALL MSG_BLNK()

*  connect Grafix Control Block
      IF (G_OPEN) THEN
        CALL GCB_ATTACH('GRAFIX',STATUS)
      ELSE
        CALL GCB_ATTACH('LOCAL',STATUS)
      ENDIF

*  initialise flags
      ID=.FALSE.

*  get dataset if not already open
      IF (.NOT.G_OPEN) THEN
        CALL AST_INIT()
        CALL USI_ASSOCI('INP','READ',LOC,PRIM,STATUS)
*  otherwise get locator to open dataset
      ELSE
        CALL USI_INIT()
        IF (G_MULTI) THEN
          LOC=G_MLOC
        ELSE
          LOC=G_LOC
        ENDIF
      ENDIF

      IF (STATUS.EQ.SAI__OK) THEN


*  display dataset name
        CALL STR_OBNAME(LOC,NAME,L,STATUS)
        CALL MSG_PRNT('Dataset: '//NAME(:L))
        CALL MSG_BLNK()

*  get device if not already open
        CALL GDV_STATUS(ACTIVE,STATUS)
        IF (.NOT.ACTIVE) THEN
          CALL USI_GET0C('DEV',DEV,STATUS)
        ELSE
*  otherwise see if display surface to be cleared
          CALL USI_GET0L('CLEAR',CLEAR,STATUS)
          CALL GDV_FRESH(FRESH,STATUS)
        ENDIF


*  see if user-id and date wanted on plot
        CALL USI_GET0L('ID',ID,STATUS)

*  open device, clear display, set default attributes
        IF (.NOT.ACTIVE) THEN
          CALL GDV_OPEN(DEV,1,1,STATUS)
        ELSEIF (CLEAR.AND..NOT.FRESH) THEN
          CALL GDV_CLEAR(STATUS)
        ENDIF

*  enforce default plotting attributes
        CALL GCB_SETDEF(STATUS)

*  do plotting
        CALL GDRAW_SUB(LOC,STATUS)


*  put user-id and date on plot if required
        IF (ID) THEN
          CALL PGIDEN()
        ENDIF

*  close device
        IF (.NOT.ACTIVE) THEN
          CALL GDV_CLOSE(STATUS)
        ENDIF


      ENDIF

      IF (.NOT.G_OPEN) THEN
        IF (G_MULTI) THEN
          CALL USI_ANNUL(LOC,STATUS)
        ELSE
          CALL BDA_RELEASE(LOC,STATUS)
          CALL USI_ANNUL(LOC,STATUS)
        ENDIF

        CALL GCB_DETACH(STATUS)

        CALL AST_CLOSE()

      ELSE

        CALL USI_CLOSE()

      ENDIF

      END


*+  GDRAW_SUB
      SUBROUTINE GDRAW_SUB(LOC,STATUS)

*    Description :
*    Parameters :
*    Method :
*
*    Deficiencies :
*    Bugs :
*    Authors :
*             (BHVAD::RJV)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*    Import :
      CHARACTER*(DAT__SZLOC) LOC	! locator to input object
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Functions :
*    Local Constants :
*    Local variables :
      LOGICAL MULTI
*    Global variables :
      INCLUDE 'GFX_CMN'
      INCLUDE 'GMD_CMN'
*-
      IF (STATUS.EQ.SAI__OK) THEN

        CALL GMD_QMULT(LOC,MULTI,STATUS)
*  multiple dataset
        IF (MULTI) THEN
          G_MLOC=LOC
          G_MULTI=.TRUE.
*  single NDF
        ELSE
          CALL GFX_NDFTYPE(LOC,STATUS)
          G_LOC=LOC
          G_MULTI=.FALSE.
        ENDIF

*  zero transformation details
        CALL GTR_ZERO(STATUS)

*  do plotting according to dataset type
        IF (G_MULTI) THEN
          CALL GDRAW_MULTI(STATUS)
        ELSEIF (G_1DSET) THEN
          CALL GDRAW_1DSET(STATUS)
        ELSEIF (G_NDF1) THEN
          CALL GDRAW_1DGRAF(STATUS)
        ELSEIF (G_NDF2) THEN
          CALL GDRAW_2DGRAF(STATUS)
        ENDIF

      ENDIF


      END


*+  GDRAW_1DGRAF - draws 1D GRAFIX dataset
      SUBROUTINE GDRAW_1DGRAF(STATUS)

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
*    Import :
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Local Constants :
      LOGICAL OVLY
      PARAMETER (OVLY=.FALSE.)
*    Global variables :
      INCLUDE 'GFX_CMN'
*    Local variables :
      REAL X1,X2,Y1,Y2
*    External References :
*    Local data :
*-
      IF (STATUS.EQ.SAI__OK) THEN

*  set default position for single plot
        X1=0.0
        X2=1.0
        Y1=0.0
        Y2=1.0

        IF (.NOT.G_OPEN) THEN
          CALL GCB_LOAD(G_LOC,STATUS)
        ENDIF

*  now plot it
        CALL GDRAW_1DGRAF_INT(G_LOC,X1,X2,Y1,Y2,OVLY,STATUS)


        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP('1DGRAF','from GDRAW_1DGRAF',STATUS)
        ENDIF
      ENDIF
      END


*+  GDRAW_1DGRAF_INT
      SUBROUTINE GDRAW_1DGRAF_INT(LOC,X1,X2,Y1,Y2,OVLY,STATUS)

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
*    Import :
      CHARACTER*(DAT__SZLOC) LOC
      REAL X1,X2,Y1,Y2
      LOGICAL OVLY
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Local Constants :
*    Global variables :
      INCLUDE 'GMD_CMN'
*    Local variables :
      CHARACTER*(DAT__SZLOC) DLOC		! locator to DATA_ARRAY
      CHARACTER*80 TITLE,LABEL,UNITS
      CHARACTER*80 XLABEL,YLABEL
      REAL XW1,XW2,YW1,YW2			! world coords of inner window
      INTEGER ID				! identifier for BDA_
      INTEGER NVAL				! number of data points
      INTEGER NDIM				! dimensionality
      INTEGER DIMS(DAT__MXDIM)			! dimensions
      INTEGER DPTR				! pointer to data
      INTEGER VPTR				! pointer to variance
      INTEGER QPTR				! pointer to quality
      INTEGER APTR				! pointer to axis values
      INTEGER WPTR				! pointer to axis width
      INTEGER XLPTR,XUPTR
      INTEGER YLPTR,YUPTR
      INTEGER NXERR,NYERR
      BYTE MASK
      LOGICAL VOK				! variance OK
      LOGICAL XERROK,YERROK
      LOGICAL AERR				! asymmetric errors
      LOGICAL QOK				! QUALITY OK
      LOGICAL XLOG,YLOG				! if axes logarithmic
      LOGICAL POLY,STEP,POINTS,ERRS		! plotting style
      LOGICAL ABS				! if plot pos. in abs units
      LOGICAL SCALED				! if axes scaled evenly
*-

      IF (STATUS.EQ.SAI__OK) THEN
*  use integer ID instead of locator
        CALL BDA_FIND(LOC,ID,STATUS)
*  get pointer to data
        CALL BDA_MAPDATA_INT(ID,'R',DPTR,STATUS)

*  take number of values from data
        CALL BDA_LOCDATA_INT(ID,DLOC,STATUS)
        CALL DAT_SIZE(DLOC,NVAL,STATUS)

*  check for QUALITY
        CALL BDA_CHKQUAL_INT(ID,QOK,NDIM,DIMS,STATUS)
        IF (QOK) THEN
          CALL BDA_MAPQUAL_INT(ID,'R',QPTR,STATUS)
          CALL BDA_GETMASK_INT(ID,MASK,STATUS)
        ENDIF
*  map x-axis values
        CALL BDA_MAPAXVAL_INT(ID,'R',1,APTR,STATUS)


*  check errors
        CALL BDA_CHKVAR_INT(ID,VOK,NDIM,DIMS,STATUS)
        CALL BDA_CHKXERR_INT(ID,XERROK,NXERR,STATUS)
        CALL BDA_CHKYERR_INT(ID,YERROK,NYERR,STATUS)
        AERR=(YERROK.OR.XERROK)
*  get default title and labels
        CALL BDA_GETTITLE_INT(ID,TITLE,STATUS)
        CALL BDA_GETAXLABEL_INT(ID,1,LABEL,STATUS)
        CALL BDA_GETAXUNITS_INT(ID,1,UNITS,STATUS)
        CALL GFX_DEFLBL(LABEL,UNITS,XLABEL,STATUS)
        CALL BDA_GETLABEL_INT(ID,LABEL,STATUS)
        CALL BDA_GETUNITS_INT(ID,UNITS,STATUS)
        CALL GFX_DEFLBL(LABEL,UNITS,YLABEL,STATUS)
*  determine plotting style
        CALL GFX_1DSTYLE(POLY,STEP,ERRS,POINTS,STATUS)

*  if nothing set then choose suitable default
        IF (.NOT.(POLY.OR.STEP.OR.ERRS.OR.POINTS)) THEN
*  if data errors present plot error bars
          IF (VOK.OR.YERROK) THEN
            ERRS=.TRUE.
*  if only one point then plot a marker
          ELSEIF (NVAL.EQ.1) THEN
            POINTS=.TRUE.
*  otherwise draw polyline
          ELSE
            POLY=.TRUE.
          ENDIF
        ENDIF

*  map error information if needed
        IF (ERRS) THEN
          IF (AERR) THEN
            CALL BDA_MAPXERR_INT(ID,'R',XLPTR,XUPTR,STATUS)
            CALL BDA_MAPYERR_INT(ID,'R',YLPTR,YUPTR,STATUS)
          ELSE

            CALL BDA_MAPVAR_INT(ID,'R',VPTR,STATUS)
            CALL BDA_MAPAXWID_INT(ID,'R',1,WPTR,STATUS)
          ENDIF
        ENDIF

*  if step-line need axis widths
        IF (STEP) THEN
          CALL BDA_MAPAXWID_INT(ID,'R',1,WPTR,STATUS)
        ENDIF


*  set up transformations etc for base plot
        IF (.NOT.OVLY) THEN

*  see if axes logarithmic for subsequent overlays
          CALL GFX_QLOG(XLOG,YLOG,STATUS)

*  set up viewport
          CALL GFX_VPORT(X1,X2,Y1,Y2,ABS,STATUS)
*  get default axis ranges
          IF (QOK) THEN
            IF (VOK.AND.ERRS) THEN
              CALL GFX_DEF1DWNDVQ(NVAL,%val(APTR),%val(DPTR),
     :              %val(VPTR),%val(QPTR),MASK,XW1,XW2,YW1,YW2,STATUS)
            ELSE
              CALL GFX_DEF1DWNDQ(NVAL,%val(APTR),%val(DPTR),
     :               %val(QPTR),MASK,XW1,XW2,YW1,YW2,STATUS)
            ENDIF
          ELSE
            IF (VOK.AND.ERRS) THEN
              CALL GFX_DEF1DWNDV(NVAL,%val(APTR),%val(DPTR),
     :                     %val(VPTR),XW1,XW2,YW1,YW2,STATUS)
            ELSE
              CALL GFX_DEF1DWND(NVAL,%val(APTR),%val(DPTR),
     :                                XW1,XW2,YW1,YW2,STATUS)
            ENDIF
          ENDIF

*  set axis ranges
          CALL GFX_WINDOW(XW1,XW2,YW1,YW2,SCALED,STATUS)


*  store details of transformation
          CALL GTR_SAVE(G_MULTI,X1,X2,Y1,Y2,ABS,XW1,XW2,YW1,YW2,
     :                                             SCALED,STATUS)
*  draw axes
          CALL GFX_AXES(STATUS)

*  plot titles
          CALL GFX_TITLE(TITLE,STATUS)

*  plot labels
          CALL GFX_LABELS(XLABEL,YLABEL,STATUS)


        ENDIF
*  end of set up for base plot

*  if overlay make sure log/lin axes are same as base
        IF (OVLY) THEN
          CALL GCB_SETL('XAXIS_LOG',XLOG,STATUS)
          CALL GCB_SETL('YAXIS_LOG',YLOG,STATUS)
        ENDIF

*  now do the plotting

*  polyline
        IF (POLY) THEN
          IF (QOK) THEN
            CALL GFX_POLYQ(NVAL,1,NVAL,%val(APTR),%val(DPTR),
     :                                 %val(QPTR),MASK,STATUS)
          ELSE
            CALL GFX_POLY(NVAL,1,NVAL,%val(APTR),%val(DPTR),
     :                                                 STATUS)
          ENDIF
        ENDIF

*  stepped line
        IF (STEP) THEN
          IF (QOK) THEN
            CALL GFX_STEPQ(NVAL,1,NVAL,%val(APTR),%val(WPTR),
     :                                 %val(DPTR),%val(QPTR),
     :                                          MASK,STATUS)
          ELSE
            CALL GFX_STEP(NVAL,1,NVAL,%val(APTR),%val(WPTR),
     :                                    %val(DPTR),STATUS)
          ENDIF
        ENDIF

*  point marker
        IF (POINTS) THEN
          IF (QOK) THEN
            CALL GFX_POINTQ(NVAL,1,NVAL,%val(APTR),%val(DPTR),
     :                                  %val(QPTR),MASK,STATUS)
          ELSE
            CALL GFX_POINT(NVAL,1,NVAL,%val(APTR),%val(DPTR),STATUS)
          ENDIF
        ENDIF

*  error bars
        IF (ERRS) THEN
          IF (AERR) THEN
            IF (QOK) THEN
              CALL GFX_AERRQ(NVAL,1,NVAL,%val(APTR),%val(XLPTR),
     :                                   %val(XUPTR),%val(DPTR),
     :                                   %val(YLPTR),%val(YUPTR),
     :                                   %val(QPTR),MASK,STATUS)
            ELSE
              CALL GFX_AERR(NVAL,1,NVAL,%val(APTR),%val(XLPTR),
     :                                   %val(XUPTR),%val(DPTR),
     :                                   %val(YLPTR),%val(YUPTR),
     :                                                   STATUS)
            ENDIF
          ELSE
            IF (QOK) THEN
              CALL GFX_ERRQ(NVAL,1,NVAL,%val(APTR),%val(WPTR),
     :                                  %val(DPTR),%val(VPTR),
     :                                  %val(QPTR),MASK,STATUS)
            ELSE
              CALL GFX_ERR(NVAL,1,NVAL,%val(APTR),%val(WPTR),
     :                                  %val(DPTR),%val(VPTR),
     :                                                 STATUS)
            ENDIF
          ENDIF
        ENDIF


*  put on annotation
        CALL GFX_NOTES(STATUS)
        CALL GFX_SHAPES(STATUS)
        CALL GFX_MARKS(STATUS)
        CALL GFX_STAMP(STATUS)

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GDRAW_1DGRAF_INT',STATUS)
        ENDIF
      ENDIF
      END


*+  draws set of 1D graphs stored in 2D array
      SUBROUTINE GDRAW_1DSET(STATUS)
*    Description :
*    Method :
*    Deficiencies :
*    Authors :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Global variables :
      INCLUDE 'GFX_CMN'
*    Structure definitions :
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
      INTEGER CHR_LEN
*    Local constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) DLOC
      CHARACTER*(DAT__SZLOC) SDLOC
      CHARACTER*(DAT__SZLOC) VLOC
      CHARACTER*(DAT__SZLOC) SVLOC
      CHARACTER*(DAT__SZLOC) QLOC
      CHARACTER*(DAT__SZLOC) SQLOC
      CHARACTER*80 TITLE,XLBL,YLBL,UNIT
      INTEGER NDIM,DIMS(2),DIML(2),DIMU(2)
      INTEGER NQDIM,QDIMS(2)
      INTEGER ISET,NSET
      INTEGER NVAL,NEL,NXERR,NYERR,VPTR
      INTEGER DPTR,APTR,QPTR,WPTR,XLPTR,XUPTR,YLPTR,YUPTR
      INTEGER L1,L2
      REAL X1,X2,Y1,Y2,YDIFF
      REAL XMIN,XMAX,YMIN,YMAX
      LOGICAL POLY,STEP,POINTS,ERRS		! plotting style
      LOGICAL QOK,SCALED,VOK,XERROK,YERROK,AERR
      BYTE MASK
*-

      IF (STATUS.EQ.SAI__OK) THEN

        IF (.NOT.G_OPEN) THEN
          CALL GCB_LOAD(G_LOC,STATUS)
        ENDIF

*  get number of sets and number of points in each set
        CALL BDA_LOCDATA(G_LOC,DLOC,STATUS)
        CALL DAT_SHAPE(DLOC,2,DIMS,NDIM,STATUS)
        NSET=DIMS(2)
        NVAL=DIMS(1)
*  map common axis and get range
        CALL BDA_MAPAXVAL(G_LOC,'R',1,APTR,STATUS)

*  check QUALITY
        CALL BDA_CHKQUAL(G_LOC,QOK,NQDIM,QDIMS,STATUS)
        IF (QOK) THEN
          CALL BDA_LOCQUAL(G_LOC,QLOC,STATUS)
          CALL BDA_GETMASK(G_LOC,MASK,STATUS)
        ENDIF

*  check errors
        CALL BDA_CHKVAR(G_LOC,VOK,NDIM,DIMS,STATUS)
        CALL BDA_CHKXERR(G_LOC,XERROK,NXERR,STATUS)
        CALL BDA_CHKYERR(G_LOC,YERROK,NYERR,STATUS)
        AERR=(YERROK.OR.XERROK)

*  get labels
        CALL BDA_GETTITLE(G_LOC,TITLE,STATUS)
        CALL BDA_GETAXLABEL(G_LOC,2,YLBL,STATUS)
        CALL BDA_GETAXLABEL(G_LOC,1,XLBL,STATUS)
        CALL BDA_GETAXUNITS(G_LOC,1,UNIT,STATUS)
        IF (UNIT.NE.' ') THEN
          L1=CHR_LEN(XLBL)+1
          L2=CHR_LEN(UNIT)
          XLBL=XLBL(:L1)//'('//UNIT(:L2)//')'
        ENDIF

*  get vertical extent of each member of set
        YDIFF=0.85/REAL(NSET)
*  set left and right edges
        X1=0.1
        X2=0.98

*  determine plotting style
        CALL GFX_1DSTYLE(POLY,STEP,ERRS,POINTS,STATUS)

*  if nothing set then choose suitable default
        IF (.NOT.(POLY.OR.STEP.OR.ERRS.OR.POINTS)) THEN
*  if data errors present plot error bars
          IF (VOK.OR.YERROK) THEN
            ERRS=.TRUE.
*  if only one point then plot a marker
          ELSEIF (NVAL.EQ.1) THEN
            POINTS=.TRUE.
*  otherwise draw polyline
          ELSE
            POLY=.TRUE.
          ENDIF
        ENDIF

*  map error information if needed
        IF (ERRS) THEN
          IF (AERR) THEN
            CALL BDA_MAPXERR(G_LOC,'R',XLPTR,XUPTR,STATUS)
            CALL BDA_MAPYERR(G_LOC,'R',YLPTR,YUPTR,STATUS)
          ELSE
            CALL BDA_LOCVAR(G_LOC,VLOC,STATUS)
            CALL BDA_MAPAXWID(G_LOC,'R',1,WPTR,STATUS)
          ENDIF
        ENDIF

*  loop through sets
        DO ISET=1,NSET

*  set top and bottom edges
          Y1=0.1+REAL(ISET-1)*YDIFF
          Y2=Y1+YDIFF

*  set viewport for this plot
          CALL PGVPORT(X1,X2,Y1,Y2)

*  get data slice
          DIML(1)=1
          DIML(2)=ISET
          DIMU(1)=NVAL
          DIMU(2)=ISET
          CALL DAT_SLICE(DLOC,2,DIML,DIMU,SDLOC,STATUS)
          CALL DAT_MAPV(SDLOC,'_REAL','READ',DPTR,NEL,STATUS)

*  and quality if present
          IF (QOK) THEN
            CALL DAT_SLICE(QLOC,2,DIML,DIMU,SQLOC,STATUS)
            CALL DAT_MAPV(SQLOC,'_UBYTE','READ',QPTR,NEL,STATUS)
          ENDIF

*  get variance
          IF ( ERRS .AND. .NOT. AERR ) THEN
            CALL DAT_SLICE(VLOC,2,DIML,DIMU,SVLOC,STATUS)
            CALL DAT_MAPV(SVLOC,'_REAL','READ',VPTR,NEL,STATUS)
          END IF

*  get default axis ranges
          IF (QOK) THEN
            CALL GFX_DEF1DWNDQ(NVAL,%val(APTR),%val(DPTR),
     :               %val(QPTR),MASK,XMIN,XMAX,YMIN,YMAX,STATUS)
          ELSE
            CALL GFX_DEF1DWND(NVAL,%val(APTR),%val(DPTR),
     :                             XMIN,XMAX,YMIN,YMAX,STATUS)
          ENDIF


*  set coordinate transformation
*  set axis ranges
          CALL GFX_WINDOW(XMIN,XMAX,YMIN,YMAX,SCALED,STATUS)
C          CALL PGWINDOW(XMIN,XMAX,YMIN,YMAX)

*  draw axes
          IF (ISET.EQ.1) THEN
            CALL PGBOX('BCNTS',0.0,0,'BCNT',0.0,0)
          ELSE
            CALL PGBOX('BCTS',0.0,0,'BCNT',0.0,0)
          ENDIF

*  now draw it
*  polyline
        IF (POLY) THEN
          IF (QOK) THEN
            CALL GFX_POLYQ(NVAL,1,NVAL,%val(APTR),%val(DPTR),
     :                                 %val(QPTR),MASK,STATUS)
          ELSE
            CALL GFX_POLY(NVAL,1,NVAL,%val(APTR),%val(DPTR),
     :                                                 STATUS)
          ENDIF
        ENDIF

*  stepped line
        IF (STEP) THEN
          IF (QOK) THEN
            CALL GFX_STEPQ(NVAL,1,NVAL,%val(APTR),%val(WPTR),
     :                                 %val(DPTR),%val(QPTR),
     :                                          MASK,STATUS)
          ELSE
            CALL GFX_STEP(NVAL,1,NVAL,%val(APTR),%val(WPTR),
     :                                    %val(DPTR),STATUS)
          ENDIF
        ENDIF

*  point marker
        IF (POINTS) THEN
          IF (QOK) THEN
            CALL GFX_POINTQ(NVAL,1,NVAL,%val(APTR),%val(DPTR),
     :                                  %val(QPTR),MASK,STATUS)
          ELSE
            CALL GFX_POINT(NVAL,1,NVAL,%val(APTR),%val(DPTR),STATUS)
          ENDIF
        ENDIF

*  error bars
        IF (ERRS) THEN
          IF (AERR) THEN
            IF (QOK) THEN
              CALL GFX_AERRQ(NVAL,1,NVAL,%val(APTR),%val(XLPTR),
     :                                   %val(XUPTR),%val(DPTR),
     :                                   %val(YLPTR),%val(YUPTR),
     :                                   %val(QPTR),MASK,STATUS)
            ELSE
              CALL GFX_AERRQ(NVAL,1,NVAL,%val(APTR),%val(XLPTR),
     :                                   %val(XUPTR),%val(DPTR),
     :                                   %val(YLPTR),%val(YUPTR),
     :                                                   STATUS)
            ENDIF
          ELSE
            IF (QOK) THEN
              CALL GFX_ERRQ(NVAL,1,NVAL,%val(APTR),%val(WPTR),
     :                                  %val(DPTR),%val(VPTR),
     :                                  %val(QPTR),MASK,STATUS)
            ELSE
              CALL GFX_ERR(NVAL,1,NVAL,%val(APTR),%val(WPTR),
     :                                  %val(DPTR),%val(VPTR),
     :                                                 STATUS)
            ENDIF
          ENDIF
        ENDIF

          CALL DAT_ANNUL(SDLOC,STATUS)
          IF (QOK) THEN
            CALL DAT_ANNUL(SQLOC,STATUS)
          ENDIF
          IF ( ERRS .AND. .NOT. AERR ) THEN
            CALL DAT_ANNUL(SVLOC,STATUS)
          END IF

*  put on annotation
        CALL GFX_NOTES(STATUS)
        CALL GFX_SHAPES(STATUS)
        CALL GFX_MARKS(STATUS)


        ENDDO

*  put labels on
        Y1=0.1
        CALL PGVPORT(X1,X2,Y1,Y2)
        CALL PGLABEL(XLBL,YLBL,' ')
        CALL PGMTEXT('T',0.2,0.5,0.5,TITLE)

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GDRAW_1DSET',STATUS)
        ENDIF

      ENDIF
      END

*+  GDRAW_2DGRAF - draws 2D GRAFIX dataset
      SUBROUTINE GDRAW_2DGRAF(STATUS)

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
*    Import :
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Local Constants :
      LOGICAL OVLY
      PARAMETER (OVLY=.FALSE.)
*    Global variables :
      INCLUDE 'GFX_CMN'
*    Local variables :
      REAL X1,X2,Y1,Y2
*-
      IF (STATUS.EQ.SAI__OK) THEN

*  set default position for single plot
        X1=0.0
        X2=1.0
        Y1=0.0
        Y2=1.0

        IF (.NOT.G_OPEN) THEN
          CALL GCB_LOAD(G_LOC,STATUS)
        ENDIF

*  now plot it
        CALL GDRAW_2DGRAF_INT(G_LOC,X1,X2,Y1,Y2,OVLY,STATUS)


        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GDRAW_2DGRAF',STATUS)
        ENDIF
      ENDIF
      END


*+  GDRAW_2DGRAF_INT
      SUBROUTINE GDRAW_2DGRAF_INT(LOC,X1,X2,Y1,Y2,OVLY,STATUS)

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
*    Import :
      CHARACTER*(DAT__SZLOC) LOC
      REAL X1,X2,Y1,Y2
      LOGICAL OVLY
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    External references :
      EXTERNAL GFX_XYTOSKY,GFX_SKYTOXY
*    Local Constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) DLOC		! locator to DATA_ARRAY
      CHARACTER*80 LABEL,XUNITS,YUNITS,TITLE,XLABEL,YLABEL
      DOUBLE PRECISION RA,DEC,ROLL
      REAL XW1,XW2,YW1,YW2
      REAL MIN,MAX
      REAL DMIN,DMAX
      INTEGER ID				! identifier for BDA_
      INTEGER NDIM				! dimensionality
      INTEGER XSIZ,YSIZ
      INTEGER XDIM,YDIM
      INTEGER DIMS(2)				! dimensions
      INTEGER QDIMS(2)
      INTEGER DPTR				! pointer to data
      INTEGER QPTR				! pointer to quality
      INTEGER XPTR				! pointer to x-axis values
      INTEGER YPTR				! pointer to y-axis values
      INTEGER XWPTR				! pointer to axis width
      INTEGER YWPTR				! pointer to axis width
      INTEGER IX1,IX2,IY1,IY2
      BYTE MASK					! QUALITY mask
      LOGICAL XOK,YOK
      LOGICAL XREG,YREG,REG
      LOGICAL QOK				! QUALITY OK
      LOGICAL PIXEL,CONTOUR,SURFACE
      LOGICAL ABS
      LOGICAL SCALED
      LOGICAL RADEC
      LOGICAL ATTOK
*    Global variables :
      INCLUDE 'GMD_CMN'
*-

      IF (STATUS.EQ.SAI__OK) THEN

*  use integer ID instead of locator
        CALL BDA_FIND(LOC,ID,STATUS)

*  get plotting style
        CALL GFX_2DSTYLE(PIXEL,CONTOUR,SURFACE,STATUS)
*  if no plotting style set default to pixel
        IF (.NOT.(PIXEL.OR.CONTOUR.OR.SURFACE)) THEN
          PIXEL=.TRUE.
        ELSEIF (SURFACE) THEN
*  cannot mix 3D with other plots
          PIXEL=.FALSE.
          CONTOUR=.FALSE.
        ENDIF

*  map data
        CALL BDA_MAPDATA_INT(ID,'R',DPTR,STATUS)
*  get locator to data
        CALL BDA_LOCDATA_INT(ID,DLOC,STATUS)
*  and dimensions
        CALL DAT_SHAPE(DLOC,2,DIMS,NDIM,STATUS)
        XDIM=DIMS(1)
        YDIM=DIMS(2)

*  map x-axis values
        CALL BDA_CHKAXVAL_INT(ID,1,XOK,XREG,XSIZ,STATUS)
        CALL BDA_MAPAXVAL_INT(ID,'R',1,XPTR,STATUS)
        IF (.NOT.XOK) THEN
          XREG=.TRUE.
        ENDIF

*  map y-axis values
        CALL BDA_CHKAXVAL_INT(ID,2,YOK,YREG,YSIZ,STATUS)
        CALL BDA_MAPAXVAL_INT(ID,'R',2,YPTR,STATUS)
        IF (.NOT.YOK) THEN
          YREG=.TRUE.
        ENDIF

        REG=(XREG.AND.YREG)

*  width needed for irregular image and pixel overlays
        IF (.NOT.REG.OR.(PIXEL.AND.OVLY)) THEN
          CALL BDA_MAPAXWID_INT(ID,'R',1,XWPTR,STATUS)
          CALL BDA_MAPAXWID_INT(ID,'R',2,YWPTR,STATUS)
        ENDIF

*  get default labels and title
        CALL BDA_GETTITLE_INT(ID,TITLE,STATUS)
        CALL BDA_GETAXLABEL_INT(ID,1,LABEL,STATUS)
        CALL BDA_GETAXUNITS_INT(ID,1,XUNITS,STATUS)
        CALL GFX_DEFLBL(LABEL,XUNITS,XLABEL,STATUS)
        CALL BDA_GETAXLABEL_INT(ID,2,LABEL,STATUS)
        CALL BDA_GETAXUNITS_INT(ID,2,YUNITS,STATUS)
        CALL GFX_DEFLBL(LABEL,YUNITS,YLABEL,STATUS)

*  decide whether axes are to be scaled equally
        SCALED=(XUNITS.EQ.YUNITS.AND.XUNITS.NE.' ')

*  check for QUALITY
        CALL BDA_CHKQUAL_INT(ID,QOK,NDIM,QDIMS,STATUS)
        IF (QOK) THEN
          CALL BDA_MAPQUAL_INT(ID,'R',QPTR,STATUS)
          CALL BDA_GETMASK_INT(ID,MASK,STATUS)
        ENDIF

*  get attitude information
        CALL GDRAW_GETATT(LOC,ATTOK,XUNITS,RA,DEC,ROLL,STATUS)


        IF (.NOT.OVLY) THEN

*  set colours for pixel plot
          IF (PIXEL) THEN
            CALL GFX_SETCOLS(STATUS)
          ENDIF


*  set axis ranges
          CALL GDRAW_GET2DWND(LOC,DIMS,XREG,YREG,XW1,XW2,YW1,YW2,
     :                                                   STATUS)

*  set viewport
          ABS=.FALSE.
          CALL GFX_VPORT(X1,X2,Y1,Y2,ABS,STATUS)

*  set up plotting window for contour or pixel plots
          IF (CONTOUR.OR.PIXEL) THEN

            CALL GFX_WINDOW(XW1,XW2,YW1,YW2,SCALED,STATUS)

*  for surface get limits of window to determine data slice
          ELSEIF (SURFACE) THEN
            CALL GFX_QWINDOW(XW1,XW2,YW1,YW2,STATUS)

          ENDIF

*  store details of transformation
          CALL GTR_SAVE(G_MULTI,X1,X2,Y1,Y2,ABS,XW1,XW2,YW1,YW2,
     :                                              SCALED,STATUS)

        ENDIF

*  get slice of data to fit window used
        CALL GDRAW_GET2DSLICE(LOC,DIMS,XREG,YREG,XW1,XW2,YW1,YW2,
     :                                          IX1,IX2,IY1,IY2,
     :                                                   STATUS)

*  get default min/max scaling levels
        IF (PIXEL.OR.CONTOUR) THEN
          CALL GDRAW_GET2DRANGE(XDIM,YDIM,IX1,IX2,IY1,IY2,%val(DPTR),
     :                                            DMIN,DMAX,STATUS)
        ENDIF

        IF (PIXEL) THEN
          MIN=DMIN
          MAX=DMAX
          IF (QOK) THEN
            IF (OVLY) THEN
*  if an overlay then don't plot bad pixels
              CALL GFX_PIXELQ2(XDIM,YDIM,IX1,IX2,IY1,IY2,
     :                              %val(XPTR),%val(YPTR),
     :                              %val(XWPTR),%val(YWPTR),
     :                                   %val(DPTR),MIN,MAX,
     :                                   %val(QPTR),MASK,STATUS)
*  otherwise plot all pixels with bad ones in background colour
            ELSE
              CALL GFX_PIXELQ(0,XDIM,YDIM,IX1,IX2,IY1,IY2,REG,
     :                              %val(XPTR),%val(YPTR),
     :                              %val(XWPTR),%val(YWPTR),
     :                                   %val(DPTR),MIN,MAX,
     :                                   %val(QPTR),MASK,STATUS)
            ENDIF
          ELSE
            CALL GFX_PIXEL(0,XDIM,YDIM,IX1,IX2,IY1,IY2,REG,
     :                              %val(XPTR),%val(YPTR),
     :                              %val(XWPTR),%val(YWPTR),
     :                             %val(DPTR),MIN,MAX,STATUS)
          ENDIF
        ENDIF

        IF (CONTOUR) THEN
          IF (QOK) THEN
            CALL GFX_CONTOURQ(XDIM,YDIM,IX1,IX2,IY1,IY2,
     :                        %val(XPTR),%val(YPTR),%val(DPTR),
     :                           DMIN,DMAX,%val(QPTR),MASK,STATUS)
          ELSE
            CALL GFX_CONTOUR(XDIM,YDIM,IX1,IX2,IY1,IY2,
     :                        %val(XPTR),%val(YPTR),%val(DPTR),
     :                                          DMIN,DMAX,STATUS)
          ENDIF
        ENDIF

        IF (.NOT.SURFACE) THEN
          IF (.NOT.OVLY) THEN
            CALL GFX_QRADEC(RADEC,STATUS)
*  RA/DEC axis labels
            IF (RADEC.AND.ATTOK) THEN
              CALL GFX_RADEC(XUNITS,RA,DEC,ROLL,STATUS)
              CALL GFX_LABELS('Right Ascension','Declination',STATUS)
*  normal axes
            ELSE
              CALL GFX_AXES(STATUS)
              CALL GFX_LABELS(XLABEL,YLABEL,STATUS)
            ENDIF

*  titles at top of plot
            CALL GFX_TITLE(TITLE,STATUS)


*  coordinate grid
            IF (ATTOK) THEN
              CALL GFX_TMAT(XUNITS,RA,DEC,ROLL,STATUS)
              CALL GFX_GRID(GFX_XYTOSKY,GFX_SKYTOXY,STATUS)
            ENDIF

          ENDIF

*  colour bar
          CALL GFX_KEY(DMIN,DMAX,STATUS)

*  annotations, position markers etc.
          CALL GFX_NOTES(STATUS)
          CALL GFX_SHAPES(STATUS)
          CALL GFX_MARKS(STATUS)
          CALL GFX_STAMP(STATUS)

*  surface plot
        ELSE
          CALL GFX_SURFACE(XDIM,YDIM,IX1,IX2,IY1,IY2,%val(DPTR),
     :                                                   STATUS)
        ENDIF



        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GDRAW_2DGRAF_INT',STATUS)
        ENDIF

      ENDIF
      END



*+  GDRAW_GET2DWND - get default 2D axis ranges for base plot
      SUBROUTINE GDRAW_GET2DWND(LOC,DIMS,XREG,YREG,XW1,XW2,YW1,YW2,
     :                                                      STATUS)

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
*    Import :
      CHARACTER*(DAT__SZLOC) LOC	! top level structure
      INTEGER DIMS(2)
      LOGICAL XREG,YREG
*    Import-export :
*    Export :
      REAL XW1,XW2,YW1,YW2
*    Status :
      INTEGER STATUS
*    Local Constants :
*    Local variables :
      REAL LO,HI
      REAL XBASE,YBASE
      REAL XSCALE,YSCALE
      REAL HALFWID
      INTEGER XSIZ,YSIZ
      INTEGER ID
      INTEGER XPTR,YPTR
      INTEGER XWPTR,YWPTR
*    Global variables :
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL BDA_FIND(LOC,ID,STATUS)

        IF (XREG) THEN
          CALL BDA_GETAXVAL_INT(ID,1,XBASE,XSCALE,XSIZ,STATUS)
        ELSE
          CALL BDA_MAPAXVAL_INT(ID,'R',1,XPTR,STATUS)
          CALL BDA_MAPAXWID_INT(ID,'R',1,XWPTR,STATUS)
        ENDIF
        IF (YREG) THEN
          CALL BDA_GETAXVAL_INT(ID,2,YBASE,YSCALE,YSIZ,STATUS)
        ELSE
          CALL BDA_MAPAXVAL_INT(ID,'R',2,YPTR,STATUS)
          CALL BDA_MAPAXWID_INT(ID,'R',2,YWPTR,STATUS)
        ENDIF


        IF (XREG) THEN

          HALFWID=XSCALE/2.0
          LO=XBASE-HALFWID
          HI=XBASE+(DIMS(1)-1)*XSCALE+HALFWID

        ELSE
          CALL GDRAW_GET2DWND_IRREG(%val(XPTR),%val(XWPTR),DIMS(1),
     :                                                LO,HI,STATUS)
        ENDIF

        XW1=LO
        XW2=HI

        IF (YREG) THEN

          HALFWID=YSCALE/2.0
          LO=YBASE-HALFWID
          HI=YBASE+(DIMS(2)-1)*YSCALE+HALFWID

        ELSE
          CALL GDRAW_GET2DWND_IRREG(%val(YPTR),%val(YWPTR),DIMS(2),
     :                                                  LO,HI,STATUS)
        ENDIF

        YW1=LO
        YW2=HI

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GDRAW_GET2DWND',STATUS)
        ENDIF
      ENDIF
      END

*+
      SUBROUTINE GDRAW_GET2DSLICE(LOC,DIMS,XREG,YREG,XW1,XW2,YW1,YW2,
     :                                              IX1,IX2,IY1,IY2,
     :                                                       STATUS)

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
*    Import :
      CHARACTER*(DAT__SZLOC) LOC	! top level structure
      INTEGER DIMS(2)
      LOGICAL XREG,YREG
      REAL XW1,XW2,YW1,YW2
*    Import-export :
*    Export :
      INTEGER IX1,IX2,IY1,IY2
*    Status :
      INTEGER STATUS
*    Local Constants :
*    Local variables :
      REAL XBASE,YBASE
      REAL XSCALE,YSCALE
      INTEGER XSIZ,YSIZ
      INTEGER ID
      INTEGER XPTR,YPTR
      INTEGER XWPTR,YWPTR
*    Global variables :
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL BDA_FIND(LOC,ID,STATUS)

        IF (XREG) THEN
          CALL BDA_GETAXVAL_INT(ID,1,XBASE,XSCALE,XSIZ,STATUS)
        ELSE
          CALL BDA_MAPAXVAL_INT(ID,'R',1,XPTR,STATUS)
          CALL BDA_MAPAXWID_INT(ID,'R',1,XWPTR,STATUS)
        ENDIF
        IF (YREG) THEN
          CALL BDA_GETAXVAL_INT(ID,2,YBASE,YSCALE,YSIZ,STATUS)
        ELSE
          CALL BDA_MAPAXVAL_INT(ID,'R',2,YPTR,STATUS)
          CALL BDA_MAPAXWID_INT(ID,'R',2,YWPTR,STATUS)
        ENDIF

*  get array indices of relevant slice of array
        IF (XREG) THEN
          IX1=NINT((XW1-XBASE)/XSCALE)+1
        ELSE
          CALL GDRAW_GET2DSLICE_SUB(%val(XPTR),%val(XWPTR),DIMS(1),
     :                                              XW1,IX1,STATUS)
        ENDIF

        IF (IX1.LT.1.OR.IX1.GT.DIMS(1)) THEN
          IX1=1
        ENDIF


        IF (XREG) THEN
          IX2=NINT((XW2-XBASE)/XSCALE)+1
        ELSE
          CALL GDRAW_GET2DSLICE_SUB(%val(XPTR),%val(XWPTR),DIMS(1),
     :                                              XW2,IX2,STATUS)
        ENDIF

        IF (IX2.GT.DIMS(1).OR.IX2.LT.1) THEN
          IX2=DIMS(1)
        ENDIF


        IF (YREG) THEN
          IY1=NINT((YW1-YBASE)/YSCALE)+1
        ELSE
          CALL GDRAW_GET2DSLICE_SUB(%val(YPTR),%val(YWPTR),DIMS(2),
     :                                               YW1,IY1,STATUS)
        ENDIF

        IF (IY1.LT.1.OR.IY1.GT.DIMS(2)) THEN
          IY1=1
        ENDIF

        IF (YREG) THEN
          IY2=NINT((YW2-YBASE)/YSCALE)+1
        ELSE
          CALL GDRAW_GET2DSLICE_SUB(%val(YPTR),%val(YWPTR),DIMS(2),
     :                                              YW2,IY2,STATUS)
        ENDIF

        IF (IY2.GT.DIMS(2).OR.IY2.LT.1) THEN
          IY2=DIMS(2)
        ENDIF


        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GDRAW_GET2DSLICE',STATUS)
        ENDIF
      ENDIF
      END


      SUBROUTINE GDRAW_GET2DSLICE_SUB(X,W,N,XX,IX,STATUS)

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
*    Import :
      INTEGER N			! number of values
      REAL X(*)			! axis values
      REAL W(*)
      REAL XX
*    Import-export :
*    Export :
      INTEGER IX
*    Status :
      INTEGER STATUS
*    Local Constants :
*    Local variables :
      REAL HALFWID
      REAL LO,HI
      INTEGER I
*-

      IF (STATUS.EQ.SAI__OK) THEN

        I=1
        IX=0
        DO WHILE (I.LE.N.AND.IX.EQ.0)
          HALFWID=W(I)/2.0
          LO=X(I)-HALFWID
          HI=X(I)+HALFWID
          IF (XX.GE.LO.AND.XX.LE.HI) THEN
            IX=I
          ENDIF
          I=I+1
        ENDDO


      ENDIF
      END



*+
      SUBROUTINE GDRAW_GET2DWND_IRREG(X,W,N,LO,HI,STATUS)

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
*    Import :
      INTEGER N			! number of values
      REAL X(*)			! axis values
      REAL W(*)                 ! axis widths
*    Import-export :
*    Export :
      REAL LO,HI
*    Status :
      INTEGER STATUS
*    Local Constants :
*    Local variables :
      INTEGER INV
*-

      IF (STATUS.EQ.SAI__OK) THEN

        IF (X(N).LT.X(1)) THEN
          INV=-1
        ELSE
          INV=1
        ENDIF

        LO=X(1)-REAL(INV)*W(1)/2.0
        HI=X(N)+REAL(INV)*W(N)/2.0

      ENDIF
      END



*+
      SUBROUTINE GDRAW_GET2DRANGE(NX,NY,IX1,IX2,IY1,IY2,Z,ZMIN,ZMAX,
     :                                                       STATUS)

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
*    Import :
      INTEGER NX,NY
      INTEGER IX1,IX2,IY1,IY2
      REAL Z(NX,NY)
*    Import-export :
*    Export :
      REAL ZMIN,ZMAX
*    Status :
      INTEGER STATUS
*    Local Constants :
*    Local variables :
      INTEGER I,J
*-

      IF (STATUS.EQ.SAI__OK) THEN

        ZMIN=Z(IX1,IY1)
        ZMAX=ZMIN

        DO J=IY1,IY2
          DO I=IX1,IX2
            ZMIN=MIN(ZMIN,Z(I,J))
            ZMAX=MAX(ZMAX,Z(I,J))
          ENDDO
        ENDDO


        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GDRAW_GET2DRANGE',STATUS)
        ENDIF
      ENDIF
      END



*+
      SUBROUTINE GDRAW_GETATT(LOC,ATTOK,UNITS,RA,DEC,ROLL,STATUS)

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
*    Import :
      CHARACTER*(DAT__SZLOC) LOC
*    Import-export :
*    Export :
      LOGICAL ATTOK
      CHARACTER*(*) UNITS
      DOUBLE PRECISION RA,DEC,ROLL
*    Status :
      INTEGER STATUS
*    Local Constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) HLOC
      LOGICAL RAOK,DECOK,PAOK
      LOGICAL HOK
*-


      IF (STATUS.EQ.SAI__OK) THEN

        ATTOK=.FALSE.

*  check header components
        CALL BDA_CHKHEAD(LOC,HOK,STATUS)
        IF (HOK) THEN
          CALL BDA_LOCHEAD(LOC,HLOC,STATUS)
          CALL HDX_OK(HLOC,'AXIS_RA',RAOK,STATUS)
          CALL HDX_OK(HLOC,'AXIS_DEC',DECOK,STATUS)
          CALL HDX_OK(HLOC,'POSITION_ANGLE',PAOK,STATUS)
          IF (RAOK.AND.DECOK.AND.PAOK) THEN
            CALL BDA_GETAXUNITS(LOC,1,UNITS,STATUS)
            CALL CMP_GET0D(HLOC,'POSITION_ANGLE',ROLL,STATUS)
            CALL CMP_GET0D(HLOC,'AXIS_DEC',DEC,STATUS)
            CALL CMP_GET0D(HLOC,'AXIS_RA',RA,STATUS)
            ATTOK=.TRUE.
          ENDIF

        ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GDRAW_GETATT',STATUS)
        ENDIF
      ENDIF
      END




*+  GDRAW_MULTI - draw multiple plots
      SUBROUTINE GDRAW_MULTI(STATUS)

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
*    Import :
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Local Constants :
      INTEGER MXOVLY
      PARAMETER (MXOVLY=100)
*    Global variables :
      INCLUDE 'GFX_CMN'
      INCLUDE 'GMD_CMN'
*    Local variables :
      CHARACTER*(DAT__SZLOC) GLOC		! locator to indivual graph
      CHARACTER*132 OVLY
      REAL X1,X2,Y1,Y2
      INTEGER NNDF
      INTEGER NPLOT
      INTEGER IP,NP
      INTEGER NX,NY
      INTEGER IGRAF,JGRAF
      INTEGER NOVLY
      INTEGER OVLYS(MXOVLY)
      INTEGER BASE
      LOGICAL DEF
      LOGICAL OVER			! whether graph is an overlay
*-
      IF (STATUS.EQ.SAI__OK) THEN


*  find out how many datasets
        CALL GMD_QNDF(G_MLOC,NNDF,STATUS)
*  see if combination plots are defined
        CALL GMD_QPLOTS(G_MLOC,NPLOT,STATUS)
        IF (NPLOT.EQ.0) THEN
          NP=NNDF
          DEF=.TRUE.
        ELSE
          NP=NPLOT
          DEF=.FALSE.
        ENDIF


* set layout of plots (may be overidden by positioning of individual graph)
        CALL GDRAW_MULTI_LAYOUT(G_MLOC,NP,NX,NY,STATUS)


* draw each plot
        DO IP=1,NP

*  get default position of plot
          CALL GDRAW_MULTI_DEFPOS(IP,NX,NY,X1,X2,Y1,Y2,STATUS)

*  default is one graph per plot
          IF (DEF) THEN
            NOVLY=0
            BASE=IP

*  otherwise get description of plot from dataset
          ELSE
            CALL GMD_GETPLOT(G_MLOC,IP,BASE,OVLY,STATUS)
            CALL PRS_PRSLIST(OVLY,MXOVLY,OVLYS,NOVLY,STATUS)
          ENDIF

*  draw each graph in plot
          DO IGRAF=0,NOVLY

*  base graph
            IF (IGRAF.EQ.0) THEN
              JGRAF=BASE
              OVER=.FALSE.
*  overlay
            ELSE
              JGRAF=OVLYS(IGRAF)
*  check for base duplicated in overlays
              IF (JGRAF.EQ.BASE) THEN
                JGRAF=0
              ENDIF
              OVER=.TRUE.
            ENDIF

            IF (JGRAF.GT.0.AND.JGRAF.LE.NNDF) THEN
*  locate dataset, determine type, load Control Block, plot
              CALL GMD_LOCNDF(G_MLOC,JGRAF,GLOC,STATUS)
              CALL GFX_NDFTYPE(GLOC,STATUS)
              CALL GCB_LOAD(GLOC,STATUS)
              IF (G_NDF1) THEN
                CALL GDRAW_1DGRAF_INT(GLOC,X1,X2,Y1,Y2,OVER,STATUS)
              ELSEIF (G_NDF2) THEN
                CALL GDRAW_2DGRAF_INT(GLOC,X1,X2,Y1,Y2,OVER,STATUS)
              ENDIF
              CALL BDA_RELEASE(GLOC,STATUS)
              CALL DAT_ANNUL(GLOC,STATUS)
            ENDIF


          ENDDO
        ENDDO

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GDRAW_MULTI',STATUS)
        ENDIF
      ENDIF
      END



*+
      SUBROUTINE GDRAW_MULTI_LAYOUT(LOC,NPLOT,NX,NY,STATUS)

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
*    Import :
      CHARACTER*(DAT__SZLOC) LOC
      INTEGER NPLOT
*    Import-export :
*    Export :
      INTEGER NX,NY
*    Status :
      INTEGER STATUS
*    Local Constants :
*    Global variables :
*    Local variables :
      REAL SQ
      REAL REM
      LOGICAL OK
      LOGICAL XSET,YSET
*-
      IF (STATUS.EQ.SAI__OK) THEN

*  get layout spec. from dataset
        CALL GMD_GETLAYOUT(LOC,XSET,NX,YSET,NY,STATUS)

        IF (XSET.AND.YSET) THEN
          OK=(NPLOT.LE.(NX*NY))
        ELSEIF (XSET) THEN
          IF (NPLOT.LE.NX) THEN
            NY=NPLOT/NX
            IF (MOD(NPLOT,NX).GT.0) THEN
              NY=NY+1
            ENDIF
            OK=.TRUE.
          ELSE
            OK=.FALSE.
          ENDIF
        ELSEIF (YSET) THEN
          IF (NPLOT.LE.NY) THEN
            NX=NPLOT/NY
            IF (MOD(NPLOT,NY).GT.0) THEN
              NX=NX+1
            ENDIF
            OK=.TRUE.
          ELSE
            OK=.FALSE.
          ENDIF
        ELSE
          OK=.FALSE.
        ENDIF

* if nothing sensible make suitable default layout
        IF (.NOT.OK) THEN
          IF (NPLOT.EQ.2) THEN
            NX=1
            NY=2
          ELSE
            SQ=SQRT(REAL(NPLOT))
            NX=INT(SQ)
            NY=NX
            REM=SQ-REAL(NX)
            IF (REM.GT.0.00001) THEN
              NX=NX+1
              IF (REM.GT.0.5) THEN
                NY=NY+1
              ENDIF
            ENDIF
          ENDIF
        ENDIF


        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GDRAW_MULTI_LAYOUT',STATUS)
        ENDIF
      ENDIF
      END



*+
      SUBROUTINE GDRAW_MULTI_DEFPOS(IPLOT,NX,NY,X1,X2,Y1,Y2,STATUS)

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
*    Import :
      INTEGER IPLOT
      INTEGER NX,NY
*    Import-export :
*    Export :
      REAL X1,X2,Y1,Y2
*    Status :
      INTEGER STATUS
*    Local Constants :
*    Global variables :
*    Local variables :
      REAL XCELL
      REAL YCELL
      INTEGER IX,IY
*-
      IF (STATUS.EQ.SAI__OK) THEN

        IF (MOD(IPLOT,NX).EQ.0) THEN
          IY=IPLOT/NX
        ELSE
          IY=IPLOT/NX+1
        ENDIF
        IX=MOD(IPLOT+IY-1,NX+1)
        XCELL=1.0/REAL(NX)
        YCELL=1.0/REAL(NY)
        X1=(IX-1)*XCELL
        X2=IX*XCELL
        Y1=1.0-IY*YCELL
        Y2=1.0-(IY-1)*YCELL

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GDRAW_MULTI_DEFPOS',STATUS)
        ENDIF
      ENDIF
      END






*+  GDRAW_STAMP - put user-specified stamp on plot
      SUBROUTINE GDRAW_STAMP(LOC,STATUS)
*    Description :
*    Method :
*    Deficiencies :
*    Authors :
*            BHVAD::RJV
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Global variables :
*    Structure definitions :
*    Import :
      CHARACTER*(DAT__SZLOC) LOC
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      CHARACTER*80 TEXT
      LOGICAL FOK,TOK
      LOGICAL FLAG
      REAL X,Y
*-
      IF (STATUS.EQ.SAI__OK) THEN

        CALL GCB_GETL('STAMP_FLAG',FOK,FLAG,STATUS)
        IF (.NOT.FOK) THEN
          FLAG=.FALSE.
        ENDIF

        IF (FLAG) THEN

          CALL GCB_GETC('STAMP_TEXT',TOK,TEXT,STATUS)

          IF (TOK) THEN

            CALL GCB_SETDEF(STATUS)

*  select whole display surface
            CALL PGVPORT(0.0,1.0,0.0,1.0)
            CALL PGWINDOW(0.0,1.0,0.0,1.0)
            X=1.0-0.75/40.0
            Y=0.5
*  plot text
            CALL PGSCH(0.75)
            CALL PGPTEXT(X,Y,270.0,0.5,TEXT)

          ELSE
*  no text specified - default to ID, time and date
            CALL PGIDEN()
          ENDIF

          CALL GCB_SETDEF(STATUS)
          CALL GTR_RESTORE(STATUS)

        ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GDRAW_STAMP',STATUS)
        ENDIF

      ENDIF

      END
