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
*      6 Dec 94 : V1.8-1  Asymmetric widths for stepped line (RJV)
*      9 Jan 95 : V1.8-2  Double checks for regular axis values (RJV)
*     11 Sep 95 : V2.0-0  Full ADI port (DJA)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS
*    Functions :
*    Local Constants :
      CHARACTER*30 VERSION
      PARAMETER (VERSION='GDRAW Version 2.0-0')
*    Local variables :
      CHARACTER*20 DEV
      CHARACTER*132		FILE, PATH
      INTEGER                   IFID                    ! Input dataset

      INTEGER NLEV
      LOGICAL CLEAR
      LOGICAL FRESH
      LOGICAL ID
      LOGICAL ACTIVE
*    Global variables :
      INCLUDE 'GFX_CMN'
      INCLUDE 'GMD_CMN'
*-

*  Version id
      CALL MSG_PRNT(VERSION)
      CALL MSG_BLNK()

*  Connect Grafix Control Block
      IF ( G_OPEN ) THEN
	CALL GCB_ATTACH('GRAFIX',STATUS)
      ELSE
	CALL GCB_ATTACH('LOCAL',STATUS)
      ENDIF

*  Initialise flags
      ID = .FALSE.

*  Get dataset if not already open
      IF (.NOT.G_OPEN) THEN
	CALL AST_INIT()
	CALL USI_ASSOC('INP','MultiGraph|BinDS','READ',IFID,STATUS)

*  Otherwise get identifier to open dataset
      ELSE
	CALL USI_INIT()
	IF (G_MULTI) THEN
          IFID = G_MFID
	ELSE
          IFID = G_ID
	ENDIF
      ENDIF

      IF (STATUS.EQ.SAI__OK) THEN


*  display dataset name
        CALL ADI_FTRACE( IFID, NLEV, PATH, FILE, STATUS )
	CALL MSG_PRNT('Dataset: '//FILE )
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
	CALL GDRAW_SUB( IFID, STATUS )

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
	CALL USI_ANNUL( 'INP', STATUS )

	CALL GCB_DETACH(STATUS)

	CALL AST_CLOSE()

      ELSE

	CALL USI_CLOSE()

      ENDIF

      END


*+  GDRAW_SUB
      SUBROUTINE GDRAW_SUB(ID,STATUS)

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
*    Import :
      INTEGER                   ID
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

	CALL GMI_QMULT( ID, MULTI, STATUS )
*  multiple dataset
	IF (MULTI) THEN
	  G_MFID = ID
	  G_MULTI = .TRUE.

*  single NDF
	ELSE
	  CALL GFX_NDFTYPE(ID,STATUS)
	  G_ID = ID
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
	  CALL GCB_FLOAD(G_ID,STATUS)
	ENDIF

*  now plot it
	CALL GDRAW_1DGRAF_INT(G_ID,X1,X2,Y1,Y2,OVLY,STATUS)


	IF (STATUS.NE.SAI__OK) THEN
	  CALL AST_REXIT( 'GDRAW_1DGRAF',STATUS)
	ENDIF
      ENDIF
      END


*+  GDRAW_1DGRAF_INT
      SUBROUTINE GDRAW_1DGRAF_INT(ID,X1,X2,Y1,Y2,OVLY,STATUS)

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
*    Import :
      INTEGER                           ID
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
      CHARACTER*80 TITLE,LABEL,UNITS
      CHARACTER*80 XLABEL,YLABEL
      REAL XW1,XW2,YW1,YW2                      ! world coords of inner window
      INTEGER NVAL                              ! number of data points
      INTEGER DPTR                              ! pointer to data
      INTEGER VPTR                              ! pointer to variance
      INTEGER QPTR                              ! pointer to quality
      INTEGER APTR                              ! pointer to axis values
      INTEGER WPTR                              ! pointer to axis width
      INTEGER ASXPTR(2)                         ! asymmetric axis widths
      INTEGER ASYPTR(2)
      INTEGER			IDUM
      LOGICAL VOK                               ! variance OK
      LOGICAL XERROK,YERROK
      LOGICAL AERR                              ! asymmetric errors
      LOGICAL QOK                               ! QUALITY OK
      LOGICAL XLOG,YLOG                         ! if axes logarithmic
      LOGICAL POLY,STEP,POINTS,ERRS             ! plotting style
      LOGICAL ABS                               ! if plot pos. in abs units
      LOGICAL SCALED                            ! if axes scaled evenly
      LOGICAL WOK,AWID,CHK(2)

      BYTE MASK
*-

      IF (STATUS.EQ.SAI__OK) THEN

*  get pointer to data
	CALL BDI_MAPR( ID, 'Data', 'READ', DPTR, STATUS )

*  take number of values from data
	CALL BDI_GETNEL( ID, NVAL, STATUS )

*  check for QUALITY
	CALL BDI_CHK( ID, 'Quality', QOK, STATUS )
	IF ( QOK ) THEN
	  CALL BDI_MAP( ID, 'Quality', 'UBYTE', 'READ', QPTR, STATUS )
	  CALL BDI_GET( ID, 'QualityMask', 'UBYTE', 0, 0, MASK, IDUM,
     :                  STATUS )
	ENDIF

*  map x-axis values
	CALL BDI_MAPR( ID, 'AXIS_1_Data', 'READ', APTR, STATUS )

*  check errors
	CALL BDI_CHK( ID, 'Variance', VOK, STATUS )
	CALL BDI_CHK( ID, 'Axis_1_LoWidth,Axis_1_HiWidth', CHK, STATUS )
        XERROK = CHK(1).AND.CHK(2)
	CALL BDI_CHK( ID, 'LoError,HiError', CHK, STATUS )
        YERROK = CHK(1).AND.CHK(2)
	AERR=(YERROK.OR.XERROK)

*  get default title and labels
	CALL BDI_GET0C( ID, 'Title', TITLE, STATUS )
	CALL BDI_GET0C( ID, 'Axis_1_Label', LABEL, STATUS )
	CALL BDI_GET0C( ID, 'Axis_1_Units', UNITS, STATUS )
	CALL GFX_DEFLBL(LABEL,UNITS,XLABEL,STATUS)
	CALL BDI_GET0C( ID, 'Label', LABEL, STATUS )
	CALL BDI_GET0C( ID, 'Units', UNITS, STATUS )
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
	IF ( ERRS ) THEN
	  IF ( AERR ) THEN
	    CALL BDI_MAPR( ID, 'Axis_1_LoWidth,Axis_1_HiWidth',
     :                     'READ', ASXPTR, STATUS )
	    CALL BDI_MAPR( ID, 'LoError,HiError', 'READ', ASYPTR,
     :                     STATUS )
	  ELSE
	    CALL BDI_MAPR( ID, 'Variance', 'READ', VPTR, STATUS )
	    CALL BDI_MAPR( ID, 'Axis_1_Width', 'READ', WPTR, STATUS )
	  ENDIF
	ENDIF

*  if step-line need axis widths
	IF (STEP) THEN
	  CALL BDI_CHK( ID, 'Axis_1_Width', WOK, STATUS )
	  IF ( WOK ) THEN
	    CALL BDI_MAPR( ID, 'Axis_1_Width', 'READ', WPTR, STATUS )
	    AWID = .FALSE.
	  ELSE
	    CALL BDI_MAPR( ID, 'Axis_1_LoWidth,Axis_1_HiWidth',
     :                     'READ', ASXPTR, STATUS )
	    AWID=.TRUE.
	  ENDIF
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
	  IF (AWID) THEN
	    IF (QOK) THEN
	      CALL GFX_ASTEPQ(NVAL,1,NVAL,%val(APTR),%val(ASXPTR(1)),
     :                         %val(ASXPTR(2)),%val(DPTR),%val(QPTR),
     :                                              MASK,STATUS)
	    ELSE
	      CALL GFX_ASTEP(NVAL,1,NVAL,%val(APTR),%val(ASXPTR(1)),
     :                           %val(ASXPTR(2)),%val(DPTR),STATUS)
	    ENDIF
	  ELSE
	    IF (QOK) THEN
	      CALL GFX_STEPQ(NVAL,1,NVAL,%val(APTR),%val(WPTR),
     :                                   %val(DPTR),%val(QPTR),
     :                                            MASK,STATUS)
	    ELSE
	      CALL GFX_STEP(NVAL,1,NVAL,%val(APTR),%val(WPTR),
     :                                      %val(DPTR),STATUS)
	    ENDIF
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
	      CALL GFX_AERRQ(NVAL,1,NVAL,%val(APTR),%val(ASXPTR(1)),
     :                                   %val(ASXPTR(2)),%val(DPTR),
     :                              %val(ASYPTR(1)),%val(ASYPTR(2)),
     :                                   %val(QPTR),MASK,STATUS)
	    ELSE
	      CALL GFX_AERR(NVAL,1,NVAL,%val(APTR),%val(ASXPTR(1)),
     :                                   %val(ASXPTR(2)),%val(DPTR),
     :                              %val(ASYPTR(1)),%val(ASYPTR(2)),
     :                                                   STATUS)
	    ENDIF
	  ELSE
	    IF (QOK) THEN
	      CALL GFX_ERRQ(NVAL,1,NVAL,%val(APTR),%val(WPTR),
     :                                  %val(DPTR),%val(VPTR),
     :                                  %val(QPTR),MASK,STATUS)
	    ELSE
	      CALL GFX_ERR(NVAL,1,NVAL,%val(APTR),%val(WPTR),
     :                         %val(DPTR),%val(VPTR), STATUS )
	    ENDIF
	  ENDIF
	ENDIF

*  put on annotation
	CALL GFX_NOTES(STATUS)
	CALL GFX_SHAPES(STATUS)
	CALL GFX_MARKS(STATUS)
	CALL GFX_STAMP(STATUS)

	IF (STATUS.NE.SAI__OK) THEN
	  CALL AST_REXIT( 'GDRAW_1DGRAF_INT', STATUS )
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
      CHARACTER*80 TITLE,XLBL,YLBL,UNIT
      INTEGER NDIM,DIMS(2),DIML(2),DIMU(2)
      INTEGER ISET,NSET
      INTEGER NVAL,VPTR
      INTEGER DPTR,APTR,QPTR,WPTR,XLPTR,XUPTR,YLPTR,YUPTR
      INTEGER L1,L2,SDPTR,SVPTR,SQPTR,IDUM
      REAL X1,X2,Y1,Y2,YDIFF
      REAL XMIN,XMAX,YMIN,YMAX
      LOGICAL POLY,STEP,POINTS,ERRS             ! plotting style
      LOGICAL QOK,SCALED,VOK,XERROK,YERROK,AERR,AERROK(2)
      BYTE MASK
*-

      IF (STATUS.EQ.SAI__OK) THEN

	IF (.NOT.G_OPEN) THEN
	  CALL GCB_FLOAD(G_ID,STATUS)
	ENDIF

*  get number of sets and number of points in each set
        CALL BDI_GETSHP( G_ID, 2, DIMS, NDIM, STATUS )
	NSET=DIMS(2)
	NVAL=DIMS(1)

*  map data
        CALL BDI_MAPR( G_ID, 'Data', 'READ', DPTR, STATUS )

*  map common axis and get range
        CALL BDI_AXMAPR( G_ID, 1, 'Data', 'READ', APTR, STATUS )

*  check QUALITY
        CALL BDI_CHK( G_ID, 'Quality', QOK, STATUS )
	IF ( QOK ) THEN
          CALL BDI_MAP( G_ID, 'Quality', 'UBYTE', 'READ', QPTR, STATUS )
	  CALL BDI_GET( G_ID, 'QualityMask', 'UBYTE', 0, 0, MASK, IDUM,
     :                  STATUS )
	ENDIF

*  check errors
        CALL BDI_AXCHK( G_ID, 1, 'LoWidth,HiWidth', AERROK, STATUS )
        XERROK = (AERROK(1).AND.AERROK(2))
        CALL BDI_CHK( G_ID, 'Variance', VOK, STATUS )
        IF ( VOK ) THEN
          CALL BDI_MAPR( G_ID, 'Variance', 'READ', VPTR, STATUS )
        ELSE
          CALL BDI_CHK( G_ID, 'LoError,HiError', AERROK, STATUS )
          YERROK = (AERROK(1).AND.AERROK(2))
	  AERR=(YERROK.OR.XERROK)
        END IF

*  get labels
        CALL BDI_GET0C( G_ID, 'Title', TITLE, STATUS )
        CALL BDI_AXGET0C( G_ID, 2, 'Label', YLBL, STATUS )
        CALL BDI_AXGET0C( G_ID, 1, 'Label', XLBL, STATUS )
        CALL BDI_AXGET0C( G_ID, 1, 'Units', UNIT, STATUS )
	IF ( UNIT .NE. ' ' ) THEN
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
	IF ( ERRS ) THEN
	  IF ( AERR ) THEN
            CALL BDI_AXMAPR( G_ID, 1, 'LoWidth', XLPTR, STATUS )
            CALL BDI_AXMAPR( G_ID, 1, 'HiWidth', XUPTR, STATUS )
            CALL BDI_MAPR( G_ID, 'LoError', YLPTR, STATUS )
            CALL BDI_MAPR( G_ID, 'HiError', YUPTR, STATUS )
	  ELSE
            CALL BDI_AXMAPR( G_ID, 1, 'Width', 'READ', WPTR, STATUS )
	  END IF
	END IF

*  make buffers for data, variance and quality
        CALL DYN_MAPR( 1, NVAL, SDPTR, STATUS )
        IF ( VOK ) THEN
          CALL DYN_MAPR( 1, NVAL, SVPTR, STATUS )
        END IF
        IF ( QOK ) THEN
          CALL DYN_MAPB( 1, NVAL, SQPTR, STATUS )
        END IF

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

*  Copy data into slices
          CALL ARR_SLCOPR( NDIM, DIMS, %VAL(DPTR), DIML, DIMU,
     :                     %VAL(SDPTR), STATUS )

*  and quality if present
	  IF (QOK) THEN
            CALL ARR_SLCOPB( NDIM, DIMS, %VAL(QPTR), DIML, DIMU,
     :                       %VAL(SQPTR), STATUS )
	  ENDIF

*  get variance
	  IF ( ERRS .AND. .NOT. AERR ) THEN
            CALL ARR_SLCOPR( NDIM, DIMS, %VAL(VPTR), DIML, DIMU,
     :                       %VAL(SVPTR), STATUS )
	  END IF

*  get default axis ranges
	  IF (QOK) THEN
	    CALL GFX_DEF1DWNDQ(NVAL,%val(APTR),%val(SDPTR),
     :               %val(SQPTR),MASK,XMIN,XMAX,YMIN,YMAX,STATUS)
	  ELSE
	    CALL GFX_DEF1DWND(NVAL,%val(APTR),%val(SDPTR),
     :                             XMIN,XMAX,YMIN,YMAX,STATUS)
	  ENDIF

*  set coordinate transformation
*  set axis ranges
	  CALL GFX_WINDOW(XMIN,XMAX,YMIN,YMAX,SCALED,STATUS)

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
	    CALL GFX_POLYQ(NVAL,1,NVAL,%val(APTR),%val(SDPTR),
     :                                 %val(SQPTR),MASK,STATUS)
	  ELSE
	    CALL GFX_POLY(NVAL,1,NVAL,%val(APTR),%val(SDPTR),
     :                                                 STATUS)
	  ENDIF
	ENDIF

*  stepped line
	IF (STEP) THEN
	  IF (QOK) THEN
	    CALL GFX_STEPQ(NVAL,1,NVAL,%val(APTR),%val(WPTR),
     :                                 %val(SDPTR),%val(SQPTR),
     :                                          MASK,STATUS)
	  ELSE
	    CALL GFX_STEP(NVAL,1,NVAL,%val(APTR),%val(WPTR),
     :                                    %val(SDPTR),STATUS)
	  ENDIF
	ENDIF

*  point marker
	IF (POINTS) THEN
	  IF (QOK) THEN
	    CALL GFX_POINTQ(NVAL,1,NVAL,%val(APTR),%val(SDPTR),
     :                                  %val(SQPTR),MASK,STATUS)
	  ELSE
	    CALL GFX_POINT(NVAL,1,NVAL,%val(APTR),%val(SDPTR),STATUS)
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
     :                                  %val(SDPTR),%val(SVPTR),
     :                                  %val(SQPTR),MASK,STATUS)
	    ELSE
	      CALL GFX_ERR(NVAL,1,NVAL,%val(APTR),%val(WPTR),
     :                                  %val(SDPTR),%val(SVPTR),
     :                                                 STATUS)
	    ENDIF
	  ENDIF
	ENDIF

*  put on annotation
	  CALL GFX_NOTES(STATUS)
	  CALL GFX_SHAPES(STATUS)
	  CALL GFX_MARKS(STATUS)

	END DO

*  release buffers
        CALL DYN_UNMAP( SDPTR, STATUS )
        IF ( VOK ) THEN
          CALL DYN_UNMAP( SVPTR, STATUS )
        END IF
        IF ( QOK ) THEN
          CALL DYN_UNMAP( SQPTR, STATUS )
        END IF

*  put labels on
	Y1=0.1
	CALL PGVPORT(X1,X2,Y1,Y2)
	CALL PGLABEL(XLBL,YLBL,' ')
	CALL PGMTEXT('T',0.2,0.5,0.5,TITLE)

	IF (STATUS.NE.SAI__OK) THEN
	  CALL AST_REXIT('GDRAW_1DSET',STATUS)
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
	  CALL GCB_FLOAD(G_ID,STATUS)
	ENDIF

*  now plot it
	CALL GDRAW_2DGRAF_INT(G_ID,X1,X2,Y1,Y2,OVLY,STATUS)

	IF (STATUS.NE.SAI__OK) THEN
	  CALL AST_REXIT('GDRAW_2DGRAF',STATUS)
	ENDIF
      ENDIF
      END


*+  GDRAW_2DGRAF_INT
      SUBROUTINE GDRAW_2DGRAF_INT(ID,X1,X2,Y1,Y2,OVLY,STATUS)

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
*    Import :
      INTEGER                   ID
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
      CHARACTER*80 LABEL,XUNITS,YUNITS,TITLE,XLABEL,YLABEL
      REAL XW1,XW2,YW1,YW2
      REAL MIN,MAX
      REAL DMIN,DMAX
      REAL XBASE,XSCALE,YBASE,YSCALE
      INTEGER NDIM                              ! dimensionality
      INTEGER XDIM,YDIM
      INTEGER DIMS(2)                           ! dimensions
      INTEGER DPTR                              ! pointer to data
      INTEGER QPTR                              ! pointer to quality
      INTEGER XPTR                              ! pointer to x-axis values
      INTEGER YPTR                              ! pointer to y-axis values
      INTEGER XWPTR                             ! pointer to axis width
      INTEGER YWPTR                             ! pointer to axis width
      INTEGER IX1,IX2,IY1,IY2
      BYTE MASK                                 ! QUALITY mask
      INTEGER		IDUM
      INTEGER			PIXID,PRJID,SYSID	! Astrometry info
      LOGICAL XOK,YOK
      LOGICAL XREG,YREG,REG
      LOGICAL QOK                               ! QUALITY OK
      LOGICAL PIXEL,CONTOUR,SURFACE
      LOGICAL ABS
      LOGICAL SCALED
      LOGICAL RADEC
      LOGICAL ATTOK
*    Global variables :
      INCLUDE 'GMD_CMN'
*-

      IF (STATUS.EQ.SAI__OK) THEN

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
	CALL BDI_MAPR( ID, 'Data', 'READ', DPTR, STATUS )
        IF ( STATUS .NE. SAI__OK ) RETURN

*  Get dimensions
	CALL BDI_GETSHP( ID, 2, DIMS, NDIM, STATUS )
        XDIM = DIMS(1)
        YDIM = DIMS(2)

*  map x-axis values
        CALL BDI_CHK( ID, 'Axis_1_Data', XOK, STATUS )
	IF ( .NOT. XOK ) THEN
	  XREG=.TRUE.
          XBASE=1.0
          XSCALE=1.0
          CALL DYN_MAPR( 1, XDIM, XPTR, STATUS )
          CALL ARR_REG1R( XBASE, XSCALE, XDIM, %VAL(XPTR), STATUS )
	ELSE
	  CALL BDI_MAPR( ID, 'Axis_1_Data', 'READ', XPTR, STATUS )
	  CALL ARR_CHKREG(%val(XPTR),XDIM,XREG,XBASE,XSCALE,STATUS)
	END IF

*  map y-axis values
        CALL BDI_CHK( ID, 'Axis_2_Data', YOK, STATUS )
	IF ( .NOT. YOK ) THEN
	  YREG=.TRUE.
          YBASE=1.0
          YSCALE=1.0
          CALL DYN_MAPR( 1, YDIM, YPTR, STATUS )
          CALL ARR_REG1R( YBASE, YSCALE, YDIM, %VAL(YPTR), STATUS )
	ELSE IF ( .NOT. YREG ) THEN
	  CALL BDI_MAPR( ID, 'Axis_2_Data', 'READ', YPTR, STATUS )
	  CALL ARR_CHKREG(%val(YPTR),YDIM,YREG,YBASE,YSCALE,STATUS)
	ENDIF

	REG=(XREG.AND.YREG)

*  width needed for irregular image and pixel overlays
	IF (.NOT.REG.OR.(PIXEL.AND.OVLY)) THEN
          CALL BDI_AXMAPR( ID, 1, 'Width', 'READ', XWPTR, STATUS )
          CALL BDI_AXMAPR( ID, 2, 'Width', 'READ', YWPTR, STATUS )
	ENDIF

*  get default labels and title
	CALL BDI_GET0C( ID, 'Title', TITLE, STATUS )
	CALL BDI_AXGET0C( ID, 1, 'Label', LABEL, STATUS )
	CALL BDI_AXGET0C( ID, 1, 'Units', XUNITS, STATUS )
	CALL GFX_DEFLBL(LABEL,XUNITS,XLABEL,STATUS)
	CALL BDI_AXGET0C( ID, 2, 'Label', LABEL, STATUS )
	CALL BDI_AXGET0C( ID, 2, 'Units', YUNITS, STATUS )
	CALL GFX_DEFLBL(LABEL,YUNITS,YLABEL,STATUS)

*  decide whether axes are to be scaled equally
	SCALED=(XUNITS.EQ.YUNITS.AND.XUNITS.NE.' ')

*  check for QUALITY
        CALL BDI_CHK( ID, 'Quality', QOK, STATUS )
	IF ( QOK ) THEN
          CALL BDI_MAP( ID, 'Quality', 'UBYTE', 'READ', QPTR, STATUS )
	  CALL BDI_GET( ID, 'QualityMask', 'UBYTE', 0, 0, MASK, IDUM,
     :                  STATUS )
	END IF

*  get attitude information
	CALL GDRAW_GETATT(ID,ATTOK,PIXID,PRJID,SYSID,STATUS)

	IF (.NOT.OVLY) THEN

*  set colours for pixel plot
	  IF (PIXEL) THEN
	    CALL GFX_SETCOLS(STATUS)
	  ENDIF

*  set axis ranges
	  CALL GDRAW_GET2DWND(ID,DIMS,XREG,XBASE,XSCALE,%VAL(XPTR),
     :                        %VAL(XWPTR), YREG,YBASE,YSCALE,%VAL(YPTR),
     :                        %VAL(YWPTR), XW1,XW2,YW1,YW2, STATUS )

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
	CALL GDRAW_GET2DSLICE(ID,DIMS,XREG,XBASE,XSCALE,%VAL(XPTR),
     :                        %VAL(XWPTR),YREG,YBASE,YSCALE,%VAL(YPTR),
     :                        %VAL(YWPTR), XW1,XW2,YW1,YW2,
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
	      CALL GFX_RADEC(PIXID,PRJID,SYSID,STATUS)
	      CALL GFX_LABELS('Right Ascension','Declination',STATUS)
*  normal axes
	    ELSE
	      CALL GFX_AXES(STATUS)
	      CALL GFX_LABELS(XLABEL,YLABEL,STATUS)
	    END IF

*  titles at top of plot
	    CALL GFX_TITLE(TITLE,STATUS)

*  coordinate grid
	    IF ( ATTOK ) THEN
	      CALL GFX_TMAT( PIXID, PRJID, SYSID, STATUS )
	      CALL GFX_GRID( GFX_XYTOSKY, GFX_SKYTOXY, STATUS )
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

*    Release dynamic axes
        IF ( .NOT. XOK ) THEN
          CALL DYN_UNMAP(XPTR,STATUS)
        END IF
        IF ( .NOT. YOK ) THEN
          CALL DYN_UNMAP(YPTR,STATUS)
        END IF

	IF (STATUS.NE.SAI__OK) THEN
	  CALL AST_REXIT('GDRAW_2DGRAF_INT',STATUS)
	ENDIF

      ENDIF
      END



*+  GDRAW_GET2DWND - get default 2D axis ranges for base plot
      SUBROUTINE GDRAW_GET2DWND(ID,DIMS,XREG,XBASE,XSCALE,XVAL,XWID,
     :                                  YREG,YBASE,YSCALE,YVAL,YWID,
     :                                  XW1,XW2,YW1,YW2, STATUS )

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
*    Import :
      INTEGER ID,DIMS(2)
      LOGICAL XREG,YREG
      REAL	XBASE,XSCALE,YBASE,YSCALE
      REAL       XVAL(*),XWID(*),YVAL(*),YWID(*)
*    Import-export :
*    Export :
      REAL XW1,XW2,YW1,YW2
*    Status :
      INTEGER STATUS
*    Local variables :
      REAL LO,HI
      REAL HALFWID
*-

      IF (STATUS.EQ.SAI__OK) THEN

	IF (XREG) THEN

	  HALFWID=XSCALE/2.0
	  LO=XBASE-HALFWID
	  HI=XBASE+(DIMS(1)-1)*XSCALE+HALFWID

	ELSE
	  CALL GDRAW_GET2DWND_IRREG(XVAL,XWID,DIMS(1),LO,HI,STATUS)
	ENDIF

	XW1=LO
	XW2=HI

	IF (YREG) THEN

	  HALFWID=YSCALE/2.0
	  LO=YBASE-HALFWID
	  HI=YBASE+(DIMS(2)-1)*YSCALE+HALFWID

	ELSE
	  CALL GDRAW_GET2DWND_IRREG(YVAL,YWID,DIMS(2),LO,HI,STATUS)
	ENDIF

	YW1=LO
	YW2=HI

	IF (STATUS.NE.SAI__OK) THEN
	  CALL AST_REXIT('GDRAW_GET2DWND',STATUS)
	ENDIF
      ENDIF
      END

*+
      SUBROUTINE GDRAW_GET2DSLICE(ID,DIMS,XREG,XBASE,XSCALE,XVAL,XWID,
     :                            YREG,YBASE,YSCALE,YVAL,YWID,
     :                                 XW1,XW2,YW1,YW2,
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
*    Import :
      INTEGER			ID			! Top level object
      INTEGER DIMS(2)
      LOGICAL XREG,YREG
      REAL	XBASE,XSCALE,YBASE,YSCALE
      REAL       XVAL(*),XWID(*),YVAL(*),YWID(*)
      REAL XW1,XW2,YW1,YW2
*    Import-export :
*    Export :
      INTEGER IX1,IX2,IY1,IY2
*    Status :
      INTEGER STATUS
*    Local Constants :
*    Local variables :
*-

      IF (STATUS.EQ.SAI__OK) THEN

*  get array indices of relevant slice of array
	IF (XREG) THEN
	  IX1=NINT((XW1-XBASE)/XSCALE)+1
	ELSE
	  CALL GDRAW_GET2DSLICE_SUB(XVAL,XWID,DIMS(1), XW1,IX1,STATUS)
	ENDIF

	IF (IX1.LT.1.OR.IX1.GT.DIMS(1)) THEN
	  IX1=1
	ENDIF

	IF (XREG) THEN
	  IX2=NINT((XW2-XBASE)/XSCALE)+1
	ELSE
	  CALL GDRAW_GET2DSLICE_SUB(XVAL,XWID,DIMS(1),XW2,IX2,STATUS)
	ENDIF

	IF (IX2.GT.DIMS(1).OR.IX2.LT.1) THEN
	  IX2=DIMS(1)
	ENDIF

	IF (YREG) THEN
	  IY1=NINT((YW1-YBASE)/YSCALE)+1
	ELSE
	  CALL GDRAW_GET2DSLICE_SUB(YVAL,YWID,DIMS(2),YW1,IY1,STATUS)
	ENDIF

	IF (IY1.LT.1.OR.IY1.GT.DIMS(2)) THEN
	  IY1=1
	ENDIF

	IF (YREG) THEN
	  IY2=NINT((YW2-YBASE)/YSCALE)+1
	ELSE
	  CALL GDRAW_GET2DSLICE_SUB(YVAL,YWID,DIMS(2),YW2,IY2,STATUS)
	ENDIF

	IF (IY2.GT.DIMS(2).OR.IY2.LT.1) THEN
	  IY2=DIMS(2)
	ENDIF

	IF (STATUS.NE.SAI__OK) THEN
	  CALL AST_REXIT('GDRAW_GET2DSLICE',STATUS)
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
*    Import :
      INTEGER N                 ! number of values
      REAL X(*)                 ! axis values
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
*    Import :
      INTEGER N                 ! number of values
      REAL X(*)                 ! axis values
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

      ENDIF
      END



*+
      SUBROUTINE GDRAW_GETATT(ID,ATTOK,PIXID,PRJID,SYSID,STATUS)

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
*    Import :
      INTEGER			ID
*    Import-export :
*    Export :
      LOGICAL ATTOK
      INTEGER PIXID,PRJID,SYSID
*    Status :
      INTEGER STATUS
*-

*  Check status on entry
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Try to locate astrometry
      CALL WCI_GETIDS( ID, PIXID, PRJID, SYSID, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
        ATTOK = .TRUE.
      ELSE
        ATTOK = .FALSE.
        CALL ERR_ANNUL( STATUS )
      END IF

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
      INTEGER			GFID			! Individual graph
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
      LOGICAL OVER                      ! whether graph is an overlay
*-
      IF (STATUS.EQ.SAI__OK) THEN

*  find out how many datasets
	CALL GMI_QNDF(G_MFID,NNDF,STATUS)

*  see if combination plots are defined
	CALL GMI_QPLOTS(G_MFID,NPLOT,STATUS)
	IF (NPLOT.EQ.0) THEN
	  NP=NNDF
	  DEF=.TRUE.
	ELSE
	  NP=NPLOT
	  DEF=.FALSE.
	ENDIF

* set layout of plots (may be overidden by positioning of individual graph)
	CALL GDRAW_MULTI_LAYOUT(G_MFID,NP,NX,NY,STATUS)

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
	    CALL GMI_GETPLOT( G_MFID, IP, BASE, OVLY, STATUS )
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
	      CALL GMI_LOCNDF( G_MFID, JGRAF, 'BinDS', GFID, STATUS )
	      CALL GFX_NDFTYPE(GFID,STATUS)
	      CALL GCB_FLOAD(GFID,STATUS)
	      IF (G_NDF1) THEN
		CALL GDRAW_1DGRAF_INT(GFID,X1,X2,Y1,Y2,OVER,STATUS)
	      ELSEIF (G_NDF2) THEN
		CALL GDRAW_2DGRAF_INT(GFID,X1,X2,Y1,Y2,OVER,STATUS)
	      ENDIF
	      CALL ADI_ERASE(GFID,STATUS)
	    ENDIF

	  ENDDO
	ENDDO

	IF (STATUS.NE.SAI__OK) THEN
	  CALL AST_REXIT( 'GDRAW_MULTI',STATUS)
	ENDIF
      ENDIF
      END



*+
      SUBROUTINE GDRAW_MULTI_LAYOUT(ID,NPLOT,NX,NY,STATUS)

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
*    Import :
      INTEGER ID,NPLOT
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
	CALL GMI_GETLAYOUT( ID,XSET,NX,YSET,NY,STATUS)

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
	  CALL AST_REXIT('GDRAW_MULTI_LAYOUT',STATUS)
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
	  CALL AST_REXIT('GDRAW_MULTI_DEFPOS',STATUS)
	ENDIF
      ENDIF
      END
