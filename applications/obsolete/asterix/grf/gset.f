*+  GSET - set Grafix CONtrol parameter
      SUBROUTINE GSET(STATUS)

*    Description :
*    Authors :
*             (BHVAD::RJV)
*    History :
*       9 Jun 94 : 1.7-2 fix for char things not going to upper case (RJV)
*      10 Jun 94 : 1.7-3 DUMP option (RJV)
*       7 Sep 94 : 1.7-4 OFF added as alternative to CANCEL (RJV)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'GMD_PAR'
*    Import :
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'GFX_CMN'
      INCLUDE 'GMD_CMN'
*    Local Constants :
      CHARACTER*30 VERSION
      PARAMETER (VERSION='GSET Version 1.7-4')
*    Local variables :
      CHARACTER*(DAT__SZLOC) LOC
      CHARACTER*10 SWITCH
      CHARACTER*20 CONTEXT
      LOGICAL CANCEL
      INTEGER NNDF,NSEL
      INTEGER NDFS(GMD__MXNDF)
      LOGICAL X,Y
      LOGICAL PRIM
      LOGICAL MULTI
      LOGICAL ACTIVE,LIVE
      LOGICAL HELP
      LOGICAL SHOW
      LOGICAL DUMP
*-
      CALL MSG_PRNT(VERSION)

      LIVE=.TRUE.
      NSEL=1

*  determine if in live mode
      CALL GDV_STATUS(ACTIVE,STATUS)
      IF (ACTIVE) THEN
        CALL GCB_GETCONTXT(CONTEXT,STATUS)
      ENDIF
      IF (.NOT.ACTIVE) THEN
        LIVE=.FALSE.
      ELSEIF (ACTIVE.AND.CONTEXT.NE.'GRAFIX'.AND.
     :                   CONTEXT.NE.' ') THEN
        LIVE=.TRUE.
      ELSEIF (ACTIVE.AND.G_OPEN) THEN
        IF (G_MULTI) THEN
          LIVE=.FALSE.
        ELSE
          LIVE=.TRUE.
        ENDIF
      ELSE
        LIVE=.FALSE.
      ENDIF

      IF (LIVE) THEN
        CALL GCB_ATTACH('CURRENT',STATUS)
      ELSE
        CALL GCB_ATTACH('LOCAL',STATUS)
      ENDIF

*  see if diagnostic dump wanted
      CALL PAR_GET0L('DUMP',DUMP,STATUS)

      IF (.NOT.DUMP.AND.STATUS.EQ.SAI__OK) THEN

*  get control parameter keyword
        HELP=.TRUE.
        DO WHILE (HELP.AND.STATUS.EQ.SAI__OK)
          CALL PAR_GET0C('SWITCH',SWITCH,STATUS)
          CALL CHR_UCASE(SWITCH)
          SWITCH=SWITCH(:3)
          IF (SWITCH.EQ.'HEL') THEN
            CALL GSET_HELP(STATUS)
            CALL PAR_CANCL('SWITCH',STATUS)
          ELSE
            HELP=.FALSE.
          ENDIF
        ENDDO

        IF (SWITCH.EQ.'ALL') THEN
          SWITCH='*'
        ENDIF

*  cancel show or set
        CALL PAR_GET0L('SHOW',SHOW,STATUS)
        CALL PAR_GET0L('OFF',CANCEL,STATUS)
        IF (.NOT.CANCEL) THEN
          CALL PAR_GET0L('CANCEL',CANCEL,STATUS)
        ENDIF

      ENDIF

*  get dataset if not live
      IF (.NOT.LIVE) THEN
        IF (.NOT.G_OPEN) THEN
          CALL AST_INIT()
*  get locator to top level
          CALL USI_ASSOCI('INP','UPDATE',LOC,PRIM,STATUS)
*  check if multiple dataset
          CALL GMD_QMULT(LOC,MULTI,STATUS)
*  if not check NDF type
          IF (.NOT.MULTI) THEN
            CALL GFX_NDFTYPE(LOC,STATUS)
            G_LOC=LOC
            G_MULTI=.FALSE.
          ELSE
            G_MULTI=.TRUE.
            G_MLOC=LOC
          ENDIF
        ENDIF
        IF (STATUS.EQ.SAI__OK) THEN
*  if multiple dataset get NDF numbers
          IF (G_MULTI) THEN
            CALL GMD_QNDF(G_MLOC,NNDF,STATUS)
            IF (NNDF.LE.GMD__MXNDF) THEN
              CALL PRS_GETLIST('NDF',NNDF,NDFS,NSEL,STATUS)
            ELSE
              CALL MSG_PRNT('AST_ERR:too many NDFs in dataset')
              STATUS=SAI__ERROR
            ENDIF
          ENDIF
        ENDIF
      ENDIF

*  raw diagnostic dump
      IF (DUMP) THEN
        CALL GSET_DUMP(LIVE,NSEL,NDFS,STATUS)

      ELSEIF (SHOW) THEN
*  summary of current attributes
        CALL GSET_SHOW(LIVE,NSEL,NDFS,SWITCH,STATUS)

*  clear everything
      ELSEIF (SWITCH.EQ.'*'.OR.SWITCH.EQ.'ALL') THEN
        IF (CANCEL) THEN
          CALL GSET_ALL_CANCEL(LIVE,NSEL,NDFS,STATUS)
        ENDIF

*  axes (both)
      ELSEIF     (SWITCH.EQ.'AXE') THEN
        X=.TRUE.
        Y=.TRUE.
        IF (CANCEL) THEN
          CALL GSET_AXES_CANCEL(LIVE,NSEL,NDFS,X,Y,STATUS)
        ELSE
          CALL GSET_AXES_SET(LIVE,NSEL,NDFS,X,Y,STATUS)
        ENDIF
*  x-axis
      ELSEIF (SWITCH.EQ.'XAX') THEN
        X=.TRUE.
        Y=.FALSE.
        IF (CANCEL) THEN
          CALL GSET_AXES_CANCEL(LIVE,NSEL,NDFS,X,Y,STATUS)
        ELSE
          CALL GSET_AXES_SET(LIVE,NSEL,NDFS,X,Y,STATUS)
        ENDIF
*  y-axis
      ELSEIF (SWITCH.EQ.'YAX') THEN
        X=.FALSE.
        Y=.TRUE.
        IF (CANCEL) THEN
          CALL GSET_AXES_CANCEL(LIVE,NSEL,NDFS,X,Y,STATUS)
        ELSE
          CALL GSET_AXES_SET(LIVE,NSEL,NDFS,X,Y,STATUS)
        ENDIF
*  axis labels (both)
      ELSEIF (SWITCH.EQ.'LAB') THEN
        X=.TRUE.
        Y=.TRUE.
        IF (CANCEL) THEN
          CALL GSET_LABELS_CANCEL(LIVE,NSEL,NDFS,X,Y,STATUS)
        ELSE
          CALL GSET_LABELS_SET(LIVE,NSEL,NDFS,X,Y,STATUS)
        ENDIF
*  x-axis label
      ELSEIF (SWITCH.EQ.'XLA') THEN
        X=.TRUE.
        Y=.FALSE.
        IF (CANCEL) THEN
          CALL GSET_LABELS_CANCEL(LIVE,NSEL,NDFS,X,Y,STATUS)
        ELSE
          CALL GSET_LABELS_SET(LIVE,NSEL,NDFS,X,Y,STATUS)
        ENDIF
*  y-axis label
      ELSEIF (SWITCH.EQ.'YLA') THEN
        X=.FALSE.
        Y=.TRUE.
        IF (CANCEL) THEN
          CALL GSET_LABELS_CANCEL(LIVE,NSEL,NDFS,X,Y,STATUS)
        ELSE
          CALL GSET_LABELS_SET(LIVE,NSEL,NDFS,X,Y,STATUS)
        ENDIF
*  titles (at top)
      ELSEIF (SWITCH.EQ.'TIT') THEN
        IF (.NOT.LIVE.AND.NSEL.GT.1) THEN
          CALL MSG_SETI('NSEL',NDFS(1))
          CALL MSG_PRNT('Only 1 selection allowed - using ^NSEL')
        ENDIF
        IF (CANCEL) THEN
          CALL GSET_TITLE_CANCEL(LIVE,NDFS(1),STATUS)
        ELSE
          CALL GSET_TITLE_SET(LIVE,NDFS(1),STATUS)
        ENDIF
*  polyline
      ELSEIF (SWITCH.EQ.'POL') THEN
        IF (CANCEL) THEN
          CALL GSET_POLY_CANCEL(LIVE,NSEL,NDFS,STATUS)
        ELSE
          CALL GSET_POLY_SET(LIVE,NSEL,NDFS,STATUS)
        ENDIF
*  stepped line
      ELSEIF (SWITCH.EQ.'STE') THEN
        IF (CANCEL) THEN
          CALL GSET_STEP_CANCEL(LIVE,NSEL,NDFS,STATUS)
        ELSE
          CALL GSET_STEP_SET(LIVE,NSEL,NDFS,STATUS)
        ENDIF
*  error bars
      ELSEIF (SWITCH.EQ.'ERR') THEN
        IF (CANCEL) THEN
          CALL GSET_ERR_CANCEL(LIVE,NSEL,NDFS,STATUS)
        ELSE
          CALL GSET_ERR_SET(LIVE,NSEL,NDFS,STATUS)
        ENDIF
*  points
      ELSEIF (SWITCH.EQ.'POI') THEN
        IF (CANCEL) THEN
          CALL GSET_POINTS_CANCEL(LIVE,NSEL,NDFS,STATUS)
        ELSE
          CALL GSET_POINTS_SET(LIVE,NSEL,NDFS,STATUS)
        ENDIF
*  pixel plot
      ELSEIF (SWITCH.EQ.'PIX') THEN
        IF (CANCEL) THEN
          CALL GSET_PIXEL_CANCEL(LIVE,NSEL,NDFS,STATUS)
        ELSE
          CALL GSET_PIXEL_SET(LIVE,NSEL,NDFS,STATUS)
        ENDIF
*  contour plot
      ELSEIF (SWITCH.EQ.'CON') THEN
        IF (CANCEL) THEN
          CALL GSET_CONTOUR_CANCEL(LIVE,NSEL,NDFS,STATUS)
        ELSE
          CALL GSET_CONTOUR_SET(LIVE,NSEL,NDFS,STATUS)
        ENDIF
*  surface plot
      ELSEIF (SWITCH.EQ.'SUR') THEN
        IF (CANCEL) THEN
          CALL GSET_SURF_CANCEL(LIVE,NSEL,NDFS,STATUS)
        ELSE
          CALL GSET_SURF_SET(LIVE,NSEL,NDFS,STATUS)
        ENDIF
*  coordinate grid
      ELSEIF (SWITCH.EQ.'GRI') THEN
        IF (CANCEL) THEN
          CALL GSET_GRID_CANCEL(LIVE,NSEL,NDFS,STATUS)
        ELSE
          CALL GSET_GRID_SET(LIVE,NSEL,NDFS,STATUS)
        ENDIF
*  key (colour bar etc)
      ELSEIF (SWITCH.EQ.'KEY') THEN
        IF (CANCEL) THEN
          CALL GSET_KEY_CANCEL(LIVE,NSEL,NDFS,STATUS)
        ELSE
          CALL GSET_KEY_SET(LIVE,NSEL,NDFS,STATUS)
        ENDIF
*  ID stamp
      ELSEIF (SWITCH.EQ.'STA') THEN
        IF (CANCEL) THEN
          CALL GSET_STAMP_CANCEL(LIVE,NSEL,NDFS,STATUS)
        ELSE
          CALL GSET_STAMP_SET(LIVE,NSEL,NDFS,STATUS)
        ENDIF
*  shapes
      ELSEIF (SWITCH.EQ.'SHA') THEN
        IF (.NOT.LIVE.AND.NSEL.GT.1) THEN
          CALL MSG_SETI('NSEL',NDFS(1))
          CALL MSG_PRNT('Only 1 selection allowed - using ^NSEL')
        ENDIF
        IF (CANCEL) THEN
          CALL GSET_SHAPE_CANCEL(LIVE,NDFS(1),STATUS)
        ELSE
          CALL GSET_SHAPE_SET(LIVE,NDFS(1),STATUS)
        ENDIF
*  annotation
      ELSEIF (SWITCH.EQ.'NOT') THEN
        IF (.NOT.LIVE.AND.NSEL.GT.1) THEN
          CALL MSG_SETI('NSEL',NDFS(1))
          CALL MSG_PRNT('Only 1 selection allowed - using ^NSEL')
        ENDIF
        IF (CANCEL) THEN
          CALL GSET_NOTE_CANCEL(LIVE,NDFS(1),STATUS)
        ELSE
          CALL GSET_NOTE_SET(LIVE,NDFS(1),STATUS)
        ENDIF
*  markers
      ELSEIF (SWITCH.EQ.'MAR') THEN
        IF (.NOT.LIVE.AND.NSEL.GT.1) THEN
          CALL MSG_SETI('NSEL',NDFS(1))
          CALL MSG_PRNT('Only 1 selection allowed - using ^NSEL')
        ENDIF
        IF (CANCEL) THEN
          CALL GSET_MARKER_CANCEL(LIVE,NDFS(1),STATUS)
        ELSE
          CALL GSET_MARKER_SET(LIVE,NDFS(1),STATUS)
        ENDIF
*  colour table
      ELSEIF (SWITCH.EQ.'COL') THEN
        IF (CANCEL) THEN
          CALL GSET_COLOUR_CANCEL(LIVE,NSEL,NDFS,STATUS)
        ELSE
          CALL GSET_COLOUR_SET(LIVE,NSEL,NDFS,STATUS)
        ENDIF
*  position
      ELSEIF (SWITCH.EQ.'POS') THEN
        IF (CANCEL) THEN
          CALL GSET_POSIT_CANCEL(LIVE,NSEL,NDFS,STATUS)
        ELSE
          CALL GSET_POSIT_SET(LIVE,NSEL,NDFS,STATUS)
        ENDIF
*  default attributes
      ELSEIF (SWITCH.EQ.'DEF') THEN
        IF (CANCEL) THEN
          CALL GSET_DEFAULT_CANCEL(LIVE,NSEL,NDFS,STATUS)
        ELSE
          CALL GSET_DEFAULT_SET(LIVE,NSEL,NDFS,STATUS)
        ENDIF


      ELSEIF (STATUS.EQ.SAI__OK) THEN
        CALL MSG_PRNT('AST_ERR: unknown keyword')
        STATUS=SAI__ERROR
      ENDIF

      IF (.NOT.LIVE) THEN
        IF (.NOT.G_OPEN) THEN
          IF (G_MULTI) THEN
            CALL USI_ANNUL(LOC,STATUS)
          ELSE
            CALL BDA_RELEASE(LOC,STATUS)
            CALL USI_ANNUL(LOC,STATUS)
          ENDIF
        ENDIF

        CALL GCB_DETACH(STATUS)

      ENDIF

      CALL AST_ERR(STATUS)

      END

*+
      SUBROUTINE GSET_ALL_CANCEL(LIVE,NSEL,NDFS,STATUS)

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
      LOGICAL LIVE
      INTEGER NSEL
      INTEGER NDFS(*)
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'GFX_CMN'
      INCLUDE 'GMD_CMN'
*    Local Constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) GLOC
      INTEGER ISEL
*-

      IF (STATUS.EQ.SAI__OK) THEN

        DO ISEL=1,NSEL

          IF (.NOT.LIVE) THEN
            IF (G_MULTI) THEN
              IF (ISEL.EQ.1) THEN
                CALL MSG_PRNT('Multiple dataset:-')
              ENDIF
              CALL MSG_SETI('NDF',NDFS(ISEL))
              CALL MSG_PRNT(' processing NDF ^NDF...')
              CALL GMD_LOCNDF(G_MLOC,NDFS(ISEL),GLOC,STATUS)
              CALL GCB_LOAD(GLOC,STATUS)
            ELSE
              CALL GCB_LOAD(G_LOC,STATUS)
            ENDIF
          ENDIF

          CALL GCB_CLEAR(STATUS)

          IF (.NOT.LIVE) THEN
            IF (G_MULTI) THEN
              CALL GCB_SAVE(GLOC,STATUS)
              CALL BDA_RELEASE(GLOC,STATUS)
              CALL DAT_ANNUL(GLOC,STATUS)
            ELSE
              CALL GCB_SAVE(G_LOC,STATUS)
            ENDIF
          ENDIF

        ENDDO


        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GSET_ALL_CANCEL',STATUS)
        ENDIF

      ENDIF
      END

*+
      SUBROUTINE GSET_AXES_CANCEL(LIVE,NSEL,NDFS,X,Y,STATUS)

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
      LOGICAL LIVE
      INTEGER NSEL
      INTEGER NDFS(*)
      LOGICAL X,Y
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'GFX_CMN'
      INCLUDE 'GMD_CMN'
*    Local Constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) GLOC
      INTEGER ISEL
      LOGICAL OK,RADEC
*-

      IF (STATUS.EQ.SAI__OK) THEN

        DO ISEL=1,NSEL

          IF (.NOT.LIVE) THEN
            IF (G_MULTI) THEN
              IF (ISEL.EQ.1) THEN
                CALL MSG_PRNT('Multiple dataset:-')
              ENDIF
              CALL MSG_SETI('NDF',NDFS(ISEL))
              CALL MSG_PRNT(' processing NDF ^NDF...')
              CALL GMD_LOCNDF(G_MLOC,NDFS(ISEL),GLOC,STATUS)
              CALL GCB_LOAD(GLOC,STATUS)
            ELSE
              CALL GCB_LOAD(G_LOC,STATUS)
            ENDIF
          ENDIF

*  specific to x-axis
          IF (X) THEN
            CALL GCB_CANL('XAXIS_LOG',STATUS)
            CALL GCB_CANR('XAXIS_HI',STATUS)
            CALL GCB_CANR('XAXIS_LO',STATUS)
            CALL GCB_CANC('XAXIS_OPT',STATUS)
            CALL GCB_CANI('XAXIS_DIV',STATUS)
            CALL GCB_CANR('XAXIS_TICK',STATUS)
          ENDIF
*  specific to y-axis
          IF (Y) THEN
            CALL GCB_CANL('YAXIS_LOG',STATUS)
            CALL GCB_CANR('YAXIS_HI',STATUS)
            CALL GCB_CANR('YAXIS_LO',STATUS)
            CALL GCB_CANC('YAXIS_OPT',STATUS)
            CALL GCB_CANI('YAXIS_DIV',STATUS)
            CALL GCB_CANR('YAXIS_TICK',STATUS)
          ENDIF
*  common to both axes
          CALL GCB_CANI('AXES_BOLD',STATUS)
          CALL GCB_CANR('AXES_SIZE',STATUS)
          CALL GCB_CANI('AXES_FONT',STATUS)
          CALL GCB_CANI('AXES_WIDTH',STATUS)
          CALL GCB_CANL('AXES_SCALING',STATUS)
          CALL GCB_GETL('AXES_RADEC',OK,RADEC,STATUS)
          IF (OK.AND.RADEC) THEN
            CALL GCB_CANL('AXES_RADEC',STATUS)
            CALL GCB_CANC('XLABEL_TEXT',STATUS)
            CALL GCB_CANC('YLABEL_TEXT',STATUS)
          ENDIF

          IF (.NOT.LIVE) THEN
            IF (G_MULTI) THEN
              CALL GCB_SAVE(GLOC,STATUS)
              CALL BDA_RELEASE(GLOC,STATUS)
              CALL DAT_ANNUL(GLOC,STATUS)
            ELSE
              CALL GCB_SAVE(G_LOC,STATUS)
            ENDIF
          ENDIF

        ENDDO


        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GSET_AXES_CANCEL',STATUS)
        ENDIF

      ENDIF
      END

*+
      SUBROUTINE GSET_AXES_SET(LIVE,NSEL,NDFS,X,Y,STATUS)

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
      LOGICAL LIVE
      INTEGER NSEL
      INTEGER NDFS(*)
      LOGICAL X,Y
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'GFX_CMN'
      INCLUDE 'GMD_CMN'
*    Local Constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) GLOC
      CHARACTER*15 OPT
      REAL SIZE
      REAL TICK
      REAL HI,LO
      INTEGER ISEL
      INTEGER WIDTH
      INTEGER FONT
      INTEGER BOLD
      INTEGER DIV
      LOGICAL SETW,SETT,SETD,SETF,SETS,SETB,SETO,SETL,SETHI,SETLO
      LOGICAL LOGAX
      LOGICAL SCALE,SETSCALE
      LOGICAL RADEC,SETRADEC
*-

      SETW=.TRUE.
      SETT=.TRUE.
      SETD=.TRUE.
      SETF=.TRUE.
      SETS=.TRUE.
      SETB=.TRUE.
      SETO=.TRUE.
      SETL=.TRUE.
      SETHI=.TRUE.
      SETLO=.TRUE.
      SETSCALE=.TRUE.
      SETRADEC=.TRUE.

      IF (STATUS.EQ.SAI__OK) THEN

*  get individual control parameters - NULL means no change
        CALL PAR_GET0I('WIDTH',WIDTH,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
          SETW=.FALSE.
        ENDIF

        CALL PAR_GET0I('FONT',FONT,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
          SETF=.FALSE.
        ENDIF

        CALL PAR_GET0R('SIZE',SIZE,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
          SETS=.FALSE.
        ENDIF

        CALL PAR_GET0I('BOLD',BOLD,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
          SETB=.FALSE.
        ENDIF

        CALL PAR_GET0R('TICK',TICK,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
          SETT=.FALSE.
        ENDIF

        CALL PAR_GET0I('DIV',DIV,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
          SETD=.FALSE.
        ENDIF

        CALL PAR_GET0C('OPT',OPT,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
          SETO=.FALSE.
        ENDIF

        CALL PAR_GET0R('LO',LO,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
          SETLO=.FALSE.
        ENDIF

        CALL PAR_GET0R('HI',HI,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
          SETHI=.FALSE.
        ENDIF

        CALL PAR_GET0L('LOG',LOGAX,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
          SETL=.FALSE.
        ENDIF

        CALL PAR_GET0L('RADEC',RADEC,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
          SETRADEC=.FALSE.
        ENDIF

        CALL PAR_GET0L('SCALE',SCALE,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
          SETSCALE=.FALSE.
        ENDIF

*  NSEL is set to 1 for LIVE mode or single dataset
*  otherwise to number of NDFs selected from multiple dataset

        DO ISEL=1,NSEL

*  if not in interactive mode
          IF (.NOT.LIVE) THEN
*  load existing GCB
            IF (G_MULTI) THEN
              IF (ISEL.EQ.1) THEN
                CALL MSG_PRNT('Multiple dataset:-')
              ENDIF
              CALL MSG_SETI('NDF',NDFS(ISEL))
              CALL MSG_PRNT(' processing NDF ^NDF...')
*  from component of multiple dataset
              CALL GMD_LOCNDF(G_MLOC,NDFS(ISEL),GLOC,STATUS)
              CALL GCB_LOAD(GLOC,STATUS)
            ELSE
*  or single dataset
              CALL GCB_LOAD(G_LOC,STATUS)
            ENDIF
          ENDIF

*  set new values

*  specific to x-axis
          IF (X) THEN
            IF (SETL) THEN
              CALL GCB_SETL('XAXIS_LOG',LOGAX,STATUS)
            ENDIF
            IF (SETHI) THEN
              CALL GCB_SETR('XAXIS_HI',HI,STATUS)
            ENDIF
            IF (SETLO) THEN
              CALL GCB_SETR('XAXIS_LO',LO,STATUS)
            ENDIF
            IF (SETO) THEN
              CALL GCB_SETC('XAXIS_OPT',OPT,STATUS)
            ENDIF
            IF (SETD) THEN
              CALL GCB_SETI('XAXIS_DIV',DIV,STATUS)
            ENDIF
            IF (SETT) THEN
              CALL GCB_SETR('XAXIS_TICK',TICK,STATUS)
            ENDIF
          ENDIF
*  specific to y-axis
          IF (Y) THEN
            IF (SETL) THEN
              CALL GCB_SETL('YAXIS_LOG',LOGAX,STATUS)
            ENDIF
            IF (SETHI) THEN
              CALL GCB_SETR('YAXIS_HI',HI,STATUS)
            ENDIF
            IF (SETLO) THEN
              CALL GCB_SETR('YAXIS_LO',LO,STATUS)
            ENDIF
            IF (SETO) THEN
              CALL GCB_SETC('YAXIS_OPT',OPT,STATUS)
            ENDIF
            IF (SETD) THEN
              CALL GCB_SETI('YAXIS_DIV',DIV,STATUS)
            ENDIF
            IF (SETT) THEN
              CALL GCB_SETR('YAXIS_TICK',TICK,STATUS)
            ENDIF
          ENDIF
*  common to both axes
          IF (SETB) THEN
            CALL GCB_SETI('AXES_BOLD',BOLD,STATUS)
          ENDIF
          IF (SETS) THEN
            CALL GCB_SETR('AXES_SIZE',SIZE,STATUS)
          ENDIF
          IF (SETF) THEN
            CALL GCB_SETI('AXES_FONT',FONT,STATUS)
          ENDIF
          IF (SETW) THEN
            CALL GCB_SETI('AXES_WIDTH',WIDTH,STATUS)
          ENDIF
          IF (SETSCALE) THEN
            CALL GCB_SETL('AXES_SCALING',SCALE,STATUS)
          ENDIF
          IF (SETRADEC) THEN
            CALL GCB_SETL('AXES_RADEC',RADEC,STATUS)
            IF (RADEC) THEN
              CALL GCB_SETC('XLABEL_TEXT','Right Ascension',STATUS)
              CALL GCB_SETC('YLABEL_TEXT','Declination',STATUS)
            ELSE
              CALL GCB_SETC('XLABEL_TEXT',' ',STATUS)
              CALL GCB_SETC('YLABEL_TEXT',' ',STATUS)
            ENDIF
          ENDIF

*  resave if necessary
          IF (.NOT.LIVE) THEN
            IF (G_MULTI) THEN
              CALL GCB_SAVE(GLOC,STATUS)
              CALL BDA_RELEASE(GLOC,STATUS)
              CALL DAT_ANNUL(GLOC,STATUS)
            ELSE
              CALL GCB_SAVE(G_LOC,STATUS)
            ENDIF
          ENDIF

        ENDDO

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GSET_AXES_SET',STATUS)
        ENDIF

      ENDIF
      END




*+
      SUBROUTINE GSET_LABELS_CANCEL(LIVE,NSEL,NDFS,X,Y,STATUS)

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
      LOGICAL LIVE
      INTEGER NSEL
      INTEGER NDFS(*)
      LOGICAL X,Y
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'GFX_CMN'
      INCLUDE 'GMD_CMN'
*    Local Constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) GLOC
      INTEGER ISEL
*-

      IF (STATUS.EQ.SAI__OK) THEN

        DO ISEL=1,NSEL

          IF (.NOT.LIVE) THEN
            IF (G_MULTI) THEN
              IF (ISEL.EQ.1) THEN
                CALL MSG_PRNT('Multiple dataset:-')
              ENDIF
              CALL MSG_SETI('NDF',NDFS(ISEL))
              CALL MSG_PRNT(' processing NDF ^NDF...')
              CALL GMD_LOCNDF(G_MLOC,NDFS(ISEL),GLOC,STATUS)
              CALL GCB_LOAD(GLOC,STATUS)
            ELSE
              CALL GCB_LOAD(G_LOC,STATUS)
            ENDIF
          ENDIF

*  specific to x-axis
          IF (X) THEN
            CALL GCB_CANR('XLABEL_OFFSET',STATUS)
            CALL GCB_CANC('XLABEL_TEXT',STATUS)
            CALL GCB_CANI('XLABEL_BOLD',STATUS)
            CALL GCB_CANR('XLABEL_SIZE',STATUS)
            CALL GCB_CANI('XLABEL_FONT',STATUS)
          ENDIF
*  specific to y-axis
          IF (Y) THEN
            CALL GCB_CANR('YLABEL_OFFSET',STATUS)
            CALL GCB_CANC('YLABEL_TEXT',STATUS)
            CALL GCB_CANI('YLABEL_BOLD',STATUS)
            CALL GCB_CANR('YLABEL_SIZE',STATUS)
            CALL GCB_CANI('YLABEL_FONT',STATUS)
          ENDIF

          IF (.NOT.LIVE) THEN
            IF (G_MULTI) THEN
              CALL GCB_SAVE(GLOC,STATUS)
              CALL BDA_RELEASE(GLOC,STATUS)
              CALL DAT_ANNUL(GLOC,STATUS)
            ELSE
              CALL GCB_SAVE(G_LOC,STATUS)
            ENDIF
          ENDIF

        ENDDO


        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GSET_LABELS_CANCEL',STATUS)
        ENDIF

      ENDIF
      END

*+
      SUBROUTINE GSET_LABELS_SET(LIVE,NSEL,NDFS,X,Y,STATUS)

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
      LOGICAL LIVE
      INTEGER NSEL
      INTEGER NDFS(*)
      LOGICAL X,Y
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'GFX_CMN'
      INCLUDE 'GMD_CMN'
*    Local Constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) GLOC
      CHARACTER*80 TEXT
      REAL SIZE
      REAL OFFSET
      INTEGER ISEL
      INTEGER FONT
      INTEGER BOLD
      LOGICAL SETT,SETF,SETS,SETB,SETO
*-

      SETT=.TRUE.
      SETF=.TRUE.
      SETS=.TRUE.
      SETB=.TRUE.
      SETO=.TRUE.

      IF (STATUS.EQ.SAI__OK) THEN

*  get individual control parameters - NULL means no change
        CALL PAR_GET0C('TEXT',TEXT,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
          SETT=.FALSE.
        ENDIF

        CALL PAR_GET0I('FONT',FONT,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
          SETF=.FALSE.
        ENDIF

        CALL PAR_GET0R('SIZE',SIZE,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
          SETS=.FALSE.
        ENDIF

        CALL PAR_GET0I('BOLD',BOLD,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
          SETB=.FALSE.
        ENDIF

        CALL PAR_GET0R('OFFSET',OFFSET,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
          SETO=.FALSE.
        ENDIF

*  NSEL is set to 1 for LIVE mode or single dataset
*  otherwise to number of NDFs selected from multiple dataset

        DO ISEL=1,NSEL

*  if not in interactive mode
          IF (.NOT.LIVE) THEN
*  load existing GCB
            IF (G_MULTI) THEN
              IF (ISEL.EQ.1) THEN
                CALL MSG_PRNT('Multiple dataset:-')
              ENDIF
              CALL MSG_SETI('NDF',NDFS(ISEL))
              CALL MSG_PRNT(' processing NDF ^NDF...')
*  from component of multiple dataset
              CALL GMD_LOCNDF(G_MLOC,NDFS(ISEL),GLOC,STATUS)
              CALL GCB_LOAD(GLOC,STATUS)
            ELSE
*  or single dataset
              CALL GCB_LOAD(G_LOC,STATUS)
            ENDIF
          ENDIF

*  set new values

*  specific to x-axis
          IF (X) THEN
            IF (SETO) THEN
              CALL GCB_SETR('XLABEL_OFFSET',OFFSET,STATUS)
            ENDIF
            IF (SETT) THEN
              CALL GCB_SETC('XLABEL_TEXT',TEXT,STATUS)
            ENDIF
            IF (SETB) THEN
              CALL GCB_SETI('XLABEL_BOLD',BOLD,STATUS)
            ENDIF
            IF (SETS) THEN
              CALL GCB_SETR('XLABEL_SIZE',SIZE,STATUS)
            ENDIF
            IF (SETF) THEN
              CALL GCB_SETI('XLABEL_FONT',FONT,STATUS)
            ENDIF
          ENDIF
*  specific to y-axis
          IF (Y) THEN
            IF (SETO) THEN
              CALL GCB_SETR('YLABEL_OFFSET',OFFSET,STATUS)
            ENDIF
            IF (SETT) THEN
              CALL GCB_SETC('YLABEL_TEXT',TEXT,STATUS)
            ENDIF
            IF (SETB) THEN
              CALL GCB_SETI('YLABEL_BOLD',BOLD,STATUS)
            ENDIF
            IF (SETS) THEN
              CALL GCB_SETR('YLABEL_SIZE',SIZE,STATUS)
            ENDIF
            IF (SETF) THEN
              CALL GCB_SETI('YLABEL_FONT',FONT,STATUS)
            ENDIF
          ENDIF

*  resave if necessary
          IF (.NOT.LIVE) THEN
            IF (G_MULTI) THEN
              CALL GCB_SAVE(GLOC,STATUS)
              CALL BDA_RELEASE(GLOC,STATUS)
              CALL DAT_ANNUL(GLOC,STATUS)
            ELSE
              CALL GCB_SAVE(G_LOC,STATUS)
            ENDIF
          ENDIF

        ENDDO

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GSET_LABELS_SET',STATUS)
        ENDIF

      ENDIF
      END




*+
      SUBROUTINE GSET_KEY_CANCEL(LIVE,NSEL,NDFS,STATUS)

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
      LOGICAL LIVE
      INTEGER NSEL
      INTEGER NDFS(*)
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'GFX_CMN'
      INCLUDE 'GMD_CMN'
*    Local Constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) GLOC
      INTEGER ISEL
*-

      IF (STATUS.EQ.SAI__OK) THEN

        DO ISEL=1,NSEL

          IF (.NOT.LIVE) THEN
            IF (G_MULTI) THEN
              IF (ISEL.EQ.1) THEN
                CALL MSG_PRNT('Multiple dataset:-')
              ENDIF
              CALL MSG_SETI('NDF',NDFS(ISEL))
              CALL MSG_PRNT(' processing NDF ^NDF...')
              CALL GMD_LOCNDF(G_MLOC,NDFS(ISEL),GLOC,STATUS)
              CALL GCB_LOAD(GLOC,STATUS)
            ELSE
              CALL GCB_LOAD(G_LOC,STATUS)
            ENDIF
          ENDIF

          CALL GCB_CANC('KEY_OPT',STATUS)
          CALL GCB_CANI('KEY_BOLD',STATUS)
          CALL GCB_CANR('KEY_SIZE',STATUS)
          CALL GCB_CANI('KEY_FONT',STATUS)

          IF (.NOT.LIVE) THEN
            IF (G_MULTI) THEN
              CALL GCB_SAVE(GLOC,STATUS)
              CALL BDA_RELEASE(GLOC,STATUS)
              CALL DAT_ANNUL(GLOC,STATUS)
            ELSE
              CALL GCB_SAVE(G_LOC,STATUS)
            ENDIF
          ENDIF

        ENDDO


        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GSET_KEY_CANCEL',STATUS)
        ENDIF

      ENDIF
      END

*+
      SUBROUTINE GSET_KEY_SET(LIVE,NSEL,NDFS,STATUS)

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
      LOGICAL LIVE
      INTEGER NSEL
      INTEGER NDFS(*)
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'GFX_CMN'
      INCLUDE 'GMD_CMN'
*    Local Constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) GLOC
      CHARACTER*10 OPT
      REAL SIZE
      INTEGER ISEL
      INTEGER FONT
      INTEGER BOLD
      LOGICAL SETO,SETF,SETS,SETB
*-
      SETO=.TRUE.
      SETF=.TRUE.
      SETS=.TRUE.
      SETB=.TRUE.

      IF (STATUS.EQ.SAI__OK) THEN

*  get individual control parameters - NULL means no change
        CALL PAR_DEF0C('OPT','PC',STATUS)
        CALL PAR_GET0C('OPT',OPT,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
          SETO=.FALSE.
        ENDIF
        CALL PAR_CANCL('OPT',STATUS)

        CALL PAR_GET0I('FONT',FONT,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
          SETF=.FALSE.
        ENDIF

        CALL PAR_GET0R('SIZE',SIZE,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
          SETS=.FALSE.
        ENDIF

        CALL PAR_GET0I('BOLD',BOLD,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
          SETB=.FALSE.
        ENDIF

*  NSEL is set to 1 for LIVE mode or single dataset
*  otherwise to number of NDFs selected from multiple dataset

        DO ISEL=1,NSEL

*  if not in interactive mode
          IF (.NOT.LIVE) THEN
*  load existing GCB
            IF (G_MULTI) THEN
              IF (ISEL.EQ.1) THEN
                CALL MSG_PRNT('Multiple dataset:-')
              ENDIF
              CALL MSG_SETI('NDF',NDFS(ISEL))
              CALL MSG_PRNT(' processing NDF ^NDF...')
*  from component of multiple dataset
              CALL GMD_LOCNDF(G_MLOC,NDFS(ISEL),GLOC,STATUS)
              CALL GCB_LOAD(GLOC,STATUS)
            ELSE
*  or single dataset
              CALL GCB_LOAD(G_LOC,STATUS)
            ENDIF
          ENDIF

*  set new values
          IF (SETO) THEN
            CALL GCB_SETC('KEY_OPT',OPT,STATUS)
          ENDIF
          IF (SETB) THEN
            CALL GCB_SETI('KEY_BOLD',BOLD,STATUS)
          ENDIF
          IF (SETS) THEN
            CALL GCB_SETR('KEY_SIZE',SIZE,STATUS)
          ENDIF
          IF (SETF) THEN
            CALL GCB_SETI('KEY_FONT',FONT,STATUS)
          ENDIF

*  resave if necessary
          IF (.NOT.LIVE) THEN
            IF (G_MULTI) THEN
              CALL GCB_SAVE(GLOC,STATUS)
              CALL BDA_RELEASE(GLOC,STATUS)
              CALL DAT_ANNUL(GLOC,STATUS)
            ELSE
              CALL GCB_SAVE(G_LOC,STATUS)
            ENDIF
          ENDIF

        ENDDO

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GSET_KEY_SET',STATUS)
        ENDIF

      ENDIF
      END




*+
      SUBROUTINE GSET_STAMP_CANCEL(LIVE,NSEL,NDFS,STATUS)

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
      LOGICAL LIVE
      INTEGER NSEL
      INTEGER NDFS(*)
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'GFX_CMN'
      INCLUDE 'GMD_CMN'
*    Local Constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) GLOC
      INTEGER ISEL
*-

      IF (STATUS.EQ.SAI__OK) THEN

        DO ISEL=1,NSEL

          IF (.NOT.LIVE) THEN
            IF (G_MULTI) THEN
              IF (ISEL.EQ.1) THEN
                CALL MSG_PRNT('Multiple dataset:-')
              ENDIF
              CALL MSG_SETI('NDF',NDFS(ISEL))
              CALL MSG_PRNT(' processing NDF ^NDF...')
              CALL GMD_LOCNDF(G_MLOC,NDFS(ISEL),GLOC,STATUS)
              CALL GCB_LOAD(GLOC,STATUS)
            ELSE
              CALL GCB_LOAD(G_LOC,STATUS)
            ENDIF
          ENDIF

          CALL GCB_CANL('STAMP_FLAG',STATUS)
          CALL GCB_CANC('STAMP_TEXT',STATUS)

          IF (.NOT.LIVE) THEN
            IF (G_MULTI) THEN
              CALL GCB_SAVE(GLOC,STATUS)
              CALL BDA_RELEASE(GLOC,STATUS)
              CALL DAT_ANNUL(GLOC,STATUS)
            ELSE
              CALL GCB_SAVE(G_LOC,STATUS)
            ENDIF
          ENDIF

        ENDDO


        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GSET_STAMP_CANCEL',STATUS)
        ENDIF

      ENDIF
      END

*+
      SUBROUTINE GSET_STAMP_SET(LIVE,NSEL,NDFS,STATUS)

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
      LOGICAL LIVE
      INTEGER NSEL
      INTEGER NDFS(*)
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'GFX_CMN'
      INCLUDE 'GMD_CMN'
*    Local Constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) GLOC
      CHARACTER*80 TEXT
      INTEGER ISEL
      LOGICAL SET
*-
      SET=.TRUE.

      IF (STATUS.EQ.SAI__OK) THEN

*  get individual control parameters - NULL means no change

        CALL PAR_GET0C('TEXT',TEXT,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
          SET=.FALSE.
        ENDIF

*  NSEL is set to 1 for LIVE mode or single dataset
*  otherwise to number of NDFs selected from multiple dataset

        DO ISEL=1,NSEL

*  if not in interactive mode
          IF (.NOT.LIVE) THEN
*  load existing GCB
            IF (G_MULTI) THEN
              IF (ISEL.EQ.1) THEN
                CALL MSG_PRNT('Multiple dataset:-')
              ENDIF
              CALL MSG_SETI('NDF',NDFS(ISEL))
              CALL MSG_PRNT(' processing NDF ^NDF...')
*  from component of multiple dataset
              CALL GMD_LOCNDF(G_MLOC,NDFS(ISEL),GLOC,STATUS)
              CALL GCB_LOAD(GLOC,STATUS)
            ELSE
*  or single dataset
              CALL GCB_LOAD(G_LOC,STATUS)
            ENDIF
          ENDIF

*  set new values
          CALL GCB_SETL('STAMP_FLAG',.TRUE.,STATUS)
          IF (SET) THEN
            CALL GCB_SETC('STAMP_TEXT',TEXT,STATUS)
          ENDIF

*  resave if necessary
          IF (.NOT.LIVE) THEN
            IF (G_MULTI) THEN
              CALL GCB_SAVE(GLOC,STATUS)
              CALL BDA_RELEASE(GLOC,STATUS)
              CALL DAT_ANNUL(GLOC,STATUS)
            ELSE
              CALL GCB_SAVE(G_LOC,STATUS)
            ENDIF
          ENDIF

        ENDDO

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GSET_STAMP_SET',STATUS)
        ENDIF

      ENDIF
      END





*+
      SUBROUTINE GSET_DEFAULT_CANCEL(LIVE,NSEL,NDFS,STATUS)

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
      LOGICAL LIVE
      INTEGER NSEL
      INTEGER NDFS(*)
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'GFX_CMN'
      INCLUDE 'GMD_CMN'
*    Local Constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) GLOC
      INTEGER ISEL
*-

      IF (STATUS.EQ.SAI__OK) THEN

        DO ISEL=1,NSEL

          IF (.NOT.LIVE) THEN
            IF (G_MULTI) THEN
              IF (ISEL.EQ.1) THEN
                CALL MSG_PRNT('Multiple dataset:-')
              ENDIF
              CALL MSG_SETI('NDF',NDFS(ISEL))
              CALL MSG_PRNT(' processing NDF ^NDF...')
              CALL GMD_LOCNDF(G_MLOC,NDFS(ISEL),GLOC,STATUS)
              CALL GCB_LOAD(GLOC,STATUS)
            ELSE
              CALL GCB_LOAD(G_LOC,STATUS)
            ENDIF
          ENDIF

          CALL GCB_CANR('DEFAULT_SIZE',STATUS)
          CALL GCB_CANI('DEFAULT_FONT',STATUS)
          CALL GCB_CANI('DEFAULT_STYLE',STATUS)
          CALL GCB_CANI('DEFAULT_WIDTH',STATUS)
          CALL GCB_CANI('DEFAULT_COLOUR',STATUS)

          IF (LIVE) THEN
            CALL GCB_SETDEF(STATUS)
          ELSE
            IF (G_MULTI) THEN
              CALL GCB_SAVE(GLOC,STATUS)
              CALL BDA_RELEASE(GLOC,STATUS)
              CALL DAT_ANNUL(GLOC,STATUS)
            ELSE
              CALL GCB_SAVE(G_LOC,STATUS)
            ENDIF
          ENDIF

        ENDDO


        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GSET_DEFAULT_CANCEL',STATUS)
        ENDIF

      ENDIF
      END

*+
      SUBROUTINE GSET_DEFAULT_SET(LIVE,NSEL,NDFS,STATUS)

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
      LOGICAL LIVE
      INTEGER NSEL
      INTEGER NDFS(*)
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'GFX_CMN'
      INCLUDE 'GMD_CMN'
*    Local Constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) GLOC
      REAL SIZE
      INTEGER ISEL
      INTEGER STYLE,WIDTH,COLOUR,FONT
      LOGICAL SETS,SETW,SETC,SETF,SETSZ
*-
      SETF=.TRUE.
      SETSZ=.TRUE.
      SETS=.TRUE.
      SETW=.TRUE.
      SETC=.TRUE.

      IF (STATUS.EQ.SAI__OK) THEN

*  get individual control parameters - NULL means no change
        CALL PAR_GET0I('WIDTH',WIDTH,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
          SETW=.FALSE.
        ENDIF

        CALL PAR_GET0I('STYLE',STYLE,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
          SETS=.FALSE.
        ENDIF


        CALL PAR_GET0I('COLOUR',COLOUR,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
          SETC=.FALSE.
        ENDIF

        CALL PAR_GET0I('FONT',FONT,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
          SETF=.FALSE.
        ENDIF


        CALL PAR_GET0R('SIZE',SIZE,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
          SETSZ=.FALSE.
        ENDIF

*  NSEL is set to 1 for LIVE mode or single dataset
*  otherwise to number of NDFs selected from multiple dataset

        DO ISEL=1,NSEL

*  if not in interactive mode
          IF (.NOT.LIVE) THEN
*  load existing GCB
            IF (G_MULTI) THEN
              IF (ISEL.EQ.1) THEN
                CALL MSG_PRNT('Multiple dataset:-')
              ENDIF
              CALL MSG_SETI('NDF',NDFS(ISEL))
              CALL MSG_PRNT(' processing NDF ^NDF...')
*  from component of multiple dataset
              CALL GMD_LOCNDF(G_MLOC,NDFS(ISEL),GLOC,STATUS)
              CALL GCB_LOAD(GLOC,STATUS)
            ELSE
*  or single dataset
              CALL GCB_LOAD(G_LOC,STATUS)
            ENDIF
          ENDIF


          IF (SETS) THEN
            CALL GCB_SETI('DEFAULT_STYLE',STYLE,STATUS)
          ENDIF

          IF (SETW) THEN
            CALL GCB_SETI('DEFAULT_WIDTH',WIDTH,STATUS)
          ENDIF

          IF (SETC) THEN
            CALL GCB_SETI('DEFAULT_COLOUR',COLOUR,STATUS)
          ENDIF

          IF (SETF) THEN
            CALL GCB_SETI('DEFAULT_FONT',FONT,STATUS)
          ENDIF

          IF (SETSZ) THEN
            CALL GCB_SETR('DEFAULT_SIZE',SIZE,STATUS)
          ENDIF

*  resave if necessary
          IF (LIVE) THEN
            CALL GCB_SETDEF(STATUS)
          ELSE
            IF (G_MULTI) THEN
              CALL GCB_SAVE(GLOC,STATUS)
              CALL BDA_RELEASE(GLOC,STATUS)
              CALL DAT_ANNUL(GLOC,STATUS)
            ELSE
              CALL GCB_SAVE(G_LOC,STATUS)
            ENDIF
          ENDIF

        ENDDO

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GSET_DEFAULT_SET',STATUS)
        ENDIF

      ENDIF
      END




*+
      SUBROUTINE GSET_POLY_CANCEL(LIVE,NSEL,NDFS,STATUS)

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
      LOGICAL LIVE
      INTEGER NSEL
      INTEGER NDFS(*)
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'GFX_CMN'
      INCLUDE 'GMD_CMN'
*    Local Constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) GLOC
      INTEGER ISEL
*-

      IF (STATUS.EQ.SAI__OK) THEN

        DO ISEL=1,NSEL

          IF (.NOT.LIVE) THEN
            IF (G_MULTI) THEN
              IF (ISEL.EQ.1) THEN
                CALL MSG_PRNT('Multiple dataset:-')
              ENDIF
              CALL MSG_SETI('NDF',NDFS(ISEL))
              CALL MSG_PRNT(' processing NDF ^NDF...')
              CALL GMD_LOCNDF(G_MLOC,NDFS(ISEL),GLOC,STATUS)
              CALL GCB_LOAD(GLOC,STATUS)
            ELSE
              CALL GCB_LOAD(G_LOC,STATUS)
            ENDIF
          ENDIF

          CALL GCB_CANL('POLY_FLAG',STATUS)
          CALL GCB_CANI('POLY_STYLE',STATUS)
          CALL GCB_CANI('POLY_WIDTH',STATUS)
          CALL GCB_CANI('POLY_COLOUR',STATUS)

          IF (.NOT.LIVE) THEN
            IF (G_MULTI) THEN
              CALL GCB_SAVE(GLOC,STATUS)
              CALL BDA_RELEASE(GLOC,STATUS)
              CALL DAT_ANNUL(GLOC,STATUS)
            ELSE
              CALL GCB_SAVE(G_LOC,STATUS)
            ENDIF
          ENDIF

        ENDDO


        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GSET_POLY_CANCEL',STATUS)
        ENDIF

      ENDIF
      END

*+
      SUBROUTINE GSET_POLY_SET(LIVE,NSEL,NDFS,STATUS)

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
      LOGICAL LIVE
      INTEGER NSEL
      INTEGER NDFS(*)
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'GFX_CMN'
      INCLUDE 'GMD_CMN'
*    Local Constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) GLOC
      INTEGER ISEL
      INTEGER STYLE,WIDTH,COLOUR
      LOGICAL SETS,SETW,SETC
*-

      SETS=.TRUE.
      SETW=.TRUE.
      SETC=.TRUE.

      IF (STATUS.EQ.SAI__OK) THEN

*  get individual control parameters - NULL means no change
        CALL PAR_GET0I('STYLE',STYLE,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
          SETS=.FALSE.
        ENDIF

        CALL PAR_GET0I('WIDTH',WIDTH,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
          SETW=.FALSE.
        ENDIF

        CALL PAR_GET0I('COLOUR',COLOUR,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
          SETC=.FALSE.
        ENDIF

*  NSEL is set to 1 for LIVE mode or single dataset
*  otherwise to number of NDFs selected from multiple dataset

        DO ISEL=1,NSEL

*  if not in interactive mode
          IF (.NOT.LIVE) THEN
*  load existing GCB
            IF (G_MULTI) THEN
              IF (ISEL.EQ.1) THEN
                CALL MSG_PRNT('Multiple dataset:-')
              ENDIF
              CALL MSG_SETI('NDF',NDFS(ISEL))
              CALL MSG_PRNT(' processing NDF ^NDF...')
*  from component of multiple dataset
              CALL GMD_LOCNDF(G_MLOC,NDFS(ISEL),GLOC,STATUS)
              CALL GCB_LOAD(GLOC,STATUS)
            ELSE
*  or single dataset
              CALL GCB_LOAD(G_LOC,STATUS)
            ENDIF
          ENDIF

*  set new values
          CALL GCB_SETL('POLY_FLAG',.TRUE.,STATUS)

          IF (SETS) THEN
            CALL GCB_SETI('POLY_STYLE',STYLE,STATUS)
          ENDIF

          IF (SETW) THEN
            CALL GCB_SETI('POLY_WIDTH',WIDTH,STATUS)
          ENDIF

          IF (SETC) THEN
            CALL GCB_SETI('POLY_COLOUR',COLOUR,STATUS)
          ENDIF

*  resave if necessary
          IF (.NOT.LIVE) THEN
            IF (G_MULTI) THEN
              CALL GCB_SAVE(GLOC,STATUS)
              CALL BDA_RELEASE(GLOC,STATUS)
              CALL DAT_ANNUL(GLOC,STATUS)
            ELSE
              CALL GCB_SAVE(G_LOC,STATUS)
            ENDIF
          ENDIF

        ENDDO

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GSET_POLY_SET',STATUS)
        ENDIF

      ENDIF
      END



*+
      SUBROUTINE GSET_STEP_CANCEL(LIVE,NSEL,NDFS,STATUS)

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
      LOGICAL LIVE
      INTEGER NSEL
      INTEGER NDFS(*)
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'GFX_CMN'
      INCLUDE 'GMD_CMN'
*    Local Constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) GLOC
      INTEGER ISEL
*-

      IF (STATUS.EQ.SAI__OK) THEN

        DO ISEL=1,NSEL

          IF (.NOT.LIVE) THEN
            IF (G_MULTI) THEN
              IF (ISEL.EQ.1) THEN
                CALL MSG_PRNT('Multiple dataset:-')
              ENDIF
              CALL MSG_SETI('NDF',NDFS(ISEL))
              CALL MSG_PRNT(' processing NDF ^NDF...')
              CALL GMD_LOCNDF(G_MLOC,NDFS(ISEL),GLOC,STATUS)
              CALL GCB_LOAD(GLOC,STATUS)
            ELSE
              CALL GCB_LOAD(G_LOC,STATUS)
            ENDIF
          ENDIF

          CALL GCB_CANL('STEP_FLAG',STATUS)
          CALL GCB_CANI('STEP_STYLE',STATUS)
          CALL GCB_CANI('STEP_WIDTH',STATUS)
          CALL GCB_CANI('STEP_COLOUR',STATUS)

          IF (.NOT.LIVE) THEN
            IF (G_MULTI) THEN
              CALL GCB_SAVE(GLOC,STATUS)
              CALL BDA_RELEASE(GLOC,STATUS)
              CALL DAT_ANNUL(GLOC,STATUS)
            ELSE
              CALL GCB_SAVE(G_LOC,STATUS)
            ENDIF
          ENDIF

        ENDDO


        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GSET_STEP_CANCEL',STATUS)
        ENDIF

      ENDIF
      END

*+
      SUBROUTINE GSET_STEP_SET(LIVE,NSEL,NDFS,STATUS)

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
      LOGICAL LIVE
      INTEGER NSEL
      INTEGER NDFS(*)
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'GFX_CMN'
      INCLUDE 'GMD_CMN'
*    Local Constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) GLOC
      INTEGER ISEL
      INTEGER STYLE,WIDTH,COLOUR
      LOGICAL SETS,SETW,SETC
*-

      SETS=.TRUE.
      SETW=.TRUE.
      SETC=.TRUE.

      IF (STATUS.EQ.SAI__OK) THEN

*  get individual control parameters - NULL means no change
        CALL PAR_GET0I('STYLE',STYLE,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
          SETS=.FALSE.
        ENDIF

        CALL PAR_GET0I('WIDTH',WIDTH,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
          SETW=.FALSE.
        ENDIF

        CALL PAR_GET0I('COLOUR',COLOUR,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
          SETC=.FALSE.
        ENDIF

*  NSEL is set to 1 for LIVE mode or single dataset
*  otherwise to number of NDFs selected from multiple dataset

        DO ISEL=1,NSEL

*  if not in interactive mode
          IF (.NOT.LIVE) THEN
*  load existing GCB
            IF (G_MULTI) THEN
              IF (ISEL.EQ.1) THEN
                CALL MSG_PRNT('Multiple dataset:-')
              ENDIF
              CALL MSG_SETI('NDF',NDFS(ISEL))
              CALL MSG_PRNT(' processing NDF ^NDF...')
*  from component of multiple dataset
              CALL GMD_LOCNDF(G_MLOC,NDFS(ISEL),GLOC,STATUS)
              CALL GCB_LOAD(GLOC,STATUS)
            ELSE
*  or single dataset
              CALL GCB_LOAD(G_LOC,STATUS)
            ENDIF
          ENDIF

*  set new values
          CALL GCB_SETL('STEP_FLAG',.TRUE.,STATUS)

          IF (SETS) THEN
            CALL GCB_SETI('STEP_STYLE',STYLE,STATUS)
          ENDIF

          IF (SETW) THEN
            CALL GCB_SETI('STEP_WIDTH',WIDTH,STATUS)
          ENDIF

          IF (SETC) THEN
            CALL GCB_SETI('STEP_COLOUR',COLOUR,STATUS)
          ENDIF

*  resave if necessary
          IF (.NOT.LIVE) THEN
            IF (G_MULTI) THEN
              CALL GCB_SAVE(GLOC,STATUS)
              CALL BDA_RELEASE(GLOC,STATUS)
              CALL DAT_ANNUL(GLOC,STATUS)
            ELSE
              CALL GCB_SAVE(G_LOC,STATUS)
            ENDIF
          ENDIF

        ENDDO

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GSET_STEP_SET',STATUS)
        ENDIF

      ENDIF
      END




*+
      SUBROUTINE GSET_POINTS_CANCEL(LIVE,NSEL,NDFS,STATUS)

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
      LOGICAL LIVE
      INTEGER NSEL
      INTEGER NDFS(*)
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'GFX_CMN'
      INCLUDE 'GMD_CMN'
*    Local Constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) GLOC
      INTEGER ISEL
*-

      IF (STATUS.EQ.SAI__OK) THEN

        DO ISEL=1,NSEL

          IF (.NOT.LIVE) THEN
            IF (G_MULTI) THEN
              IF (ISEL.EQ.1) THEN
                CALL MSG_PRNT('Multiple dataset:-')
              ENDIF
              CALL MSG_SETI('NDF',NDFS(ISEL))
              CALL MSG_PRNT(' processing NDF ^NDF...')
              CALL GMD_LOCNDF(G_MLOC,NDFS(ISEL),GLOC,STATUS)
              CALL GCB_LOAD(GLOC,STATUS)
            ELSE
              CALL GCB_LOAD(G_LOC,STATUS)
            ENDIF
          ENDIF

          CALL GCB_CANL('POINT_FLAG',STATUS)
          CALL GCB_CANI('POINT_SYMBOL',STATUS)
          CALL GCB_CANR('POINT_SIZE',STATUS)
          CALL GCB_CANI('POINT_BOLD',STATUS)
          CALL GCB_CANI('POINT_COLOUR',STATUS)

          IF (.NOT.LIVE) THEN
            IF (G_MULTI) THEN
              CALL GCB_SAVE(GLOC,STATUS)
              CALL BDA_RELEASE(GLOC,STATUS)
              CALL DAT_ANNUL(GLOC,STATUS)
            ELSE
              CALL GCB_SAVE(G_LOC,STATUS)
            ENDIF
          ENDIF

        ENDDO


        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GSET_POINTS_CANCEL',STATUS)
        ENDIF

      ENDIF
      END

*+
      SUBROUTINE GSET_POINTS_SET(LIVE,NSEL,NDFS,STATUS)

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
      LOGICAL LIVE
      INTEGER NSEL
      INTEGER NDFS(*)
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'GFX_CMN'
      INCLUDE 'GMD_CMN'
*    Local Constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) GLOC
      INTEGER ISEL
      INTEGER SYMBOL,BOLD,COLOUR
      REAL SIZE
      LOGICAL SETS,SETB,SETC,SETZ
*-

      SETS=.TRUE.
      SETB=.TRUE.
      SETC=.TRUE.
      SETZ=.TRUE.

      IF (STATUS.EQ.SAI__OK) THEN

*  get individual control parameters - NULL means no change
        CALL PAR_GET0I('SYMBOL',SYMBOL,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
          SETS=.FALSE.
        ENDIF

        CALL PAR_GET0I('BOLD',BOLD,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
          SETB=.FALSE.
        ENDIF

        CALL PAR_GET0I('COLOUR',COLOUR,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
          SETC=.FALSE.
        ENDIF

        CALL PAR_GET0R('SIZE',SIZE,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
          SETZ=.FALSE.
        ENDIF

*  NSEL is set to 1 for LIVE mode or single dataset
*  otherwise to number of NDFs selected from multiple dataset

        DO ISEL=1,NSEL

*  if not in interactive mode
          IF (.NOT.LIVE) THEN
*  load existing GCB
            IF (G_MULTI) THEN
              IF (ISEL.EQ.1) THEN
                CALL MSG_PRNT('Multiple dataset:-')
              ENDIF
              CALL MSG_SETI('NDF',NDFS(ISEL))
              CALL MSG_PRNT(' processing NDF ^NDF...')
*  from component of multiple dataset
              CALL GMD_LOCNDF(G_MLOC,NDFS(ISEL),GLOC,STATUS)
              CALL GCB_LOAD(GLOC,STATUS)
            ELSE
*  or single dataset
              CALL GCB_LOAD(G_LOC,STATUS)
            ENDIF
          ENDIF

*  set new values
          CALL GCB_SETL('POINT_FLAG',.TRUE.,STATUS)

          IF (SETS) THEN
            CALL GCB_SETI('POINT_SYMBOL',SYMBOL,STATUS)
          ENDIF

          IF (SETB) THEN
            CALL GCB_SETI('POINT_BOLD',BOLD,STATUS)
          ENDIF

          IF (SETC) THEN
            CALL GCB_SETI('POINT_COLOUR',COLOUR,STATUS)
          ENDIF

          IF (SETZ) THEN
            CALL GCB_SETR('POINT_SIZE',SIZE,STATUS)
          ENDIF

*  resave if necessary
          IF (.NOT.LIVE) THEN
            IF (G_MULTI) THEN
              CALL GCB_SAVE(GLOC,STATUS)
              CALL BDA_RELEASE(GLOC,STATUS)
              CALL DAT_ANNUL(GLOC,STATUS)
            ELSE
              CALL GCB_SAVE(G_LOC,STATUS)
            ENDIF
          ENDIF

        ENDDO

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GSET_POINTS_SET',STATUS)
        ENDIF

      ENDIF
      END



*+
      SUBROUTINE GSET_PIXEL_CANCEL(LIVE,NSEL,NDFS,STATUS)

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
      LOGICAL LIVE
      INTEGER NSEL
      INTEGER NDFS(*)
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'GFX_CMN'
      INCLUDE 'GMD_CMN'
*    Local Constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) GLOC
      INTEGER ISEL
*-

      IF (STATUS.EQ.SAI__OK) THEN

        DO ISEL=1,NSEL

          IF (.NOT.LIVE) THEN
            IF (G_MULTI) THEN
              IF (ISEL.EQ.1) THEN
                CALL MSG_PRNT('Multiple dataset:-')
              ENDIF
              CALL MSG_SETI('NDF',NDFS(ISEL))
              CALL MSG_PRNT(' processing NDF ^NDF...')
              CALL GMD_LOCNDF(G_MLOC,NDFS(ISEL),GLOC,STATUS)
              CALL GCB_LOAD(GLOC,STATUS)
            ELSE
              CALL GCB_LOAD(G_LOC,STATUS)
            ENDIF
          ENDIF

          CALL GCB_CANL('PIX_FLAG',STATUS)
          CALL GCB_CANR('PIX_MIN',STATUS)
          CALL GCB_CANR('PIX_MAX',STATUS)
          CALL GCB_CANC('PIX_SCALING',STATUS)
          CALL GCB_CANI('PIX_CYCLES',STATUS)

          IF (.NOT.LIVE) THEN
            IF (G_MULTI) THEN
              CALL GCB_SAVE(GLOC,STATUS)
              CALL BDA_RELEASE(GLOC,STATUS)
              CALL DAT_ANNUL(GLOC,STATUS)
            ELSE
              CALL GCB_SAVE(G_LOC,STATUS)
            ENDIF
          ENDIF

        ENDDO


        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GSET_PIXEL_CANCEL',STATUS)
        ENDIF

      ENDIF
      END

*+
      SUBROUTINE GSET_PIXEL_SET(LIVE,NSEL,NDFS,STATUS)

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
      LOGICAL LIVE
      INTEGER NSEL
      INTEGER NDFS(*)
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'GFX_CMN'
      INCLUDE 'GMD_CMN'
*    Local Constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) GLOC
      CHARACTER*10 SCALING
      INTEGER ISEL
      INTEGER CYCLES
      REAL MIN,MAX
      LOGICAL SETMIN,SETMAX,SETSCA,SETCYC
*-

      SETMIN=.TRUE.
      SETMAX=.TRUE.
      SETSCA=.TRUE.
      SETCYC=.FALSE.

      IF (STATUS.EQ.SAI__OK) THEN

*  get individual control parameters - NULL means no change
        CALL PAR_GET0C('SCALING',SCALING,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
          SETSCA=.FALSE.
        ELSE
          CALL CHR_UCASE(SCALING)
          IF (SCALING(:3).EQ.'CYC') THEN
            CALL PAR_GET0I('CYCLES',CYCLES,STATUS)
            IF (STATUS.EQ.PAR__NULL) THEN
              CALL ERR_ANNUL(STATUS)
            ELSE
              SETCYC=.TRUE.
            ENDIF
          ENDIF
        ENDIF

        CALL PAR_GET0R('MIN',MIN,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
          SETMIN=.FALSE.
        ENDIF

        CALL PAR_GET0R('MAX',MAX,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
          SETMAX=.FALSE.
        ENDIF

*  NSEL is set to 1 for LIVE mode or single dataset
*  otherwise to number of NDFs selected from multiple dataset

        DO ISEL=1,NSEL

*  if not in interactive mode
          IF (.NOT.LIVE) THEN
*  load existing GCB
            IF (G_MULTI) THEN
              IF (ISEL.EQ.1) THEN
                CALL MSG_PRNT('Multiple dataset:-')
              ENDIF
              CALL MSG_SETI('NDF',NDFS(ISEL))
              CALL MSG_PRNT(' processing NDF ^NDF...')
*  from component of multiple dataset
              CALL GMD_LOCNDF(G_MLOC,NDFS(ISEL),GLOC,STATUS)
              CALL GCB_LOAD(GLOC,STATUS)
            ELSE
*  or single dataset
              CALL GCB_LOAD(G_LOC,STATUS)
            ENDIF
          ENDIF

*  set new values
          CALL GCB_SETL('PIX_FLAG',.TRUE.,STATUS)

          IF (SETSCA) THEN
            CALL GCB_SETC('PIX_SCALING',SCALING,STATUS)
          ENDIF

          IF (SETCYC) THEN
            CALL GCB_SETI('PIX_CYCLES',CYCLES,STATUS)
          ENDIF

          IF (SETMIN) THEN
            CALL GCB_SETR('PIX_MIN',MIN,STATUS)
          ENDIF

          IF (SETMAX) THEN
            CALL GCB_SETR('PIX_MAX',MAX,STATUS)
          ENDIF

*  resave if necessary
          IF (.NOT.LIVE) THEN
            IF (G_MULTI) THEN
              CALL GCB_SAVE(GLOC,STATUS)
              CALL BDA_RELEASE(GLOC,STATUS)
              CALL DAT_ANNUL(GLOC,STATUS)
            ELSE
              CALL GCB_SAVE(G_LOC,STATUS)
            ENDIF
          ENDIF

        ENDDO

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GSET_PIXEL_SET',STATUS)
        ENDIF

      ENDIF
      END



*+
      SUBROUTINE GSET_CONTOUR_CANCEL(LIVE,NSEL,NDFS,STATUS)

*    Authors :
*             (BHVAD::RJV)
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'GCB_PAR'
*    Import :
      LOGICAL LIVE
      INTEGER NSEL
      INTEGER NDFS(*)
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'GFX_CMN'
      INCLUDE 'GMD_CMN'
*    Local Constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) GLOC
      INTEGER ISEL
      INTEGER N
      LOGICAL OK
*-

      IF (STATUS.EQ.SAI__OK) THEN

        DO ISEL=1,NSEL

          IF (.NOT.LIVE) THEN
            IF (G_MULTI) THEN
              IF (ISEL.EQ.1) THEN
                CALL MSG_PRNT('Multiple dataset:-')
              ENDIF
              CALL MSG_SETI('NDF',NDFS(ISEL))
              CALL MSG_PRNT(' processing NDF ^NDF...')
              CALL GMD_LOCNDF(G_MLOC,NDFS(ISEL),GLOC,STATUS)
              CALL GCB_LOAD(GLOC,STATUS)
            ELSE
              CALL GCB_LOAD(G_LOC,STATUS)
            ENDIF
          ENDIF

          CALL GCB_CANL('CONT_FLAG',STATUS)
          CALL GCB_GETI('CONT_N',OK,N,STATUS)
          IF (.NOT.OK) THEN
            N=1
          ENDIF
          CALL GCB_CANI('CONT_N',STATUS)
          CALL GCB_CAN1R('CONT_LEVS',1,N,STATUS)
          CALL GCB_CAN1I('CONT_STYLE',1,N,STATUS)
          CALL GCB_CAN1I('CONT_WIDTH',1,N,STATUS)
          CALL GCB_CAN1I('CONT_COLOUR',1,N,STATUS)

          IF (.NOT.LIVE) THEN
            IF (G_MULTI) THEN
              CALL GCB_SAVE(GLOC,STATUS)
              CALL BDA_RELEASE(GLOC,STATUS)
              CALL DAT_ANNUL(GLOC,STATUS)
            ELSE
              CALL GCB_SAVE(G_LOC,STATUS)
            ENDIF
          ENDIF

        ENDDO


        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GSET_CONTOUR_CANCEL',STATUS)
        ENDIF

      ENDIF
      END

*+
      SUBROUTINE GSET_CONTOUR_SET(LIVE,NSEL,NDFS,STATUS)

*    Authors :
*             (BHVAD::RJV)
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'GCB_PAR'
*    Import :
      LOGICAL LIVE
      INTEGER NSEL
      INTEGER NDFS(*)
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'GFX_CMN'
      INCLUDE 'GMD_CMN'
*    Local Constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) GLOC
      INTEGER ISEL
      INTEGER NLEV
      INTEGER NSTYLE,NWIDTH,NCOL
      INTEGER I
      INTEGER STYLE(GCB__MXCONTLEV)
      INTEGER WIDTH(GCB__MXCONTLEV)
      INTEGER COLOUR(GCB__MXCONTLEV)
      REAL LEVS(GCB__MXCONTLEV)
      LOGICAL SETL,SETS,SETW,SETC
      LOGICAL OK
*-

      SETL=.TRUE.
      SETS=.TRUE.
      SETW=.TRUE.
      SETC=.TRUE.

      IF (STATUS.EQ.SAI__OK) THEN

*  get individual control parameters - NULL means no change
        CALL PAR_GET1R('LEVELS',GCB__MXCONTLEV,LEVS,NLEV,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
          SETL=.FALSE.
        ENDIF

        CALL PAR_GET1I('STYLE',GCB__MXCONTLEV,STYLE,NSTYLE,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
          SETS=.FALSE.
        ENDIF

        CALL PAR_GET1I('WIDTH',GCB__MXCONTLEV,WIDTH,NWIDTH,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
          SETW=.FALSE.
        ENDIF

        CALL PAR_GET1I('COLOUR',GCB__MXCONTLEV,COLOUR,NCOL,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
          SETC=.FALSE.
        ENDIF

*  NSEL is set to 1 for LIVE mode or single dataset
*  otherwise to number of NDFs selected from multiple dataset

        DO ISEL=1,NSEL

*  if not in interactive mode
          IF (.NOT.LIVE) THEN
*  load existing GCB
            IF (G_MULTI) THEN
              IF (ISEL.EQ.1) THEN
                CALL MSG_PRNT('Multiple dataset:-')
              ENDIF
              CALL MSG_SETI('NDF',NDFS(ISEL))
              CALL MSG_PRNT(' processing NDF ^NDF...')
*  from component of multiple dataset
              CALL GMD_LOCNDF(G_MLOC,NDFS(ISEL),GLOC,STATUS)
              CALL GCB_LOAD(GLOC,STATUS)
            ELSE
*  or single dataset
              CALL GCB_LOAD(G_LOC,STATUS)
            ENDIF
          ENDIF

          IF (.NOT.SETL) THEN
            CALL GCB_GETI('CONT_N',OK,NLEV,STATUS)
            IF (.NOT.OK) THEN
              NLEV=0
            ENDIF
          ENDIF


*  set new values
          CALL GCB_SETL('CONT_FLAG',.TRUE.,STATUS)
          CALL GCB_SETI('CONT_N',NLEV,STATUS)

          IF (SETL) THEN
            CALL GCB_SET1R('CONT_LEVS',1,NLEV,LEVS,STATUS)
          ENDIF

          IF (SETS) THEN
            DO I=NSTYLE+1,NLEV
              STYLE(I)=STYLE(NSTYLE)
            ENDDO
            CALL GCB_SET1I('CONT_STYLE',1,NLEV,STYLE,STATUS)
          ENDIF

          IF (SETW) THEN
            DO I=NWIDTH+1,NLEV
              WIDTH(I)=WIDTH(NWIDTH)
            ENDDO
            CALL GCB_SET1I('CONT_WIDTH',1,NLEV,WIDTH,STATUS)
          ENDIF

          IF (SETC) THEN
            DO I=NCOL+1,NLEV
              COLOUR(I)=COLOUR(NCOL)
            ENDDO
            CALL GCB_SET1I('CONT_COLOUR',1,NLEV,COLOUR,STATUS)
          ENDIF

*  resave if necessary
          IF (.NOT.LIVE) THEN
            IF (G_MULTI) THEN
              CALL GCB_SAVE(GLOC,STATUS)
              CALL BDA_RELEASE(GLOC,STATUS)
              CALL DAT_ANNUL(GLOC,STATUS)
            ELSE
              CALL GCB_SAVE(G_LOC,STATUS)
            ENDIF
          ENDIF

        ENDDO

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GSET_CONTOUR_SET',STATUS)
        ENDIF

      ENDIF
      END



*+
      SUBROUTINE GSET_SURF_CANCEL(LIVE,NSEL,NDFS,STATUS)

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
      LOGICAL LIVE
      INTEGER NSEL
      INTEGER NDFS(*)
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'GFX_CMN'
      INCLUDE 'GMD_CMN'
*    Local Constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) GLOC
      INTEGER ISEL
*-

      IF (STATUS.EQ.SAI__OK) THEN

        DO ISEL=1,NSEL

          IF (.NOT.LIVE) THEN
            IF (G_MULTI) THEN
              IF (ISEL.EQ.1) THEN
                CALL MSG_PRNT('Multiple dataset:-')
              ENDIF
              CALL MSG_SETI('NDF',NDFS(ISEL))
              CALL MSG_PRNT(' processing NDF ^NDF...')
              CALL GMD_LOCNDF(G_MLOC,NDFS(ISEL),GLOC,STATUS)
              CALL GCB_LOAD(GLOC,STATUS)
            ELSE
              CALL GCB_LOAD(G_LOC,STATUS)
            ENDIF
          ENDIF

          CALL GCB_CANL('SURF_FLAG',STATUS)
          CALL GCB_CANR('SURF_ANGLE',STATUS)

          IF (.NOT.LIVE) THEN
            IF (G_MULTI) THEN
              CALL GCB_SAVE(GLOC,STATUS)
              CALL BDA_RELEASE(GLOC,STATUS)
              CALL DAT_ANNUL(GLOC,STATUS)
            ELSE
              CALL GCB_SAVE(G_LOC,STATUS)
            ENDIF
          ENDIF

        ENDDO


        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GSET_SURF_CANCEL',STATUS)
        ENDIF

      ENDIF
      END

*+
      SUBROUTINE GSET_SURF_SET(LIVE,NSEL,NDFS,STATUS)

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
      LOGICAL LIVE
      INTEGER NSEL
      INTEGER NDFS(*)
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'GFX_CMN'
      INCLUDE 'GMD_CMN'
*    Local Constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) GLOC
      INTEGER ISEL
      REAL ANGLE
      LOGICAL SETA
*-

      SETA=.TRUE.

      IF (STATUS.EQ.SAI__OK) THEN

*  get individual control parameters - NULL means no change
        CALL PAR_GET0R('ANGLE',ANGLE,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
          SETA=.FALSE.
        ENDIF

*  NSEL is set to 1 for LIVE mode or single dataset
*  otherwise to number of NDFs selected from multiple dataset

        DO ISEL=1,NSEL

*  if not in interactive mode
          IF (.NOT.LIVE) THEN
*  load existing GCB
            IF (G_MULTI) THEN
              IF (ISEL.EQ.1) THEN
                CALL MSG_PRNT('Multiple dataset:-')
              ENDIF
              CALL MSG_SETI('NDF',NDFS(ISEL))
              CALL MSG_PRNT(' processing NDF ^NDF...')
*  from component of multiple dataset
              CALL GMD_LOCNDF(G_MLOC,NDFS(ISEL),GLOC,STATUS)
              CALL GCB_LOAD(GLOC,STATUS)
            ELSE
*  or single dataset
              CALL GCB_LOAD(G_LOC,STATUS)
            ENDIF
          ENDIF

*  set new values
          CALL GCB_SETL('SURF_FLAG',.TRUE.,STATUS)

          IF (SETA) THEN
            CALL GCB_SETR('SURF_ANGLE',ANGLE,STATUS)
          ENDIF

*  resave if necessary
          IF (.NOT.LIVE) THEN
            IF (G_MULTI) THEN
              CALL GCB_SAVE(GLOC,STATUS)
              CALL BDA_RELEASE(GLOC,STATUS)
              CALL DAT_ANNUL(GLOC,STATUS)
            ELSE
              CALL GCB_SAVE(G_LOC,STATUS)
            ENDIF
          ENDIF

        ENDDO

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GSET_SURF_SET',STATUS)
        ENDIF

      ENDIF
      END



*+
      SUBROUTINE GSET_GRID_CANCEL(LIVE,NSEL,NDFS,STATUS)

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
      LOGICAL LIVE
      INTEGER NSEL
      INTEGER NDFS(*)
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'GFX_CMN'
      INCLUDE 'GMD_CMN'
*    Local Constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) GLOC
      INTEGER ISEL
*-

      IF (STATUS.EQ.SAI__OK) THEN

        DO ISEL=1,NSEL

          IF (.NOT.LIVE) THEN
            IF (G_MULTI) THEN
              IF (ISEL.EQ.1) THEN
                CALL MSG_PRNT('Multiple dataset:-')
              ENDIF
              CALL MSG_SETI('NDF',NDFS(ISEL))
              CALL MSG_PRNT(' processing NDF ^NDF...')
              CALL GMD_LOCNDF(G_MLOC,NDFS(ISEL),GLOC,STATUS)
              CALL GCB_LOAD(GLOC,STATUS)
            ELSE
              CALL GCB_LOAD(G_LOC,STATUS)
            ENDIF
          ENDIF

          CALL GCB_CANI('GRID_FRAME',STATUS)
          CALL GCB_CANI('GRID_POS',STATUS)

          IF (.NOT.LIVE) THEN
            IF (G_MULTI) THEN
              CALL GCB_SAVE(GLOC,STATUS)
              CALL BDA_RELEASE(GLOC,STATUS)
              CALL DAT_ANNUL(GLOC,STATUS)
            ELSE
              CALL GCB_SAVE(G_LOC,STATUS)
            ENDIF
          ENDIF

        ENDDO


        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GSET_GRID_CANCEL',STATUS)
        ENDIF

      ENDIF
      END

*+
      SUBROUTINE GSET_GRID_SET(LIVE,NSEL,NDFS,STATUS)

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
      LOGICAL LIVE
      INTEGER NSEL
      INTEGER NDFS(*)
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'GFX_CMN'
      INCLUDE 'GMD_CMN'
*    Local Constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) GLOC
      INTEGER ISEL
      INTEGER FRAME,POS
      LOGICAL SETF,SETP
*-

      SETF=.TRUE.
      SETP=.TRUE.

      IF (STATUS.EQ.SAI__OK) THEN

*  get individual control parameters - NULL means no change
        CALL PAR_GET0I('FRAME',FRAME,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
          SETF=.FALSE.
        ENDIF

        CALL PAR_GET0I('POS',POS,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
          SETP=.FALSE.
        ENDIF

*  NSEL is set to 1 for LIVE mode or single dataset
*  otherwise to number of NDFs selected from multiple dataset

        DO ISEL=1,NSEL

*  if not in interactive mode
          IF (.NOT.LIVE) THEN
*  load existing GCB
            IF (G_MULTI) THEN
              IF (ISEL.EQ.1) THEN
                CALL MSG_PRNT('Multiple dataset:-')
              ENDIF
              CALL MSG_SETI('NDF',NDFS(ISEL))
              CALL MSG_PRNT(' processing NDF ^NDF...')
*  from component of multiple dataset
              CALL GMD_LOCNDF(G_MLOC,NDFS(ISEL),GLOC,STATUS)
              CALL GCB_LOAD(GLOC,STATUS)
            ELSE
*  or single dataset
              CALL GCB_LOAD(G_LOC,STATUS)
            ENDIF
          ENDIF

*  set new values
          IF (SETF) THEN
            CALL GCB_SETI('GRID_FRAME',FRAME,STATUS)
          ENDIF

          IF (SETP) THEN
            CALL GCB_SETI('GRID_POS',POS,STATUS)
          ENDIF

*  resave if necessary
          IF (.NOT.LIVE) THEN
            IF (G_MULTI) THEN
              CALL GCB_SAVE(GLOC,STATUS)
              CALL BDA_RELEASE(GLOC,STATUS)
              CALL DAT_ANNUL(GLOC,STATUS)
            ELSE
              CALL GCB_SAVE(G_LOC,STATUS)
            ENDIF
          ENDIF

        ENDDO

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GSET_GRID_SET',STATUS)
        ENDIF

      ENDIF
      END




*+
      SUBROUTINE GSET_ERR_CANCEL(LIVE,NSEL,NDFS,STATUS)

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
      LOGICAL LIVE
      INTEGER NSEL
      INTEGER NDFS(*)
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'GFX_CMN'
      INCLUDE 'GMD_CMN'
*    Local Constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) GLOC
      INTEGER ISEL
*-

      IF (STATUS.EQ.SAI__OK) THEN

        DO ISEL=1,NSEL

          IF (.NOT.LIVE) THEN
            IF (G_MULTI) THEN
              IF (ISEL.EQ.1) THEN
                CALL MSG_PRNT('Multiple dataset:-')
              ENDIF
              CALL MSG_SETI('NDF',NDFS(ISEL))
              CALL MSG_PRNT(' processing NDF ^NDF...')
              CALL GMD_LOCNDF(G_MLOC,NDFS(ISEL),GLOC,STATUS)
              CALL GCB_LOAD(GLOC,STATUS)
            ELSE
              CALL GCB_LOAD(G_LOC,STATUS)
            ENDIF
          ENDIF

          CALL GCB_CANL('ERR_FLAG',STATUS)
          CALL GCB_CANI('ERR_SHAPE',STATUS)
          CALL GCB_CANI('ERR_WIDTH',STATUS)
          CALL GCB_CANI('ERR_COLOUR',STATUS)

          IF (.NOT.LIVE) THEN
            IF (G_MULTI) THEN
              CALL GCB_SAVE(GLOC,STATUS)
              CALL BDA_RELEASE(GLOC,STATUS)
              CALL DAT_ANNUL(GLOC,STATUS)
            ELSE
              CALL GCB_SAVE(G_LOC,STATUS)
            ENDIF
          ENDIF

        ENDDO


        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GSET_ERR_CANCEL',STATUS)
        ENDIF

      ENDIF
      END

*+
      SUBROUTINE GSET_ERR_SET(LIVE,NSEL,NDFS,STATUS)

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
      LOGICAL LIVE
      INTEGER NSEL
      INTEGER NDFS(*)
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'GFX_CMN'
      INCLUDE 'GMD_CMN'
*    Local Constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) GLOC
      INTEGER ISEL
      INTEGER SHAPE,WIDTH,COLOUR
      LOGICAL SETS,SETW,SETC
*-

      SETS=.TRUE.
      SETW=.TRUE.
      SETC=.TRUE.

      IF (STATUS.EQ.SAI__OK) THEN

*  get individual control parameters - NULL means no change
        CALL PAR_GET0I('SHAPE',SHAPE,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
          SETS=.FALSE.
        ENDIF

        CALL PAR_GET0I('WIDTH',WIDTH,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
          SETW=.FALSE.
        ENDIF

        CALL PAR_GET0I('COLOUR',COLOUR,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
          SETC=.FALSE.
        ENDIF

*  NSEL is set to 1 for LIVE mode or single dataset
*  otherwise to number of NDFs selected from multiple dataset

        DO ISEL=1,NSEL

*  if not in interactive mode
          IF (.NOT.LIVE) THEN
*  load existing GCB
            IF (G_MULTI) THEN
              IF (ISEL.EQ.1) THEN
                CALL MSG_PRNT('Multiple dataset:-')
              ENDIF
              CALL MSG_SETI('NDF',NDFS(ISEL))
              CALL MSG_PRNT(' processing NDF ^NDF...')
*  from component of multiple dataset
              CALL GMD_LOCNDF(G_MLOC,NDFS(ISEL),GLOC,STATUS)
              CALL GCB_LOAD(GLOC,STATUS)
            ELSE
*  or single dataset
              CALL GCB_LOAD(G_LOC,STATUS)
            ENDIF
          ENDIF

*  set new values
          CALL GCB_SETL('ERR_FLAG',.TRUE.,STATUS)

          IF (SETS) THEN
            CALL GCB_SETI('ERR_SHAPE',SHAPE,STATUS)
          ENDIF

          IF (SETW) THEN
            CALL GCB_SETI('ERR_WIDTH',WIDTH,STATUS)
          ENDIF

          IF (SETC) THEN
            CALL GCB_SETI('ERR_COLOUR',COLOUR,STATUS)
          ENDIF

*  resave if necessary
          IF (.NOT.LIVE) THEN
            IF (G_MULTI) THEN
              CALL GCB_SAVE(GLOC,STATUS)
              CALL BDA_RELEASE(GLOC,STATUS)
              CALL DAT_ANNUL(GLOC,STATUS)
            ELSE
              CALL GCB_SAVE(G_LOC,STATUS)
            ENDIF
          ENDIF

        ENDDO

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GSET_ERR_SET',STATUS)
        ENDIF

      ENDIF
      END


*+
      SUBROUTINE GSET_TITLE_CANCEL(LIVE,NDF,STATUS)

*    Authors :
*             (BHVAD::RJV)
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'GCB_PAR'
*    Import :
      LOGICAL LIVE
      INTEGER NDF
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'GFX_CMN'
      INCLUDE 'GMD_CMN'
*    Local Constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) GLOC
      INTEGER N
      LOGICAL OK
*-

      IF (STATUS.EQ.SAI__OK) THEN

        IF (.NOT.LIVE) THEN
          IF (G_MULTI) THEN
            CALL MSG_PRNT('Multiple dataset:-')
            CALL MSG_SETI('NDF',NDF)
            CALL MSG_PRNT(' processing NDF ^NDF...')
            CALL GMD_LOCNDF(G_MLOC,NDF,GLOC,STATUS)
            CALL GCB_LOAD(GLOC,STATUS)
          ELSE
            CALL GCB_LOAD(G_LOC,STATUS)
          ENDIF
        ENDIF

        CALL GCB_GETI('TITLE_N',OK,N,STATUS)
        IF (.NOT.OK) THEN
          N=1
        ENDIF
        CALL GCB_CANI('TITLE_N',STATUS)
        CALL GCB_CAN1C('TITLE_TEXT',1,N,STATUS)
        CALL GCB_CAN1I('TITLE_FONT',1,N,STATUS)
        CALL GCB_CAN1I('TITLE_BOLD',1,N,STATUS)
        CALL GCB_CAN1R('TITLE_SIZE',1,N,STATUS)
        CALL GCB_CAN1C('TITLE_JUST',1,N,STATUS)

        IF (.NOT.LIVE) THEN
          IF (G_MULTI) THEN
            CALL GCB_SAVE(GLOC,STATUS)
            CALL BDA_RELEASE(GLOC,STATUS)
            CALL DAT_ANNUL(GLOC,STATUS)
          ELSE
            CALL GCB_SAVE(G_LOC,STATUS)
          ENDIF
        ENDIF


        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GSET_TITLE_CANCEL',STATUS)
        ENDIF

      ENDIF
      END

*+
      SUBROUTINE GSET_TITLE_SET(LIVE,NDF,STATUS)

*    Authors :
*             (BHVAD::RJV)
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'GCB_PAR'
*    Import :
      LOGICAL LIVE
      INTEGER NDF
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'GFX_CMN'
      INCLUDE 'GMD_CMN'
*    Functions :
      INTEGER CHR_LEN
*    Local Constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) GLOC
      CHARACTER*80 TEXT
      CHARACTER*1 JUST
      INTEGER N,N1,N2,N3
      INTEGER FONT,BOLD
      INTEGER L
      REAL SIZE
      LOGICAL OK
      LOGICAL RPT
*-

      IF (STATUS.EQ.SAI__OK) THEN

*  if not in interactive mode
        IF (.NOT.LIVE) THEN
*  load existing GCB
          IF (G_MULTI) THEN
            CALL MSG_PRNT('Multiple dataset:-')
            CALL MSG_SETI('NDF',NDF)
            CALL MSG_PRNT(' processing NDF ^NDF...')
*  from component of multiple dataset
            CALL GMD_LOCNDF(G_MLOC,NDF,GLOC,STATUS)
            CALL GCB_LOAD(GLOC,STATUS)
          ELSE
*  or single dataset
            CALL GCB_LOAD(G_LOC,STATUS)
          ENDIF
        ENDIF

*  get existing and new number
        CALL GCB_GETI('TITLE_N',OK,N1,STATUS)
        IF (.NOT.OK) THEN
          N1=0
        ENDIF
        N2=N1+1
        CALL PAR_DEF0I('NUM',N2,STATUS)
        CALL PAR_GET0I('NUM',N2,STATUS)
        IF (N2.GT.N1) THEN
          N2=N1+1
        ENDIF


*  get individual control parameters - NULL means no change
        RPT=.TRUE.
        N3=N2
        DO WHILE (RPT)
          CALL PAR_GET0C('TEXT',TEXT,STATUS)
          IF (STATUS.EQ.PAR__NULL) THEN
            CALL ERR_ANNUL(STATUS)
            RPT=.FALSE.
          ELSE
            L=CHR_LEN(TEXT)
            RPT=(TEXT(L:L).EQ.'~')
            IF (RPT) THEN
              TEXT=TEXT(:L-1)
            ENDIF
            CALL GCB_SET1C('TITLE_TEXT',N3,1,TEXT,STATUS)
          ENDIF
          IF (RPT) THEN
            N3=N3+1
            CALL PAR_CANCL('TEXT',STATUS)
          ENDIF
        ENDDO

        IF (N3.GT.N1) THEN
          CALL GCB_SETI('TITLE_N',N3,STATUS)
        ENDIF

        CALL PAR_GET0I('FONT',FONT,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
        ELSE
          DO N=N2,N3
            CALL GCB_SET1I('TITLE_FONT',N,1,FONT,STATUS)
          ENDDO
        ENDIF

        CALL PAR_GET0I('BOLD',BOLD,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
        ELSE
          DO N=N2,N3
            CALL GCB_SET1I('TITLE_BOLD',N,1,BOLD,STATUS)
          ENDDO
        ENDIF

        CALL PAR_GET0R('SIZE',SIZE,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
        ELSE
          DO N=N2,N3
            CALL GCB_SET1R('TITLE_SIZE',N,1,SIZE,STATUS)
          ENDDO
        ENDIF

        CALL PAR_GET0C('JUST',JUST,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
        ELSE
          CALL CHR_UCASE(JUST)
          DO N=N2,N3
            CALL GCB_SET1C('TITLE_JUST',N,1,JUST,STATUS)
          ENDDO
        ENDIF


*  resave if necessary
        IF (.NOT.LIVE) THEN
          IF (G_MULTI) THEN
            CALL GCB_SAVE(GLOC,STATUS)
            CALL BDA_RELEASE(GLOC,STATUS)
            CALL DAT_ANNUL(GLOC,STATUS)
          ELSE
            CALL GCB_SAVE(G_LOC,STATUS)
          ENDIF
        ENDIF


        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GSET_TITLE_SET',STATUS)
        ENDIF

      ENDIF
      END


*+
      SUBROUTINE GSET_NOTE_CANCEL(LIVE,NDF,STATUS)

*    Authors :
*             (BHVAD::RJV)
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'GCB_PAR'
*    Import :
      LOGICAL LIVE
      INTEGER NDF
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'GFX_CMN'
      INCLUDE 'GMD_CMN'
*    Local Constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) GLOC
      INTEGER N
      LOGICAL OK
*-

      IF (STATUS.EQ.SAI__OK) THEN

        IF (.NOT.LIVE) THEN
          IF (G_MULTI) THEN
            CALL MSG_PRNT('Multiple dataset:-')
            CALL MSG_SETI('NDF',NDF)
            CALL MSG_PRNT(' processing NDF ^NDF...')
            CALL GMD_LOCNDF(G_MLOC,NDF,GLOC,STATUS)
            CALL GCB_LOAD(GLOC,STATUS)
          ELSE
            CALL GCB_LOAD(G_LOC,STATUS)
          ENDIF
        ENDIF

        CALL GCB_GETI('NOTE_N',OK,N,STATUS)
        IF (.NOT.OK) THEN
          N=1
        ENDIF
        CALL GCB_CANI('NOTE_N',STATUS)
        CALL GCB_CAN1C('NOTE_TEXT',1,N,STATUS)
        CALL GCB_CAN1R('NOTE_X',1,N,STATUS)
        CALL GCB_CAN1R('NOTE_Y',1,N,STATUS)
        CALL GCB_CAN1R('NOTE_ANGLE',1,N,STATUS)
        CALL GCB_CAN1I('NOTE_FONT',1,N,STATUS)
        CALL GCB_CAN1I('NOTE_BOLD',1,N,STATUS)
        CALL GCB_CAN1R('NOTE_SIZE',1,N,STATUS)
        CALL GCB_CAN1C('NOTE_JUST',1,N,STATUS)
        CALL GCB_CAN1I('NOTE_COLOUR',1,N,STATUS)

        IF (.NOT.LIVE) THEN
          IF (G_MULTI) THEN
            CALL GCB_SAVE(GLOC,STATUS)
            CALL BDA_RELEASE(GLOC,STATUS)
            CALL DAT_ANNUL(GLOC,STATUS)
          ELSE
            CALL GCB_SAVE(G_LOC,STATUS)
          ENDIF
        ENDIF


        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GSET_NOTE_CANCEL',STATUS)
        ENDIF

      ENDIF
      END

*+
      SUBROUTINE GSET_NOTE_SET(LIVE,NDF,STATUS)

*    Authors :
*             (BHVAD::RJV)
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'GCB_PAR'
*    Import :
      LOGICAL LIVE
      INTEGER NDF
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'GFX_CMN'
      INCLUDE 'GMD_CMN'
*    Local Constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) GLOC
      CHARACTER*80 TEXT
      CHARACTER*1 JUST
      INTEGER N1,N2
      INTEGER FONT,BOLD,COLOUR
      REAL X,Y,ANGLE
      REAL SIZE
      LOGICAL OK
*-

      IF (STATUS.EQ.SAI__OK) THEN

*  if not in interactive mode
        IF (.NOT.LIVE) THEN
*  load existing GCB
          IF (G_MULTI) THEN
            CALL MSG_PRNT('Multiple dataset:-')
            CALL MSG_SETI('NDF',NDF)
            CALL MSG_PRNT(' processing NDF ^NDF...')
*  from component of multiple dataset
            CALL GMD_LOCNDF(G_MLOC,NDF,GLOC,STATUS)
            CALL GCB_LOAD(GLOC,STATUS)
          ELSE
*  or single dataset
            CALL GCB_LOAD(G_LOC,STATUS)
          ENDIF
        ENDIF

*  get existing and new number
        CALL GCB_GETI('NOTE_N',OK,N1,STATUS)
        IF (.NOT.OK) THEN
          N1=0
        ENDIF
        N2=N1+1
        CALL PAR_DEF0I('NUM',N2,STATUS)
        CALL PAR_GET0I('NUM',N2,STATUS)

        IF (N2.GT.N1) THEN
          N2=N1+1
          CALL GCB_SETI('NOTE_N',N2,STATUS)
        ENDIF

*  get individual control parameters - NULL means no change
        CALL PAR_GET0C('TEXT',TEXT,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
        ELSE
          CALL GCB_SET1C('NOTE_TEXT',N2,1,TEXT,STATUS)
        ENDIF

        CALL PAR_GET0R('X',X,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
        ELSE
          CALL GCB_SET1R('NOTE_X',N2,1,X,STATUS)
        ENDIF

        CALL PAR_GET0R('Y',Y,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
        ELSE
          CALL GCB_SET1R('NOTE_Y',N2,1,Y,STATUS)
        ENDIF

        CALL PAR_GET0R('ANGLE',ANGLE,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
        ELSE
          CALL GCB_SET1R('NOTE_ANGLE',N2,1,ANGLE,STATUS)
        ENDIF

        CALL PAR_GET0I('FONT',FONT,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
        ELSE
          CALL GCB_SET1I('NOTE_FONT',N2,1,FONT,STATUS)
        ENDIF

        CALL PAR_GET0I('BOLD',BOLD,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
        ELSE
          CALL GCB_SET1I('NOTE_BOLD',N2,1,BOLD,STATUS)
        ENDIF

        CALL PAR_GET0R('SIZE',SIZE,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
        ELSE
          CALL GCB_SET1R('NOTE_SIZE',N2,1,SIZE,STATUS)
        ENDIF

        CALL PAR_GET0C('JUST',JUST,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
        ELSE
          CALL CHR_UCASE(JUST)
          CALL GCB_SET1C('NOTE_JUST',N2,1,JUST,STATUS)
        ENDIF

        CALL PAR_GET0I('COLOUR',COLOUR,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
        ELSE
          CALL GCB_SET1I('NOTE_COLOUR',N2,1,COLOUR,STATUS)
        ENDIF

*  resave if necessary
        IF (.NOT.LIVE) THEN
          IF (G_MULTI) THEN
            CALL GCB_SAVE(GLOC,STATUS)
            CALL BDA_RELEASE(GLOC,STATUS)
            CALL DAT_ANNUL(GLOC,STATUS)
          ELSE
            CALL GCB_SAVE(G_LOC,STATUS)
          ENDIF
        ENDIF


        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GSET_NOTE_SET',STATUS)
        ENDIF

      ENDIF
      END



*+
      SUBROUTINE GSET_MARKER_CANCEL(LIVE,NDF,STATUS)

*    Authors :
*             (BHVAD::RJV)
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'GCB_PAR'
*    Import :
      LOGICAL LIVE
      INTEGER NDF
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'GFX_CMN'
      INCLUDE 'GMD_CMN'
*    Local Constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) GLOC
      INTEGER N
      LOGICAL OK
*-

      IF (STATUS.EQ.SAI__OK) THEN

        IF (.NOT.LIVE) THEN
          IF (G_MULTI) THEN
            CALL MSG_PRNT('Multiple dataset:-')
            CALL MSG_SETI('NDF',NDF)
            CALL MSG_PRNT(' processing NDF ^NDF...')
            CALL GMD_LOCNDF(G_MLOC,NDF,GLOC,STATUS)
            CALL GCB_LOAD(GLOC,STATUS)
          ELSE
            CALL GCB_LOAD(G_LOC,STATUS)
          ENDIF
        ENDIF

        CALL GCB_GETI('MARKER_N',OK,N,STATUS)
        IF (.NOT.OK) THEN
          N=1
        ENDIF

        CALL GCB_CANI('MARKER_N',STATUS)
        CALL GCB_CAN1I('MARKER_SYMBOL',1,N,STATUS)
        CALL GCB_CAN1R('MARKER_X',1,N,STATUS)
        CALL GCB_CAN1R('MARKER_Y',1,N,STATUS)
        CALL GCB_CAN1I('MARKER_BOLD',1,N,STATUS)
        CALL GCB_CAN1R('MARKER_SIZE',1,N,STATUS)
        CALL GCB_CAN1I('MARKER_COLOUR',1,N,STATUS)
        CALL GCB_CANL('MARKER_NUMBER',STATUS)

        IF (.NOT.LIVE) THEN
          IF (G_MULTI) THEN
            CALL GCB_SAVE(GLOC,STATUS)
            CALL BDA_RELEASE(GLOC,STATUS)
            CALL DAT_ANNUL(GLOC,STATUS)
          ELSE
            CALL GCB_SAVE(G_LOC,STATUS)
          ENDIF
        ENDIF


        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GSET_MARKER_CANCEL',STATUS)
        ENDIF

      ENDIF
      END

*+
      SUBROUTINE GSET_MARKER_SET(LIVE,NDF,STATUS)

*    Authors :
*             (BHVAD::RJV)
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'GCB_PAR'
*    Import :
      LOGICAL LIVE
      INTEGER NDF
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'GFX_CMN'
      INCLUDE 'GMD_CMN'
*    Local Constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) GLOC
      INTEGER N1,N2
      INTEGER SYMBOL
      INTEGER BOLD,COLOUR
      REAL X,Y
      REAL SIZE
      LOGICAL NUMBER
      LOGICAL OK
*-

      IF (STATUS.EQ.SAI__OK) THEN

*  if not in interactive mode
        IF (.NOT.LIVE) THEN
*  load existing GCB
          IF (G_MULTI) THEN
            CALL MSG_PRNT('Multiple dataset:-')
            CALL MSG_SETI('NDF',NDF)
            CALL MSG_PRNT(' processing NDF ^NDF...')
*  from component of multiple dataset
            CALL GMD_LOCNDF(G_MLOC,NDF,GLOC,STATUS)
            CALL GCB_LOAD(GLOC,STATUS)
          ELSE
*  or single dataset
            CALL GCB_LOAD(G_LOC,STATUS)
          ENDIF
        ENDIF

*  get existing and new number
        CALL GCB_GETI('MARKER_N',OK,N1,STATUS)
        IF (.NOT.OK) THEN
          N1=0
        ENDIF
        N2=N1+1
        CALL PAR_DEF0I('NUM',N2,STATUS)
        CALL PAR_GET0I('NUM',N2,STATUS)

        IF (N2.GT.N1) THEN
          N2=N1+1
          CALL GCB_SETI('MARKER_N',N2,STATUS)
        ENDIF

*  get individual control parameters - NULL means no change
        CALL PAR_GET0I('SYMBOL',SYMBOL,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
        ELSE
          CALL GCB_SET1I('MARKER_SYMBOL',N2,1,SYMBOL,STATUS)
        ENDIF

        CALL PAR_GET0R('X',X,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
        ELSE
          CALL GCB_SET1R('MARKER_X',N2,1,X,STATUS)
        ENDIF

        CALL PAR_GET0R('Y',Y,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
        ELSE
          CALL GCB_SET1R('MARKER_Y',N2,1,Y,STATUS)
        ENDIF

        CALL PAR_GET0I('BOLD',BOLD,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
        ELSE
          CALL GCB_SET1I('MARKER_BOLD',N2,1,BOLD,STATUS)
        ENDIF

        CALL PAR_GET0R('SIZE',SIZE,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
        ELSE
          CALL GCB_SET1R('MARKER_SIZE',N2,1,SIZE,STATUS)
        ENDIF

        CALL PAR_GET0I('COLOUR',COLOUR,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
        ELSE
          CALL GCB_SET1I('MARKER_COLOUR',N2,1,COLOUR,STATUS)
        ENDIF

        CALL PAR_GET0L('NUMBER',NUMBER,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
        ELSE
          CALL GCB_SETL('MARKER_NUMBER',NUMBER,STATUS)
        ENDIF

*  resave if necessary
        IF (.NOT.LIVE) THEN
          IF (G_MULTI) THEN
            CALL GCB_SAVE(GLOC,STATUS)
            CALL BDA_RELEASE(GLOC,STATUS)
            CALL DAT_ANNUL(GLOC,STATUS)
          ELSE
            CALL GCB_SAVE(G_LOC,STATUS)
          ENDIF
        ENDIF


        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GSET_MARKER_SET',STATUS)
        ENDIF

      ENDIF
      END

*+
      SUBROUTINE GSET_SHAPE_CANCEL(LIVE,NDF,STATUS)

*    Authors :
*             (BHVAD::RJV)
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'GCB_PAR'
*    Import :
      LOGICAL LIVE
      INTEGER NDF
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'GFX_CMN'
      INCLUDE 'GMD_CMN'
*    Local Constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) GLOC
      INTEGER N
      LOGICAL OK
*-

      IF (STATUS.EQ.SAI__OK) THEN

        IF (.NOT.LIVE) THEN
          IF (G_MULTI) THEN
            CALL MSG_PRNT('Multiple dataset:-')
            CALL MSG_SETI('NDF',NDF)
            CALL MSG_PRNT(' processing NDF ^NDF...')
            CALL GMD_LOCNDF(G_MLOC,NDF,GLOC,STATUS)
            CALL GCB_LOAD(GLOC,STATUS)
          ELSE
            CALL GCB_LOAD(G_LOC,STATUS)
          ENDIF
        ENDIF

        CALL GCB_GETI('SHAPE_N',OK,N,STATUS)
        IF (.NOT.OK) THEN
          N=1
        ENDIF
        CALL GCB_CANI('SHAPE_N',STATUS)
        CALL GCB_CAN1C('SHAPE_TYPE',1,N,STATUS)
        CALL GCB_CAN1R('SHAPE_X',1,N,STATUS)
        CALL GCB_CAN1R('SHAPE_Y',1,N,STATUS)
        CALL GCB_CAN1R('SHAPE_DATA1',1,N,STATUS)
        CALL GCB_CAN1R('SHAPE_DATA2',1,N,STATUS)
        CALL GCB_CAN1R('SHAPE_DATA2',1,N,STATUS)
        CALL GCB_CAN1I('SHAPE_WIDTH',1,N,STATUS)
        CALL GCB_CAN1I('SHAPE_STYLE',1,N,STATUS)
        CALL GCB_CAN1I('SHAPE_COLOUR',1,N,STATUS)

        IF (.NOT.LIVE) THEN
          IF (G_MULTI) THEN
            CALL GCB_SAVE(GLOC,STATUS)
            CALL BDA_RELEASE(GLOC,STATUS)
            CALL DAT_ANNUL(GLOC,STATUS)
          ELSE
            CALL GCB_SAVE(G_LOC,STATUS)
          ENDIF
        ENDIF


        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GSET_SHAPE_CANCEL',STATUS)
        ENDIF

      ENDIF
      END

*+
      SUBROUTINE GSET_SHAPE_SET(LIVE,NDF,STATUS)

*    Authors :
*             (BHVAD::RJV)
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'GCB_PAR'
*    Import :
      LOGICAL LIVE
      INTEGER NDF
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'GFX_CMN'
      INCLUDE 'GMD_CMN'
*    Local Constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) GLOC
      CHARACTER*20 TYPE
      INTEGER N1,N2
      INTEGER WIDTH,STYLE,COLOUR
      REAL X,Y
      REAL DATA1,DATA2,DATA3
      LOGICAL OK
*-

      IF (STATUS.EQ.SAI__OK) THEN

*  if not in interactive mode
        IF (.NOT.LIVE) THEN
*  load existing GCB
          IF (G_MULTI) THEN
            CALL MSG_PRNT('Multiple dataset:-')
            CALL MSG_SETI('NDF',NDF)
            CALL MSG_PRNT(' processing NDF ^NDF...')
*  from component of multiple dataset
            CALL GMD_LOCNDF(G_MLOC,NDF,GLOC,STATUS)
            CALL GCB_LOAD(GLOC,STATUS)
          ELSE
*  or single dataset
            CALL GCB_LOAD(G_LOC,STATUS)
          ENDIF
        ENDIF

*  get existing and new number
        CALL GCB_GETI('SHAPE_N',OK,N1,STATUS)
        IF (.NOT.OK) THEN
          N1=0
        ENDIF
        N2=N1+1
        CALL PAR_DEF0I('NUM',N2,STATUS)
        CALL PAR_GET0I('NUM',N2,STATUS)

        IF (N2.GT.N1) THEN
          N2=N1+1
          CALL GCB_SETI('SHAPE_N',N2,STATUS)
        ENDIF

*  get individual control parameters - NULL means no change
        CALL PAR_GET0C('TYPE',TYPE,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
        ELSE
          CALL CHR_UCASE(TYPE)
          CALL GCB_SET1C('SHAPE_TYPE',N2,1,TYPE,STATUS)
        ENDIF

        CALL PAR_GET0R('X',X,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
        ELSE
          CALL GCB_SET1R('SHAPE_X',N2,1,X,STATUS)
        ENDIF

        CALL PAR_GET0R('Y',Y,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
        ELSE
          CALL GCB_SET1R('SHAPE_Y',N2,1,Y,STATUS)
        ENDIF

        IF (TYPE.EQ.'VECTOR') THEN
          CALL PAR_PROMT('DATA1','X offset',STATUS)
        ELSEIF (TYPE.EQ.'CIRCLE') THEN
          CALL PAR_PROMT('DATA1','Radius',STATUS)
        ELSEIF (TYPE.EQ.'ELLIPSE') THEN
          CALL PAR_PROMT('DATA1','Semi-major axis',STATUS)
        ELSEIF (TYPE.EQ.'BOX') THEN
          CALL PAR_PROMT('DATA1','X width',STATUS)
        ENDIF
        CALL PAR_GET0R('DATA1',DATA1,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
        ELSE
          CALL GCB_SET1R('SHAPE_DATA1',N2,1,DATA1,STATUS)
        ENDIF

        IF (TYPE.NE.'CIRCLE') THEN
          IF (TYPE.EQ.'VECTOR') THEN
            CALL PAR_PROMT('DATA2','Y offset',STATUS)
          ELSEIF (TYPE.EQ.'ELLIPSE') THEN
            CALL PAR_PROMT('DATA2','Ratio of axes',STATUS)
          ELSEIF (TYPE.EQ.'BOX') THEN
            CALL PAR_PROMT('DATA2','Y width',STATUS)
          ENDIF
          CALL PAR_GET0R('DATA2',DATA2,STATUS)
          IF (STATUS.EQ.PAR__NULL) THEN
            CALL ERR_ANNUL(STATUS)
          ELSE
            CALL GCB_SET1R('SHAPE_DATA2',N2,1,DATA2,STATUS)
          ENDIF
        ENDIF

        IF (TYPE.EQ.'ELLIPSE') THEN
          CALL PAR_PROMT('DATA3','Angle',STATUS)
          CALL PAR_GET0R('DATA3',DATA3,STATUS)
          IF (STATUS.EQ.PAR__NULL) THEN
            CALL ERR_ANNUL(STATUS)
          ELSE
            CALL GCB_SET1R('SHAPE_DATA3',N2,1,DATA3,STATUS)
          ENDIF
        ENDIF

        CALL PAR_GET0I('WIDTH',WIDTH,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
        ELSE
          CALL GCB_SET1I('SHAPE_WIDTH',N2,1,WIDTH,STATUS)
        ENDIF

        CALL PAR_GET0I('STYLE',STYLE,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
        ELSE
          CALL GCB_SET1I('SHAPE_STYLE',N2,1,STYLE,STATUS)
        ENDIF

        CALL PAR_GET0I('COLOUR',COLOUR,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
        ELSE
          CALL GCB_SET1I('SHAPE_COLOUR',N2,1,COLOUR,STATUS)
        ENDIF

*  resave if necessary
        IF (.NOT.LIVE) THEN
          IF (G_MULTI) THEN
            CALL GCB_SAVE(GLOC,STATUS)
            CALL BDA_RELEASE(GLOC,STATUS)
            CALL DAT_ANNUL(GLOC,STATUS)
          ELSE
            CALL GCB_SAVE(G_LOC,STATUS)
          ENDIF
        ENDIF


        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GSET_SHAPE_SET',STATUS)
        ENDIF

      ENDIF
      END



*+
      SUBROUTINE GSET_POSIT_CANCEL(LIVE,NSEL,NDFS,STATUS)

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
      LOGICAL LIVE
      INTEGER NSEL
      INTEGER NDFS(*)
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'GFX_CMN'
      INCLUDE 'GMD_CMN'
*    Local Constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) GLOC
      INTEGER ISEL
*-

      IF (STATUS.EQ.SAI__OK) THEN

        DO ISEL=1,NSEL

          IF (.NOT.LIVE) THEN
            IF (G_MULTI) THEN
              IF (ISEL.EQ.1) THEN
                CALL MSG_PRNT('Multiple dataset:-')
              ENDIF
              CALL MSG_SETI('NDF',NDFS(ISEL))
              CALL MSG_PRNT(' processing NDF ^NDF...')
              CALL GMD_LOCNDF(G_MLOC,NDFS(ISEL),GLOC,STATUS)
              CALL GCB_LOAD(GLOC,STATUS)
            ELSE
              CALL GCB_LOAD(G_LOC,STATUS)
            ENDIF
          ENDIF

          CALL GCB_CANR('POS_X1',STATUS)
          CALL GCB_CANR('POS_X2',STATUS)
          CALL GCB_CANR('POS_Y1',STATUS)
          CALL GCB_CANR('POS_Y2',STATUS)

          IF (.NOT.LIVE) THEN
            IF (G_MULTI) THEN
              CALL GCB_SAVE(GLOC,STATUS)
              CALL BDA_RELEASE(GLOC,STATUS)
              CALL DAT_ANNUL(GLOC,STATUS)
            ELSE
              CALL GCB_SAVE(G_LOC,STATUS)
            ENDIF
          ENDIF

        ENDDO


        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GSET_POSIT_CANCEL',STATUS)
        ENDIF

      ENDIF
      END

*+
      SUBROUTINE GSET_POSIT_SET(LIVE,NSEL,NDFS,STATUS)

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
      LOGICAL LIVE
      INTEGER NSEL
      INTEGER NDFS(*)
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'GFX_CMN'
      INCLUDE 'GMD_CMN'
*    Local Constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) GLOC
      INTEGER ISEL
      REAL X1,X2,Y1,Y2
      LOGICAL SETX1,SETX2,SETY1,SETY2
*-

      SETX1=.TRUE.
      SETX2=.TRUE.
      SETY1=.TRUE.
      SETY2=.TRUE.

      IF (STATUS.EQ.SAI__OK) THEN

*  get individual control parameters - NULL means no change
        CALL PAR_GET0R('X1',X1,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
          SETX1=.FALSE.
        ENDIF

        CALL PAR_GET0R('X2',X2,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
          SETX2=.FALSE.
        ENDIF

        CALL PAR_GET0R('Y1',Y1,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
          SETY1=.FALSE.
        ENDIF

        CALL PAR_GET0R('Y2',Y2,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
          SETY2=.FALSE.
        ENDIF

*  NSEL is set to 1 for LIVE mode or single dataset
*  otherwise to number of NDFs selected from multiple dataset

        DO ISEL=1,NSEL

*  if not in interactive mode
          IF (.NOT.LIVE) THEN
*  load existing GCB
            IF (G_MULTI) THEN
              IF (ISEL.EQ.1) THEN
                CALL MSG_PRNT('Multiple dataset:-')
              ENDIF
              CALL MSG_SETI('NDF',NDFS(ISEL))
              CALL MSG_PRNT(' processing NDF ^NDF...')
*  from component of multiple dataset
              CALL GMD_LOCNDF(G_MLOC,NDFS(ISEL),GLOC,STATUS)
              CALL GCB_LOAD(GLOC,STATUS)
            ELSE
*  or single dataset
              CALL GCB_LOAD(G_LOC,STATUS)
            ENDIF
          ENDIF

*  set new values
          IF (SETX1) THEN
            CALL GCB_SETR('POS_X1',X1,STATUS)
          ENDIF

          IF (SETX2) THEN
            CALL GCB_SETR('POS_X2',X2,STATUS)
          ENDIF

          IF (SETY1) THEN
            CALL GCB_SETR('POS_Y1',Y1,STATUS)
          ENDIF

          IF (SETY2) THEN
            CALL GCB_SETR('POS_Y2',Y2,STATUS)
          ENDIF

*  resave if necessary
          IF (.NOT.LIVE) THEN
            IF (G_MULTI) THEN
              CALL GCB_SAVE(GLOC,STATUS)
              CALL BDA_RELEASE(GLOC,STATUS)
              CALL DAT_ANNUL(GLOC,STATUS)
            ELSE
              CALL GCB_SAVE(G_LOC,STATUS)
            ENDIF
          ENDIF

        ENDDO

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GSET_POSIT_SET',STATUS)
        ENDIF

      ENDIF
      END


*+
      SUBROUTINE GSET_COLOUR_CANCEL(LIVE,NSEL,NDFS,STATUS)

*    Authors :
*             (BHVAD::RJV)
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'GCB_PAR'
*    Import :
      LOGICAL LIVE
      INTEGER NSEL
      INTEGER NDFS(*)
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'GFX_CMN'
      INCLUDE 'GMD_CMN'
*    Local Constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) GLOC
      INTEGER ISEL
      INTEGER N
      LOGICAL OK
*-

      IF (STATUS.EQ.SAI__OK) THEN

        DO ISEL=1,NSEL

          IF (.NOT.LIVE) THEN
            IF (G_MULTI) THEN
              IF (ISEL.EQ.1) THEN
                CALL MSG_PRNT('Multiple dataset:-')
              ENDIF
              CALL MSG_SETI('NDF',NDFS(ISEL))
              CALL MSG_PRNT(' processing NDF ^NDF...')
              CALL GMD_LOCNDF(G_MLOC,NDFS(ISEL),GLOC,STATUS)
              CALL GCB_LOAD(GLOC,STATUS)
            ELSE
              CALL GCB_LOAD(G_LOC,STATUS)
            ENDIF
          ENDIF

          CALL GCB_GETI('COLOUR_N',OK,N,STATUS)
          IF (.NOT.OK) THEN
            N=1
          ENDIF
          CALL GCB_CANI('COLOUR_N',STATUS)
          CALL GCB_CAN1R('COLOUR_RED',1,N,STATUS)
          CALL GCB_CAN1R('COLOUR_GREEN',1,N,STATUS)
          CALL GCB_CAN1R('COLOUR_BLUE',1,N,STATUS)
          CALL GCB_CANL('COLOUR_RGB',STATUS)
          CALL GCB_CANL('COLOUR_NEG',STATUS)

          IF (.NOT.LIVE) THEN
            IF (G_MULTI) THEN
              CALL GCB_SAVE(GLOC,STATUS)
              CALL BDA_RELEASE(GLOC,STATUS)
              CALL DAT_ANNUL(GLOC,STATUS)
            ELSE
              CALL GCB_SAVE(G_LOC,STATUS)
            ENDIF
          ENDIF

        ENDDO


        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GSET_COLOUR_CANCEL',STATUS)
        ENDIF

      ENDIF
      END

*+
      SUBROUTINE GSET_COLOUR_SET(LIVE,NSEL,NDFS,STATUS)

*    Authors :
*             (BHVAD::RJV)
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'GCB_PAR'
*    Import :
      LOGICAL LIVE
      INTEGER NSEL
      INTEGER NDFS(*)
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'GFX_CMN'
      INCLUDE 'GMD_CMN'
*    Local Constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) GLOC
      CHARACTER*(DAT__SZLOC) TLOC
      CHARACTER*(DAT__SZTYP) TYPE
      CHARACTER NAME*20,FILE*80
      REAL TABLE(3,16)
      REAL RED(16),GREEN(16),BLUE(16)
      INTEGER SIZE
      INTEGER NVAL,NCHAR
      INTEGER TABN
      INTEGER ISEL
      INTEGER I
      LOGICAL SETTAB,SETRGB,SETNEG
      LOGICAL RGB,NEG
*-

      IF (STATUS.EQ.SAI__OK) THEN

        SETTAB=.FALSE.
        SETRGB=.FALSE.
        SETNEG=.TRUE.

*  read in colour table
        CALL DAT_ASSOC('TABLE','READ',TLOC,STATUS)

        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
          CALL PAR_GET0L('RGB',RGB,STATUS)
          IF (STATUS.EQ.PAR__NULL) THEN
            CALL ERR_ANNUL(STATUS)
          ELSE
            SETRGB=.TRUE.
          ENDIF
        ELSE
          CALL DAT_TYPE(TLOC,TYPE,STATUS)
          CALL DAT_SIZE(TLOC,SIZE,STATUS)
          IF (TYPE.EQ.'_REAL'.AND.SIZE.EQ.48) THEN
            CALL DAT_GETVR(TLOC,48,TABLE,SIZE,STATUS)
            CALL DAT_ANNUL(TLOC,STATUS)
            SETTAB=.TRUE.
*  standard table number
          ELSEIF (TYPE.EQ.'_INTEGER'.AND.SIZE.EQ.1) THEN
            CALL DAT_GET0I(TLOC,TABN,STATUS)
            CALL DAT_ANNUL(TLOC,STATUS)
            CALL CHR_ITOC(TABN,NAME,NCHAR)
            NAME='AST_TAB'//NAME(:NCHAR)
            CALL PSX_GETENV(NAME,FILE,STATUS)
            CALL HDS_OPEN(FILE,'READ',TLOC,STATUS)
            IF (STATUS.NE.SAI__OK) THEN
              CALL MSG_PRNT('AST_ERR: invalid table number')
              STATUS=SAI__ERROR
            ELSE
              CALL DAT_GETVR(TLOC,48,TABLE,NVAL,STATUS)
              CALL DAT_ANNUL(TLOC,STATUS)
              SETTAB=.TRUE.
            ENDIF
          ELSE
            CALL MSG_PRNT('AST_ERR: invalid colour table')
            STATUS=SAI__ERROR
          ENDIF

        ENDIF

        IF (SETTAB) THEN
          DO I=1,16
            RED(I)=TABLE(1,I)
            GREEN(I)=TABLE(2,I)
            BLUE(I)=TABLE(3,I)
          ENDDO
        ENDIF

        CALL PAR_GET0L('NEG',NEG,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
          SETNEG=.FALSE.
        ENDIF

*  NSEL is set to 1 for LIVE mode or single dataset
*  otherwise to number of NDFs selected from multiple dataset

        DO ISEL=1,NSEL

*  if not in interactive mode
          IF (.NOT.LIVE) THEN
*  load existing GCB
            IF (G_MULTI) THEN
              IF (ISEL.EQ.1) THEN
                CALL MSG_PRNT('Multiple dataset:-')
              ENDIF
              CALL MSG_SETI('NDF',NDFS(ISEL))
              CALL MSG_PRNT(' processing NDF ^NDF...')
*  from component of multiple dataset
              CALL GMD_LOCNDF(G_MLOC,NDFS(ISEL),GLOC,STATUS)
              CALL GCB_LOAD(GLOC,STATUS)
            ELSE
*  or single dataset
              CALL GCB_LOAD(G_LOC,STATUS)
            ENDIF
          ENDIF

*  set new table
          IF (SETTAB) THEN
            CALL GCB_CANL('COLOUR_RGB',STATUS)
            CALL GCB_SETI('COLOUR_N',16,STATUS)
            CALL GCB_SET1R('COLOUR_RED',1,16,RED,STATUS)
            CALL GCB_SET1R('COLOUR_GREEN',1,16,GREEN,STATUS)
            CALL GCB_SET1R('COLOUR_BLUE',1,16,BLUE,STATUS)
          ENDIF
          IF (SETRGB) THEN
            CALL GCB_SETL('COLOUR_RGB',RGB,STATUS)
          ENDIF
          IF (SETNEG) THEN
            CALL GCB_SETL('COLOUR_NEG',NEG,STATUS)
          ENDIF

*  reset or resave
          IF (LIVE) THEN
            CALL GFX_SETCOLS(STATUS)
          ELSE
            IF (G_MULTI) THEN
              CALL GCB_SAVE(GLOC,STATUS)
              CALL BDA_RELEASE(GLOC,STATUS)
              CALL DAT_ANNUL(GLOC,STATUS)
            ELSE
              CALL GCB_SAVE(G_LOC,STATUS)
            ENDIF
          ENDIF

        ENDDO

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GSET_COLOUR_SET',STATUS)
        ENDIF

      ENDIF
      END


*+
      SUBROUTINE GSET_HELP(STATUS)

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
*    Global variables :
*    Local Constants :
      INTEGER N
      PARAMETER (N=14)
*    Local variables :
      CHARACTER*79 TEXT(N)
      INTEGER I
      DATA TEXT/
     :' AXEs     - set both axes     LABels   - set both axis labels',
     :' XAXis    - set x-axis        XLAbel   - set x-axis label',
     :' YAXis    - set y-axis        YLAbel   - set y-axis label',
     :' TITles   - put title(s) on   NOTes    - annotate plot',
     :' MARkers  - mark positions    SHApes   - put shapes on',
     :' POLyline - plot polyline     STEpline - plot stepped line',
     :' POInts   - plot points       ERRors   - plot error bars',
     :' PIXel    - plot pixels       CONtours - plot contours',
     :' SURface  - plot surface      COLours  - set colour table',
     :' GRId     - plot coord grid   KEY      - put key on plot',
     :' POSit    - set position      DEFaults - set default attributes',
     :' STAmp    - ID stamp',
     :' ',
     :' *        - wildcard for use with SHOW and CANCEL options'/
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL MSG_BLNK()
        CALL MSG_BLNK()
        CALL MSG_PRNT('The following option switches are available:-')
        CALL MSG_BLNK()
        DO I=1,N
          CALL MSG_PRNT(TEXT(I))
        ENDDO
        CALL MSG_BLNK()
        CALL MSG_PRNT('- only the first 3 characters are significant')
        CALL MSG_BLNK()
        CALL MSG_BLNK()


      ENDIF
      END


*+
      SUBROUTINE GSET_SHOW(LIVE,NSEL,NDFS,SWITCH,STATUS)

*    Authors :
*             (BHVAD::RJV)
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'GCB_PAR'
*    Import :
      LOGICAL LIVE
      INTEGER NSEL
      INTEGER NDFS(*)
      CHARACTER*(*) SWITCH
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'GFX_CMN'
      INCLUDE 'GMD_CMN'
*    Local Constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) GLOC
      CHARACTER*80 CVAL
      CHARACTER*15 ST(4)
      REAL RVAL
      INTEGER I,N
      INTEGER NST
      INTEGER ISEL
      INTEGER IVAL
      LOGICAL LVAL
      LOGICAL PIXEL,CONTOUR,SURFACE,POLY,STEP,ERRS,POINTS
      LOGICAL OK
*-

      IF (STATUS.EQ.SAI__OK) THEN

*  NSEL is set to 1 for LIVE mode or single dataset
*  otherwise to number of NDFs selected from multiple dataset

        DO ISEL=1,NSEL

*  if not in interactive mode
          IF (.NOT.LIVE) THEN
*  load existing GCB
            IF (G_MULTI) THEN
              IF (ISEL.EQ.1) THEN
                CALL MSG_PRNT('Multiple dataset:-')
              ENDIF
              CALL MSG_SETI('NDF',NDFS(ISEL))
              CALL MSG_PRNT(' processing NDF ^NDF...')
*  from component of multiple dataset
              CALL GMD_LOCNDF(G_MLOC,NDFS(ISEL),GLOC,STATUS)
              CALL GCB_LOAD(GLOC,STATUS)
            ELSE
*  or single dataset
              CALL GCB_LOAD(G_LOC,STATUS)
            ENDIF
          ENDIF

*  interrogate Control Block for each attribute

          IF (SWITCH.EQ.'*'.OR.SWITCH.EQ.'ALL'.OR.SWITCH.EQ.'DEF') THEN
            CALL MSG_BLNK()
            CALL MSG_PRNT('Default attributes(*):-')
            CALL GCB_GETI('DEFAULT_WIDTH',OK,IVAL,STATUS)
            IF (.NOT.OK) THEN
              IVAL=1
            ENDIF
            CALL MSG_SETI('WID',IVAL)
            CALL GCB_GETI('DEFAULT_STYLE',OK,IVAL,STATUS)
            IF (.NOT.OK) THEN
              IVAL=1
            ENDIF
            CALL MSG_SETI('STY',IVAL)
            CALL GCB_GETI('DEFAULT_FONT',OK,IVAL,STATUS)
            IF (.NOT.OK) THEN
              IVAL=1
            ENDIF
            CALL MSG_SETI('FON',IVAL)
            CALL GCB_GETR('DEFAULT_SIZE',OK,RVAL,STATUS)
            IF (.NOT.OK) THEN
              RVAL=1.0
            ENDIF
            CALL MSG_SETR('SIZ',RVAL)
            CALL GCB_GETI('DEFAULT_COLOUR',OK,IVAL,STATUS)
            IF (.NOT.OK) THEN
              IVAL=1
            ENDIF
            CALL MSG_SETI('COL',IVAL)
            CALL MSG_PRNT('  LineWidth=^WID   LineStyle=^STY   '//
     :                  'CharFont=^FON   CharSize=^SIZ   '//
     :                  'Colour=^COL')
            CALL MSG_BLNK()
          ENDIF
          IF (SWITCH.EQ.'*'.OR.SWITCH.EQ.'ALL') THEN
            CALL MSG_PRNT('Plotting styles:-')
            CALL GFX_1DSTYLE(POLY,STEP,ERRS,POINTS,STATUS)
            CALL GFX_2DSTYLE(PIXEL,CONTOUR,SURFACE,STATUS)
            IF (.NOT.(POLY.OR.STEP.OR.ERRS.OR.POINTS.OR.PIXEL.OR.
     :              CONTOUR.OR.SURFACE)) THEN
              CALL MSG_PRNT('  Default')
            ELSE
              DO NST=1,4
                ST(NST)=' '
              ENDDO
              IF (POLY.OR.STEP.OR.ERRS.OR.POINTS) THEN
                NST=1
                IF (POLY) THEN
                  ST(NST)='PolyLine'
                ELSEIF (STEP) THEN
                  ST(NST)='StepLine'
                ELSEIF (ERRS) THEN
                  ST(NST)='ErrorBars'
                ELSEIF (POINTS) THEN
                  ST(NST)='Points'
                ENDIF
                IF (STEP.AND.POLY) THEN
                  NST=NST+1
                  ST(NST)='+StepLine'
                ENDIF
                IF (ERRS.AND.(POLY.OR.STEP)) THEN
                  NST=NST+1
                  ST(NST)='+ErrorBars'
                ENDIF
                IF (POINTS.AND.(POLY.OR.STEP.OR.ERRS)) THEN
                  NST=NST+1
                  ST(NST)='+Points'
                ENDIF
                CALL MSG_SETC('ST1',ST(1))
                CALL MSG_SETC('ST2',ST(2))
                CALL MSG_SETC('ST3',ST(3))
                CALL MSG_SETC('ST4',ST(4))
                CALL MSG_PRNT('  Style=^ST1^ST2^ST3^ST4')
              ELSEIF (PIXEL.OR.CONTOUR.OR.SURFACE) THEN
                IF (SURFACE) THEN
                  CALL MSG_PRNT('  Style=Surface')
                ELSE
                  IF (PIXEL.AND.CONTOUR) THEN
                    CALL MSG_PRNT('  Style=Pixel+Contour')
                  ELSEIF (PIXEL) THEN
                    CALL MSG_PRNT('  Style=Pixel')
                  ELSEIF (CONTOUR) THEN
                    CALL MSG_PRNT('  Style=Contour')
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDIF

          IF (SWITCH.EQ.'*'.OR.SWITCH.EQ.'POL') THEN
            CALL MSG_BLNK()
            CALL MSG_PRNT('PolyLine:-')
            CALL GCB_GETL('POLY_FLAG',OK,LVAL,STATUS)
            IF (OK.AND.LVAL) THEN
              CALL MSG_SETC('FLG','On')
            ELSE
              CALL MSG_SETC('FLG','Off')
            ENDIF
            CALL GCB_GETI('POLY_WIDTH',OK,IVAL,STATUS)
            IF (OK) THEN
              CALL MSG_SETI('WID',IVAL)
            ELSE
              CALL MSG_SETC('WID','*')
            ENDIF
            CALL GCB_GETI('POLY_STYLE',OK,IVAL,STATUS)
            IF (OK) THEN
              CALL MSG_SETI('STY',IVAL)
            ELSE
              CALL MSG_SETC('STY','*')
            ENDIF
            CALL GCB_GETI('POLY_COLOUR',OK,IVAL,STATUS)
            IF (OK) THEN
              CALL MSG_SETI('COL',IVAL)
            ELSE
              CALL MSG_SETC('COL','*')
            ENDIF
            CALL MSG_PRNT('  PolyLineFlag=^FLG   '//
     :                        'PolyLineStyle=^STY   '//
     :                        'PolyLineWidth=^WID   '//
     :                        'PolyLineColour=^COL')
          ENDIF
          IF (SWITCH.EQ.'*'.OR.SWITCH.EQ.'STE') THEN
            CALL MSG_BLNK()
            CALL MSG_PRNT('StepLine:-')
            CALL GCB_GETL('STEP_FLAG',OK,LVAL,STATUS)
            IF (OK.AND.LVAL) THEN
              CALL MSG_SETC('FLG','On')
            ELSE
              CALL MSG_SETC('FLG','Off')
            ENDIF
            CALL GCB_GETI('STEP_WIDTH',OK,IVAL,STATUS)
            IF (OK) THEN
              CALL MSG_SETI('WID',IVAL)
            ELSE
              CALL MSG_SETC('WID','*')
            ENDIF
            CALL GCB_GETI('STEP_STYLE',OK,IVAL,STATUS)
            IF (OK) THEN
              CALL MSG_SETI('STY',IVAL)
            ELSE
              CALL MSG_SETC('STY','*')
            ENDIF
            CALL GCB_GETI('STEP_COLOUR',OK,IVAL,STATUS)
            IF (OK) THEN
              CALL MSG_SETI('COL',IVAL)
            ELSE
              CALL MSG_SETC('COL','*')
            ENDIF
            CALL MSG_PRNT('  StepLineFlag=^FLG   '//
     :                        'StepLineStyle=^STY   '//
     :                        'StepLineWidth=^WID   '//
     :                        'StepLineColour=^COL')
          ENDIF
          IF (SWITCH.EQ.'*'.OR.SWITCH.EQ.'ERR') THEN
            CALL MSG_BLNK()
            CALL MSG_PRNT('ErrorBars:-')
            CALL GCB_GETL('ERR_FLAG',OK,LVAL,STATUS)
            IF (OK.AND.LVAL) THEN
              CALL MSG_SETC('FLG','On')
            ELSE
              CALL MSG_SETC('FLG','Off')
            ENDIF
            CALL GCB_GETI('ERR_WIDTH',OK,IVAL,STATUS)
            IF (OK) THEN
              CALL MSG_SETI('WID',IVAL)
            ELSE
              CALL MSG_SETC('WID','*')
            ENDIF
            CALL GCB_GETI('ERR_SHAPE',OK,IVAL,STATUS)
            IF (OK) THEN
              CALL MSG_SETI('SHA',IVAL)
            ELSE
              CALL MSG_SETC('SHA','*')
            ENDIF
            CALL GCB_GETI('ERR_COLOUR',OK,IVAL,STATUS)
            IF (OK) THEN
              CALL MSG_SETI('COL',IVAL)
            ELSE
              CALL MSG_SETC('COL','*')
            ENDIF
            CALL MSG_PRNT('  ErrorBarFlag=^FLG   '//
     :                        'ErrorBarShape=^SHA   '//
     :                        'ErrorBarLineWidth=^WID   '//
     :                        'ErrorBarColour=^COL')
          ENDIF
          IF (SWITCH.EQ.'*'.OR.SWITCH.EQ.'POI') THEN
            CALL MSG_BLNK()
            CALL MSG_PRNT('Points:-')
            CALL GCB_GETL('POINT_FLAG',OK,LVAL,STATUS)
            IF (OK.AND.LVAL) THEN
              CALL MSG_SETC('FLG','On')
            ELSE
              CALL MSG_SETC('FLG','Off')
            ENDIF
            CALL GCB_GETI('POINT_SYMBOL',OK,IVAL,STATUS)
            IF (OK) THEN
              CALL MSG_SETI('SYM',IVAL)
            ELSE
              CALL MSG_SETC('SYM','*')
            ENDIF
            CALL GCB_GETR('POINT_SIZE',OK,RVAL,STATUS)
            IF (OK) THEN
              CALL MSG_SETR('SIZ',RVAL)
            ELSE
              CALL MSG_SETC('SIZ','*')
            ENDIF
            CALL GCB_GETI('POINT_BOLD',OK,IVAL,STATUS)
            IF (OK) THEN
              CALL MSG_SETI('BOL',IVAL)
            ELSE
              CALL MSG_SETC('BOL','*')
            ENDIF
            CALL GCB_GETI('POINT_COLOUR',OK,IVAL,STATUS)
            IF (OK) THEN
              CALL MSG_SETI('COL',IVAL)
            ELSE
              CALL MSG_SETC('COL','*')
            ENDIF
            CALL MSG_PRNT('  PointFlag=^FLG   '//
     :                        'PointSymbol=^SYM   '//
     :                        'PointSize=^SIZ   '//
     :                        'PointBoldness=^BOL   '//
     :                        'PointColour=^COL')
          ENDIF
          IF (SWITCH.EQ.'*'.OR.SWITCH.EQ.'SUR') THEN
            CALL MSG_BLNK()
            CALL MSG_PRNT('Surface:-')
            CALL GCB_GETL('SURF_FLAG',OK,LVAL,STATUS)
            IF (OK.AND.LVAL) THEN
              CALL MSG_SETC('FLG','On')
            ELSE
              CALL MSG_SETC('FLG','Off')
            ENDIF
            CALL GCB_GETR('SURF_ANGLE',OK,RVAL,STATUS)
            IF (OK) THEN
              CALL MSG_SETR('ANG',RVAL)
            ELSE
              CALL MSG_SETC('ANG','*')
            ENDIF
            CALL MSG_PRNT('  SurfaceFlag=^FLG   '//
     :                         'SurfaceViewingAngle=^ANG')
          ENDIF
          IF (SWITCH.EQ.'*'.OR.SWITCH.EQ.'PIX') THEN
            CALL MSG_BLNK()
            CALL MSG_PRNT('Pixel:-')
            CALL GCB_GETL('PIX_FLAG',OK,LVAL,STATUS)
            IF (OK.AND.LVAL) THEN
              CALL MSG_SETC('FLG','On')
            ELSE
              CALL MSG_SETC('FLG','Off')
            ENDIF
            CALL GCB_GETR('PIX_MIN',OK,RVAL,STATUS)
            IF (OK) THEN
              CALL MSG_SETR('MIN',RVAL)
            ELSE
              CALL MSG_SETC('MIN','*')
            ENDIF
            CALL GCB_GETR('PIX_MAX',OK,RVAL,STATUS)
            IF (OK) THEN
              CALL MSG_SETR('MAX',RVAL)
            ELSE
              CALL MSG_SETC('MAX','*')
            ENDIF
            CALL GCB_GETC('PIX_SCALING',OK,CVAL,STATUS)
            IF (.NOT.OK) THEN
              CVAL='*'
            ELSE
              CALL CHR_UCASE(CVAL)
            ENDIF
            CALL MSG_SETC('SCA',CVAL)
            IF (CVAL(:3).EQ.'CYC') THEN
              CALL GCB_GETI('PIX_CYCLES',OK,IVAL,STATUS)
              IF (OK) THEN
                CALL MSG_SETI('CYC',IVAL)
              ELSE
                CALL MSG_SETC('CYC','*')
              ENDIF
              CALL MSG_PRNT('  PixelFlag=^FLG   '//
     :                          'PixelScaling=^SCA  '//
     :                          'PixelScalingCycles=^CYC   '//
     :                          'PixelScalingMin=^MIN   '//
     :                          'PixelScalingMax=^MAX')
            ELSE
              CALL MSG_PRNT('  PixelFlag=^FLG   '//
     :                          'PixelScaling=^SCA  '//
     :                          'PixelScalingMin=^MIN   '//
     :                          'PixelScalingMax=^MAX')
            ENDIF
          ENDIF
          IF (SWITCH.EQ.'*'.OR.SWITCH.EQ.'CON') THEN
            CALL MSG_BLNK()
            CALL MSG_PRNT('Contours:-')
            CALL GCB_GETL('CONT_FLAG',OK,LVAL,STATUS)
            IF (OK.AND.LVAL) THEN
              CALL MSG_SETC('FLG','On')
            ELSE
              CALL MSG_SETC('FLG','Off')
            ENDIF
            CALL MSG_PRNT('  ContourFlag=^FLG ')
            CALL GCB_GETI('CONT_N',OK,N,STATUS)
            IF (.NOT.OK) THEN
              CALL MSG_PRNT('  Contours=default')
              N=0
            ENDIF

            DO I=1,N
              CALL GCB_GET1R('CONT_LEVS',I,1,OK,RVAL,STATUS)
              IF (OK) THEN
                CALL MSG_SETR('LEV',RVAL)
              ELSE
                CALL MSG_SETC('LEV','*')
              ENDIF
              CALL GCB_GET1I('CONT_STYLE',I,1,OK,IVAL,STATUS)
              IF (OK) THEN
                CALL MSG_SETI('STY',IVAL)
              ELSE
                CALL MSG_SETC('STY','*')
              ENDIF
              CALL GCB_GET1I('CONT_WIDTH',I,1,OK,IVAL,STATUS)
              IF (OK) THEN
                CALL MSG_SETI('WID',IVAL)
              ELSE
                CALL MSG_SETC('WID','*')
              ENDIF
              CALL GCB_GET1I('CONT_COLOUR',I,1,OK,IVAL,STATUS)
              IF (OK) THEN
                CALL MSG_SETI('COL',IVAL)
              ELSE
                CALL MSG_SETC('COL','*')
              ENDIF
              CALL MSG_SETI('NUM',I)
              CALL MSG_PRNT('  Contour^NUM   Level=^LEV   '//
     :                          'LineStyle=^STY   '//
     :                          'LineWidth=^WID   '//
     :                          'LineColour=^COL')


            ENDDO

          ENDIF

          IF (SWITCH.EQ.'*'.OR.SWITCH.EQ.'POS') THEN
            CALL MSG_BLNK()
            CALL MSG_PRNT('Position:-')
            CALL GCB_GETR('POS_X1',OK,RVAL,STATUS)
            IF (OK) THEN
              CALL MSG_SETR('X1',RVAL)
            ELSE
              CALL MSG_SETC('X1','*')
            ENDIF
            CALL GCB_GETR('POS_X2',OK,RVAL,STATUS)
            IF (OK) THEN
              CALL MSG_SETR('X2',RVAL)
            ELSE
              CALL MSG_SETC('X2','*')
            ENDIF
            CALL GCB_GETR('POS_Y1',OK,RVAL,STATUS)
            IF (OK) THEN
              CALL MSG_SETR('Y1',RVAL)
            ELSE
              CALL MSG_SETC('Y1','*')
            ENDIF
            CALL GCB_GETR('POS_Y2',OK,RVAL,STATUS)
            IF (OK) THEN
              CALL MSG_SETR('Y2',RVAL)
            ELSE
              CALL MSG_SETC('Y2','*')
            ENDIF
            CALL MSG_PRNT('  LeftEdge=^X1   RightEdge=^X2   '//
     :                  'BottomEdge=^Y1   TopEdge=^Y2')
          ENDIF
          IF (SWITCH.EQ.'*'.OR.SWITCH.EQ.'AXE'.OR.
     :               SWITCH.EQ.'XAX'.OR.SWITCH.EQ.'YAX') THEN

            CALL MSG_BLNK()
            CALL MSG_PRNT('Axes:-')
            CALL MSG_PRNT('  General-')
            CALL GCB_GETI('AXES_WIDTH',OK,IVAL,STATUS)
            IF (OK) THEN
              CALL MSG_SETI('WID',IVAL)
            ELSE
              CALL MSG_SETC('WID','*')
            ENDIF
            CALL GCB_GETI('AXES_COLOUR',OK,IVAL,STATUS)
            IF (OK) THEN
              CALL MSG_SETI('COL',IVAL)
            ELSE
              CALL MSG_SETC('COL','*')
            ENDIF
            CALL MSG_PRNT('  AxisLineWidth=^WID   '//
     :                                     'AxisColour=^COL')
            CALL GCB_GETI('AXES_FONT',OK,IVAL,STATUS)
            IF (OK) THEN
              CALL MSG_SETI('FON',IVAL)
            ELSE
              CALL MSG_SETC('FON','*')
            ENDIF
            CALL GCB_GETR('AXES_SIZE',OK,RVAL,STATUS)
            IF (OK) THEN
              CALL MSG_SETR('SIZ',RVAL)
            ELSE
              CALL MSG_SETC('SIZ','*')
            ENDIF
            CALL GCB_GETI('AXES_BOLD',OK,IVAL,STATUS)
            IF (OK) THEN
              CALL MSG_SETI('BOL',IVAL)
            ELSE
              CALL MSG_SETC('BOL','*')
            ENDIF
            CALL MSG_PRNT('  NumericLabelFont=^FON   '//
     :                        'NumericLabelCharSize=^SIZ   '//
     :                        'NumericLabelBoldness=^BOL')
          ENDIF
          IF (SWITCH.EQ.'*'.OR.SWITCH.EQ.'XAX'.OR.SWITCH.EQ.'AXE')
     :                                                          THEN
            CALL MSG_PRNT('  Xaxis-')
            CALL GCB_GETR('XAXIS_TICK',OK,RVAL,STATUS)
            IF (OK) THEN
              CALL MSG_SETR('TIC',RVAL)
            ELSE
              CALL MSG_SETC('TIC','*')
            ENDIF
            CALL GCB_GETI('XAXIS_DIV',OK,IVAL,STATUS)
            IF (OK) THEN
              CALL MSG_SETI('DIV',IVAL)
            ELSE
              CALL MSG_SETC('DIV','*')
            ENDIF
            CALL MSG_PRNT('  MajorTickSpacing=^TIC   '//
     :                  'DivisionsBetweenMajorTicks=^DIV')
            CALL GCB_GETL('XAXIS_LOG',OK,LVAL,STATUS)
            IF (OK.AND.LVAL) THEN
              CALL MSG_SETC('LOG','On')
            ELSE
              CALL MSG_SETC('LOG','Off')
            ENDIF
            CALL GCB_GETC('XAXIS_OPT',OK,CVAL,STATUS)
            IF (OK) THEN
              CALL MSG_SETC('OPT',CVAL)
            ELSE
              CALL MSG_SETC('OPT','*')
            ENDIF
            CALL MSG_PRNT('  LogAxisFlag=^LOG   '//
     :                  'OptionsCode=^OPT')
            CALL GCB_GETR('XAXIS_LO',OK,RVAL,STATUS)
            IF (OK) THEN
              CALL MSG_SETR('LO',RVAL)
            ELSE
              CALL MSG_SETC('LO','*')
            ENDIF
            CALL GCB_GETR('XAXIS_HI',OK,RVAL,STATUS)
            IF (OK) THEN
              CALL MSG_SETR('HI',RVAL)
            ELSE
              CALL MSG_SETC('HI','*')
            ENDIF
            CALL MSG_PRNT('  LeftLimit=^LO   '//
     :                        'RightLimit=^HI')
          ENDIF
          IF (SWITCH.EQ.'*'.OR.SWITCH.EQ.'YAX'.OR.SWITCH.EQ.'AXE')
     :                                                        THEN
            CALL MSG_PRNT('  Yaxis-')
            CALL GCB_GETR('YAXIS_TICK',OK,RVAL,STATUS)
            IF (OK) THEN
              CALL MSG_SETR('TIC',RVAL)
            ELSE
              CALL MSG_SETC('TIC','*')
            ENDIF
            CALL GCB_GETI('YAXIS_DIV',OK,IVAL,STATUS)
            IF (OK) THEN
              CALL MSG_SETI('DIV',IVAL)
            ELSE
              CALL MSG_SETC('DIV','*')
            ENDIF
            CALL MSG_PRNT('  MajorTickSpacing=^TIC   '//
     :                        'DivisionsBetweenMajorTicks=^DIV')
            CALL GCB_GETL('YAXIS_LOG',OK,LVAL,STATUS)
            IF (OK.AND.LVAL) THEN
              CALL MSG_SETC('LOG','On')
            ELSE
              CALL MSG_SETC('LOG','Off')
            ENDIF
            CALL GCB_GETC('YAXIS_OPT',OK,CVAL,STATUS)
            IF (OK) THEN
              CALL MSG_SETC('OPT',CVAL)
            ELSE
              CALL MSG_SETC('OPT','*')
            ENDIF
            CALL MSG_PRNT('  LogAxisFlag=^LOG   '//
     :                                 'OptionsCode=^OPT')
            CALL GCB_GETR('YAXIS_LO',OK,RVAL,STATUS)
            IF (OK) THEN
              CALL MSG_SETR('LO',RVAL)
            ELSE
              CALL MSG_SETC('LO','*')
            ENDIF
            CALL GCB_GETR('YAXIS_HI',OK,RVAL,STATUS)
            IF (OK) THEN
              CALL MSG_SETR('HI',RVAL)
            ELSE
              CALL MSG_SETC('HI','*')
            ENDIF
            CALL MSG_PRNT('  BottomLimit=^LO   '//
     :                                   'TopLimit=^HI')
          ENDIF
          IF (SWITCH.EQ.'*'.OR.SWITCH.EQ.'GRI') THEN
            CALL MSG_BLNK()
            CALL MSG_PRNT('Grid:-')
            CALL GCB_GETI('GRID_FRAME',OK,IVAL,STATUS)
            IF (OK) THEN
              CALL MSG_SETI('FRA',IVAL)
              CALL GCB_GETI('GRID_POS',OK,IVAL,STATUS)
              IF (OK) THEN
                CALL MSG_SETI('POS',IVAL)
              ELSE
                CALL MSG_SETC('POS','*')
              ENDIF
              CALL MSG_PRNT('  CoordGridFlag=On   '//
     :                          'CoordFrame=^FRA   '//
     :                          'GridLineLabelPosition=^POS')
            ELSE
              CALL MSG_PRNT('  CoordGridFlag=Off')
            ENDIF
          ENDIF
          IF (SWITCH.EQ.'*'.OR.SWITCH.EQ.'KEY') THEN
            CALL MSG_BLNK()
            CALL MSG_PRNT('Key:-')
            CALL GCB_GETL('KEY_FLAG',OK,LVAL,STATUS)
            IF (OK.AND.LVAL) THEN
              CALL MSG_PRNT('  KeyFlag=On')
              CALL GCB_GETC('KEY_OPT',OK,CVAL,STATUS)
              IF (OK) THEN
                CALL MSG_SETC('OPT',CVAL)
              ELSE
                CALL MSG_SETC('OPT','*')
              ENDIF
              CALL GCB_GETI('KEY_FONT',OK,IVAL,STATUS)
              IF (OK) THEN
                CALL MSG_SETI('FON',IVAL)
              ELSE
                CALL MSG_SETC('FON','*')
              ENDIF
              CALL GCB_GETR('KEY_SIZE',OK,RVAL,STATUS)
              IF (OK) THEN
                CALL MSG_SETR('SIZ',RVAL)
              ELSE
                CALL MSG_SETC('SIZ','*')
              ENDIF
              CALL GCB_GETI('KEY_BOLD',OK,IVAL,STATUS)
              IF (OK) THEN
                CALL MSG_SETI('BOL',IVAL)
              ELSE
                CALL MSG_SETC('BOL','*')
              ENDIF
              CALL MSG_PRNT('  KeyOptionCode=^OPT   '//
     :                    'KeyLabelFont=^FON   '//
     :                    'KeyLabelCharSize=^SIZ   '//
     :                    'KeyLabelBoldness=^BOL')
            ELSE
              CALL MSG_PRNT('  KeyFlag=Off')
            ENDIF
          ENDIF
          IF (SWITCH.EQ.'*'.OR.SWITCH.EQ.'LAB'.OR.SWITCH.EQ.'XLA')
     :                                                        THEN

            CALL MSG_PRNT('Labels:-')
            CALL GCB_GETC('XLABEL_TEXT',OK,CVAL,STATUS)
            IF (.NOT.OK) THEN
              CVAL='default'
            ENDIF
            CALL MSG_SETC('TEX',CVAL)
            CALL MSG_PRNT('  Xlabel=^TEX')
            CALL GCB_GETI('XLABEL_FONT',OK,IVAL,STATUS)
            IF (OK) THEN
              CALL MSG_SETI('FON',IVAL)
            ELSE
              CALL MSG_SETC('FON','*')
            ENDIF
            CALL GCB_GETR('XLABEL_SIZE',OK,RVAL,STATUS)
            IF (OK) THEN
              CALL MSG_SETR('SIZ',RVAL)
            ELSE
              CALL MSG_SETC('SIZ','*')
            ENDIF
            CALL GCB_GETI('XLABEL_BOLD',OK,IVAL,STATUS)
            IF (OK) THEN
              CALL MSG_SETI('BOL',IVAL)
            ELSE
              CALL MSG_SETC('BOL','*')
            ENDIF
            CALL GCB_GETR('XLABEL_OFFSET',OK,RVAL,STATUS)
            IF (OK) THEN
              CALL MSG_SETR('OFF',RVAL)
            ELSE
              CALL MSG_SETC('OFF','*')
            ENDIF
            CALL GCB_GETI('XLABEL_COLOUR',OK,IVAL,STATUS)
            IF (OK) THEN
              CALL MSG_SETI('COL',IVAL)
            ELSE
              CALL MSG_SETC('COL','*')
            ENDIF
            CALL MSG_PRNT('    Font=^FON   CharSize=^SIZ   '//
     :                  'Boldness=^BOL   Offset=^OFF   '//
     :                  'Colour=^COL')
          ENDIF

          IF (SWITCH.EQ.'*'.OR.SWITCH.EQ.'LAB'.OR.SWITCH.EQ.'YLA')
     :                                                        THEN
            CALL GCB_GETC('YLABEL_TEXT',OK,CVAL,STATUS)
            IF (.NOT.OK) THEN
              CVAL='default'
            ENDIF
            CALL MSG_SETC('TEX',CVAL)
            CALL MSG_PRNT('  Ylabel=^TEX')
            CALL GCB_GETI('YLABEL_FONT',OK,IVAL,STATUS)
            IF (OK) THEN
              CALL MSG_SETI('FON',IVAL)
            ELSE
              CALL MSG_SETC('FON','*')
            ENDIF
            CALL GCB_GETR('YLABEL_SIZE',OK,RVAL,STATUS)
            IF (OK) THEN
              CALL MSG_SETR('SIZ',RVAL)
            ELSE
              CALL MSG_SETC('SIZ','*')
            ENDIF
            CALL GCB_GETI('YLABEL_BOLD',OK,IVAL,STATUS)
            IF (OK) THEN
              CALL MSG_SETI('BOL',IVAL)
            ELSE
              CALL MSG_SETC('BOL','*')
            ENDIF
            CALL GCB_GETR('YLABEL_OFFSET',OK,RVAL,STATUS)
            IF (OK) THEN
              CALL MSG_SETR('OFF',RVAL)
            ELSE
              CALL MSG_SETC('OFF','*')
            ENDIF
            CALL GCB_GETI('YLABEL_COLOUR',OK,IVAL,STATUS)
            IF (OK) THEN
              CALL MSG_SETI('COL',IVAL)
            ELSE
              CALL MSG_SETC('COL','*')
            ENDIF
            CALL MSG_PRNT('    Font=^FON   CharSize=^SIZ   '//
     :                  'Boldness=^BOL   Offset=^OFF   '//
     :                  'Colour=^COL')
          ENDIF

          IF (SWITCH.EQ.'*'.OR.SWITCH.EQ.'TIT') THEN
            CALL MSG_PRNT('Titles:')
            CALL GCB_GETI('TITLE_N',OK,N,STATUS)
            IF (.NOT.OK) THEN
              CALL MSG_PRNT('  Title=default')
              CALL GCB_GET1I('TITLE_FONT',I,1,OK,IVAL,STATUS)
              IF (OK) THEN
                CALL MSG_SETI('FON',IVAL)
              ELSE
                CALL MSG_SETC('FON','*')
              ENDIF
              CALL GCB_GET1I('TITLE_BOLD',I,1,OK,IVAL,STATUS)
              IF (OK) THEN
                CALL MSG_SETI('BOL',IVAL)
              ELSE
                CALL MSG_SETC('BOL','*')
              ENDIF
              CALL GCB_GET1R('TITLE_SIZE',I,1,OK,RVAL,STATUS)
              IF (OK) THEN
                CALL MSG_SETR('SIZ',RVAL)
              ELSE
                CALL MSG_SETC('SIZ','*')
              ENDIF
              CALL GCB_GET1I('TITLE_COLOUR',I,1,OK,IVAL,STATUS)
              IF (OK) THEN
                CALL MSG_SETI('COL',IVAL)
              ELSE
                CALL MSG_SETC('COL','*')
              ENDIF
              CALL GCB_GET1C('TITLE_JUST',I,1,OK,CVAL,STATUS)
              IF (OK) THEN
                CALL MSG_SETC('JUS',CVAL)
              ELSE
                CALL MSG_SETC('JUS','*')
              ENDIF
              CALL MSG_PRNT('    Font=^FON   Boldness=^BOL   '//
     :                          'CharSize=^SIZ   Colour=^COL   '//
     :                          'Justification=^JUS')

            ELSE
              DO I=1,N
                CALL GCB_GET1C('TITLE_TEXT',I,1,OK,CVAL,STATUS)
                IF (OK) THEN
                  CALL MSG_SETC('TEX',CVAL)
                ELSE
                  CALL MSG_SETC('TEX',' ')
                ENDIF
                CALL MSG_SETI('LIN',I)
                CALL MSG_PRNT('  Line ^LIN  Text=''^TEX''')
                CALL GCB_GET1I('TITLE_FONT',I,1,OK,IVAL,STATUS)
                IF (OK) THEN
                  CALL MSG_SETI('FON',IVAL)
                ELSE
                  CALL MSG_SETC('FON','*')
                ENDIF
                CALL GCB_GET1I('TITLE_BOLD',I,1,OK,IVAL,STATUS)
                IF (OK) THEN
                  CALL MSG_SETI('BOL',IVAL)
                ELSE
                  CALL MSG_SETC('BOL','*')
                ENDIF
                CALL GCB_GET1R('TITLE_SIZE',I,1,OK,RVAL,STATUS)
                IF (OK) THEN
                  CALL MSG_SETR('SIZ',RVAL)
                ELSE
                  CALL MSG_SETC('SIZ','*')
                ENDIF
                CALL GCB_GET1I('TITLE_COLOUR',I,1,OK,IVAL,STATUS)
                IF (OK) THEN
                  CALL MSG_SETI('COL',IVAL)
                ELSE
                  CALL MSG_SETC('COL','*')
                ENDIF
                CALL GCB_GET1C('TITLE_JUST',I,1,OK,CVAL,STATUS)
                IF (OK) THEN
                  CALL MSG_SETC('JUS',CVAL)
                ELSE
                  CALL MSG_SETC('JUS','*')
                ENDIF
                CALL MSG_PRNT('    Font=^FON   Boldness=^BOL   '//
     :                          'CharSize=^SIZ   Colour=^COL   '//
     :                          'Justification=^JUS')
              ENDDO
            ENDIF
          ENDIF
          IF (SWITCH.EQ.'*'.OR.SWITCH.EQ.'STA') THEN
            CALL MSG_BLNK()
            CALL MSG_PRNT('ID-stamp:-')
            CALL GCB_GETL('STAMP_FLAG',OK,LVAL,STATUS)
            IF (OK.AND.LVAL) THEN
              CALL GCB_GETC('STAMP_TEXT',OK,CVAL,STATUS)
              IF (.NOT.OK) THEN
                CVAL='default'
              ENDIF
              CALL MSG_SETC('TXT',CVAL)
              CALL MSG_PRNT('  IDStampFlag=On   '//
     :                          'Text=^TXT')
            ELSE
              CALL MSG_PRNT('  IDStampFlag=Off')
            ENDIF
          ENDIF

          IF (SWITCH.EQ.'*'.OR.SWITCH.EQ.'NOT') THEN
            CALL MSG_BLNK()
            CALL MSG_PRNT('Annotation:')
            CALL GCB_GETI('NOTE_N',OK,N,STATUS)
            IF (.NOT.OK) THEN
              CALL MSG_PRNT('  Notes=none')

            ELSE
              DO I=1,N
                CALL GCB_GET1C('NOTE_TEXT',I,1,OK,CVAL,STATUS)
                IF (OK) THEN
                  CALL MSG_SETC('TEX',CVAL)
                ELSE
                  CALL MSG_SETC('TEX',' ')
                ENDIF
                CALL GCB_GET1R('NOTE_X',I,1,OK,RVAL,STATUS)
                IF (OK) THEN
                  CALL MSG_SETR('X',RVAL)
                ELSE
                  CALL MSG_SETC('X','*')
                ENDIF
                CALL GCB_GET1R('NOTE_Y',I,1,OK,RVAL,STATUS)
                IF (OK) THEN
                  CALL MSG_SETR('Y',RVAL)
                ELSE
                  CALL MSG_SETC('Y','*')
                ENDIF
                CALL GCB_GET1R('NOTE_ANGLE',I,1,OK,RVAL,STATUS)
                IF (OK) THEN
                  CALL MSG_SETR('ANG',RVAL)
                ELSE
                  CALL MSG_SETC('ANG','*')
                ENDIF
                CALL MSG_SETI('LIN',I)
                CALL MSG_PRNT('  Note ^LIN  XPos=^X YPos=^Y '//
     :                            'Angle=^ANG  Text=''^TEX''')
                CALL GCB_GET1I('NOTE_FONT',I,1,OK,IVAL,STATUS)
                IF (OK) THEN
                  CALL MSG_SETI('FON',IVAL)
                ELSE
                  CALL MSG_SETC('FON','*')
                ENDIF
                CALL GCB_GET1I('NOTE_BOLD',I,1,OK,IVAL,STATUS)
                IF (OK) THEN
                  CALL MSG_SETI('BOL',IVAL)
                ELSE
                  CALL MSG_SETC('BOL','*')
                ENDIF
                CALL GCB_GET1R('NOTE_SIZE',I,1,OK,RVAL,STATUS)
                IF (OK) THEN
                  CALL MSG_SETR('SIZ',RVAL)
                ELSE
                  CALL MSG_SETC('SIZ','*')
                ENDIF
                CALL GCB_GET1I('NOTE_COLOUR',I,1,OK,IVAL,STATUS)
                IF (OK) THEN
                  CALL MSG_SETI('COL',IVAL)
                ELSE
                  CALL MSG_SETC('COL','*')
                ENDIF
                CALL GCB_GET1C('NOTE_JUST',I,1,OK,CVAL,STATUS)
                IF (OK) THEN
                  CALL MSG_SETC('JUS',CVAL)
                ELSE
                  CALL MSG_SETC('JUS','*')
                ENDIF
                CALL MSG_PRNT('    Font=^FON   Boldness=^BOL   '//
     :                          'CharSize=^SIZ   Colour=^COL   '//
     :                          'Justification=^JUS')
              ENDDO

            ENDIF
          ENDIF

          IF (SWITCH.EQ.'*'.OR.SWITCH.EQ.'MAR') THEN
            CALL MSG_BLNK()
            CALL MSG_PRNT('Markers:-')
            CALL GCB_GETI('MARKER_N',OK,N,STATUS)
            IF (.NOT.OK) THEN
              CALL MSG_PRNT('  Markers=none')

            ELSE
              CALL GCB_GETL('MARKER_NUMBER',OK,LVAL,STATUS)
              IF (OK.AND.LVAL) THEN
                CALL MSG_SETC('MKNUM','on')
              ELSE
                CALL MSG_SETC('MKNUM','off')
              ENDIF
              CALL MSG_PRNT('  MarkerNumbering=^MKNUM')
              DO I=1,N
                CALL GCB_GET1I('MARKER_SYMBOL',I,1,OK,IVAL,STATUS)
                IF (OK) THEN
                  CALL MSG_SETI('SYM',IVAL)
                ELSE
                  CALL MSG_SETC('SYM','*')
                ENDIF
                CALL GCB_GET1R('MARKER_X',I,1,OK,RVAL,STATUS)
                IF (OK) THEN
                  CALL MSG_SETR('X',RVAL)
                ELSE
                  CALL MSG_SETC('X','*')
                ENDIF
                CALL GCB_GET1R('MARKER_Y',I,1,OK,RVAL,STATUS)
                IF (OK) THEN
                  CALL MSG_SETR('Y',RVAL)
                ELSE
                  CALL MSG_SETC('Y','*')
                ENDIF
                CALL GCB_GET1I('MARKER_BOLD',I,1,OK,IVAL,STATUS)
                IF (OK) THEN
                  CALL MSG_SETI('BOL',IVAL)
                ELSE
                  CALL MSG_SETC('BOL','*')
                ENDIF
                CALL GCB_GET1R('MARKER_SIZE',I,1,OK,RVAL,STATUS)
                IF (OK) THEN
                  CALL MSG_SETR('SIZ',RVAL)
                ELSE
                  CALL MSG_SETC('SIZ','*')
                ENDIF
                CALL GCB_GET1I('MARKER_COLOUR',I,1,OK,IVAL,STATUS)
                IF (OK) THEN
                  CALL MSG_SETI('COL',IVAL)
                ELSE
                  CALL MSG_SETC('COL','*')
                ENDIF
                CALL MSG_SETI('MAR',I)
                CALL MSG_PRNT('  Marker ^MAR  XPos=^X  YPos=^Y'//
     :                           '   Symbol=^SYM   Size=^SIZ   '//
     :                           'Boldness=^BOL   Colour=^COL')
              ENDDO
            ENDIF
          ENDIF

          IF (SWITCH.EQ.'*'.OR.SWITCH.EQ.'SHA') THEN
            CALL MSG_BLNK()
            CALL MSG_PRNT('Shapes:-')
            CALL GCB_GETI('SHAPE_N',OK,N,STATUS)
            IF (.NOT.OK) THEN
              CALL MSG_PRNT('  Shapes=none')
            ELSE

              DO I=1,N

                CALL GCB_GET1C('SHAPE_TYPE',I,1,OK,CVAL,STATUS)
                IF (OK) THEN
                  CALL CHR_UCASE(CVAL)
                  CALL MSG_SETC('TYP',CVAL)
                  CALL GCB_GET1R('SHAPE_X',I,1,OK,RVAL,STATUS)
                  IF (OK) THEN
                    CALL MSG_SETR('X',RVAL)
                  ELSE
                    CALL MSG_SETC('X','*')
                  ENDIF
                  CALL GCB_GET1R('SHAPE_Y',I,1,OK,RVAL,STATUS)
                  IF (OK) THEN
                    CALL MSG_SETR('Y',RVAL)
                  ELSE
                    CALL MSG_SETC('Y','*')
                  ENDIF
                  CALL GCB_GET1R('SHAPE_DATA1',I,1,OK,RVAL,STATUS)
                  IF (OK) THEN
                    CALL MSG_SETR('D1',RVAL)
                  ELSE
                    CALL MSG_SETC('D1','*')
                  ENDIF
                  CALL GCB_GET1R('SHAPE_DATA2',I,1,OK,RVAL,STATUS)
                  IF (OK) THEN
                    CALL MSG_SETR('D2',RVAL)
                  ELSE
                    CALL MSG_SETC('D2','*')
                  ENDIF
                  CALL GCB_GET1R('SHAPE_DATA3',I,1,OK,RVAL,STATUS)
                  IF (OK) THEN
                    CALL MSG_SETR('D3',RVAL)
                  ELSE
                    CALL MSG_SETC('D3','*')
                  ENDIF
                  CALL MSG_SETI('SHP',I)
                  IF (CVAL(:3).EQ.'LIN') THEN
                    CALL MSG_PRNT('  Shape ^SHP  Type=^TYP  '//
     :                         'X=^X  Y=^Y  X2=^D1  Y2=^D2')
                  ELSEIF (CVAL(:3).EQ.'VEC') THEN
                    CALL MSG_PRNT('  Shape ^SHP  Type=^TYP  '//
     :                  'X=^X  Y=^Y  XOffset=^D1  YOffset=^D2')
                  ELSEIF (CVAL(:3).EQ.'CIR') THEN
                    CALL MSG_PRNT('  Shape ^SHP  Type=^TYP  '//
     :                  'X=^X  Y=^Y  Radius=^D1')
                  ELSEIF (CVAL(:3).EQ.'ELL') THEN
                    CALL MSG_PRNT('  Shape ^SHP  Type=^TYP  '//
     :                  'X=^X  Y=^Y  SemiMajorAxis=^D1  '//
     :                  'AxisRatio=^D2  Angle=^D3')
                  ELSEIF (CVAL.EQ.'BOX') THEN
                    CALL MSG_PRNT('  Shape ^SHP  Type=^TYP  '//
     :                  'X=^X  Y=^Y  Side1=^D1  Side2=^D2')
                  ENDIF
                  CALL GCB_GET1I('SHAPE_STYLE',I,1,OK,IVAL,STATUS)
                  IF (OK) THEN
                    CALL MSG_SETI('STY',IVAL)
                  ELSE
                    CALL MSG_SETC('STY','*')
                  ENDIF
                  CALL GCB_GET1I('SHAPE_WIDTH',I,1,OK,IVAL,STATUS)
                  IF (OK) THEN
                    CALL MSG_SETI('WID',IVAL)
                  ELSE
                    CALL MSG_SETC('WID','*')
                  ENDIF
                  CALL GCB_GET1I('SHAPE_COLOUR',I,1,OK,IVAL,STATUS)
                  IF (OK) THEN
                    CALL MSG_SETI('COL',IVAL)
                  ELSE
                    CALL MSG_SETC('COL','*')
                  ENDIF
                  CALL MSG_PRNT('    LineStyle=^STY   '//
     :                                   'LineWidth=^WID'//
     :                               '   LineColour=^COL')

                ENDIF

              ENDDO
            ENDIF
          ENDIF

          IF (SWITCH.EQ.'*'.OR.SWITCH.EQ.'COL') THEN
            CALL MSG_BLNK()
            CALL MSG_PRNT('ColourTable:-')
            CALL GCB_GETL('COLOUR_RGB',OK,LVAL,STATUS)
            IF (OK.AND.LVAL) THEN
              CALL MSG_PRNT('  Table=3-colour-RGB')

            ELSE
              CALL GCB_GETI('COLOUR_N',OK,N,STATUS)
              IF (.NOT.OK) THEN
                CALL MSG_PRNT('  Table=default')
              ELSE

                CVAL='  Colour  Red   Green   Blue'
                CALL MSG_PRNT(CVAL)

                DO I=1,N
                  CVAL=' '
                  WRITE(CVAL(5:6),'(I2)') I
                  CALL GCB_GET1R('COLOUR_RED',I,1,OK,RVAL,STATUS)
                  IF (OK) THEN
                    WRITE(CVAL(11:14),'(F4.2)') RVAL
                  ELSE
                    CVAL(11:14)='****'
                  ENDIF
                  CALL GCB_GET1R('COLOUR_GREEN',I,1,OK,RVAL,STATUS)
                  IF (OK) THEN
                    WRITE(CVAL(17:20),'(F4.2)') RVAL
                  ELSE
                    CVAL(17:20)='****'
                  ENDIF
                  CALL GCB_GET1R('COLOUR_BLUE',I,1,OK,RVAL,STATUS)
                  IF (OK) THEN
                    WRITE(CVAL(25:28),'(F4.2)') RVAL
                  ELSE
                    CVAL(25:28)='****'
                  ENDIF
                  CALL MSG_PRNT(CVAL)
                ENDDO

              ENDIF
            ENDIF

            CALL GCB_GETL('COLOUR_NEG',OK,LVAL,STATUS)
            IF (OK.AND.LVAL) THEN
              CALL MSG_PRNT('  ColourNegative=true')
            ENDIF

          ENDIF

        ENDDO

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GSET_SHOW',STATUS)
        ENDIF

      ENDIF
      END




*+
      SUBROUTINE GSET_DUMP(LIVE,NSEL,NDFS,STATUS)

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
      LOGICAL LIVE
      INTEGER NSEL
      INTEGER NDFS(*)
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'GFX_CMN'
      INCLUDE 'GMD_CMN'
*    Local Constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) GLOC
      INTEGER ISEL
*-


      IF (STATUS.EQ.SAI__OK) THEN

*  NSEL is set to 1 for LIVE mode or single dataset
*  otherwise to number of NDFs selected from multiple dataset

        DO ISEL=1,NSEL

*  if not in interactive mode
          IF (.NOT.LIVE) THEN
*  load existing GCB
            IF (G_MULTI) THEN
              IF (ISEL.EQ.1) THEN
                CALL MSG_PRNT('Multiple dataset:-')
              ENDIF
              CALL MSG_SETI('NDF',NDFS(ISEL))
              CALL MSG_PRNT(' processing NDF ^NDF...')
*  from component of multiple dataset
              CALL GMD_LOCNDF(G_MLOC,NDFS(ISEL),GLOC,STATUS)
              CALL GCB_LOAD(GLOC,STATUS)
            ELSE
*  or single dataset
              CALL GCB_LOAD(G_LOC,STATUS)
            ENDIF
          ENDIF

*  dump it
          CALL GCB_DUMP(STATUS)

          IF (.NOT.LIVE) THEN
            IF (G_MULTI) THEN
              CALL BDA_RELEASE(GLOC,STATUS)
              CALL DAT_ANNUL(GLOC,STATUS)
            ENDIF
          ENDIF

        ENDDO

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GSET_DUMP',STATUS)
        ENDIF

      ENDIF
      END
