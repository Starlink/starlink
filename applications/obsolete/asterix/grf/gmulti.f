*+  GMULTI - manipulates multiple dataset
      SUBROUTINE GMULTI(STATUS)

*    Description :
*    Parameters :
*    Method :
*
*    Deficiencies :
*    Bugs :
*    Authors :
*             (BHVAD::RJV)
*    History :
*      27/4/89 : v1.0-1  append and delete functions added
*       4/10/89: V1.0-2  now merges multiple datasets
*      30/1/90 : v1.0-3  max graphs increased to 24 (RJV)
*       8/2/90 : v1.0-4  add SPLIT option (RJV)
*       2/12/91: v1.0-5  indexing added (RJV)
*       23/3/92: v1.0-6  ERR_ANNUL used (RJV)
*       11/1/93: V1.7-0  all multi-dataset handling included (RJV)
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
      INCLUDE 'GFX_CMN'
      INCLUDE 'GMD_CMN'
*    Local Constants :
      CHARACTER*30 VERSION
      PARAMETER (VERSION='GMULTI Version 1.7-0')
      INTEGER MAXGRAF
      PARAMETER (MAXGRAF=24)
*    Local variables :
      CHARACTER*20 SWITCH
      CHARACTER*(DAT__SZLOC) LOC         ! locator to multiple graph dataset
      CHARACTER*(DAT__SZLOC) GLOC        ! locator to dataset to be added
      CHARACTER*(DAT__SZLOC) CLOC        ! locator to individual graph
      CHARACTER*132 ENTRY
      CHARACTER*5 GRAFPAR                ! parameter name assoc with each graph
      CHARACTER*2 ICHAR                  ! integer stored as characters
      INTEGER NCHAR                      ! number of characters in above
      INTEGER NGRAF                      ! number of graphs
      INTEGER IGRAF                      ! graph number
      INTEGER JGRAF
      INTEGER NDIM,DIMS(DAT__MXDIM)
      INTEGER L
      LOGICAL OK
      LOGICAL NEW,ADD,DEL,SPLIT,SHOW,LAYOUT,COMBINE,HELP
      LOGICAL PRIM
      LOGICAL MULTI
*-
      CALL MSG_PRNT(VERSION)

      CALL AST_INIT()

*  get mode

      HELP=.TRUE.
      DO WHILE (HELP.AND.STATUS.EQ.SAI__OK)
        CALL PAR_GET0C('SWITCH',SWITCH,STATUS)
        CALL CHR_UCASE(SWITCH)
        SWITCH=SWITCH(:3)
        IF (SWITCH.EQ.'HEL') THEN
          CALL GMULTI_HELP(STATUS)
          CALL PAR_CANCL('SWITCH',STATUS)
        ELSE
          HELP=.FALSE.
        ENDIF
      ENDDO

      ADD=.FALSE.
      DEL=.FALSE.
      NEW=.FALSE.
      SPLIT=.FALSE.
      SHOW=.FALSE.
      LAYOUT=.FALSE.
      COMBINE=.FALSE.
      IF (SWITCH.EQ.'NEW') THEN
        NEW=.TRUE.
      ELSEIF (SWITCH.EQ.'ADD'.OR.SWITCH.EQ.'APP') THEN
        ADD=.TRUE.
      ELSEIF (SWITCH.EQ.'DEL') THEN
        DEL=.TRUE.
      ELSEIF (SWITCH.EQ.'SPL') THEN
        SPLIT=.TRUE.
      ELSEIF (SWITCH.EQ.'SHO') THEN
        SHOW=.TRUE.
      ELSEIF (SWITCH.EQ.'LAY') THEN
        LAYOUT=.TRUE.
      ELSEIF (SWITCH.EQ.'COM'.OR.SWITCH.EQ.'OVE') THEN
        COMBINE=.TRUE.
      ELSEIF (STATUS.EQ.SAI__OK) THEN
        CALL MSG_PRNT('AST_ERR: unknown mode ')
        STATUS=SAI__ERROR
      ENDIF

      IF (G_OPEN.AND.(NEW.OR.SPLIT)) THEN
        CALL MSG_PRNT(
     :   'AST_ERR: must close current dataset before creating new one')
          STATUS=SAI__ERROR
      ELSEIF (G_OPEN.AND..NOT.G_MULTI) THEN
        CALL MSG_PRNT(
     :   'AST_ERR: dataset currently open is not multiple')
        STATUS=SAI__ERROR
      ENDIF

*  get locator to multiple dataset - creating if necessary
      IF (STATUS.EQ.SAI__OK) THEN
        IF (NEW.OR.SPLIT) THEN
          CALL USI_ASSOCO('OUT','GRAFIX',LOC,STATUS)
        ELSE
          IF (G_OPEN) THEN
            LOC=G_MLOC
            MULTI=G_MULTI
          ELSE
            CALL USI_ASSOCI('INOUT','UPDATE',LOC,PRIM,STATUS)
            CALL GMD_QMULT(LOC,MULTI,STATUS)
            IF (.NOT.MULTI) THEN
              CALL MSG_PRNT('AST_ERR: not a multiple dataset')
              STATUS=SAI__ERROR
            ENDIF
           ENDIF
        ENDIF
      ENDIF

      IF (NEW.OR.ADD.AND.STATUS.EQ.SAI__OK) THEN

        IGRAF=1
        DO WHILE (IGRAF.LE.MAXGRAF.AND.STATUS.EQ.SAI__OK)
          CALL CHR_ITOC(IGRAF,ICHAR,NCHAR)
*  construct parameter name
          GRAFPAR='INP'//ICHAR(:NCHAR)
*  get locator to dataset to be added
          CALL DAT_ASSOC(GRAFPAR,'READ',GLOC,STATUS)
          CALL GMD_QMULT(GLOC,MULTI,STATUS)
          IF (.NOT.MULTI) THEN
            CALL GFX_NDFTYPE(GLOC,STATUS)
          ENDIF
          IF (STATUS.EQ.SAI__OK) THEN
*  add an individual graph
            IF (G_NDF1.OR.G_NDF2) THEN
              CALL STR_OBNAME(GLOC,ENTRY,L,STATUS)
              CALL MSG_SETC('DS',ENTRY)
              CALL MSG_PRNT('  Copying....^DS')
              CALL GMD_ADDNDF(LOC,GLOC,STATUS)
              CALL GMD_PUTINDEX(LOC,0,ENTRY,STATUS)
              CALL DAT_ANNUL(GLOC,STATUS)
*  merge another multiple dataset
            ELSEIF (MULTI) THEN
              CALL STR_OBNAME(GLOC,ENTRY,L,STATUS)
              CALL MSG_SETC('DS',ENTRY)
              CALL MSG_PRNT('  Merging....^DS')
              CALL GMD_QNDF(GLOC,NGRAF,STATUS)
              DO JGRAF=1,NGRAF
                CALL GMD_LOCNDF(GLOC,JGRAF,CLOC,STATUS)
                CALL GMD_GETINDEX(GLOC,JGRAF,ENTRY,STATUS)
                CALL GMD_ADDNDF(LOC,CLOC,STATUS)
                CALL GMD_PUTINDEX(LOC,0,ENTRY,STATUS)
                CALL DAT_ANNUL(CLOC,STATUS)
              ENDDO
            ELSE
              CALL MSG_PRNT('AST_ERR: invalid dataset')
              STATUS=SAI__ERROR
            ENDIF
            IGRAF=IGRAF+1
          ENDIF
        ENDDO
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
        ENDIF

*  delete mode
      ELSEIF (DEL.AND.STATUS.EQ.SAI__OK) THEN
        CALL PAR_GET0I('NDF',IGRAF,STATUS)
        CALL GMD_DELNDF(LOC,IGRAF,STATUS)

*  split mode
      ELSEIF (SPLIT.AND.STATUS.EQ.SAI__OK) THEN
        CALL DAT_ASSOC('INP1','READ',GLOC,STATUS)
        CALL BDA_CHKDATA(GLOC,OK,NDIM,DIMS,STATUS)
        IF (OK.AND.NDIM.EQ.2) THEN
          CALL GMULTI_SPLIT(GLOC,DIMS(1),DIMS(2),LOC,STATUS)
        ELSEIF (OK.AND.NDIM.NE.2) THEN
          CALL MSG_PRNT('AST_ERR: can only split 2D dataset')
          STATUS=SAI__ERROR
        ELSE
          CALL MSG_PRNT('AST_ERR: invalid dataset')
          STATUS=SAI__ERROR
        ENDIF

      ELSEIF (SHOW) THEN
        CALL GMULTI_SHOW(LOC,STATUS)

      ELSEIF (LAYOUT) THEN
        CALL GMULTI_LAYOUT(LOC,STATUS)

      ELSEIF (COMBINE) THEN
        CALL GMULTI_COMBINE(LOC,STATUS)

      ENDIF

      IF (.NOT.G_OPEN) THEN
        CALL USI_ANNUL(LOC,STATUS)
        CALL AST_CLOSE()
      ENDIF

      CALL AST_ERR(STATUS)

      END


      SUBROUTINE GMULTI_SPLIT(ILOC,NVAL,NSET,OLOC,STATUS)
*    Description :
*    Method :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*    Import :
      CHARACTER*(*) ILOC,OLOC
      INTEGER NSET,NVAL
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
      INTEGER CHR_LEN
*    Local constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) GLOC
      CHARACTER*(DAT__SZLOC) DLOC,VLOC,QLOC
      CHARACTER*(DAT__SZLOC) SDLOC,SVLOC,SQLOC
      CHARACTER*75 NAME
      CHARACTER*80 LABEL
      CHARACTER*20 ASTR
      REAL AVAL
      BYTE MASK
      INTEGER NDIM,DIMS(DAT__MXDIM)
      INTEGER ISET
      INTEGER NEL
      INTEGER DIML(2),DIMU(2)
      INTEGER IDPTR,IQPTR,IVPTR
      INTEGER ODPTR,OQPTR,OVPTR
      INTEGER APTR
      INTEGER L,L1,L2
      LOGICAL VOK,QOK
*-
      IF (STATUS.EQ.SAI__OK) THEN

*  create  multiple dataset
        CALL GMD_CREMULT(OLOC,NSET,STATUS)

*  get locators etc to relevant bits of input
        CALL BDA_LOCDATA(ILOC,DLOC,STATUS)
        CALL BDA_CHKVAR(ILOC,VOK,NDIM,DIMS,STATUS)
        IF (VOK) THEN
          CALL BDA_LOCVAR(ILOC,VLOC,STATUS)
        ENDIF
        CALL BDA_CHKQUAL(ILOC,QOK,NDIM,DIMS,STATUS)
        IF (QOK) THEN
          CALL BDA_LOCQUAL(ILOC,QLOC,STATUS)
          CALL BDA_GETMASK(ILOC,MASK,STATUS)
        ENDIF
        CALL BDA_MAPAXVAL(ILOC,'R',2,APTR,STATUS)
        CALL BDA_GETAXLABEL(ILOC,2,LABEL,STATUS)
        L1=CHR_LEN(LABEL)+1

*  get name of donator dataset
        CALL STR_OBNAME(ILOC,NAME,L,STATUS)
        NAME=NAME(:L)//' '//LABEL(:L1)//'='
        L=L+L1+2

        DIML(1)=1
        DIMU(1)=NVAL

        DO ISET=1,NSET

*  locate graph component
          CALL GMD_LOCNDF(OLOC,ISET,GLOC,STATUS)

*  take slice of input data and copy to output
          DIML(2)=ISET
          DIMU(2)=ISET
          CALL DAT_SLICE(DLOC,2,DIML,DIMU,SDLOC,STATUS)
          CALL DAT_MAPV(SDLOC,'_REAL','R',IDPTR,NEL,STATUS)
          CALL BDA_CREDATA(GLOC,1,NVAL,STATUS)
          CALL BDA_MAPDATA(GLOC,'W',ODPTR,STATUS)
          CALL ARR_COP1R(NVAL,%VAL(IDPTR),%VAL(ODPTR),STATUS)
          CALL DAT_ANNUL(SDLOC,STATUS)
          CALL BDA_COPTEXT(ILOC,GLOC,STATUS)
          CALL ARR_ELEM1R(APTR,NSET,ISET,AVAL,STATUS)
          CALL CHR_RTOC(AVAL,ASTR,L2)
          CALL BDA_PUTTITLE(GLOC,LABEL(:L1)//ASTR(:L2),STATUS)
          NAME=NAME(:L)//ASTR(:L2)

*  Variance if there
          IF (VOK) THEN
            CALL DAT_SLICE(VLOC,2,DIML,DIMU,SVLOC,STATUS)
            CALL DAT_MAPV(SVLOC,'_REAL','R',IVPTR,NEL,STATUS)
            CALL BDA_CREVAR(GLOC,1,NVAL,STATUS)
            CALL BDA_MAPVAR(GLOC,'W',OVPTR,STATUS)
            CALL ARR_COP1R(NVAL,%VAL(IVPTR),%VAL(OVPTR),STATUS)
            CALL DAT_ANNUL(SVLOC,STATUS)
          ENDIF
*  QUALITY if there
          IF (QOK) THEN
            CALL DAT_SLICE(QLOC,2,DIML,DIMU,SQLOC,STATUS)
            CALL DAT_MAPV(SQLOC,'_UBYTE','R',IQPTR,NEL,STATUS)
            CALL BDA_CREQUAL(GLOC,1,NVAL,STATUS)
            CALL BDA_PUTMASK(GLOC,MASK,STATUS)
            CALL BDA_MAPQUAL(GLOC,'W',OQPTR,STATUS)
            CALL ARR_COP1B(NVAL,%VAL(IQPTR),%VAL(OQPTR),STATUS)
            CALL DAT_ANNUL(SQLOC,STATUS)
          ENDIF
*  copy axis data
          CALL BDA_COPAXIS(ILOC,GLOC,1,1,STATUS)

*  update index
          CALL GMD_PUTINDEX(OLOC,ISET,NAME,STATUS)

*  finished with this graph
          CALL BDA_RELEASE(GLOC,STATUS)
          CALL DAT_ANNUL(GLOC,STATUS)

        ENDDO

        CALL BDA_UNMAP(ILOC,STATUS)

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GMULTI_SPLIT',STATUS)
        ENDIF

      ENDIF
      END



*+
      SUBROUTINE GMULTI_SHOW(LOC,STATUS)
*    Description :
*    Method :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
      CHARACTER*(*) LOC
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      CHARACTER LINE*80,OVLY*132
      INTEGER NPLOT,IPLOT,BASE
      INTEGER IGRAF,NGRAF
      INTEGER L
      INTEGER NX,NY
      LOGICAL XOK,YOK
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL GMD_QNDF(LOC,NGRAF,STATUS)

        CALL MSG_BLNK()
        CALL STR_OBNAME(LOC,LINE,L,STATUS)
        CALL MSG_PRNT('Content of '//LINE(:L))
        CALL MSG_BLNK()
        CALL MSG_PRNT('NDF   Source dataset')
        DO IGRAF=1,NGRAF

          CALL GMD_GETINDEX(LOC,IGRAF,LINE(6:),STATUS)
          LINE(:5)=' '
          IF (IGRAF.LT.10) THEN
            WRITE(LINE(2:2),'(I1)') IGRAF
          ELSE
            WRITE(LINE(2:3),'(I2)') IGRAF
          ENDIF

          CALL MSG_PRNT(LINE)

        ENDDO

        CALL MSG_BLNK()
        CALL MSG_BLNK()

        CALL GMD_QPLOTS(LOC,NPLOT,STATUS)
        IF (NPLOT.EQ.0) THEN
          CALL MSG_PRNT(' No plots defined:')
          CALL MSG_PRNT('   - will default to one plot per dataset')
        ELSE
          CALL MSG_PRNT(' The following plots are defined:')
          CALL MSG_PRNT('  Plot  Base  Overlays')
          DO IPLOT=1,NPLOT
            CALL GMD_GETPLOT(LOC,IPLOT,BASE,OVLY,STATUS)
            IF (OVLY.EQ.'0'.OR.OVLY.EQ.' ') THEN
              OVLY='none'
            ENDIF
            LINE=' '
            WRITE(LINE(3:5),'(I3)') IPLOT
            WRITE(LINE(9:11),'(I3)') BASE
            LINE(16:)=OVLY
            CALL MSG_PRNT(LINE)
          ENDDO
        ENDIF

        CALL MSG_BLNK()
        CALL MSG_BLNK()

        CALL GMD_GETLAYOUT(LOC,XOK,NX,YOK,NY,STATUS)
        IF (XOK.AND.YOK) THEN
          CALL MSG_SETI('NX',NX)
          CALL MSG_SETI('NY',NY)
          CALL MSG_PRNT( 'Plot layout is ^NX horiz. by ^NY vert.')
        ELSEIF (XOK) THEN
          CALL MSG_SETI('NX',NX)
          CALL MSG_PRNT( 'Plot layout is ^NX horizontally')
        ELSEIF (YOK) THEN
          CALL MSG_SETI('NY',NY)
          CALL MSG_PRNT( 'Plot layout is ^NY vertically')
        ELSE
          CALL MSG_PRNT(' Plot layout is undefined')
        ENDIF

        CALL MSG_BLNK()

      ENDIF

      END



*+
      SUBROUTINE GMULTI_LAYOUT(LOC,STATUS)
*    Description :
*    Method :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*    Import :
      CHARACTER*(*) LOC
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      INTEGER NX,NY                        ! layout parameters
      LOGICAL CANCEL
      LOGICAL SETX,SETY
*-

      IF (STATUS.EQ.SAI__OK) THEN


        SETX=.TRUE.
        SETY=.TRUE.

        CALL PAR_GET0L('CANCEL',CANCEL,STATUS)
* cancel ?
        IF (CANCEL) THEN
          CALL GMD_CANLAYOUT(LOC,STATUS)
        ELSE
*  otherwise get x by y
          CALL PAR_GET0I('NX',NX,STATUS)
          IF (STATUS.EQ.PAR__NULL) THEN
            SETX=.FALSE.
            CALL ERR_ANNUL(STATUS)
          ENDIF
          CALL PAR_GET0I('NY',NY,STATUS)
          IF (STATUS.EQ.PAR__NULL) THEN
            SETY=.FALSE.
            CALL ERR_ANNUL(STATUS)
          ENDIF

*  write out values
          CALL GMD_SETLAYOUT(LOC,SETX,NX,SETY,NY,STATUS)

        ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GMULTI_LAYOUT',STATUS)
        ENDIF

      ENDIF

      END



*+
      SUBROUTINE GMULTI_COMBINE(LOC,STATUS)
*    Description :
*    Method :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*    Import :
      CHARACTER*(*) LOC
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      CHARACTER*132 OVLY
      INTEGER PLOT
      INTEGER NPLOT
      INTEGER BASE
      LOGICAL CANCEL
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL PAR_GET0L('CANCEL',CANCEL,STATUS)
        IF (CANCEL) THEN
          CALL GMD_CANPLOTS(LOC,STATUS)
        ELSE

          CALL GMD_QPLOTS(LOC,NPLOT,STATUS)

*  get plot number
          CALL PAR_GET0I('PLOT',PLOT,STATUS)
          IF (PLOT.GT.NPLOT+1) THEN
            PLOT=NPLOT+1
            CALL MSG_SETI('NPL',NPLOT)
            CALL MSG_SETI('PLO',PLOT)
            CALL MSG_PRNT(' ** only ^NPL plots already defined '//
     :                      '- new plot will be ^PLO')
          ENDIF
          CALL PAR_GET0I('BASE',BASE,STATUS)
          CALL PAR_GET0C('OVLY',OVLY,STATUS)
          IF (STATUS.EQ.PAR__NULL) THEN
            OVLY=' '
            CALL ERR_ANNUL(STATUS)
          ELSEIF (OVLY.EQ.'0') THEN
            OVLY=' '
          ENDIF

*  write it out
          CALL GMD_SETPLOT(LOC,PLOT,BASE,OVLY,STATUS)

        ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GMULTI_COMBINE',STATUS)
        ENDIF

      ENDIF

      END


*+
      SUBROUTINE GMULTI_HELP(STATUS)
*    Description :
*    Method :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
      INTEGER NLINE
      PARAMETER (NLINE=7)
*    Local variables :
      CHARACTER*79 TEXT(NLINE)
      INTEGER ILINE
*    Local data :
      DATA TEXT/
     :'  NEW     - create a new multiple dataset',
     :'  APPend  - add data to an existing multiple dataset',
     :'  DELete  - delete data from an existing dataset',
     :'  SPLit   - split a 2D dataset into multiple 1D data',
     :'  SHOw    - list details of existing dataset',
     :'  LAYout  - specify layout of plots on display surface',
     :'  COMbine - combine data into composite plots (eg overlays)'/
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL MSG_BLNK()
        CALL MSG_PRNT('The following modes are available:-')
        CALL MSG_BLNK()
        DO ILINE=1,NLINE
          CALL MSG_PRNT(TEXT(ILINE))
        ENDDO
        CALL MSG_BLNK()

      ENDIF

      END
