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
*      12 Sep 95 : V2.0-0 ADI port (DJA)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'
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
      PARAMETER (VERSION='GMULTI Version 2.2-0')
      INTEGER MAXGRAF
      PARAMETER (MAXGRAF=24)
*    Local variables :
      CHARACTER*20		SWITCH
      CHARACTER*132 ENTRY
      CHARACTER*5 GRAFPAR                ! parameter name assoc with each graph
      CHARACTER*2 ICHAR                  ! integer stored as characters

      INTEGER NCHAR                      ! number of characters in above
      INTEGER NGRAF                      ! number of graphs
      INTEGER IGRAF                      ! graph number
      INTEGER JGRAF
      INTEGER NDIM,DIMS(ADI__MXDIM)
      INTEGER L

      INTEGER			CFID			! M/g component id
      INTEGER			FID			! Input file identifier
      INTEGER			GFID			! Input graph file id
      INTEGER			MOBJ			! New multi-graph d/s

      LOGICAL OK
      LOGICAL NEW,ADD,DEL,SPLIT,SHOW,LAYOUT,COMBINE,HELP
      LOGICAL MULTI
*-
      CALL MSG_PRNT(VERSION)

      CALL AST_INIT()

*  get mode

      HELP=.TRUE.
      DO WHILE (HELP.AND.STATUS.EQ.SAI__OK)
        CALL USI_GET0C('SWITCH',SWITCH,STATUS)
        CALL CHR_UCASE(SWITCH)
        SWITCH=SWITCH(:3)
        IF (SWITCH.EQ.'HEL') THEN
          CALL GMULTI_HELP(STATUS)
          CALL USI_CANCL('SWITCH',STATUS)
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
          CALL ADI_NEW0( 'MultiGraph', MOBJ, STATUS )
          CALL USI_CREAT( 'OUT', MOBJ, FID, STATUS )

        ELSE
          IF ( G_OPEN ) THEN
            FID = G_MFID
            MULTI = G_MULTI
          ELSE
            CALL USI_ASSOC( 'INOUT','MultiGraph','UPDATE',FID,STATUS)
            CALL GMI_QMULT( FID, MULTI, STATUS )
            IF (.NOT.MULTI) THEN
              CALL MSG_PRNT('AST_ERR: not a multiple dataset')
              STATUS=SAI__ERROR
            ENDIF
           ENDIF
        ENDIF
      ENDIF

*  New file, or extending existing file
      IF (NEW.OR.ADD.AND.STATUS.EQ.SAI__OK) THEN

        IGRAF=1
        DO WHILE (IGRAF.LE.MAXGRAF.AND.STATUS.EQ.SAI__OK)
          CALL CHR_ITOC(IGRAF,ICHAR,NCHAR)
*  construct parameter name
          GRAFPAR='INP'//ICHAR(:NCHAR)

*  get identifier to dataset to be added
          CALL USI_ASSOC( GRAFPAR, 'MultiGraph|BinDS', 'READ',
     :                    GFID, STATUS )

*      Is input a multi-grpah itself?
          CALL GMI_QMULT( GFID,MULTI,STATUS)
          IF (.NOT.MULTI) THEN
            CALL GFX_NDFTYPE( GFID,STATUS)
          ENDIF

          IF (STATUS.EQ.SAI__OK) THEN
*  add an individual graph
            IF ( G_NDF1 .OR. G_NDF2 ) THEN

              CALL ADI_FOBNAM( GFID, ENTRY, L, STATUS )
              CALL MSG_SETC('DS',ENTRY(:L) )
              CALL MSG_PRNT('  Copying....^DS')
              CALL GMI_ADDNDF( FID, GFID,STATUS)
              CALL GMI_PUTINDEX( FID,0,ENTRY,STATUS)
              CALL USI_CANCL( GRAFPAR, STATUS )

*  merge another multiple dataset
            ELSE IF (MULTI) THEN

              CALL ADI_FOBNAM( GFID, ENTRY, L, STATUS )
              CALL MSG_SETC('DS',ENTRY)
              CALL MSG_PRNT('  Merging....^DS')
              CALL GMI_QNDF( GFID, NGRAF, STATUS )
              DO JGRAF = 1, NGRAF
                CALL GMI_LOCNDF(GFID,JGRAF,'*',CFID,STATUS)
                CALL GMI_GETINDEX(GFID,JGRAF,ENTRY,STATUS)
                CALL GMI_ADDNDF(FID,CFID,STATUS)
                CALL GMI_PUTINDEX(FID,0,ENTRY,STATUS)
                CALL ADI_ERASE(CFID,STATUS)
              END DO

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
        CALL USI_GET0I('NDF',IGRAF,STATUS)
        CALL GMI_DELNDF( FID, IGRAF,STATUS)

*  split mode
      ELSEIF (SPLIT.AND.STATUS.EQ.SAI__OK) THEN
        CALL USI_ASSOC( 'INP1','BinDS', 'READ', GFID, STATUS )
        CALL BDI_CHK( GFID, 'Data', OK, STATUS )
        CALL BDI_GETSHP( GFID, ADI__MXDIM, DIMS, NDIM, STATUS )
        IF (OK.AND.NDIM.EQ.2) THEN
          CALL GMULTI_SPLIT( GFID, NDIM, DIMS, FID, STATUS )
        ELSEIF (OK.AND.NDIM.NE.2) THEN
          CALL MSG_PRNT('AST_ERR: can only split 2D dataset')
          STATUS=SAI__ERROR
        ELSE
          CALL MSG_PRNT('AST_ERR: invalid dataset')
          STATUS=SAI__ERROR
        ENDIF

      ELSEIF (SHOW) THEN
        CALL GMULTI_SHOW(FID,STATUS)

      ELSEIF (LAYOUT) THEN
        CALL GMULTI_LAYOUT(FID,STATUS)

      ELSEIF (COMBINE) THEN
        CALL GMULTI_COMBINE(FID,STATUS)

      ENDIF

      IF (.NOT.G_OPEN) THEN
        IF ( NEW .OR. SPLIT ) THEN
          CALL USI_ANNUL( 'OUT',STATUS)
        ELSE
          CALL USI_ANNUL( 'INOUT',STATUS)
        END IF
        CALL AST_CLOSE()
      ENDIF

      CALL AST_ERR(STATUS)

      END


*+
      SUBROUTINE GMULTI_SPLIT(IFID,NDIM,DIMS,OFID,STATUS)
*    Description :
*    Method :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'ADI_PAR'
*    Import :
      INTEGER IFID,NDIM,DIMS(*),OFID
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
      INTEGER CHR_LEN
*    Local constants :
*    Local variables :
      CHARACTER*20 		ASTR
      CHARACTER*75 		NAME
      CHARACTER*80 		LABEL

      REAL 			AVAL			! Slice axis value

      INTEGER			GFID

      INTEGER 			APTR			! Input axis data
      INTEGER 			ISET			! Loop over sets
      INTEGER 			DIML(2),DIMU(2)		! Slicing bounds
      INTEGER 			IDPTR,IQPTR,IVPTR	! Input data
      INTEGER 			ODPTR,OQPTR,OVPTR	! Output slice data
      INTEGER L,L1,L2

      BYTE			QMASK			! Input quality mask

      LOGICAL 			VOK,QOK			! Variance/quality ok?
*-
      IF (STATUS.EQ.SAI__OK) THEN

*    Create  multiple dataset
        CALL GMI_CREMULT( OFID, DIMS(2), STATUS )

*    Check bits of input
        CALL BDI_CHK( IFID, 'Variance', VOK, STATUS )
        CALL BDI_CHK( IFID, 'Quality', QOK, STATUS )
        CALL BDI_AXMAPR( IFID, 2, 'READ', APTR, STATUS )
        CALL BDI_AXGET0C( IFID, 2, 'Label', LABEL,STATUS)
        L1=CHR_LEN(LABEL)+1

*    Get name of donator dataset
        CALL ADI_FOBNAM( IFID, NAME, L, STATUS )
        NAME=NAME(:L)//' '//LABEL(:L1)//'='
        L=L+L1+2

        DIML(1) = 1
        DIMU(1) = DIMS(1)

*    Map the input data, and the variance and quality if present
        CALL BDI_MAPR( IFID, 'Data', 'READ', IDPTR, STATUS )
        IF ( VOK ) THEN
          CALL BDI_MAPR( IFID, 'Variance', 'READ', IVPTR, STATUS )
        END IF
        IF ( QOK ) THEN
          CALL BDI_MAPUB( IFID, 'Quality', 'READ', IQPTR, STATUS )
          CALL BDI_GET0UB( IFID, 'QualityMask', QMASK, STATUS )
        END IF

*    Loop over sets
        DO ISET = 1, DIMS(2)

*      Define bounds in 2nd dimension of array to be copied
          DIML(2) = ISET
          DIMU(2) = ISET

*      Locate graph component
          CALL GMI_LOCNDF( OFID, ISET, 'BinDS', GFID, STATUS )

*      Copy input data slice to output
          CALL BDI_MAPR( GFID, 'Data', 'WRITE', ODPTR, STATUS )
          CALL ARR_SLCOPR( NDIM, DIMS, %VAL(IDPTR), DIML, DIMU,
     :                     %VAL(ODPTR), STATUS )
          CALL BDI_UNMAP( GFID, 'Data', ODPTR, STATUS )

*      Copy input variance slice to output, if present
          IF ( VOK ) THEN
            CALL BDI_MAPR( GFID, 'Variance', 'WRITE', OVPTR, STATUS )
            CALL ARR_SLCOPR( NDIM, DIMS, %VAL(IVPTR), DIML, DIMU,
     :                       %VAL(OVPTR), STATUS )
            CALL BDI_UNMAP( GFID, 'Variance', ODPTR, STATUS )
          END IF

*      Copy input quality slice to output, if present
          IF ( QOK ) THEN
            CALL BDI_MAPUB( GFID, 'Quality', 'WRITE', OQPTR, STATUS )
            CALL ARR_SLCOPB( NDIM, DIMS, %VAL(IQPTR), DIML, DIMU,
     :                       %VAL(OQPTR), STATUS )
            CALL BDI_UNMAP( GFID, 'Quality', ODPTR, STATUS )
            CALL BDI_PUT0UB( IFID, 'QualityMask', QMASK, STATUS )
          END IF

*      Copy ancillaries
          CALL BDI_COPY( IFID, 'Label,Units', GFID, STATUS )

*      Construct new title
          CALL ARR_ELEM1R( APTR, DIMS(2), ISET, AVAL, STATUS )
          CALL CHR_RTOC(AVAL,ASTR,L2)
          CALL BDI_PUT0C( GFID, 'Title', LABEL(:L1)//ASTR(:L2), STATUS )
          NAME = NAME(:L)//ASTR(:L2)

*      Copy axis data
          CALL BDI_COPY( IFID, 'Axis_1', GFID, STATUS )

*      Update index
          CALL GMI_PUTINDEX( OFID, ISET, NAME, STATUS )

*      Finished with this graph
          CALL ADI_ERASE( GFID, STATUS )

*    Next set
        ENDDO

        IF (STATUS.NE.SAI__OK) THEN
          CALL AST_REXIT('GMULTI_SPLIT',STATUS)
        ENDIF

      ENDIF
      END



*+
      SUBROUTINE GMULTI_SHOW(FID,STATUS)
*    Description :
*    Method :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER	FID
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

        CALL GMI_QNDF(FID,NGRAF,STATUS)

        CALL MSG_BLNK()
        CALL ADI_FOBNAM( FID, LINE, L, STATUS )
        CALL MSG_PRNT('Content of '//LINE(:L))
        CALL MSG_BLNK()
        CALL MSG_PRNT('NDF   Source dataset')
        DO IGRAF=1,NGRAF

          CALL GMI_GETINDEX(FID,IGRAF,LINE(6:),STATUS)
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

        CALL GMI_QPLOTS(FID,NPLOT,STATUS)
        IF (NPLOT.EQ.0) THEN
          CALL MSG_PRNT(' No plots defined:')
          CALL MSG_PRNT('   - will default to one plot per dataset')
        ELSE
          CALL MSG_PRNT(' The following plots are defined:')
          CALL MSG_PRNT('  Plot  Base  Overlays')
          DO IPLOT=1,NPLOT
            CALL GMI_GETPLOT(FID,IPLOT,BASE,OVLY,STATUS)
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

        CALL GMI_GETLAYOUT(FID,XOK,NX,YOK,NY,STATUS)
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
      SUBROUTINE GMULTI_LAYOUT(FID,STATUS)
*    Description :
*    Method :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PAR_ERR'
*    Import :
      INTEGER			FID
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

        CALL USI_GET0L('CANCEL',CANCEL,STATUS)
* cancel ?
        IF (CANCEL) THEN
          CALL GMI_CANLAYOUT(FID,STATUS)
        ELSE
*  otherwise get x by y
          CALL USI_GET0I('NX',NX,STATUS)
          IF (STATUS.EQ.PAR__NULL) THEN
            SETX=.FALSE.
            CALL ERR_ANNUL(STATUS)
          ENDIF
          CALL USI_GET0I('NY',NY,STATUS)
          IF (STATUS.EQ.PAR__NULL) THEN
            SETY=.FALSE.
            CALL ERR_ANNUL(STATUS)
          ENDIF

*  write out values
          CALL GMI_SETLAYOUT( FID, SETX,NX,SETY,NY,STATUS)

        ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL AST_REXIT( 'GMULTI_LAYOUT',STATUS)
        ENDIF

      ENDIF

      END



*+
      SUBROUTINE GMULTI_COMBINE(FID,STATUS)
*    Description :
*    Method :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PAR_ERR'
*    Import :
      INTEGER			FID
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

        CALL USI_GET0L('CANCEL',CANCEL,STATUS)
        IF (CANCEL) THEN
          CALL GMI_CANPLOTS(FID,STATUS)
        ELSE

          CALL GMI_QPLOTS(FID,NPLOT,STATUS)

*  get plot number
          CALL USI_GET0I('PLOT',PLOT,STATUS)
          IF (PLOT.GT.NPLOT+1) THEN
            PLOT=NPLOT+1
            CALL MSG_SETI('NPL',NPLOT)
            CALL MSG_SETI('PLO',PLOT)
            CALL MSG_PRNT(' ** only ^NPL plots already defined '//
     :                      '- new plot will be ^PLO')
          ENDIF
          CALL USI_GET0I('BASE',BASE,STATUS)
          CALL USI_GET0C('OVLY',OVLY,STATUS)
          IF (STATUS.EQ.PAR__NULL) THEN
            OVLY=' '
            CALL ERR_ANNUL(STATUS)
          ELSEIF (OVLY.EQ.'0') THEN
            OVLY=' '
          ENDIF

*  write it out
          CALL GMI_SETPLOT(FID,PLOT,BASE,OVLY,STATUS)

        ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL AST_REXIT( 'GMULTI_COMBINE',STATUS)
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
