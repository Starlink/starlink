*+  IREGION - select a region of the plot
      SUBROUTINE IREGION(STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*    3 Oct 94 : v1.7-0 original (RJV)
*   13 Dec 94 : v1.8-0 sub-mode added (RJV)
*    6 Jan 95 : v1.8-1 writes ARD (RJV)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      CHARACTER*10 MODE,SUBMODE
      INTEGER RPTR
      LOGICAL EXCLUDE
      LOGICAL MERGE
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'IREGION Version 1.8-1')
*-
      CALL USI_INIT()

      CALL MSG_PRNT(VERSION)

      IF (.NOT.I_OPEN) THEN
        CALL MSG_PRNT('AST_ERR: image processing system not active')
      ELSEIF (.NOT.I_DISP) THEN
        CALL MSG_PRNT('AST_ERR: no image currently displayed')
      ELSE

*  make sure transformations are correct
        CALL GTR_RESTORE(STATUS)

*  get main mode
        MERGE=.FALSE.
        EXCLUDE=.FALSE.
        MODE=' '
        DO WHILE (MODE.EQ.' '.AND.STATUS.EQ.SAI__OK)
          CALL USI_GET0C('MODE',MODE,STATUS)
          CALL CHR_UCASE(MODE)
          IF (MODE.EQ.'HELP') THEN
            CALL IREGION_HELP()
            CALL USI_CANCL('MODE',STATUS)
            MODE=' '
          ENDIF
        ENDDO
        CALL IREGION_PARSE_MODE(MODE,MERGE,STATUS)


*  if main mode is mergeable with previous region - get sub-mode
        IF (MERGE) THEN
          SUBMODE=' '
          DO WHILE (SUBMODE.EQ.' '.AND.STATUS.EQ.SAI__OK)
            CALL USI_GET0C('SUBMODE',SUBMODE,STATUS)
            CALL CHR_UCASE(SUBMODE)
            IF (SUBMODE.EQ.'HELP') THEN
              CALL IREGION_SUBHELP()
              CALL USI_CANCL('SUBMODE',STATUS)
              MODE=' '
            ENDIF
          ENDDO
          CALL IREGION_PARSE_SUBMODE(SUBMODE,EXCLUDE,STATUS)
*  if fresh region - clear decks
          IF (SUBMODE.EQ.'NEW') THEN
            CALL IMG_SETWHOLE(STATUS)
            MERGE=.FALSE.
            CALL ARX_RESET(I_ARD_ID,STATUS)
          ELSE
*  otherwise save current region mask and get memory to store new stuff
            RPTR=I_REG_PTR
            CALL DYN_MAPB(1,I_NX*I_NY,I_REG_PTR,STATUS)
            IF (EXCLUDE) THEN
              CALL ARR_INIT1B('01'X,I_NX*I_NY,%val(I_REG_PTR),STATUS)
            ELSE
              CALL ARR_INIT1B('00'X,I_NX*I_NY,%val(I_REG_PTR),STATUS)
            ENDIF
          ENDIF
        ENDIF

        IF (EXCLUDE.AND.I_REG_TYPE.EQ.'NONE') THEN
          CALL ARR_INIT1B('01'X,I_NX*I_NY,%val(I_REG_PTR),STATUS)
          I_REG_TYPE='COMPLEX'
        ENDIF

        IF (STATUS.EQ.SAI__OK) THEN


          IF (MODE.EQ.'CIR') THEN
            CALL IREGION_CIRCLE(SUBMODE,EXCLUDE,STATUS)
          ELSEIF (MODE.EQ.'BOX') THEN
            CALL IREGION_BOX(SUBMODE,EXCLUDE,STATUS)
          ELSEIF (MODE.EQ.'POL') THEN
            CALL IREGION_POLYGON(SUBMODE,EXCLUDE,STATUS)
          ELSEIF (MODE.EQ.'ANN') THEN
            CALL IREGION_ANNULUS(SUBMODE,EXCLUDE,STATUS)
          ELSEIF (MODE.EQ.'ELL') THEN
            CALL IREGION_ELLIPSE(EXCLUDE,STATUS)
          ELSEIF (MODE.EQ.'WHO') THEN
            CALL IREGION_WHOLE(STATUS)
          ELSEIF (MODE.EQ.'SLI') THEN
            CALL IREGION_SLICE(SUBMODE,EXCLUDE,STATUS)
          ELSEIF (MODE.EQ.'GTE') THEN
            CALL IREGION_GTE(SUBMODE,EXCLUDE,STATUS)
          ELSEIF (MODE.EQ.'SHO') THEN
            CALL IREGION_SHOW(STATUS)
          ELSEIF (MODE.EQ.'LIS') THEN
            CALL IREGION_LIST(STATUS)
          ELSEIF (MODE.EQ.'EXP') THEN
            CALL IREGION_EXPORT(STATUS)
          ELSEIF (MODE.EQ.'IMP') THEN
            CALL IREGION_IMPORT(STATUS)
          ELSEIF (MODE.EQ.'INV') THEN
            CALL IMG_SETINV(STATUS)
          ENDIF


        ENDIF

        IF (MERGE) THEN
          CALL IREGION_MERGE(SUBMODE,%val(I_REG_PTR),%val(RPTR),STATUS)
          CALL DYN_UNMAP(I_REG_PTR,STATUS)
          I_REG_PTR=RPTR
        ENDIF

      ENDIF

      CALL USI_CLOSE()

      END



*+
      SUBROUTINE IREGION_MERGE(MODE,NEWREG,OLDREG,STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Import:
      CHARACTER*(*) MODE
      BYTE NEWREG(I_NX,I_NY)
*    Import/Export :
      BYTE OLDREG(I_NX,I_NY)
*    Status :
      INTEGER STATUS
*    Function declarations :
      BYTE BIT_ANDUB,BIT_ORUB
*    Local constants :
*    Local variables :
      INTEGER I,J
*-
      IF (STATUS.EQ.SAI__OK) THEN

        IF (MODE.EQ.'AND') THEN

          DO J=1,I_NY
            DO I=1,I_NX
              OLDREG(I,J)=BIT_ANDUB(OLDREG(I,J),NEWREG(I,J))
            ENDDO
          ENDDO

        ELSEIF (MODE.EQ.'ADD') THEN

          DO J=1,I_NY
            DO I=1,I_NX
              OLDREG(I,J)=BIT_ORUB(OLDREG(I,J),NEWREG(I,J))
            ENDDO
          ENDDO

        ENDIF

      ENDIF

      END



*+
      SUBROUTINE IREGION_CIRCLE(MODE,EXCLUDE,STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Import :
      CHARACTER*(*) MODE
      LOGICAL EXCLUDE
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
      INTEGER CHR_LEN
*    Local constants :
*    Local variables :
      CHARACTER*80 TEXT
      INTEGER L
      REAL XC,YC,RAD
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL IMG_GETCIRC('XC','YC','RAD',XC,YC,RAD,STATUS)
        CALL IMG_SETCIRC(XC,YC,RAD,EXCLUDE,STATUS)

        IF (MODE.EQ.'AND') THEN
          TEXT=' .AND.'
          L=7
        ELSE
          TEXT=' '
          L=1
        ENDIF

        IF (EXCLUDE) THEN
          TEXT(L:)=' .NOT. (CIRCLE( '
        ELSE
          TEXT(L:)=' CIRCLE( '
        ENDIF
        L=CHR_LEN(TEXT)

        CALL MSG_SETR('XC',XC)
        CALL MSG_SETR('YC',YC)
        CALL MSG_SETR('RAD',RAD)
        CALL MSG_MAKE(TEXT(:L)//' ^XC , ^YC , ^RAD ',TEXT,L)
        IF (EXCLUDE) THEN
          TEXT(L:)='))'
          L=L+1
        ELSE
          TEXT(L:L)=')'
        ENDIF

        CALL ARX_PUT(I_ARD_ID,0,TEXT(:L),STATUS)

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from IREGION_CIRCLE',STATUS)
        ENDIF

      ENDIF

      END




*+
      SUBROUTINE IREGION_ANNULUS(MODE,EXCLUDE,STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Import :
      CHARACTER*(*) MODE
      LOGICAL EXCLUDE
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
      INTEGER CHR_LEN
*    Local constants :
*    Local variables :
      CHARACTER*80 TEXT
      REAL XC,YC,IRAD,ORAD
      INTEGER L
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL IMG_GETANNULUS('XC','YC','IRAD','ORAD',XC,YC,IRAD,ORAD,
     :                                                       STATUS)
        CALL IMG_SETANNULUS(XC,YC,IRAD,ORAD,EXCLUDE,STATUS)

        IF (MODE.EQ.'AND') THEN
          TEXT=' .AND.'
          L=7
        ELSE
          TEXT=' '
          L=1
        ENDIF

        IF (EXCLUDE) THEN
          TEXT(L:)=' .NOT. (CIRCLE( '
        ELSE
          TEXT(L:)=' (CIRCLE( '
        ENDIF
        L=CHR_LEN(TEXT)

        CALL MSG_SETR('XC',XC)
        CALL MSG_SETR('YC',YC)
        CALL MSG_SETR('RAD',ORAD)
        CALL MSG_MAKE(TEXT(:L)//' ^XC , ^YC , ^RAD )',TEXT,L)

        CALL ARX_PUT(I_ARD_ID,0,TEXT(:L),STATUS)


        TEXT='     .AND..NOT.CIRCLE('
        L=CHR_LEN(TEXT)

        CALL MSG_SETR('XC',XC)
        CALL MSG_SETR('YC',YC)
        CALL MSG_SETR('RAD',IRAD)
        CALL MSG_MAKE(TEXT(:L)//' ^XC , ^YC , ^RAD ))',TEXT,L)
        IF (EXCLUDE) THEN
          L=L+1
          TEXT(L:)=')'
        ENDIF

        CALL ARX_PUT(I_ARD_ID,0,TEXT(:L),STATUS)




        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from IREGION_ANNULUS',STATUS)
        ENDIF

      ENDIF

      END




*+
      SUBROUTINE IREGION_ELLIPSE(EXCLUDE,STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Import :
      LOGICAL EXCLUDE
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      REAL XC,YC,ANGLE,MAJOR,MINOR
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL IMG_GETELLIPSE('XC','YC','MAJOR','MINOR','ANGLE',
     :                           XC,YC,MAJOR,MINOR,ANGLE,STATUS)
        CALL IMG_SETELLIPSE(XC,YC,MAJOR,MINOR,ANGLE,EXCLUDE,STATUS)


        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from IREGION_ELLIPSE',STATUS)
        ENDIF

      ENDIF

      END





*+
      SUBROUTINE IREGION_SLICE(MODE,EXCLUDE,STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Import :
      CHARACTER*(*) MODE
      LOGICAL EXCLUDE
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
      INTEGER CHR_LEN
*    Local constants :
      REAL RTOD
      PARAMETER (RTOD=180.0/3.14159265)
*    Local variables :
      CHARACTER*80 TEXT
      INTEGER L
      REAL XC,YC,ANGLE,LENGTH,WIDTH
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL IMG_GETSLICE('XC','YC','ANGLE','LENGTH','WIDTH',
     :                           XC,YC,ANGLE,LENGTH,WIDTH,STATUS)
        CALL IMG_SETSLICE(XC,YC,ANGLE,LENGTH,WIDTH,EXCLUDE,STATUS)

        IF (MODE.EQ.'AND') THEN
          TEXT=' .AND.'
          L=7
        ELSE
          TEXT=' '
          L=1
        ENDIF

        IF (EXCLUDE) THEN
          TEXT(L:)=' .NOT. (ROTBOX( '
        ELSE
          TEXT(L:)=' ROTBOX( '
        ENDIF
        L=CHR_LEN(TEXT)

        CALL MSG_SETR('XC',XC)
        CALL MSG_SETR('YC',YC)
        CALL MSG_SETR('LN',LENGTH)
        CALL MSG_SETR('WD',WIDTH)
        ANGLE=ANGLE*RTOD*I_XSCALE/ABS(I_XSCALE)*I_YSCALE/ABS(I_YSCALE)
        CALL MSG_SETR('AN',ANGLE)
        CALL MSG_MAKE(TEXT(:L)//' ^XC , ^YC , ^LN , ^WD , ^AN ',
     :                                                    TEXT,L)
        IF (EXCLUDE) THEN
          TEXT(L:)='))'
          L=L+1
        ELSE
          TEXT(L:L)=')'
        ENDIF

        CALL ARX_PUT(I_ARD_ID,0,TEXT(:L),STATUS)



        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from IREGION_SLICE',STATUS)
        ENDIF

      ENDIF

      END






*+
      SUBROUTINE IREGION_POLYGON(MODE,EXCLUDE,STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Import :
      CHARACTER*(*) MODE
      LOGICAL EXCLUDE
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
      INTEGER CHR_LEN
*    Local constants :
      INTEGER NVMAX
      PARAMETER (NVMAX=100)
*    Local variables :
      CHARACTER*80 TEXT
      REAL XV(NVMAX),YV(NVMAX)
      INTEGER NV
      INTEGER L
      INTEGER I
      INTEGER NPAIR
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL IMG_GETPOLY(NVMAX,XV,YV,NV,STATUS)
        CALL IMG_SETPOLY(NV,XV,YV,EXCLUDE,STATUS)

        IF (MODE.EQ.'AND') THEN
          TEXT=' .AND.'
          L=7
        ELSE
          TEXT=' '
          L=1
        ENDIF

        IF (EXCLUDE) THEN
          TEXT(L:)=' .NOT. (POLYGON( '
        ELSE
          TEXT(L:)=' POLYGON( '
        ENDIF
        L=CHR_LEN(TEXT)
        CALL ARX_PUT(I_ARD_ID,0,TEXT(:L),STATUS)
        TEXT = ' '
        L = 1
        NPAIR=0


        DO I=1,NV

*  write each vertex allowing for line continuation
          NPAIR=NPAIR+1
          CALL MSG_SETR( 'X', XV(I))
          CALL MSG_SETR( 'Y', YV(I))
          CALL MSG_MAKE( TEXT(:L)//' ^X , ^Y ,', TEXT, L )
          IF (NPAIR.EQ.3.AND.I.LT.NV) THEN
            CALL ARX_PUT(I_ARD_ID,0,TEXT(:L),STATUS)
            TEXT = ' '
            L = 1
            NPAIR=0
          ENDIF


        ENDDO

        IF (EXCLUDE) THEN
          TEXT(L:)='))'
          L=L+1
        ELSE
          TEXT(L:L)=')'
        ENDIF

        CALL ARX_PUT(I_ARD_ID,0,TEXT(:L),STATUS)



        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from IREGION_POLYGON',STATUS)
        ENDIF

      ENDIF

      END




*+
      SUBROUTINE IREGION_BOX(MODE,EXCLUDE,STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Import :
      CHARACTER*(*) MODE
      LOGICAL EXCLUDE
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
      INTEGER CHR_LEN
*    Local constants :
*    Local variables :
      CHARACTER*80 TEXT
      REAL XC,YC,DX,DY
      INTEGER L
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL IMG_GETBOX('XC','YC','XWID','YWID',XC,YC,DX,DY,STATUS)
        CALL IMG_BOX(XC,YC,DX,DY,STATUS)
        CALL IMG_SETBOX(XC,YC,DX,DY,EXCLUDE,STATUS)

        IF (MODE.EQ.'AND') THEN
          TEXT=' .AND.'
          L=7
        ELSE
          TEXT=' '
          L=1
        ENDIF

        IF (EXCLUDE) THEN
          TEXT(L:)=' .NOT. (BOX( '
        ELSE
          TEXT(L:)=' BOX( '
        ENDIF
        L=CHR_LEN(TEXT)

        CALL MSG_SETR('XC',XC)
        CALL MSG_SETR('YC',YC)
        CALL MSG_SETR('XW',2.0*DX)
        CALL MSG_SETR('YW',2.0*DY)
        CALL MSG_MAKE(TEXT(:L)//' ^XC , ^YC , ^XW , ^YW ',TEXT,L)
        IF (EXCLUDE) THEN
          TEXT(L:)='))'
          L=L+1
        ELSE
          TEXT(L:L)=')'
        ENDIF

        CALL ARX_PUT(I_ARD_ID,0,TEXT(:L),STATUS)


        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from IREGION_BOX',STATUS)
        ENDIF

      ENDIF

      END

*+
      SUBROUTINE IREGION_WHOLE(STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Import :
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
*-

      IF (STATUS.EQ.SAI__OK) THEN


        CALL IMG_SETWHOLE(STATUS)

        CALL ARX_RESET(I_ARD_ID,STATUS)
        CALL ARX_PUT(I_ARD_ID,0,'WHOLE',STATUS)

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from IREGION_WHOLE',STATUS)
        ENDIF

      ENDIF

      END



*+
      SUBROUTINE IREGION_GTE(MODE,EXCLUDE,STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Import :
      CHARACTER*(*) MODE
      LOGICAL EXCLUDE
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      REAL LEV
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL USI_GET0R('LEV',LEV,STATUS)

        CALL IREGION_GTE_SUB(LEV,%val(I_DPTR),MODE,EXCLUDE,
     :                               %val(I_REG_PTR),STATUS)
        I_REG_TYPE='COMPLEX'

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from IREGION_GTE',STATUS)
        ENDIF

      ENDIF

      END



*+
      SUBROUTINE IREGION_GTE_SUB(LEV,D,MODE,EXCLUDE,REG,STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Import :
      REAL LEV
      REAL D(I_NX,I_NY)
      CHARACTER*(*) MODE
      LOGICAL EXCLUDE
*    Export :
      BYTE REG(I_NX,I_NY)
*    Status :
      INTEGER STATUS
*    Function declarations :
      INTEGER CHR_LEN
*    Local constants :
*    Local variables :
      CHARACTER*80 TEXT
      REAL X,Y
      REAL XP,YP
      INTEGER L
      INTEGER I,J
      INTEGER NPIX
      BYTE FLAG
*-

      IF (STATUS.EQ.SAI__OK) THEN

        IF (EXCLUDE) THEN
          FLAG='00'X
        ELSE
          FLAG='01'X
        ENDIF

        IF (MODE.EQ.'AND') THEN
          TEXT=' .AND.'
          L=7
        ELSE
          TEXT=' '
          L=1
        ENDIF

        IF (EXCLUDE) THEN
          TEXT(L:)=' .NOT. (PIXEL( '
        ELSE
          TEXT(L:)=' PIXEL( '
        ENDIF
        L=CHR_LEN(TEXT)
        CALL ARX_PUT(I_ARD_ID,0,TEXT(:L),STATUS)
        TEXT = ' '
        L = 1
        NPIX=0


        DO J=1,I_NY
          DO I=1,I_NX

            IF (D(I,J).GE.LEV) THEN

              REG(I,J)=FLAG

*  write each pixel allowing for line continuation
              NPIX=NPIX+1
              XP=REAL(I)
              YP=REAL(J)
              CALL IMG_PIXTOWORLD(XP,YP,X,Y,STATUS)
              CALL MSG_SETR( 'X', X)
              CALL MSG_SETR( 'Y', Y)
              CALL MSG_MAKE( TEXT(:L)//' ^X , ^Y ,', TEXT, L )
              IF (NPIX.EQ.3.AND.I.LT.I_NX.AND.J.LT.I_NY) THEN
                CALL ARX_PUT(I_ARD_ID,0,TEXT(:L),STATUS)
                TEXT = ' '
                L = 1
                NPIX=0
              ENDIF



            ENDIF

          ENDDO
        ENDDO

        IF (EXCLUDE) THEN
          TEXT(L:)='))'
          L=L+1
        ELSE
          TEXT(L:L)=')'
        ENDIF

        CALL ARX_PUT(I_ARD_ID,0,TEXT(:L),STATUS)


        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from IREGION_GTE_SUB',STATUS)
        ENDIF

      ENDIF

      END




*+
      SUBROUTINE IREGION_IMPORT(STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Import :
      LOGICAL EXCLUDE
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL ARX_RESET(I_ARD_ID,STATUS)
        CALL ARX_READ('TEXT',I_ARD_ID,STATUS)

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from IREGION_IMPORT',STATUS)
        ENDIF

      ENDIF

      END



*+
      SUBROUTINE IREGION_SHOW(STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'QUAL_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Import :
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL GFX_QCONTOUR(I_NX,I_NY,I_IX1,I_IX2,I_IY1,I_IY2,
     :                   %val(I_XPTR),%val(I_YPTR),%val(I_REG_PTR),
     :                                            QUAL__MASK,STATUS)

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from IREGION_SHOW',STATUS)
        ENDIF

      ENDIF

      END





*+
      SUBROUTINE IREGION_EXPORT(STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'QUAL_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Import :
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) LOC
      INTEGER DIMS(2)
      INTEGER PTR
      LOGICAL MASK,ARDFILE
*-

      IF (STATUS.EQ.SAI__OK) THEN

c        CALL USI_GET0L('MASK',MASK,STATUS)
c        IF (MASK) THEN
c          CALL USI_ASSOCO('OUT','REGION_MASK',LOC,STATUS)
c          DIMS(1)=I_NX
c          DIMS(2)=I_NY
c          CALL BDA_CRETDATA(LOC,'_BYTE',2,DIMS,STATUS)
c          CALL BDA_MAPTDATA(LOC,'_BYTE','W',PTR,STATUS)
c          CALL ARR_COP1B(I_NX*I_NY,%val(I_REG_PTR),%val(PTR),STATUS)
c          CALL BDA_COPAXES(I_LOC,LOC,STATUS)
c          CALL BDA_COPMORE(I_LOC,LOC,STATUS)
c          CALL BDA_RELEASE(LOC,STATUS)
c          CALL USI_ANNUL(LOC,STATUS)
c        ENDIF

c        CALL USI_GET0L('ARDFILE',ARDFILE,STATUS)
c        IF (ARDFILE) THEN
          CALL ARX_WRITE('FILE',I_ARD_ID,STATUS)
c        ENDIF


        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from IREGION_EXPORT',STATUS)
        ENDIF

      ENDIF

      END



*+
      SUBROUTINE IREGION_IMPORT_MASK(STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'QUAL_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Import :
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC) LOC
      INTEGER PTR
      LOGICAL PRIM
      LOGICAL MATCH
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL USI_ASSOCI('INP','READ',LOC,PRIM,STATUS)
        CALL IMG_MATCH(LOC,MATCH,STATUS)
        CALL BDA_MAPTDATA(LOC,'_BYTE','R',PTR,STATUS)
        CALL ARR_COP1B(I_NX*I_NY,%val(PTR),%val(I_REG_PTR),STATUS)
        I_REG_TYPE='COMPLEX'
        CALL BDA_RELEASE(LOC,STATUS)
        CALL USI_ANNUL(LOC,STATUS)

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from IREGION_IMPORT',STATUS)
        ENDIF

      ENDIF

      END



*+
      SUBROUTINE IREGION_LIST(STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Import :
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL MSG_BLNK()
        CALL ARX_LIST(I_ARD_ID,STATUS)
        CALL MSG_BLNK()

      ENDIF

      END




*+
      SUBROUTINE IREGION_PARSE_MODE(MODE,MERGE,STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Global variables :
*    Import/Export :
      CHARACTER*(*) MODE
*    Export :
      LOGICAL MERGE
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
*-
      IF (STATUS.EQ.SAI__OK) THEN

        MODE=MODE(:3)

        IF (MODE.EQ.'CIR'.OR.		! circle
     :      MODE.EQ.'BOX'.OR.		! box
     :      MODE.EQ.'POL'.OR.		! polygon
     :      MODE.EQ.'ANN'.OR.		! annulus
     :      MODE.EQ.'ELL'.OR.		! ellipse
     :      MODE.EQ.'SLI'.OR.		! rectangular slice
     :      MODE.EQ.'GTE'.OR.		! >= level
     :      MODE.EQ.'IMP') THEN		! ARD text

           MERGE=.TRUE.

        ELSEIF (MODE.EQ.'WHO'.OR.
     :          MODE.EQ.'SHO'.OR.
     :          MODE.EQ.'EXP'.OR.
     :          MODE.EQ.'INV'.OR.
     :          MODE.EQ.'LIS') THEN

           MERGE=.FALSE.

        ELSE
           CALL MSG_PRNT('AST_ERR: invalid mode')
           STATUS=SAI__ERROR
        ENDIF


      ENDIF

      END



*+
      SUBROUTINE IREGION_PARSE_SUBMODE(SUBMODE,EXCLUDE,STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Global variables :
*    Import/Export :
      CHARACTER*(*) SUBMODE
*    Export :
      LOGICAL EXCLUDE
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
*-
      IF (STATUS.EQ.SAI__OK) THEN

        EXCLUDE=(INDEX(SUBMODE,'NOT').GT.0.OR.
     :           INDEX(SUBMODE,'EXC').GT.0)

        IF (INDEX(SUBMODE,'ADD').GT.0.OR.
     :      INDEX(SUBMODE,'OR').GT.0) THEN
          SUBMODE='ADD'
        ELSEIF (INDEX(SUBMODE,'AND').GT.0) THEN
          SUBMODE='AND'
        ELSEIF (INDEX(SUBMODE,'NEW').GT.0) THEN
          SUBMODE='NEW'
        ELSEIF (EXCLUDE) THEN
          SUBMODE='NEW'

        ELSE
           CALL MSG_PRNT('AST_ERR: invalid sub-mode')
           STATUS=SAI__ERROR
        ENDIF


      ENDIF

      END



*+
      SUBROUTINE IREGION_HELP()
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
*    Global variables :
*    Import :
*    Export :
*    Status :
*    Function declarations :
*    Local constants :
      INTEGER NLINE
      PARAMETER (NLINE=6)
*    Local variables :
      CHARACTER*79 TEXT(NLINE)
     :/' CIRcle  - circular region     BOX     - box parallel to axes',
     : ' POLygon - irregular polygon   SLICE   - rectangular slice',
     : ' ANNulus - annular region      ELLipse - elliptical region',
     : ' WHOle   - whole image         GTE     - pixels >= level',
     : ' SHOw    - outline all regions LISt    - list ARD text',
     : ' IMPort  - input ARD           EXPort  - output ARD'/
      INTEGER ILINE
*-

      CALL MSG_BLNK()
      CALL MSG_PRNT('Valid modes are:')
      CALL MSG_BLNK()
      DO ILINE=1,NLINE
        CALL MSG_PRNT(TEXT(ILINE))
      ENDDO

      CALL MSG_BLNK()
      CALL MSG_PRNT('*** WARNING - ellipse doesn''t work yet!!')
      CALL MSG_BLNK()

      END




*+
      SUBROUTINE IREGION_SUBHELP()
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
*    Global variables :
*    Import :
*    Export :
*    Status :
*    Function declarations :
*    Local constants :
      INTEGER NLINE
      PARAMETER (NLINE=6)
*    Local variables :
      CHARACTER*79 TEXT(NLINE)
     :/' ',
     : ' ',
     : ' ',
     : ' ',
     : ' ',
     : ' '/
      INTEGER ILINE
*-

      CALL MSG_BLNK()
      CALL MSG_PRNT('Valid sub-modes are:')
      CALL MSG_BLNK()
      DO ILINE=1,NLINE
        CALL MSG_PRNT(TEXT(ILINE))
      ENDDO

      CALL MSG_BLNK()

      END
