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
      PARAMETER (VERSION = 'IREGION Version 1.8-0')
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
            CALL IREGION_CIRCLE(EXCLUDE,STATUS)
          ELSEIF (MODE.EQ.'BOX') THEN
            CALL IREGION_BOX(EXCLUDE,STATUS)
          ELSEIF (MODE.EQ.'POL') THEN
            CALL IREGION_POLYGON(EXCLUDE,STATUS)
          ELSEIF (MODE.EQ.'ANN') THEN
            CALL IREGION_ANNULUS(EXCLUDE,STATUS)
          ELSEIF (MODE.EQ.'ELL') THEN
            CALL IREGION_ELLIPSE(EXCLUDE,STATUS)
          ELSEIF (MODE.EQ.'WHO') THEN
            CALL IREGION_WHOLE(STATUS)
          ELSEIF (MODE.EQ.'SLI') THEN
            CALL IREGION_SLICE(EXCLUDE,STATUS)
          ELSEIF (MODE.EQ.'GTE') THEN
            CALL IREGION_GTE(EXCLUDE,STATUS)
          ELSEIF (MODE.EQ.'ARD') THEN
            CALL IREGION_ARD(EXCLUDE,STATUS)
          ELSEIF (MODE.EQ.'SHO') THEN
            CALL IREGION_SHOW(STATUS)
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
      SUBROUTINE IREGION_CIRCLE(EXCLUDE,STATUS)
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
      REAL XC,YC,RAD
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL IMG_GETCIRC('XC','YC','RAD',XC,YC,RAD,STATUS)
        CALL IMG_SETCIRC(XC,YC,RAD,EXCLUDE,STATUS)


        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from IREGION_CIRCLE',STATUS)
        ENDIF

      ENDIF

      END




*+
      SUBROUTINE IREGION_ANNULUS(EXCLUDE,STATUS)
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
      REAL XC,YC,IRAD,ORAD
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL IMG_GETANNULUS('XC','YC','IRAD','ORAD',XC,YC,IRAD,ORAD,
     :                                                       STATUS)
        CALL IMG_SETANNULUS(XC,YC,IRAD,ORAD,EXCLUDE,STATUS)




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
      SUBROUTINE IREGION_SLICE(EXCLUDE,STATUS)
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
      REAL XC,YC,ANGLE,LENGTH,WIDTH
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL IMG_GETSLICE('XC','YC','ANGLE','LENGTH','WIDTH',
     :                           XC,YC,ANGLE,LENGTH,WIDTH,STATUS)
        CALL IMG_SETSLICE(XC,YC,ANGLE,LENGTH,WIDTH,EXCLUDE,STATUS)


        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from IREGION_SLICE',STATUS)
        ENDIF

      ENDIF

      END






*+
      SUBROUTINE IREGION_POLYGON(EXCLUDE,STATUS)
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
      INTEGER NVMAX
      PARAMETER (NVMAX=100)
*    Local variables :
      REAL XV(NVMAX),YV(NVMAX)
      INTEGER NV
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL IMG_GETPOLY(NVMAX,XV,YV,NV,STATUS)
        CALL IMG_SETPOLY(NV,XV,YV,EXCLUDE,STATUS)


        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from IREGION_POLYGON',STATUS)
        ENDIF

      ENDIF

      END




*+
      SUBROUTINE IREGION_BOX(EXCLUDE,STATUS)
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
      REAL XC,YC,DX,DY
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL IMG_GETBOX('XC','YC','XWID','YWID',XC,YC,DX,DY,STATUS)
        CALL IMG_BOX(XC,YC,DX,DY,STATUS)
        CALL IMG_SETBOX(XC,YC,DX,DY,EXCLUDE,STATUS)


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


        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from IREGION_WHOLE',STATUS)
        ENDIF

      ENDIF

      END



*+
      SUBROUTINE IREGION_GTE(EXCLUDE,STATUS)
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
      REAL LEV
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL USI_GET0R('LEV',LEV,STATUS)

        CALL IREGION_GTE_SUB(LEV,%val(I_DPTR),EXCLUDE,
     :                            %val(I_REG_PTR),STATUS)
        I_REG_TYPE='COMPLEX'

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from IREGION_GTE',STATUS)
        ENDIF

      ENDIF

      END



*+
      SUBROUTINE IREGION_GTE_SUB(LEV,D,EXCLUDE,REG,STATUS)
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
      LOGICAL EXCLUDE
*    Export :
      BYTE REG(I_NX,I_NY)
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      INTEGER I,J
      BYTE FLAG
*-

      IF (STATUS.EQ.SAI__OK) THEN

        IF (EXCLUDE) THEN
          FLAG='00'X
        ELSE
          FLAG='01'X
        ENDIF

        DO J=1,I_NY
          DO I=1,I_NX

            IF (D(I,J).GE.LEV) THEN
              REG(I,J)=FLAG
            ENDIF

          ENDDO
        ENDDO

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from IREGION_GTE_SUB',STATUS)
        ENDIF

      ENDIF

      END




*+
      SUBROUTINE IREGION_ARD(EXCLUDE,STATUS)
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
      INTEGER MPTR
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL DYN_MAPI(1,I_NX*I_NY,MPTR,STATUS)
        CALL ARR_INIT1I(0.0,I_NX*I_NY,%val(MPTR),STATUS)
        CALL IMG_GETARD('TEXT',%val(MPTR),STATUS)
        CALL IMG_SETARD(%val(MPTR),EXCLUDE,STATUS)
        CALL DYN_UNMAP(MPTR,STATUS)

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from IREGION_ARD',STATUS)
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
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL USI_ASSOCO('OUT','REGION_MASK',LOC,STATUS)
        DIMS(1)=I_NX
        DIMS(2)=I_NY
        CALL BDA_CRETDATA(LOC,'_BYTE',2,DIMS,STATUS)
        CALL BDA_MAPTDATA(LOC,'_BYTE','W',PTR,STATUS)
        CALL ARR_COP1B(I_NX*I_NY,%val(I_REG_PTR),%val(PTR),STATUS)
        CALL BDA_COPAXES(I_LOC,LOC,STATUS)
        CALL BDA_COPMORE(I_LOC,LOC,STATUS)
        CALL BDA_RELEASE(LOC,STATUS)
        CALL USI_ANNUL(LOC,STATUS)

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from IREGION_EXPORT',STATUS)
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
     :      MODE.EQ.'ARD') THEN		! ARD text

           MERGE=.TRUE.

        ELSEIF (MODE.EQ.'WHO'.OR.
     :          MODE.EQ.'SHO'.OR.
     :          MODE.EQ.'EXP'.OR.
     :          MODE.EQ.'INV'.OR.
     :          MODE.EQ.'IMP') THEN

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
     : ' ARD     - ARD input           WHOle   - whole image',
     : ' GTE     - pixels >= level     SHOw    - outline all regions',
     : ' IMPort  - read region mask    EXPort  - output mask'/
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
