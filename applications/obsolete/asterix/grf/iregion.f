*+  IREGION - select a region of the plot
      SUBROUTINE IREGION(STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*    3 Oct 94 : v1.7-0 original (RJV)
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
      CHARACTER*10 MODE
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'IREGION Version 1.7-0')
*-
      CALL MSG_PRNT(VERSION)

      IF (.NOT.I_OPEN) THEN
        CALL MSG_PRNT('AST_ERR: image processing system not active')
      ELSEIF (.NOT.I_DISP) THEN
        CALL MSG_PRNT('AST_ERR: no image currently displayed')
      ELSE

*  make sure transformations are correct
        CALL GTR_RESTORE(STATUS)

        MODE=' '
        DO WHILE (MODE.EQ.' '.AND.STATUS.EQ.SAI__OK)
          CALL PAR_GET0C('MODE',MODE,STATUS)
          CALL CHR_UCASE(MODE)
          IF (MODE.EQ.'HELP') THEN
            CALL IREGION_HELP()
            CALL PAR_CANCL('MODE',STATUS)
            MODE=' '
          ENDIF
        ENDDO

        IF (STATUS.EQ.SAI__OK) THEN

          MODE=MODE(:3)
          IF (MODE.EQ.'CIR') THEN
            CALL IREGION_CIRCLE(STATUS)
          ELSEIF (MODE.EQ.'BOX') THEN
            CALL IREGION_BOX(STATUS)
          ELSEIF (MODE.EQ.'POL') THEN
            CALL IREGION_POLYGON(STATUS)
          ELSEIF (MODE.EQ.'ANN') THEN
            CALL IREGION_ANNULUS(STATUS)
          ELSEIF (MODE.EQ.'WHO') THEN
            CALL IREGION_WHOLE(STATUS)
          ELSEIF (MODE.EQ.'GTE') THEN
            CALL IREGION_GTE(STATUS)
          ELSEIF (MODE.EQ.'ARD') THEN
            CALL IREGION_ARD(STATUS)
          ELSEIF (MODE.EQ.'SHO') THEN
            CALL IREGION_SHOW(STATUS)
          ELSE
            CALL MSG_PRNT('AST_ERR: unknown mode '//MODE)
          ENDIF


        ENDIF

      ENDIF

      END



*+
      SUBROUTINE IREGION_CIRCLE(STATUS)
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
      REAL XC,YC,RAD
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL IMG_GETCIRC('XC','YC','RAD',XC,YC,RAD,STATUS)
        CALL IMG_SETCIRC(XC,YC,RAD,STATUS)
        CALL IMG_CIRCLE(XC,YC,RAD,STATUS)


        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from IREGION_CIRCLE',STATUS)
        ENDIF

      ENDIF

      END




*+
      SUBROUTINE IREGION_ANNULUS(STATUS)
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
      REAL XC,YC,IRAD,ORAD
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL IMG_GETANNULUS('XC','YC','IRAD','ORAD',XC,YC,IRAD,ORAD,
     :                                                       STATUS)
        CALL IMG_SETANNULUS(XC,YC,IRAD,ORAD,STATUS)
        CALL IMG_CIRCLE(XC,YC,IRAD,STATUS)
        CALL IMG_CIRCLE(XC,YC,ORAD,STATUS)




        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from IREGION_ANNULUS',STATUS)
        ENDIF

      ENDIF

      END




*+
      SUBROUTINE IREGION_POLYGON(STATUS)
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
      INTEGER NVMAX
      PARAMETER (NVMAX=100)
*    Local variables :
      REAL XV(NVMAX),YV(NVMAX)
      INTEGER NV
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL IMG_GETPOLY(NVMAX,XV,YV,NV,STATUS)
        CALL IMG_SETPOLY(NV,XV,YV,STATUS)


        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from IREGION_POLYGON',STATUS)
        ENDIF

      ENDIF

      END




*+
      SUBROUTINE IREGION_BOX(STATUS)
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
      REAL XC,YC,DX,DY
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL IMG_GETBOX('XC','YC','XWID','YWID',XC,YC,DX,DY,STATUS)
        CALL IMG_SETBOX(XC,YC,DX,DY,STATUS)
        CALL IMG_BOX(XC,YC,DX,DY,STATUS)


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
      SUBROUTINE IREGION_GTE(STATUS)
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
      REAL LEV
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL PAR_GET0R('LEV',LEV,STATUS)

        CALL IREGION_GTE_SUB(LEV,%val(I_DPTR),%val(I_REG_PTR),STATUS)
        I_REG_TYPE='COMPLEX'

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from IREGION_GTE',STATUS)
        ENDIF

      ENDIF

      END



*+
      SUBROUTINE IREGION_GTE_SUB(LEV,D,REG,STATUS)
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
*    Export :
      BYTE REG(I_NX,I_NY)
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      INTEGER I,J
*-

      IF (STATUS.EQ.SAI__OK) THEN

        DO J=1,I_NY
          DO I=1,I_NX

            IF (D(I,J).GE.LEV) THEN
              REG(I,J)='01'X
            ENDIF

          ENDDO
        ENDDO

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from IREGION_GTE_SUB',STATUS)
        ENDIF

      ENDIF

      END




*+
      SUBROUTINE IREGION_ARD(STATUS)
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
      INTEGER MPTR
      INTEGER I1,I2,J1,J2
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL DYN_MAPI(1,I_NX*I_NY,MPTR,STATUS)
        CALL ARR_INIT1I(0.0,I_NX*I_NY,%val(MPTR),STATUS)
        CALL IMG_GETARD('TEXT',%val(MPTR),I1,I2,J1,J2,STATUS)
        CALL IMG_SETARD(%val(MPTR),I1,I2,J1,J2,STATUS)
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
     :                   %val(I_XPTR),%val(I-YPTR),%val(I_REGPTR),
     :                                           QUAL_MASK,STATUS)

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from IREGION_SHOW',STATUS)
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
*    Local variables :
*-


      END
