*+  ILOAD - load image into image processing system
      SUBROUTINE ILOAD(STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*      3 Sep 92 : V1.7-0 amalgamation of ISTART and INEW (RJV)
*     19 Mar 93 : V1.7-1 option to load GCB (RJV)
*     17 jun 93 : V1.7-2 GTR used (RJV)
*     25 Nov 93 : V1.7-3 BDA_ASSOC used so FITS can be read (RJV)
*      1 Jul 94 : V1.7-4 screen clearing more intelligent (RJV)
*     20 Sep 94 : V1.7-5 region mask incorporated (RJV)
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
      CHARACTER*(DAT__SZLOC) ILOC
      CHARACTER*80 IMG
      CHARACTER*20 DEV
      INTEGER NX,NY
      INTEGER MODE
      LOGICAL DISP
      LOGICAL ACTIVE
      LOGICAL NEW
      LOGICAL GCB
      LOGICAL FRESH
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'ILOAD Version 1.7-5')
*-
      CALL MSG_PRNT(VERSION)

*  get input image
      CALL BDA_ASSOCI('INP','R',ILOC,STATUS)

*  see if GCB wanted
      CALL PAR_GET0L('GCB',GCB,STATUS)

      IF (STATUS.EQ.SAI__OK) THEN

*  close previous image if loaded
        IF (I_OPEN) THEN
          NEW=.FALSE.
          CALL PSF_RELEASE(I_PSF,STATUS)
          I_PSF=0

          CALL BDA_ANNUL(I_LOC,STATUS)

          CALL DYN_UNMAP(I_DPTR,STATUS)
          CALL DYN_UNMAP(I_DPTR_W,STATUS)
          CALL DYN_UNMAP(I_XPTR,STATUS)
          CALL DYN_UNMAP(I_YPTR,STATUS)
          CALL DYN_UNMAP(I_XPTR_W,STATUS)
          CALL DYN_UNMAP(I_YPTR_W,STATUS)
          CALL DYN_UNMAP(I_VPTR,STATUS)
          CALL DYN_UNMAP(I_VPTR_W,STATUS)
          CALL DYN_UNMAP(I_QPTR,STATUS)
          CALL DYN_UNMAP(I_QPTR_W,STATUS)
          CALL DYN_UNMAP(I_WKPTR,STATUS)
          CALL DYN_UNMAP(I_REG_PTR,STATUS)

          I_OPEN=.FALSE.
        ELSE

          CALL AST_INIT()
          NEW=.TRUE.

        ENDIF

        CALL MSG_PRNT(' ')
        CALL MSG_PRNT('Checking image...')
        CALL IMG_CHECK(ILOC,STATUS)

        IF (I_CUBE) THEN
          CALL MSG_PRNT('Loading image from cube....')
          CALL IMG_LOADCUBE(ILOC,'SLICE',STATUS)
        ELSE
          CALL MSG_PRNT('Loading image....')
          CALL IMG_LOAD(ILOC,STATUS)
          CALL IMG_SETWHOLE(STATUS)
          CALL IMG_SETPOS(0.0,0.0,STATUS)
        ENDIF


*  get graphics device
        CALL GDV_STATUS(ACTIVE,STATUS)
        IF (.NOT.ACTIVE.AND.STATUS.EQ.SAI__OK) THEN
          CALL PAR_GET0C('DEV',DEV,STATUS)
          CALL PAR_GET0I('NX',NX,STATUS)
          CALL PAR_GET0I('NY',NY,STATUS)
          CALL MSG_PRNT('Opening device...')
          CALL GDV_OPEN(DEV,NX,NY,STATUS)
        ENDIF


        CALL PAR_GET0I('MODE',MODE,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
        ELSE
          I_MODE=MODE
        ENDIF
        CALL PAR_GET0L('DISP',DISP,STATUS)

        CALL GCB_ATTACH('IMAGE',STATUS)

*  if new session create caches
        IF (NEW) THEN
          CALL GCB_CRECACHE(I_CACHE,STATUS)
          I_CACHE_1D=0
        ELSE
          CALL IMG_2DGCB(STATUS)
        ENDIF

        IF (GCB) THEN
          CALL GCB_LOAD(ILOC,STATUS)
        ELSE
          CALL GCB_CLEAR(STATUS)
          CALL GCB_SETL('PIX_FLAG',.TRUE.,STATUS)
        ENDIF



*  zero transformations
        CALL GTR_ZERO(STATUS)

*  set default attributes and colours
        CALL GCB_SETDEF(STATUS)
        CALL GFX_SETCOLS(STATUS)

        IF (STATUS.EQ.SAI__OK) THEN

*  set status flags
          I_OPEN=.TRUE.
          I_DISP=.FALSE.
          I_DISP_1D=.FALSE.
          I_CLEAR=.TRUE.
          I_PROC_COUNT=0
          I_CAN_UNDO=.FALSE.
          I_DPTR_1D=0
          I_VPTR_1D=0
          I_QPTR_1D=0
          I_PSF=0
*  display if required
          IF (DISP) THEN

*  display image
            CALL MSG_PRNT('Displaying image...')
            CALL GDV_FRESH(FRESH,STATUS)
            IF (.NOT.FRESH) THEN
              CALL GDV_CLEAR(STATUS)
            ENDIF
            CALL IMG_WINDOW(STATUS)
            CALL IMG_DISP(STATUS)
            CALL IMG_AXES(STATUS)


*  flag current plotting status
            I_DISP=.TRUE.
            I_CLEAR=.FALSE.

          ENDIF

        ELSE
          CALL MSG_PRNT('AST_ERR: no image loaded')
          I_OPEN=.FALSE.
          CALL AST_CLOSE()
          CALL BDA_ANNUL(ILOC,STATUS)
          CALL GCB_DETACH(STATUS)
        ENDIF

      ENDIF

      END
