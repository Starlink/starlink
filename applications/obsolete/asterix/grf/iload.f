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
*      6 Jan 95 : V1.8-0 ARD for regions (RJV)
*     27 Apr 95 : V1.8-1 SPLIT option added (RJV)
*     21 Sep 95 : V2.0-0 ADI port (DJA)
*      1 Apr 96 : V2.0-1 Checks cached image for compatibility (RJV)
*      5 Mar 97 : V2.0-2 Avoid PSF problem with null ID (RJV)
*     13 Mar 97 : V2.0-3 Send success flag to GUI (RJV)
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
      CHARACTER*132		IFILE			! Input file name
      CHARACTER*20 DEV
      INTEGER NX,NY
      INTEGER MX,MY
      INTEGER IFID
      INTEGER MODE
      LOGICAL DISP
      LOGICAL ACTIVE
      LOGICAL NEW
      LOGICAL GCB
      LOGICAL FRESH
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'ILOAD Version 2.2-0')
*-
*  first invocation do global initialisation
      IF (.NOT.I_OPEN) THEN
        CALL AST_INIT()
        I_BGM_ON = .FALSE.
      ELSE
        CALL USI_INIT()
      ENDIF

      CALL MSG_PRNT(VERSION)

*  get input image
      CALL USI_GET0C( 'INP', IFILE, STATUS )
      CALL ADI_FOPEN( IFILE, 'BinDS', 'READ', IFID, STATUS )

*  see if GCB wanted
      CALL USI_GET0L('GCB',GCB,STATUS)

      IF (STATUS.EQ.SAI__OK) THEN

*  close previous image if loaded
        IF (I_OPEN) THEN
          NEW=.FALSE.
          IF (I_PSF.NE.0) THEN
            CALL PSF_RELEASE(I_PSF,STATUS)
            I_PSF=0
          ENDIF

          CALL ADI_FCLOSE( I_FID, STATUS )

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

          NEW=.TRUE.

        ENDIF

*  if image in cache note size
        IF (I_MEM) THEN
          MX=I_NX
          MY=I_NY
        ELSE
          I_DPTR_M=0
          I_XPTR_M=0
          I_YPTR_M=0
          I_VPTR_M=0
          I_QPTR_M=0
        ENDIF

        CALL MSG_PRNT(' ')
        CALL MSG_PRNT('Checking image...')
        CALL IMG_CHECK(IFID,STATUS)

        IF (I_CUBE) THEN
          CALL MSG_PRNT('Loading image from cube....')
          CALL IMG_LOADCUBE(IFID,'SLICE',STATUS)
          CALL IMG_SETWHOLE(STATUS)
          CALL IMG_SETPOS(0.0,0.0,STATUS)
          CALL IMG_MINMAX(STATUS)
        ELSE
          CALL MSG_PRNT('Loading image....')
          CALL IMG_LOAD(IFID,STATUS)
          CALL IMG_SETWHOLE(STATUS)
          CALL IMG_SETPOS(0.0,0.0,STATUS)
          CALL IMG_MINMAX(STATUS)
        ENDIF

*  check compatibility with image incache
        IF (I_MEM.AND.STATUS.EQ.SAI__OK) THEN
          IF (MX.NE.I_NX.OR.MY.NE.I_NY) THEN
            CALL MSG_PRNT(
     :        'AST_ERR: image is different size to cached image - '/
     :        /'deleting cacheds image')
            CALL IMG_DELCACHE(STATUS)
            I_MEM=.FALSE.
          ENDIF
        ENDIF

*  get graphics device
        CALL GDV_STATUS(ACTIVE,STATUS)
        IF (.NOT.ACTIVE.AND.STATUS.EQ.SAI__OK) THEN
          CALL USI_GET0C('DEV',DEV,STATUS)
          CALL USI_GET0I('NX',NX,STATUS)
          CALL USI_GET0I('NY',NY,STATUS)
          CALL MSG_PRNT('Opening device...')
          CALL GDV_OPEN(DEV,NX,NY,STATUS)
        ENDIF

*  is each zone to be split into 2D and 1D
        CALL USI_GET0L('SPLIT',I_SPLIT_DISP,STATUS)

        CALL USI_GET0I('MODE',MODE,STATUS)
        IF (STATUS.EQ.PAR__NULL) THEN
          CALL ERR_ANNUL(STATUS)
        ELSE
          I_MODE=MODE
        ENDIF
        CALL USI_GET0L('DISP',DISP,STATUS)

        CALL GCB_ATTACH('IMAGE',STATUS)

*  if new session create caches and group for ARD text
        IF (NEW) THEN
          CALL GCB_CRECACHE(I_CACHE,STATUS)
          I_CACHE_1D=0
          CALL ARX_OPEN('WRITE',I_ARD_ID,STATUS)
        ELSE
          CALL IMG_2DGCB(STATUS)
        ENDIF

        IF (GCB) THEN
          CALL GCB_FLOAD(IFID,STATUS)
        ELSE
          CALL GCB_CLEAR(STATUS)
          CALL GCB_SETL('PIX_FLAG',.TRUE.,STATUS)
        ENDIF

        CALL GCB_CACHE(I_CACHE,STATUS)

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
          IF (I_GUI) THEN
            CALL IMG_NBPUT0I('BUFFER',0,STATUS)
          ENDIF
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
          CALL ADI_FCLOSE( IFID, STATUS )
          CALL AST_CLOSE()
          CALL GCB_DETACH(STATUS)
        ENDIF

      ENDIF

*  send success flag to GUI - flag will be set non-zero by GUI
      IF (I_GUI) THEN
        CALL IMG_NBPUT0I('FLAG',STATUS,STATUS)
      ENDIF


      CALL USI_CLOSE()

      END
