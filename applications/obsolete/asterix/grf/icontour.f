*+  ICONTOUR - contour current image
      SUBROUTINE ICONTOUR(STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*      20 May 91 :v1.2-2 plots without pixel plot (RJV)
*      21 May 91 :v1.2-3 saves contour levels (RJV)
*       3 Sep 92 :v1.7-0 uses GDV and GCB (RJV)
*      19 Jan 93 :v1.7-1 uses GFX plotting routines (RJV)
*      31 Mar 93 :v1.7-2 OFF option (RJV)
*       1 Jul 93 :v1.7-3 GTR used (RJV)
*       8 Sep 93 :v1.7-4 RJV
*       1 Jul 94 :v1.7-5 more intelligent screen clearing (RJV)
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
      CHARACTER*20 CONTEXT
      REAL LEVELS(16)
      INTEGER NLEV,ILEV
      LOGICAL ACTIVE
      LOGICAL OFF
      LOGICAL FRESH
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'ICONTOUR Version 1.7-5')
*-
      CALL USI_INIT()

      CALL MSG_PRNT(VERSION)

      IF (.NOT.I_OPEN) THEN
        CALL MSG_PRNT('AST_ERR: no image loaded')
        STATUS=SAI__ERROR
      ELSE
        CALL GDV_STATUS(ACTIVE,STATUS)
        IF (.NOT.ACTIVE) THEN
          CALL MSG_PRNT('AST_ERR: no graphics device open')
          STATUS=SAI__ERROR
        ENDIF
      ENDIF
      IF (STATUS.EQ.SAI__OK) THEN

        CALL GCB_GETCONTXT(CONTEXT,STATUS)
        CALL GCB_ATTACH('IMAGE',STATUS)
        CALL IMG_2DGCB(STATUS)

        CALL USI_GET0L('OFF',OFF,STATUS)
        IF (OFF) THEN
          CALL GCB_CANL('CONT_FLAG',STATUS)
        ELSE

          CALL USI_GET1R('LEVELS',16,LEVELS,NLEV,STATUS)
          IF (STATUS.EQ.PAR__NULL) THEN
            CALL ERR_ANNUL(STATUS)
          ELSE
            CALL GCB_SETI('CONT_N',NLEV,STATUS)
            CALL GCB_SET1R('CONT_LEVS',1,NLEV,LEVELS,STATUS)
          ENDIF

          IF (STATUS.EQ.SAI__OK) THEN
*  clear display and plot axes if necessary
            CALL GDV_FRESH(FRESH,STATUS)

*  device not being used by image proc
            IF (CONTEXT(:5).NE.'IMAGE') THEN
              IF (.NOT.FRESH) THEN
                CALL GDV_CLEAR(STATUS)
              ENDIF
              CALL IMG_WINDOW(STATUS)
              CALL IMG_AXES(STATUS)
*  image proc using device but for 1D
            ELSEIF (CONTEXT(:5).EQ.'IMAGE'.AND.I_DISP_1D) THEN
              IF (.NOT.FRESH) THEN
                CALL GDV_CLEAR(STATUS)
              ENDIF
              CALL IMG_WINDOW(STATUS)
              CALL IMG_AXES(STATUS)
*  image being displayed so confirm transformations
            ELSEIF (CONTEXT(:5).EQ.'IMAGE'.AND.I_DISP) THEN
*  new device has been opened since image last displayed
              IF (FRESH) THEN
                CALL IMG_WINDOW(STATUS)
                CALL IMG_AXES(STATUS)
*  otherwise just reconfirm transformations
              ELSE
                CALL GTR_RESTORE(STATUS)
              ENDIF
*  image proc active but nothing displayed
            ELSE
              CALL IMG_WINDOW(STATUS)
              CALL IMG_AXES(STATUS)
            ENDIF


*  plot contours
            CALL IMG_CONTOUR(STATUS)

            CALL GCB_SETL('CONT_FLAG',.TRUE.,STATUS)
            CALL GCB_CANL('SURF_FLAG',STATUS)
            I_CLEAR=.FALSE.
            I_DISP=.TRUE.
          ENDIF

        ENDIF

      ENDIF

      CALL USI_CLOSE()

      END


