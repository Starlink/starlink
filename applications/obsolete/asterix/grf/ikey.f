*+  IKEY - puts colour key at side of image
      SUBROUTINE IKEY(STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*      26 Jan 93 : V1.7-0 use GCB, GFX (RJV)
*      31 Mar 93 : V1.7-1 OFF option (RJV)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      CHARACTER*8 OPT
      LOGICAL OFF
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'IKEY Version 1.7-1')
*-
      CALL USI_INIT()

      CALL MSG_PRNT(VERSION)

      IF (.NOT.I_OPEN) THEN
        CALL MSG_PRNT('AST_ERR: image processing system not active')
      ELSEIF (.NOT.I_DISP) THEN
        CALL MSG_PRNT('AST_ERR: no image currently displayed')
      ELSE

*  ensure correct transformations and GCB
        CALL GTR_RESTORE(STATUS)
        CALL GCB_ATTACH('IMAGE',STATUS)
        CALL IMG_2DGCB(STATUS)

        CALL USI_GET0L('OFF',OFF,STATUS)
        IF (OFF) THEN
          CALL GCB_CANL('KEY_FLAG',STATUS)
          CALL GCB_CANC('KEY_OPT',STATUS)
        ELSE


          OPT='PC'
          I_PMIN=I_DMIN
          I_PMAX=I_DMAX
          CALL GCB_SETL('KEY_FLAG',.TRUE.,STATUS)
          CALL GCB_SETC('KEY_OPT',OPT,STATUS)
          CALL GFX_KEY(I_PMIN,I_PMAX,STATUS)


        ENDIF

      ENDIF

      CALL USI_CLOSE()

      END
