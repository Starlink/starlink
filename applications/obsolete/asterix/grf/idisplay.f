*+  IDISPLAY - display current image
      SUBROUTINE IDISPLAY(STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*      19 Jan 93: V1.7-0
*       1 Jul 94: V1.7-1 screen clearing more intelligent (RJV)
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
      LOGICAL FRESH
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'IDISPLAY Version 1.7-1')
*-
      CALL MSG_PRNT(VERSION)


      IF (.NOT.I_OPEN) THEN
        CALL MSG_PRNT('AST_ERR: image processing system not active')
      ELSE

*  advance to next window unless freshly opened device
        CALL GDV_FRESH(FRESH,STATUS)
        IF (.NOT.FRESH) THEN
          CALL GDV_CLEAR(STATUS)
        ENDIF

*  reset transformation database
        CALL GTR_ZERO(STATUS)

        CALL GCB_ATTACH('IMAGE',STATUS)
        CALL IMG_2DGCB(STATUS)

*  display image
        CALL IMG_WINDOW(STATUS)
        CALL IMG_DISP(STATUS)
        CALL IMG_AXES(STATUS)

*  flag current plotting status
        CALL GCB_SETL('SURF_FLAG',.FALSE.,STATUS)
        I_DISP=.TRUE.
        I_DISP_1D=.FALSE.
        I_CLEAR=.FALSE.

      ENDIF

      END
