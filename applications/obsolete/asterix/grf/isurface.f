*+  ISURFACE - draw image as a 3-D surface
      SUBROUTINE ISURFACE(STATUS)
*    Description :
*    Authors :
*    History :
*     19 Jan 93 : v1.7-0 use GFX plotting routines (RJV)
*      1 Jul 94 : v1.7-1 more intelligent screen clearing (RJV)
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
      REAL ANGLE                               ! Display angle
      LOGICAL FRESH
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'ISURFACE Version 1.7-1')
*-
      CALL USI_INIT()

      CALL MSG_PRNT(VERSION)

      IF (.NOT.I_OPEN) THEN
        CALL MSG_PRNT('AST_ERR: image processing system not active')
      ELSE

*  clear screen unless device has been freshly opened
        CALL GDV_FRESH(FRESH,STATUS)
        IF (.NOT.FRESH) THEN
          CALL GDV_CLEAR(STATUS)
        ENDIF

*  get display angle

        CALL USI_GET0R('ANGLE', ANGLE, STATUS)

        CALL GCB_ATTACH('IMAGE',STATUS)
        CALL IMG_2DGCB(STATUS)

        CALL GCB_SETL('SURF_FLAG',.TRUE.,STATUS)
        CALL GCB_SETR('SURF_ANGLE',ANGLE,STATUS)

        IF (STATUS.EQ.SAI__OK) THEN

*  draw surface
          CALL GFX_SURFACE(I_NX,I_NY,I_IX1,I_IX2,I_IY1,I_IY2,
     :                                     %VAL(I_DPTR),STATUS)


          I_DISP=.FALSE.
          I_DISP_1D=.FALSE.
          I_CLEAR=.FALSE.

        ENDIF

      ENDIF

      CALL USI_CLOSE()

      END
