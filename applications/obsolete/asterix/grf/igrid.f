*+  IGRID - puts grid over image in specified coords
      SUBROUTINE IGRID(STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    History :
*      19 Aug 92: V1.2-1 label position option (RJV)
*      19 Jan 93: V1.7-0 GCB and GFX routines used (RJV)
*      31 Mar 93: V1.7-1 OFF option added (RJV)
*       1 Jul 93: V1.7-2 GTR used (RJV)
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
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'IGRID Version 1.7-2')
*    Local constants :
*    Local variables :
        INTEGER NFRAME                 	! Reference frame code
        INTEGER POS			! Position of label
        LOGICAL OFF
*
*-
      CALL MSG_PRNT(VERSION)

      IF (.NOT.I_OPEN) THEN
        CALL MSG_PRNT('AST_ERR: image processing system not active')
      ELSEIF (.NOT.I_DISP) THEN
        CALL MSG_PRNT('AST_ERR: no image currently displayed')
      ELSE

        CALL GCB_ATTACH('IMAGE',STATUS)
        CALL IMG_2DGCB(STATUS)

        CALL PAR_GET0L('OFF',OFF,STATUS)
        IF (OFF) THEN
          CALL GCB_CANI('GRID_FRAME',STATUS)
        ELSE

* get frame
          CALL PAR_GET0I('FRAME',NFRAME,STATUS)
          CALL GCB_SETI('GRID_FRAME',NFRAME,STATUS)

* get label position
          CALL PAR_GET0I('POS',POS,STATUS)
          CALL GCB_SETI('GRID_POS',POS,STATUS)


*  ensure transformations correct
          CALL GTR_RESTORE(STATUS)

          CALL IMG_GRID(STATUS)

        ENDIF

      ENDIF

      END
