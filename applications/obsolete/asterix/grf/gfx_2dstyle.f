*+  GFX_2DSTYLE - gets style of 2D plot
      SUBROUTINE GFX_2DSTYLE(PIXEL,CONTOUR,SURFACE,STATUS)
*    Description :
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*         (BHVAD::DJA)
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
*    Import-Export :
*    Export :
      LOGICAL PIXEL,CONTOUR,SURFACE
*    Status :
      INTEGER STATUS
*    Functions :
*    Local Constants :
*    Local variables :
      LOGICAL OK
*-
      IF (STATUS.EQ.SAI__OK) THEN

        CALL GCB_GETL('PIX_FLAG',OK,PIXEL,STATUS)
        IF (.NOT.OK) THEN
          PIXEL=.FALSE.
        ENDIF
        CALL GCB_GETL('CONT_FLAG',OK,CONTOUR,STATUS)
        IF (.NOT.OK) THEN
          CONTOUR=.FALSE.
        ENDIF
        CALL GCB_GETL('SURF_FLAG',OK,SURFACE,STATUS)
        IF (.NOT.OK) THEN
          SURFACE=.FALSE.
        ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GFX_2DSTYLE',STATUS)
        ENDIF
      ENDIF
      END
