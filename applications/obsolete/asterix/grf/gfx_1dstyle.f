*+  GFX_1DSTYLE - gets style of 1D plot
      SUBROUTINE GFX_1DSTYLE(POLY,STEP,ERRS,POINTS,STATUS)
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
      LOGICAL POLY,STEP,ERRS,POINTS
*    Status :
      INTEGER STATUS
*    Functions :
*    Local Constants :
*    Local variables :
      LOGICAL OK
*-
      IF (STATUS.EQ.SAI__OK) THEN

        CALL GCB_GETL('POLY_FLAG',OK,POLY,STATUS)
        IF (.NOT.OK) THEN
          POLY=.FALSE.
        ENDIF
        CALL GCB_GETL('STEP_FLAG',OK,STEP,STATUS)
        IF (.NOT.OK) THEN
          STEP=.FALSE.
        ENDIF
        CALL GCB_GETL('ERR_FLAG',OK,ERRS,STATUS)
        IF (.NOT.OK) THEN
          ERRS=.FALSE.
        ENDIF
        CALL GCB_GETL('POINT_FLAG',OK,POINTS,STATUS)
        IF (.NOT.OK) THEN
          POINTS=.FALSE.
        ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GFX_1DSTYLE',STATUS)
        ENDIF
      ENDIF
      END
