
*+  GFX_QLOG - inquire if axes log or lin
      SUBROUTINE GFX_QLOG(XLOG,YLOG,STATUS)

*    Description :
*    Parameters :
*    Method :
*
*    Deficiencies :
*    Bugs :
*    Authors :
*             (BHVAD::RJV)
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
*    Import-export :
      LOGICAL XLOG,YLOG
*    Export :
*    Status :
      INTEGER STATUS
*    Local Constants :
*    Local variables :
      LOGICAL OK
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL GCB_GETL('XAXIS_LOG',OK,XLOG,STATUS)
        IF (.NOT.OK) THEN
          XLOG=.FALSE.
        ENDIF
        CALL GCB_GETL('YAXIS_LOG',OK,YLOG,STATUS)
        IF (.NOT.OK) THEN
          YLOG=.FALSE.
        ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GFX_QLOG',STATUS)
        ENDIF
      ENDIF
      END
