*+  GDV_DEVICE - get current device name
      SUBROUTINE GDV_DEVICE(NAME,STATUS)
*    Description :
*    Authors :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Global variables :
      INCLUDE 'GDV_CMN'
*    Structure definitions :
*    Import :
*    Import-Export :
*    Export :
      CHARACTER*(*) NAME
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      LOGICAL OK
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL GDV_STATUS(OK,STATUS)
        IF (OK) THEN
          NAME=G_DEVICE(:39)
        ELSE
          CALL MSG_PRNT('AST_ERR: no graphics device active')
          STATUS=SAI__ERROR
        ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GDV_DEVICE',STATUS)
        ENDIF

      ENDIF

      END
