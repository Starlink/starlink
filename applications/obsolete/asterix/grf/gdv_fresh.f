*+  GDV_FRESH - check if device just opened
      SUBROUTINE GDV_FRESH(FRESH,STATUS)
*    Description :
*    Authors :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*    Global variables :
      INCLUDE 'GDV_CMN'
*    Structure definitions :
*    Import :
*    Import-Export :
*    Export :
      LOGICAL FRESH
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      CHARACTER*40 DEV
      LOGICAL OK
*-
      IF (STATUS.EQ.SAI__OK) THEN

        CALL GDV_STATUS(OK,STATUS)
        IF (OK) THEN
          DEV=G_DEVICE
          FRESH=((DEV(40:40)).EQ.'F')
          G_DEVICE(40:40)=' '
        ELSE
          CALL MSG_PRNT('AST_ERR: no device active')
          STATUS=SAI__ERROR
        ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GDV_FRESH',STATUS)
        ENDIF

      ENDIF

      END
