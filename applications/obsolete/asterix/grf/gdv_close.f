*+  GDV_CLOSE - close device
      SUBROUTINE GDV_CLOSE(STATUS)
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
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      LOGICAL ACTIVE
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL GDV_STATUS(ACTIVE,STATUS)
        IF (ACTIVE) THEN
          CALL PGEND()
          G_DEVICE=' '
        ENDIF

      ENDIF

      END
