*+  GDV_CLEAR - clear display surface
      SUBROUTINE GDV_CLEAR(STATUS)
*    Description :
*    Authors :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Global variables :
*    Structure definitions :
*    Import :
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL PGPAGE()

      ENDIF

      END
