*+  GCB_GETCONTXT - get current graphics context (eg GRAFIX, IMAGE etc)
      SUBROUTINE GCB_GETCONTXT(CONTEXT,STATUS)
*    Description :
*    Authors :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'GCB_PAR'
*    Global variables :
      INCLUDE 'GCB_CMN'
*    Structure definitions :
*    Import :
      CHARACTER*(*) CONTEXT
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
*-

      IF (STATUS.EQ.SAI__OK) THEN

        IF (G_CONTEXT(1:1).EQ.CHAR(0)) THEN
          CONTEXT=' '
        ELSE
          CONTEXT=G_CONTEXT
        ENDIF

      ENDIF

      END
