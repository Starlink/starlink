*+  GDV_HARD - is it hardcopy device?
      SUBROUTINE GDV_HARD(HARD,STATUS)
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
      LOGICAL HARD
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
          HARD=G_HARDCOPY
        ELSE
          CALL MSG_PRNT('AST_ERR: no graphics device active')
          STATUS=SAI__ERROR
        ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GDV_HARD',STATUS)
        ENDIF

      ENDIF

      END
