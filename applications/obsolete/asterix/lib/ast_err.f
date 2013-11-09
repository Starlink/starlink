*+  AST_ERR - output ASTERIX error messages
      SUBROUTINE AST_ERR(STATUS)
*    Description :
*    History :
*     27/4/89 : Original (RJV)
*    Type definitions :
      IMPLICIT NONE
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PAR_ERR'
*    Local Constants :
*    Local variables :
*-
      IF (STATUS.NE.SAI__OK) THEN

        IF (STATUS.EQ.PAR__NULL.OR.STATUS.EQ.PAR__ABORT) THEN
          CALL MSG_PRNT('**task aborted**')
          STATUS=SAI__OK
        ELSEIF (STATUS.EQ.SAI__ERROR) THEN
          STATUS=SAI__OK
        ENDIF

      ENDIF

      END
