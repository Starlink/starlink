*+  AST_CLOSE - shutdown ASTERIX sub-systems
      SUBROUTINE AST_CLOSE()
*    Description :
*      Closes BDA_, DYN_, HIST_, USI_ systems
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global variables :
      INCLUDE 'SAE_PAR'
*    Local Constants :
*    Local variables :
*-
      CALL BDA_CLOSE()
      CALL DYN_CLOSE()
      CALL HIST_CLOSE()
      CALL USI_CLOSE()

      END
