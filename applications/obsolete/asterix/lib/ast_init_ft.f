*+  AST_INIT - Initializes ASTERIX88 common blocks
      SUBROUTINE AST_INIT
*    Description :
*     Initializes:
*       BDA_, DYN_, & USI_ common blocks
*    History :
*     2/11/88: original (PLA)
*    Type definitions :
      IMPLICIT NONE
*    Global variables :
      INCLUDE 'SAE_PAR'
*    Local Constants :
*    Local variables :
*-

*    Initialize USI_CMN
      CALL USI_INIT()

*    Initialize DYN_CMN
      CALL DYN_INIT()

*    Initialize BDA_CMN
      CALL BDA_INIT()

*    Initialize HIST_CMN
      CALL HIST_INIT()

      END
