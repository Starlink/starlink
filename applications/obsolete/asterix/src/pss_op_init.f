*+  PSS_OP_INIT - Initialise PSS text output
      SUBROUTINE PSS_OP_INIT( STATUS )
*
*    Description :
*
*     Initialise PSS text output. The device may be any one supported by
*     UTIL_SELOUT, currently terminal,files and printer stream.
*
*    Environment parameters :
*
*     DEV = CHAR(R)
*       Ascii device for PSS output. Default is TERMINAL.
*
*    History :
*
*     16 Nov 91 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'PSS_PAR'
*
*    Global variables :
*
      INCLUDE 'PSS_DIAG_CMN'
      INCLUDE 'PSS_IO_CMN'
*
*    Status :
*
      INTEGER STATUS
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Open output channel
      CALL AIO_ASSOCO( 'DEV', 'LIST', GE_OP_LUN, GE_OP_WIDTH, STATUS )

*    Report errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'PSS_OP_INIT', STATUS )
      END IF

      END
