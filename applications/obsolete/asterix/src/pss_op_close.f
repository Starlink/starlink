*+  PSS_OP_CLOSE - Initialise PSS text output
      SUBROUTINE PSS_OP_CLOSE( STATUS )
*
*    Description :
*
*     Shut down PSS's output stream.
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
*
*    Status :
*
      INTEGER STATUS
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Close channel
      CALL AIO_CANCL( 'DEV', STATUS )

*    Report errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'PSS_OP_CLOSE', STATUS )
      END IF

      END
