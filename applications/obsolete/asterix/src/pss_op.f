*+  PSS_OP - Output a line of text
      SUBROUTINE PSS_OP( CLASS, MESSAGE )
*
*    Description :
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
      INCLUDE 'DAT_PAR'
      INCLUDE 'PSS_PAR'
*
*    Global variables :
*
      INCLUDE 'PSS_DIAG_CMN'
      INCLUDE 'PSS_IO_CMN'
*
*    Import :
*
      CHARACTER*(*)             CLASS                  ! Message class
      CHARACTER*(*)             MESSAGE                ! Message text
*
*    Local variables :
*
      INTEGER			STATUS			! Local status
*-

*    Only output INFO messages if in diagnostic mode
      IF ( (CLASS .EQ. 'INFO') .AND. .NOT. DI_ON ) RETURN

*    Evaluate message
      STATUS = SAI__OK
      CALL AIO_WRITE( GE_OP_LUN, MESSAGE, STATUS )

      END
