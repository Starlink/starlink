*+  AIO_CANCL - Close Asterix output text channel
      SUBROUTINE AIO_CANCL( PAR, STATUS )
*
*    Description :
*
*     Closes output text channel. If the channel maps to a file then the
*     file is closed. If the channel maps to the printer the file is
*     spooled and deleted.
*
*    Method :
*     <description of how the subroutine works - for programmer info>
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*
*     David J. Allan (JET-X, University of Birmingham)
*
*    History :
*
*      4 May 94 : Original. Derived from old UTIL_SELOUT routine (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'PAR_PAR'
      INCLUDE 'AIO_PAR'
*
*    Global variables :
*
      INCLUDE 'ASTLIB(AIO_CMN)'
*
*    Import :
*
      CHARACTER*(*)		PAR			! ADAM parameter name
*
*    Status :
*
      INTEGER STATUS
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Check parameter
      IF ( PAR .EQ. AIO_PARNAM ) THEN
        CALL AIO_CLOSE( AIO_MODE, STATUS )
        CALL PAR_CANCL( PAR, STATUS )
      END IF

*    Tidy up
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'AIO_CANCL', STATUS )
      END IF

      END
