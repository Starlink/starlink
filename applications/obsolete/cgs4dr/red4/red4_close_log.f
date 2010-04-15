*+  RED4_CLOSE_LOG - Close the (engineering) log file
      SUBROUTINE RED4_CLOSE_LOG( STATUS )
*    Invocation :
*     CALL RED4_CLOSE_LOG( STATUS )
*    Authors :
*     P N Daly  (JACH.HAWAII.EDU::PND)
*    History :
*     19-Jan-1995: Original Unix version.           (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS             ! Global status
*    Global variables :
      INCLUDE 'RED4_ENG.INC'     ! RED4 (engineering) common block
*-

*   Check for error on entry.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Close the file
      IF ( LOG_OPEN ) CALL FIO_CLOSE( LOG_UNIT, STATUS )

      END
