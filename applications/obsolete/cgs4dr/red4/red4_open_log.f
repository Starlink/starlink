*+  RED4_OPEN_LOG - Open the (engineering) log file
      SUBROUTINE RED4_OPEN_LOG( STATUS )
*    Invocation :
*     CALL RED4_OPEN_LOG( STATUS )
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
      INCLUDE 'RED4_COMMON.INC'  ! RED4 common block
      INCLUDE 'RED4_ENG.INC'     ! RED4 (engineering) common block
*    External references :
      INTEGER CHR_LEN            ! Finds used length of string
*    Local variables :
      CHARACTER*255 FILE         ! The name of the log file to be opened
*-

*   Check for error on entry.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Generate a filename
      CALL CHR_FILL( ' ', FILE )
      FILE = CGS4_ENG(1:CHR_LEN(CGS4_ENG)) // 'dr' // CGS4_DATE(1:CHR_LEN(CGS4_DATE)) // '.log'
      CALL CHR_RMBLK( FILE )

*   If a log file is alread open, close it
      IF ( LOG_OPEN ) CALL FIO_CLOSE( LOG_UNIT, STATUS )

*   Open the file
      CALL FIO_OPEN( FILE(1:CHR_LEN(FILE)), 'APPEND', 'LIST', 132, LOG_UNIT, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        LOG_OPEN = .FALSE.
        STATUS = SAI__ERROR
        CALL MSG_SETC( 'FILE', FILE )
        CALL ERR_REP( ' ', 'RED4_OPEN_LOG: Unable to open ^FILE', STATUS )
      ELSE
        LOG_OPEN = .TRUE.
      ENDIF
      END
