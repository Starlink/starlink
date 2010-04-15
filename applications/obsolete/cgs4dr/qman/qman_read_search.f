*+  QMAN_READ_SEARCH - Reads a record matching the search string
      SUBROUTINE QMAN_READ_SEARCH( STATUS )
*    Invocation :
*     CALL QMAN_READ_SEARCH( STATUS )
*    Authors :
*     P. N. Daly (PND@JACH.HAWAII.EDU)
*    History :
*     27-May-1994: Original version                                   (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'               ! Defines SAI__OK and others
      INCLUDE 'MESSYS_LEN'                 ! Defines MSG_VAL_LEN etc
*    Status :
      INTEGER STATUS                  ! Inherited global ADAM status
*    Global variables :
      INCLUDE 'QMAN_GLOBAL.PAR'       ! QMAN common block
      INCLUDE 'QMAN_COMMON.BLK'       ! QMAN global parameter constants
*    Local variables :
      CHARACTER*10 MODE               ! Mode to search for string
*-

*   Return immediately if status bad.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Get the search string and the search mode
      CALL CHR_FILL( ' ', STRING )
      CALL PAR_GET0C( 'STRING', STRING, STATUS )
      CALL CHR_LDBLK( STRING )
      CALL CHR_FILL( ' ', MODE )
      CALL PAR_GET0C( 'SEARCH_MODE', MODE, STATUS )
      CALL CHR_UCASE( MODE )

      IF ( VERBOSE ) THEN
        CALL MSG_SETC( 'STRING', STRING )
        CALL MSG_SETC( 'MODE', MODE )
        CALL MSG_OUT( ' ',
     :    'Searching for ^MODE string ^STRING', STATUS )
      ENDIF

*   Get oldest record matching criteria
      IF ( MODE .EQ. 'OLDEST' ) THEN
        CALL QMAN_READ_SOLD( STATUS )

*   Get newest record matching criteria
      ELSE IF ( MODE .EQ. 'NEWEST' ) THEN
        CALL QMAN_READ_SNEW( STATUS )
      ENDIF

*   Read the record
      CALL QMAN_READ_REC( STATUS )

*   Exit subroutine
      END
