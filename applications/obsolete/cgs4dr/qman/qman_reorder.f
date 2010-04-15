*+  QMAN_REORDER - Re-order routine for QMAN task
      SUBROUTINE QMAN_REORDER( STATUS )
*    Invocation :
*     CALL QMAN_REORDER( STATUS )
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
      CHARACTER*( MSG_VAL_LEN )
     :  CTEMP( MAX_QENTRIES )         ! Temporary array for command strings
      DOUBLE PRECISION
     :  DTEMP( MAX_QENTRIES )         ! Temporary array for timestamps
      INTEGER ICOUNT, JCOUNT          ! Counters
*-

*   Return immediately if status bad
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Put out an informational message
      IF ( VERBOSE ) CALL MSG_OUT( ' ',
     :  'Removing holes from command queue array', STATUS )

*   Loop around array and write out acceptable records
      JCOUNT = 0
      DO ICOUNT = 0, MAX_QENTRIES, 1
        IF ( CHARQ(ICOUNT).NE.' ' .AND. DATEQ(ICOUNT).NE.0.0 ) THEN
          JCOUNT = JCOUNT + 1
          CTEMP( JCOUNT ) = CHARQ( ICOUNT )
          DTEMP( JCOUNT ) = DATEQ( ICOUNT )
        END IF
      END DO

*   Issue a message
      IF ( VERBOSE ) THEN
        CALL MSG_SETI( 'UREC', USED_RECORDS )
        CALL MSG_SETI( 'JCOUNT', JCOUNT )
        CALL MSG_OUT( ' ', '^JCOUNT commands have been saved '/
     :    /' from ^UREC originals', STATUS )
      END IF

*   Initialise the command queue array
      CALL QMAN_INIT_POINTERS( STATUS )

*   If records were written, copy them back
      IF ( JCOUNT .GT. 0 ) THEN

        DO ICOUNT = 1, JCOUNT, 1
          CHARQ( ICOUNT ) = CTEMP( ICOUNT )
          DATEQ( ICOUNT ) = DTEMP( ICOUNT )
        END DO

        MINREC_PTR = 1
        MAXREC_PTR = JCOUNT
        USED_RECORDS = MAXREC_PTR - MINREC_PTR + 1
      END IF

*   Exit subroutine
      END
