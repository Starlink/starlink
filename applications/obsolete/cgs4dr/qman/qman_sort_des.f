*+  QMAN_SORT_DES - Sorts database into descending order
      SUBROUTINE QMAN_SORT_DES( STATUS )
*    Invocation :
*     CALL QMAN_SORT_DES( STATUS )
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
      INTEGER SWAP_FLAG, I            ! Flag for swapping (0=done), counter
      CHARACTER*( MSG_VAL_LEN ) CTEMP ! Temporary character variable
      DOUBLE PRECISION TEMP           ! Temporary date variable
*-

*   Return immediately if status bad.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   First re-order the queue
      CALL QMAN_REORDER( STATUS )

*   Loop around using a `swap-sort' technique on used records
 10   SWAP_FLAG = 0
      DO I = MINREC_PTR, MAXREC_PTR-1, 1

        IF ( DATEQ(I) .LT. DATEQ(I+1) ) THEN
          IF ( VERBOSE ) THEN
            CALL MSG_SETI( 'I', I )
            CALL MSG_SETI( 'J', I+1 )
            CALL MSG_OUT( ' ', 'Swapping records ^I and ^J', STATUS )
          ENDIF
          TEMP  = DATEQ(I)
          CTEMP = CHARQ(I)
          DATEQ(I) = DATEQ(I+1)
          CHARQ(I) = CHARQ(I+1)
          DATEQ(I+1) = TEMP
          CHARQ(I+1) = CTEMP
          SWAP_FLAG = 1
        ENDIF
      ENDDO

      IF ( SWAP_FLAG .NE. 0 ) GOTO 10

*   Exit subroutine
      END
