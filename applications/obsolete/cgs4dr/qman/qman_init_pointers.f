*+  QMAN_INIT_POINTERS - Initialises pointers and array for QMAN task
      SUBROUTINE QMAN_INIT_POINTERS( STATUS )
*    Description :
*    Invocation :
*     CALL QMAN_INIT_POINTERS( STATUS )
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
      INTEGER ICOUNT                  ! A counter
*-

*   Return immediately if status bad.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Initialize the arrays
      DO ICOUNT = 0, MAX_QENTRIES, 1
        CHARQ( ICOUNT ) = ' '
        DATEQ( ICOUNT ) = 0.0
      END  DO

*   Initialize some variables
      MINREC_PTR   = 1
      MAXREC_PTR   = 0
      READREC_PTR  = 0
      USED_RECORDS = 0

*    Exit subroutine
      END
