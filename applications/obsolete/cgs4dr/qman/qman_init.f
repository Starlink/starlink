*+  QMAN_INIT - Initialisation routine for QMAN task
      SUBROUTINE QMAN_INIT( STATUS )
*    Description :
*    Invocation :
*     CALL QMAN_INIT( STATUS )
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
      INCLUDE 'QMAN_GLOBAL.PAR'       ! QMAN_INIT common block
      INCLUDE 'QMAN_COMMON.BLK'       ! QMAN_INIT global parameter constants
*    External references :
      INTEGER CHR_LEN                 ! Function to find used length of string
*    Local constants :
      INTEGER STRLEN                  ! Maximum string length used by PoSiX
      PARAMETER ( STRLEN = 32 )
*    Local variables :
      CHARACTER*( STRLEN ) USERNAME   ! PoSiX User idenitifier
      CHARACTER*( STRLEN ) SYSTEM     ! PoSiX system name
      CHARACTER*( STRLEN ) NODE       ! PoSiX nodename
      CHARACTER*( STRLEN ) RELEASE    ! PoSiX operating system release
      CHARACTER*( STRLEN ) VERSION    ! PoSiX operating system version
      CHARACTER*( STRLEN ) MACHINE    ! PoSiX machine type
      CHARACTER*( STRLEN ) BASETIME   ! PoSiX time
      INTEGER NTICKS                  ! PoSiX time ticks
*-

*   Return immediately if status bad.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Set variables if we have not been initialised before
      IF ( INIT_STRING .NE. TASK_INIT ) THEN

*     Get the password if there is one
        CALL CHR_FILL( ' ', PASSWORD )
        CALL PAR_GET0C( 'PASSWORD', PASSWORD, STATUS )
        CALL PAR_CANCL( 'PASSWORD', STATUS )

        IF ( PASSWORD .EQ. ' ' ) THEN
          SECURE = .FALSE.
          IF ( VERBOSE ) CALL MSG_OUT( ' ',
     :      'A password has been NOT specified', STATUS )
        ELSE
          SECURE = .TRUE.
          IF ( VERBOSE ) CALL MSG_OUT( ' ',
     :      'A password has been specified', STATUS )
        END IF

*     Initialize some variables
        FRACTION  = 0
        CALL QMAN_INIT_POINTERS( STATUS )

*     Do some PoSiX calls
        CALL PSX_TIME( NTICKS, STATUS )
        CALL PSX_CTIME( NTICKS, BASETIME, STATUS )
        CALL PSX_CUSERID( USERNAME, STATUS )
        CALL PSX_UNAME( SYSTEM, NODE, RELEASE,
     :    VERSION, MACHINE, STATUS )

*   Using these, create a new string
        INIT_USER = 'Task initialised by ' /
     :    / USERNAME(1:CHR_LEN(USERNAME)) /
     :    / ' on ' // SYSTEM(1:CHR_LEN(SYSTEM)) /
     :    / ' ' // NODE(1:CHR_LEN(NODE)) /
     :    / ' ' // RELEASE(1:CHR_LEN(RELEASE)) /
     :    / ' ' // VERSION(1:CHR_LEN(VERSION)) /
     :    / ' ' // MACHINE(1:CHR_LEN(MACHINE)) /
     ;    / ' on ' // BASETIME(1:CHR_LEN(BASETIME))

*     Change the initialised settings
        READREC_OK  = .FALSE.
        INITIALISED = .TRUE.
        INIT_STRING = TASK_INIT
        IF ( VERBOSE ) THEN
          CALL MSG_SETC( 'IS', INIT_USER )
          CALL MSG_OUT( ' ', '^IS', STATUS )
        END IF
      ELSE

*    Task has already been initialised
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'QMAN_INIT: '/
     :    /'Task already initialised', STATUS )
      END IF

*    Exit subroutine
      END
