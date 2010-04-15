*+  QMAN_CHECK_PWD - Checks that QMAN password is OK
      SUBROUTINE QMAN_CHECK_PWD( STATUS )
*    Invocation :
*     CALL QMAN_CHECK_PWD( STATUS )
*    Authors :
*     P. N. Daly (PND@JACH.HAWAII.EDU)
*    History :
*     27-May-1994: Original version                                   (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'                ! Defines SAI__OK and others
      INCLUDE 'MESSYS_LEN'                  ! Defines MSG_VAL_LEN etc
*    Status :
      INTEGER STATUS                   ! Inherited global ADAM status
*    Global variables :
      INCLUDE 'QMAN_GLOBAL.PAR'        ! QMAN common block
      INCLUDE 'QMAN_COMMON.BLK'        ! QMAN global parameter constants
*    External references :
      INTEGER CHR_LEN                  ! Function to find length of string
*    Local variables :
      CHARACTER*( MSG_VAL_LEN ) PACCESS ! A temporary access string
      CHARACTER*( MSG_VAL_LEN ) LACCESS ! A temporary access string
*-

*   Return immediately if status bad.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Get a password if in secure mode
      IF ( SECURE ) THEN

        CALL CHR_FILL( ' ', PACCESS )
        CALL PAR_GET0C( 'PASSWORD', PACCESS, STATUS )
        CALL PAR_CANCL( 'PASSWORD', STATUS )
        IF ( PACCESS(1:CHR_LEN(PACCESS)) .NE.
     :       PASSWORD(1:CHR_LEN(PASSWORD)) ) THEN

          STATUS = SAI__ERROR
          CALL ERR_REP( ' ', 'QMAN_CHECK_PWD: Illegal operation '/
     :      /'- invalid password specified', STATUS )
        ENDIF
      ENDIF

*   Get a password if in lock mode
      IF ( DB_LOCKED ) THEN

        CALL CHR_FILL( ' ', LACCESS )
        CALL PAR_GET0C( 'LOCKWORD', LACCESS, STATUS )
        CALL PAR_CANCL( 'LOCKWORD', STATUS )
        IF ( LACCESS(1:CHR_LEN(LACCESS)) .NE.
     :       LOCKWORD(1:CHR_LEN(LOCKWORD)) ) THEN

          STATUS = SAI__ERROR
          CALL ERR_REP( ' ', 'QMAN_CHECK_PWD: Illegal operation '/
     :      /'- invalid lockword specified', STATUS )
        ENDIF
      ENDIF

*   Exit subroutine
      END
