*+  CRED4_WAIT -  Wait for all data reduction actions to finish.
      SUBROUTINE CRED4_WAIT( STATUS )
*    Description :
*     Ignores input status and waits 10 minutes for pending P4 or
*     RED4 actions in progress to finish.
*    Invocation :
*     CALL CRED4_WAIT( STATUS )
*    Parameters :
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     J.Lightfoot (REVAD::JFL)
*     S.M.Beard  (REVAD::SMB)
*     P.N.Daly (JACH::PND)
*    History :
*     September 1989 : Original version.                      (JFL)
*     11-Jan-1990: Error reporting improved.                  (SMB)
*      9-Mar-1990: DSA__ERROR parameter added. Code spaced out
*                  more. Error messages made more meaningful
*                  and tokens used. Do not allow DSA__ERROR to
*                  be returned to ADAM.                       (SMB)
*      8-Aug-1990: I have just discovered that ERR_REP resets
*                  STATUS back to SAI__OK, which messes up
*                  the error handling. Mistake fixed.         (SMB)
*     29-Oct-1990: Timeout increased to 10 minutes, as some
*                  plotting actions can be very very slow.    (SMB)
*     14-Apr-1992: Add MSG_VAL_LEN.                           (SMB)
*     11-Feb-1993: Conform to error strategy                  (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DTASK_ERR'
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'CRED4COM.INC'         ! CRED4 common block
*    Local variables :
      INTEGER TATERS                 ! temporary STATUS
      CHARACTER*( MSG_VAL_LEN)
     :   OUTVAL                      ! Returned string
      INTEGER ERR_STATUS             ! Temporary status for ERR_REP
*-

*    copy STATUS and reset
      TATERS = STATUS
      CALL ERR_ANNUL( STATUS )

*    wait if P4 is active, report failure
      IF ( P4_ACTIVE ) THEN

*      use a 600000 millisecond (=10 minutes) timeout
         CALL TASK_DONE( 600000, P4_PATH, P4_MESSID, OUTVAL, STATUS )

         P4_ACTIVE = .FALSE.

         IF ( STATUS .NE. DTASK__ACTCOMPLETE ) THEN
            IF ( STATUS .EQ. DTASK__ACTINFORM ) THEN

               ERR_STATUS = STATUS
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'ES', ERR_STATUS )
               CALL ERR_REP( ' ', 'CRED4_WAIT: '/
     :            /'Failure reported from plotting task '/
     :            /'(Status = ^ES, message follows)', STATUS )
               CALL MSG_SETC( 'OUTVAL', OUTVAL )
               CALL ERR_REP (' ', 'CRED4_WAIT: '/
     :            /'^OUTVAL', STATUS )

            ELSE IF (STATUS .EQ. DSA__ERROR) THEN

*             explicit check for DSA error
               STATUS = SAI__ERROR
               CALL ERR_REP (' ', 'CRED4_WAIT: '/
     :            /'First DSA error has occured', STATUS)
            ELSE

               ERR_STATUS = STATUS
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'ES', ERR_STATUS )
               CALL ERR_REP( ' ', 'CRED4_WAIT: '/
     :            /'Bad status from P4 wait '/
     :            /'(Status = ^ES)', STATUS )
            ENDIF
         ELSE

            CALL ERR_ANNUL( STATUS )
         ENDIF
      ENDIF

*    same for RED4
      IF ( RED4_ACTIVE ) THEN

*      use a 600000 millisecond (=10 minutes) timeout
         CALL TASK_DONE (600000, RED4_PATH, RED4_MESSID, OUTVAL, STATUS)

         RED4_ACTIVE = .FALSE.

         IF (STATUS .NE. DTASK__ACTCOMPLETE) THEN
            IF (STATUS .EQ. DTASK__ACTINFORM) THEN

               ERR_STATUS = STATUS
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'ES', ERR_STATUS )
               CALL ERR_REP( ' ', 'CRED4_WAIT: '/
     :            /'Failure reported by RED4 task '/
     :            /'(Status = ^ES, message follows)', STATUS )
               CALL MSG_SETC( 'OUTVAL', OUTVAL )
               CALL ERR_REP (' ', 'CRED4_WAIT: '/
     :            /'^OUTVAL', STATUS )
            ELSE IF (STATUS .EQ. DSA__ERROR) THEN

*             explicit check for DSA error
               STATUS = SAI__ERROR
               CALL ERR_REP (' ', 'CRED4_WAIT: '/
     :            /'Second DSA error has occured', STATUS)
            ELSE

               ERR_STATUS = STATUS
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'ES', ERR_STATUS )
               CALL ERR_REP (' ', 'CRED4_WAIT: '/
     :            /'Bad status from RED4 wait '/
     :            /'(Status = ^ES)', STATUS)
            ENDIF
         ELSE

            CALL ERR_ANNUL( STATUS )
         ENDIF
      ENDIF

*    restore STATUS to its value on entry
      STATUS = TATERS

      END
