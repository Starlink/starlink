      PROGRAM FAMSMASTER
* A test of fams - run in conjunction with famsslave
*   % famsslave &
*   % famsmaster
*
* History:
*   17_NOV-1994 (AJC):
*      Original

      INCLUDE 'SAE_PAR'
      INCLUDE 'ADAM_DEFNS'
      INCLUDE 'MESSYS_LEN'
      INCLUDE 'MESSYS_PAR'

      INTEGER OUTMSG_STATUS
      INTEGER OUTMSG_FUNCTION
      INTEGER OUTMSG_CONTEXT
      INTEGER OUTMSG_LENGTH
      INTEGER INMSG_STATUS
      INTEGER INMSG_CONTEXT
      INTEGER INMSG_LENGTH
      INTEGER STATUS
      INTEGER PATH
      INTEGER MESSID
      INTEGER J
      CHARACTER*(MSG_NAME_LEN) SLAVE_NAME
      CHARACTER*32 OUTMSG_NAME
      CHARACTER*(MSG_VAL_LEN) OUTMSG_VALUE
      CHARACTER*32 INMSG_NAME
      CHARACTER*(MSG_VAL_LEN) INMSG_VALUE

      STATUS = 0
      SLAVE_NAME = 'slave'
      OUTMSG_STATUS = SAI__OK
      OUTMSG_FUNCTION = MESSYS__MESSAGE
      OUTMSG_CONTEXT = OBEY
      OUTMSG_LENGTH = 16

      OUTMSG_NAME = 'junk'
      OUTMSG_VALUE = 'master calling'

      CALL FAMS_INIT( 'master', STATUS )

      IF ( STATUS .NE. SAI__OK )
     :   PRINT *, 'master - bad status after FAMS_INIT'

      CALL FAMS_PATH ( SLAVE_NAME, PATH, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         PRINT *, 'master - bad status after FAMS_PATH'

      ELSE
         PRINT *, 'master - path set up ok'

      END IF

      DO J = 1, 1000
         CALL FAMS_SEND ( PATH, OUTMSG_FUNCTION, OUTMSG_STATUS, 
     :    OUTMSG_CONTEXT, OUTMSG_NAME, OUTMSG_LENGTH, OUTMSG_VALUE, 
     :    MESSID, STATUS )

         CALL FAMS_GETREPLY ( MESSYS__INFINITE, PATH, MESSID,
     :    INMSG_STATUS, INMSG_CONTEXT, INMSG_NAME, INMSG_LENGTH, 
     :    INMSG_VALUE, STATUS )

         CALL FAMS_GETREPLY ( MESSYS__INFINITE, PATH, MESSID,
     :    INMSG_STATUS, INMSG_CONTEXT, INMSG_NAME, INMSG_LENGTH, 
     :    INMSG_VALUE, STATUS )
      END DO

      IF ( STATUS .NE. SAI__OK ) THEN
         PRINT *, 'master: bad status = ', STATUS 

      ELSE
         PRINT *, 'master: received - ', INMSG_VALUE(1:INMSG_LENGTH)

      END IF

      END
