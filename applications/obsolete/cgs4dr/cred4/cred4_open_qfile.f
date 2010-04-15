*+  CRED4_OPEN_QFILE - Open the data reduction queue file
      SUBROUTINE CRED4_OPEN_QFILE( STATUS )
*    Description :
*     Opens the file containing the list of pending instructions.
*    Invocation :
*     CALL CRED4_OPEN_QFILE( STATUS )
*    Authors :
*     J.Lightfoot (REVAD::JFL)
*     S.M.Beard (REVAD::SMB)
*     P.N.Daly (JACH::PND)
*    History :
*     September 1989 : Original version.                   (JFL)
*     11-Jan-1990: History added. GOTOs removed. Fortran
*                   I/O error reporting improved.          (SMB)
*     15-Jan-1990: Documentation improved.                 (SMB)
*      4-Jun-1990: FOR__OK added.                          (SMB)
*      5-Jun-1990: Name made more consistent.              (SMB)
*     11-Jun-1990: Setting of status to ACT__END removed.
*                  Now done in CRED4_ENDACTION.            (SMB)
*     11-Jun-1990: Terminology changed: "Pending file"
*                  changed to "Data reduction queue file". (SMB)
*     18-Jun-1990: Renamed from CRED4_OPEN_FILE to
*                  CRED4_OPEN_QFILE, to be more consistent
*                  with the ENG4 task.                     (SMB)
*     19-Jun-1990: QFILE parameter added, so the name of
*                  the queue may be altered more easily.
*                  (Not being able to alter it has made
*                  testing the software without interfering
*                  with CGS4 system tests difficult).      (SMB)
*      8-Aug-1990: I have just discovered that ERR_REP resets
*                  STATUS back to ADAM__OK, which messes up
*                  the error handling. Mistake fixed.      (SMB)
*      5-Oct-1990: Format of data reduction queue changed
*                  to use integer date/time as key field.  (SMB)
*     11-Feb-1993: Conform to error strategy               (PND)
*     18-Feb-1993: Replace LIB$GET_LUN with IO_NXTLUN      (PND)
*     28-Jul-1994: Port to Unix (re-write to use QMAN)     (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DTASK_ERR'
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'CRED4COM.INC'
*    External references :
      INTEGER CHR_LEN
*    Local variables :
      CHARACTER*( MSG_VAL_LEN ) OUTVAL
      CHARACTER*( MSG_VAL_LEN ) INVAL
*-

*   Return if status on entry is not SAI__OK
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Get the name of the QMAN task
      CALL PAR_GET0C( 'QMAN_ALIAS', QMAN_ALIAS, STATUS )
      CALL PAR_GET0C( 'QMAN_PWRD', QMAN_PWRD, STATUS )
      CALL PAR_GET0C( 'QMAN_LWRD', QMAN_LWRD, STATUS )

*   Set the QMAN_OK flag if we can talk to the task
      INVAL = 'PASSWORD="'//QMAN_PWRD(1:CHR_LEN(QMAN_PWRD))/
     :     /'" LOCKWORD="'//QMAN_LWRD(1:CHR_LEN(QMAN_LWRD))//'"'
      QMAN_OK = .FALSE.
      CALL TASK_OBEY( QMAN_ALIAS(1:CHR_LEN(QMAN_ALIAS)), 'STATUS',
     :  INVAL(1:CHR_LEN(INVAL)), OUTVAL,
     :  QMAN_PATH, QMAN_MESSID, STATUS )
      IF ( STATUS .NE. DTASK__ACTSTART ) THEN
         QMAN_OK = .FALSE.
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'CRED4_OPEN_QFILE: '/
     :     /'Unable to connect to QMAN task', STATUS )
         CALL MSG_SETC( 'OUTVAL', OUTVAL )
         CALL ERR_REP( ' ', 'VALUE string is ^OUTVAL', STATUS )
      ELSE
        CALL ERR_ANNUL( STATUS )
      ENDIF

*   Wait for reply (not that we care what it is)
      CALL TASK_DONE( -1, QMAN_PATH, QMAN_MESSID, OUTVAL, STATUS )
      IF ( ( STATUS .NE. DTASK__ACTCOMPLETE ) .AND.
     :     ( STATUS .NE. DTASK__ACTINFORM ) ) THEN
        QMAN_OK = .FALSE.
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'CRED4_OPEN_QFILE: '/
     :    /'Failed to complete connection to QMAN', STATUS )
        CALL MSG_SETC( 'OUTVAL', OUTVAL )
        CALL ERR_REP( ' ', 'VALUE string is ^OUTVAL', STATUS )
      ELSE
        CALL ERR_ANNUL( STATUS )
        QMAN_OK = .TRUE.
      ENDIF

*  Write out a message
      IF ( QMAN_OK ) THEN
        CALL MSG_SETC( 'QMAN_ALIAS', QMAN_ALIAS )
        CALL MSG_OUT( ' ', 'Connected to QMAN task ^QMAN_ALIAS', STATUS )
      ELSE
        CALL MSG_SETC( 'QMAN_ALIAS', QMAN_ALIAS )
        CALL MSG_OUT( ' ', 'QMAN task ^QMAN_ALIAS unavailable', STATUS )
      END IF
      END
