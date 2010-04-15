*+  CRED4_READ_CONFIG - Read a configuration from an ASCII file
      SUBROUTINE CRED4_READ_CONFIG( STATUS )
*    Description :
*     This routine reads an ASCII configuration file and sets
*     values in the noticeboard.
*    Invocation :
*     CALL CRED4_READ_CONFIG( STATUS )
*    Authors :
*     P N Daly (JACH::PND)
*    History :
*     11-Jan-1993: Original version (replaces previous routine)       (PND)
*     29-Jul-1994: Use FIO for Unix port                              (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'FIO_ERR'
*    Status :
      INTEGER STATUS             ! Global status
*    External references :
      INTEGER CHR_LEN            ! Length of string routine
*    Global variables :
      INCLUDE 'CRED4COM.INC'     ! CRED4 common block
*    Local variables :
      CHARACTER*100
     :  CONFIG_FILE              ! Name of configuration file
      CHARACTER*255
     :  RECORD,                  ! Record read from config file
     :  ITEM,                    ! Noticeboard item
     :  CVALUE,                  ! Noticeboard value
     :  TYPE                     ! Noticeboard data-type
      INTEGER
     :  LUN,                     ! Fortran logical unit number
     :  RECLEN,                  ! Length of a record
     :  ERR_STAT                 !  Error status
      LOGICAL
     :  EOF                      ! The end-of-file marker
*    Local data :
*-

*   Check for error on entry.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Obtain the name of the configuration file to be read.
      CALL PAR_GET0C( 'CONFIG_FILE', CONFIG_FILE, STATUS )

*   Issue a message
       CALL MSG_SETC( 'CONFIG', CONFIG_FILE )
       CALL MSG_OUT( ' ', 'Restoring configuration from ^CONFIG', STATUS )

*   Open the configuration file
      IF ( INDEX( CONFIG_FILE, SEPARATOR ) .EQ. 0 )
     :  CONFIG_FILE = CGS4_CONFIG(1:CHR_LEN(CGS4_CONFIG)) // CONFIG_FILE(1:CHR_LEN(CONFIG_FILE))
      IF ( INDEX( CONFIG_FILE, '.cred4' ) .EQ. 0 )
     :  CONFIG_FILE = CONFIG_FILE(1:CHR_LEN(CONFIG_FILE)) // '.cred4'
      CALL CHR_RMBLK( CONFIG_FILE )
      CALL FIO_OPEN( CONFIG_FILE(1:CHR_LEN(CONFIG_FILE)), 'READ',
     :   'LIST', 0, LUN, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         ERR_STAT = STATUS
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'ES', ERR_STAT )
         CALL ERR_REP( ' ', 'CRED4_READ_CONFIG: '/
     :      /'Failed to open configuration file (Status = ^ES)', STATUS )
      END IF

*    Recursively read the contents of the file
      EOF = .FALSE.
      DO WHILE ( ( .NOT. EOF ) .AND. ( STATUS .EQ. SAI__OK ) )

*      Read the next record ignoring comment lines and blank lines
         CALL FIO_READ( LUN, RECORD, RECLEN, STATUS )

*      Test the status return for EOF
         IF ( STATUS .EQ. FIO__EOF ) THEN
            EOF = .TRUE.
            CALL ERR_ANNUL( STATUS )

*      Test the status return for an error
         ELSE IF ( STATUS .NE. SAI__OK ) THEN
            ERR_STAT = STATUS
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'ES', ERR_STAT )
            CALL ERR_REP( ' ', 'CRED4_READ_CONFIG: '/
     :         /'Error reading configuration file (Status = ^ES)', STATUS )

*         Populate the parameter file with this value (ignore comments)
         ELSE

            IF ( RECORD(1:1).EQ.'!' .OR. RECORD(1:1).EQ.'#' .OR.
     :           RECORD(1:1).EQ.'{' .OR. RECORD(1:1).EQ.'*' )  THEN
            ELSE
              CALL CRED4_PARSREC( RECORD, ITEM, CVALUE, TYPE, STATUS )
              CALL CRED4_PUT_PARAMETERS( ITEM, CVALUE, TYPE, STATUS )
            END IF
         END IF
      END DO

*    Close the file
      CALL FIO_CLOSE( LUN, STATUS )
      END



