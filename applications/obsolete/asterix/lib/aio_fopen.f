*+  AIO_FOPEN - Open a file for nested read, read, write or update
      SUBROUTINE AIO_FOPEN( FILENAME, ACCESS, ID, STATUS )
*
*    Description :
*
*
*
*    Method :
*     <description of how the subroutine works - for programmer info>
*    Deficiencies :
*
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*
*     David J. Allan (JET-X, University of Birmingham)
*
*    History :
*
*     11 Nov 94 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'AIO_PAR'
*
*    Global variables :
*
      INCLUDE 'ASTLIB(AIO_CMN)'
*
*    Import :
*
      CHARACTER*(*)		FILENAME		! Device name
      CHARACTER*(*)		ACCESS			! Access mode
*
*    Export :
*
      INTEGER			ID			! Channel id
*
*    Status :
*
      INTEGER STATUS
*
*    Function declarations :
*
      LOGICAL			STR_ABBREV
      LOGICAL			CHR_INSET
*
*    External references :
*
      EXTERNAL                  AIO_BLK
*
*    Local variables :
*
      CHARACTER*6		LACCESS			! Local copy of ACCESS
      INTEGER			WIDTH			! Stream width
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Check access
      LACCESS = ACCESS
      CALL CHR_UCASE( LACCESS )
      IF ( .NOT. CHR_INSET( 'NREAD,READ,WRITE,UPDATE', LACCESS ) ) THEN
        STATUS = SAI__ERROR
        CALL MSG_SETC( 'ACC', ACCESS )
        CALL ERR_REP( ' ', 'Invalid file access mode', STATUS )

*    Open the file
      ELSE

*      READ or NREAD access
        IF ( CHR_INSET( 'NREAD,READ', LACCESS ) ) THEN

*        Check read channel not already open
          IF ( AIO_IDEF ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'Input channel already in use', STATUS )

          ELSE

*          Open the file
            CALL AFIO_OPEN1( FILENAME, 'READ', 'LIST', 0, ID, STATUS )

*          Initialise channel
            IF ( STATUS .EQ. SAI__OK ) THEN
              AIO_IFID(1) = ID
              AIO_ILEV = 1
              AIO_IDEF = .TRUE.
              AIO_INEST = (LACCESS(1:1).EQ.'N')
            END IF

          END IF

*      WRITE access
        ELSE IF ( STR_ABBREV( 'READ', LACCESS ) ) THEN

*        Open new file
          CALL AIO_OPEN( 'NEWFILE='//FILENAME, 'LIST', ID, WIDTH,
     :                   STATUS )

*      UPDATE access
        ELSE

*        Open existing file
          CALL AIO_OPEN( 'OLDFILE='//FILENAME, 'LIST', ID, WIDTH,
     :                    STATUS )

        END IF

      END IF

*    Tidy up
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'AIO_FOPEN', STATUS )
      END IF

      END



*+  AIO_FOPEN1 - Open a FIO file in the path and with default extension
      SUBROUTINE AIO_FOPEN1( FILE, ACCESS, MODE, RECSZ, FD, STATUS )
*    Description :
*    Method :
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'AIO_PAR'
*
*    Global variables :
*
      INCLUDE 'ASTLIB(AIO_CMN)'
*
*    Import :
*
      CHARACTER*(*)          FILE               ! File name
      CHARACTER*(*)          ACCESS             ! Access mode
      CHARACTER*(*)          MODE               ! Carriage control mode
      INTEGER                RECSZ              ! Record size if any
*
*    Export :
*
      INTEGER                FD                 ! FIO descriptor
*
*    Status :
*
      INTEGER STATUS
*
*    Functions :
*
      INTEGER                CHR_LEN
*
*    Local variables :
*
      CHARACTER*132          PATH               ! Directory path

      INTEGER                CPOS               ! position of colon in PATH
      INTEGER                ELEN               ! Useful length of AIO_DEFEXT
      INTEGER                FLEN               ! Useful length of FILE
      INTEGER                IC, D_S, D_E       ! Character pointers
      INTEGER                PLEN               ! Useful length of PATH

      LOGICAL                FOUND              ! Located file?
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Length of FILE
      FLEN = CHR_LEN(FILE)

*    Length of default extension
      ELEN = CHR_LEN(AIO_DEFEXT)

*    If path is blank,
      IF ( AIO_IPATH(1:1) .EQ.' ' ) THEN

*      Just use FIO directly
        CALL FIO_OPEN( FILE, ACCESS, MODE, RECSZ, FD, STATUS )

*      If that failed, and default extension is non-blank, try that
        IF ( (STATUS .NE. SAI__OK) .AND. (ELEN.GT.0) ) THEN
          CALL ERR_CANCL( STATUS )
          CALL FIO_OPEN( FILE//AIO_DEFEXT(:ELEN),
     :                   ACCESS, MODE, RECSZ, FD, STATUS )

        END IF

*    Path is non-blank
      ELSE

*      Translate path variable
        CALL PSX_GETENV( AIO_IPATH(:CHR_LEN(AIO_IPATH)), PATH, STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_ANNUL( STATUS )
          PATH = AIO_IPATH
        END IF

*      Search through directories
        IC = 1
        FOUND = .FALSE.
        PLEN = CHR_LEN(PATH)
        DO WHILE ( (IC.LE.PLEN) .AND. .NOT. FOUND )

*        Locate directory within path
          D_S = IC
          CPOS = INDEX(PATH(IC:),':')
          IF ( CPOS .EQ. 0 ) THEN
            D_E = PLEN
          ELSE
            D_E = IC + CPOS - 2
          END IF

*        Try and open file with extension
          CALL FIO_OPEN( PATH(D_S:D_E)//'/'//FILE, ACCESS, MODE,
     :                                       RECSZ, FD, STATUS )

*        Ok?
          IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )

*          Try and open file with extension if defined
            IF ( ELEN .GT. 0 ) THEN
              CALL FIO_OPEN( PATH(D_S:D_E)//'/'//FILE(:FLEN)//
     :                       AIO_DEFEXT(:ELEN), ACCESS, MODE,
     :                       RECSZ, FD, STATUS )
              IF ( STATUS .NE. SAI__OK ) THEN
                CALL ERR_ANNUL( STATUS )
              ELSE
                FOUND = .TRUE.
              END IF
            END IF
          ELSE
            FOUND = .TRUE.
          END IF

*        If not found advance pointer
          IF ( .NOT. FOUND ) IC = D_E + 2

        END DO

*      Found the file?
        IF ( .NOT. FOUND ) THEN
          STATUS = SAI__ERROR
          CALL MSG_SETC( 'FILE', FILE )
          CALL MSG_SETC( 'PATH', PATH )
          CALL ERR_REP( ' ', 'Unable to locate file ^FILE on '/
     :                                  /'path ^PATH', STATUS )
        END IF

      END IF

      END
