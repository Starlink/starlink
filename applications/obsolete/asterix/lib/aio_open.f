*+  AIO_OPEN - Open an Asterix text output channel
      SUBROUTINE AIO_OPEN( DEVICE, TYPE, ID, WIDTH, STATUS )
*
*    Description :
*
*     Opens an output text device specified by DEVICE. It can take the
*     following values,
*
*       TERMINAL/CONSOLE/STDOUT     - The standard output stream
*       PRINTER	             - A temporary file is created which is spooled
*                              when closed
*       OLDFILE[=default]    - An existing file defined by the environment
*                              variable OLDFILE if defined, otherwise the
*                              value given by the default, or "ast_print.lis"
*       NEWFILE[=default]    - A new file whose name is given by the value
*                              of the environment variable NEWFILE if
*                              defined, otherwise the default, or
*                              "ast_print.lis"
*       any other value	     - Interpreted as a file name
*
*    Method :
*     <description of how the subroutine works - for programmer info>
*    Deficiencies :
*
*     AIO can only use the first 79 columns of console output because EMS
*     splits a string of length 80 on to 2 lines. When this restriction is
*     removed from EMS the width of the console channel should be 80.
*
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*
*     David J. Allan (JET-X, University of Birmingham)
*
*    History :
*
*      4 May 94 : Original. Derived from old UTIL_SELOUT routine (DJA)
*     24 Jul 94 : Renames existing output file on UNIX to allow cobbering (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'AIO_PAR'
      INCLUDE 'PAR_PAR'
*
*    Global variables :
*
      INCLUDE 'AIO_CMN'
*
*    Import :
*
      CHARACTER*(*)		DEVICE			! Device name
      CHARACTER*(*)		TYPE			! Type of file
*
*    Export :
*
      INTEGER			ID			! Channel id
      INTEGER			WIDTH			! Width in characters
*
*    Status :
*
      INTEGER STATUS
*
*    Function declarations :
*
      LOGICAL			STR_ABBREV
      LOGICAL			CHR_INSET
      LOGICAL			CHR_SIMLR
*
*    External references :
*
      EXTERNAL                  AIO_BLK
*
*    Local variables :
*
      CHARACTER*30		DEVARG			! Option device arg

      INTEGER			IFILE			! File counter
      INTEGER			LD			! Length of DEVICE

      LOGICAL			THERE			! File exists?
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Validate TYPE
      IF ( .NOT. CHR_INSET( 'LIST,FORTRAN', TYPE ) ) THEN
        STATUS = SAI__ERROR
        CALL MSG_SETC( 'TYPE', TYPE )
        CALL ERR_REP( ' ', 'Unsupport output access type /^TYPE/',
     :                STATUS )
        GOTO 99

      ELSE IF ( CHR_SIMLR( 'LIST', TYPE ) ) THEN
        AIO_ITYPE = AIO__T_LIST

      ELSE IF ( CHR_SIMLR( 'FORTRAN', TYPE ) ) THEN
        AIO_ITYPE = AIO__T_FORTRAN

      END IF
      AIO_TYPE = TYPE

*    Locate last non-blank character in DEVICE
      IF ( INDEX( DEVICE, '=' ) .NE. 0 ) THEN
        LD = INDEX( DEVICE, '=' )
        DEVARG = DEVICE(LD+1:)
        DEVICE = DEVICE(:LD-1)
        CALL CHR_LDBLK( DEVARG )
      ELSE
        DEVARG = ' '
      END IF

*    Printer device?
      IF ( STR_ABBREV( DEVICE, 'PRINTER' ) ) THEN

*      Create a filename which doesn't already exist
        IFILE = 0
        WRITE( AIO_FILE, '(A,I2.2)' ) 'ast_io.temp', IFILE
        THERE = .TRUE.
        DO WHILE ( .NOT. THERE )
          INQUIRE( FILE=AIO_FILE, EXIST=THERE )
          IFILE = IFILE + 1
        END DO

*      Open file
        CALL FIO_OPEN( AIO_FILE, 'WRITE', TYPE, 0, AIO_FID, STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_REP( ' ', 'Unable to create temporary output file',
     :                  STATUS )
          GOTO 99
        END IF
        CALL MSG_OUT( ' ', 'Writing output to printer...', STATUS )

*      Set mode
        ID = AIO__M_PRINT
        AIO_WIDTH = 132

*    Existing file
      ELSE IF ( STR_ABBREV( DEVICE, 'OLDFILE' ) ) THEN

*      Is there an environment variable translation?
        CALL PSX_GETENV( 'OLDFILE', AIO_FILE, STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_ANNUL( STATUS )
          IF ( DEVARG .GT. ' ' ) THEN
            AIO_FILE = DEVARG
          ELSE
            AIO_FILE = 'ast_print.lis'
          END IF
        END IF

*      Open named file
        CALL FIO_OPEN( AIO_FILE, 'APPEND', TYPE, 0, AIO_FID, STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL MSG_SETC( 'FILE', DEVICE )
          CALL ERR_REP( ' ', 'Unable to append to file /^FILE/',
     :                  STATUS )
          GOTO 99
        END IF
        CALL MSG_SETC( 'FILE', AIO_FILE )
        CALL MSG_OUT( ' ', 'Appending output to ^FILE...', STATUS )

*      Set mode
        ID = AIO__M_FILE
        AIO_WIDTH = 132

*    New file
      ELSE IF ( STR_ABBREV( DEVICE, 'NEWFILE' ) ) THEN

*      Is there an environment variable translation?
        CALL PSX_GETENV( 'NEWFILE', AIO_FILE, STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_ANNUL( STATUS )
          IF ( DEVARG .GT. ' ' ) THEN
            AIO_FILE = DEVARG
          ELSE
            AIO_FILE = 'ast_print.lis'
          END IF
        END IF

*      Rename old copy of this file if required
        CALL AIO_OPEN_REN( AIO_FILE, STATUS )

*      Open named file
        CALL FIO_OPEN( AIO_FILE, 'WRITE', TYPE, 0, AIO_FID, STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL MSG_SETC( 'FILE', DEVICE )
          CALL ERR_REP( ' ', 'Unable to create output file /^FILE/',
     :                  STATUS )
          GOTO 99
        END IF
        CALL MSG_SETC( 'FILE', AIO_FILE )
        CALL MSG_OUT( ' ', 'Writing output to ^FILE...', STATUS )

*      Set mode
        ID = AIO__M_FILE
        AIO_WIDTH = 132

*    Console?
      ELSE IF ( STR_ABBREV( DEVICE, 'TERMINAL' ) .OR.
     :          STR_ABBREV( DEVICE, 'STDOUT' ) .OR.
     :          STR_ABBREV( DEVICE, 'CONSOLE' ) ) THEN

*      Set mode
        ID = AIO__M_CONSOLE
        AIO_WIDTH = 79

*    Simple filename mode
      ELSE

*      Rename old copy of this file if required
        CALL AIO_OPEN_REN( DEVICE, STATUS )

*      Open named file
        CALL FIO_OPEN( DEVICE, 'WRITE', TYPE, 0, AIO_FID, STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL MSG_SETC( 'FILE', DEVICE )
          CALL ERR_REP( ' ', 'Unable to create output file /^FILE/',
     :                  STATUS )
          GOTO 99
        END IF

*      Set mode
        ID = AIO__M_FILE
        AIO_WIDTH = 132

      END IF

*    Set output width
      AIO_MODE = ID
      WIDTH = AIO_WIDTH

*    Channel now defined
      AIO_DEF = .TRUE.

*    Tidy up
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'AIO_OPEN', STATUS )
      END IF

      END



*+  AIO_OPEN_REN - Rename existing file to enable write access
      SUBROUTINE AIO_OPEN_REN( FILE, STATUS )
*
*    Description :
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
*     24 Jul 94 : Original. (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Import :
*
      CHARACTER*(*)		FILE			! File to be opened
*
*    Status :
*
      INTEGER STATUS
*
*    Function declarations :
*
      INTEGER			CHR_LEN
*
*    Local variables :
*
      CHARACTER*20              SYSNAME, NODENAME,	! Operating system
     :                          RELEASE, VERSION, 	! description
     :                          MACHINE
      INTEGER			FLEN			! File name length

      LOGICAL			THERE			! File exists?
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Don't bother with this on VMS
      CALL PSX_UNAME( SYSNAME, NODENAME, RELEASE, VERSION, MACHINE,
     :                STATUS )
      IF ( (INDEX(SYSNAME,'VMS').EQ.0) .AND.
     :     (INDEX(SYSNAME,'vms').EQ.0) ) THEN

*      Get length of filename
        FLEN = CHR_LEN(FILE)

*      Does file exist?
        INQUIRE( FILE=FILE(:FLEN), EXIST=THERE )
        IF ( THERE ) THEN

*        Does a backup file already exist?
          INQUIRE( FILE=FILE(:FLEN)//'~', EXIST=THERE )

*        If so, delete it
          IF ( THERE ) THEN
            CALL UTIL_DELETE( FILE(:FLEN)//'~', STATUS )
          END IF

*        Rename old file
          CALL UTIL_RENAME( FILE(:FLEN), FILE(:FLEN)//'~', STATUS )

        END IF

*    End of VMS test
      END IF

      END
