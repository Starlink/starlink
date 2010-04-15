      SUBROUTINE COMNDS( PARAMS, OWNER, LOCAL, SYSTEM, STATUS )
*+
*  Name:
*     COMNDS

*  Purpose:
*     Displays lists of commands with brief descriptions

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COMNDS( PARAMS, OWNER, LOCAL, SYSTEM, STATUS )

*  Description:
*     Lists of commands with associated brief descripions and
*     classification information are read from files called
*     "command.hlp" in each of the supplied directories in turn
*     (if such files exist). Commands which match the criteria supplied
*     in PARAMS are displayed on the screen, with the associated brief
*     descriptions.
*
*     In order to maintain compatibility with the previous version of
*     the "COMMANDS" command, a search is first made for an old-style
*     "command.lis" file in the local dipso directory. If such a file is
*     found, a warning is issued, and its contents displayed without
*     processing. If this happens, no attempt is made to use any new
*     style "command.hlp" files which may exist.

*  Arguments:
*     PARAMS = CHARACTER * ( * ) (Given)
*        The command arguments supplied by the user.
*     OWNER = CHARACTER * ( * ) (Given)
*        The path to the OWNERDIR directory, including a trailing "/"
*        on Unix systems.
*     LOCAL = CHARACTER * ( * ) (Given)
*        The path to the LDIPSODIR directory, including a trailing "/"
*        on Unix systems.
*     SYSTEM = CHARACTER * ( * ) (Given)
*        The path to the DIPSODIR directory, including a trailing "/"
*        on Unix systems.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     5-MAY-1995 (DSB):
*        Original version.
*     11-JUN-1996 (DSB):
*        Fixed bug which resulted in no attempt being made to find a
*        command.hlp file if LDIPSODIR is defined, even if it it does
*        not contain command.lis.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER PARAMS*(*)
      CHARACTER OWNER*(*)
      CHARACTER LOCAL*(*)
      CHARACTER SYSTEM*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Constants:
      INTEGER FW                 ! Field width in a plain list
      PARAMETER ( FW = 10 )

*  Local Variables:
      CHARACTER
     :     CLIST*30,             ! Class list
     :     COMND*30,             ! Command name
     :     FILE*256,             ! Full spec. of command file
     :     RECORD*256,           ! Input record buffer
     :     RECUP*256,            ! Upper case version of input record buffer
     :     TYPE( 3 )*6,          ! Command types
     :     OUT*80                ! Output buffer.

      INTEGER
     :     CLLEN,                ! Length of class list
     :     CMLEN,                ! Length of command name
     :     I,                    ! Directory count
     :     IOSTAT,               ! Fortran I/O status
     :     IOUT,                 ! Next free cell in output buffer
     :     IPOSN,                ! Index of search word
     :     NMATCH,               ! No. of matching commands
     :     TLEN( 3 ),            ! Length of each command type
     :     WLEN                  ! Length of search word

      LOGICAL
     :     SOME,                 ! Have any command files been found?
     :     MATCH,                ! Does current command match class list?
     :     SEARCH,               ! Search for a given word?
     :     THERE                 ! Does the command file exist?

*  Local Initialisations:
      DATA TYPE / 'Owner', 'Local', 'System' /,
     :     TLEN /       5,       5,        6 /
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the paged screen output routine, and the output buffer.
      CALL PRPAGE( 'COMMANDS', ' ', .TRUE., STATUS )
      IF( STATUS .NE. SAI__OK ) GO TO 999

      OUT = ' '
      IOUT = 1

*  Initialise a flag to indicate that no commands file has yet been
*  found.
      THERE = .FALSE.

*  Check to see if an old-style "commands.lis" file exists in the local
*  dipso directory. If it does, issue a warning message and display the
*  contents of the file.
      IF( LOCAL .NE. ' ' ) THEN
         FILE = LOCAL//'command.lis'

*  See if the file exists.
         INQUIRE ( FILE = FILE, EXIST = THERE, IOSTAT = IOSTAT )
         IF( IOSTAT .NE. 0 ) THEN
            STATUS = SAI__ERROR
            CALL ERR_FIOER( 'ERR', IOSTAT )
            CALL MSG_SETC( 'F', FILE )
            CALL ERR_REP( 'COMNDS_ERR1', 'Error inquiring ' //
     :           'existence of file ''^F'': ^ERR', STATUS )
            GO TO 999
         END IF

*  If it does, open it on unit 77 (first attempt to close it in case it
*  was left open last time).
         IF( THERE ) THEN
            CLOSE( UNIT = 77, IOSTAT = IOSTAT )
            OPEN ( UNIT = 77, FILE = FILE, STATUS = 'OLD',
     :           IOSTAT = IOSTAT )
            IF( IOSTAT .NE. 0 ) THEN
               STATUS = SAI__ERROR
               CALL ERR_FIOER( 'ERR', IOSTAT )
               CALL MSG_SETC( 'F', FILE )
               CALL ERR_REP( 'COMNDS_ERR2', 'Error opening file' //
     :              '''^F'': ^ERR', STATUS )
               GO TO 999
            END IF

*  Tell the user where we are looking:
            CALL PRPAGE( 'COMMANDS', ' ', .FALSE., STATUS )
            CALL PRPAGE( 'COMMANDS', 'WARNING: An old style ' //
     :                   '''commands.lis'' has been found in the ' //
     :                   'local DIPSO directory, and will be used.',
     :                   .FALSE., STATUS )

*  Warn the user that searches and classes are not supported by
*  old-style files.
            IF( PARAMS .NE. ' ' ) THEN
               CALL PRPAGE( 'COMMANDS', ' ', .FALSE., STATUS )
               CALL PRPAGE( 'COMMANDS', 'Word seacrhes and command '//
     :                      'classifications are not available using '//
     :                      'old style ''commands.lis'' files.',
     :                      .FALSE., STATUS )
            END IF

*  Abort if an error has occurred.
            IF( STATUS .NE. SAI__OK ) GO TO 999

*  Read the next record. Jump to label 20 when the end of file is
*  reached.
 10         READ ( 77, '(A)', IOSTAT = IOSTAT, END = 20 ) RECORD
            IF( IOSTAT .NE. 0 ) THEN
               STATUS = SAI__ERROR
               CALL ERR_FIOER( 'ERR', IOSTAT )
               CALL MSG_SETC( 'F', FILE )
               CALL ERR_REP( 'COMNDS_ERR3', 'Error reading file' //
     :                       '''^F'': ^ERR', STATUS )
               GO TO 999
            END IF

*  Display the record, pausing at the end of each page-full.
            CALL PRPAGE( 'COMMANDS', RECORD, .FALSE., STATUS )

*  Go back for the next record.
            GO TO 10

*  Arrive here when the end-of-file has been reached.
 20         CONTINUE

*  Close the file.
            CLOSE ( UNIT = 77, IOSTAT = IOSTAT )
            IF( IOSTAT .NE. 0 ) THEN
               STATUS = SAI__ERROR
               CALL ERR_FIOER( 'ERR', IOSTAT )
               CALL MSG_SETC( 'F', FILE )
               CALL ERR_REP( 'COMNDS_ERR4', 'Error closing file' //
     :                       '''^F'': ^ERR', STATUS )
               GO TO 999
            END IF

         END IF

      END IF

*  If no local commands.lis file was found, use the new-style "commands.hlp"
*  file.
      IF( .NOT. THERE ) THEN

*  See if a search word has been given (as indicated by the first
*  character being a minus sign). If it has, set a flag and remove
*  the minus sign.
         CALL CHR_LDBLK( PARAMS )
         IF( PARAMS( 1 : 1 ) .EQ. '-' .AND.
     :       PARAMS( 2 : ) .NE. ' ' ) THEN
            SEARCH = .TRUE.
            PARAMS( 1 : 1 ) = ' '
            CALL CHR_LDBLK( PARAMS )
            CALL CHR_UCASE( PARAMS )
            WLEN = CHR_LEN( PARAMS )
         ELSE
            SEARCH = .FALSE.
         END IF

*  Set a flag saying that no command information has been found yet.
         SOME = .FALSE.

*  Do the owner, local and system commands in turn.
         DO I = 1, 3

*  Get the name of the file containing the command classifications and
*  descriptions. Store a blank file name if the directory is not
*  defined.
            FILE = ' '
            IF( I .EQ. 1 ) THEN
               IF( OWNER .NE. ' ' ) FILE = OWNER//'command.hlp'

            ELSE IF( I .EQ. 2 ) THEN
               IF( LOCAL .NE. ' ' ) FILE = LOCAL//'command.hlp'

            ELSE
               IF( SYSTEM .NE. ' ' ) FILE = SYSTEM//'command.hlp'

            END IF

*  If the directory is defined...
            IF( FILE .NE. ' ' ) THEN

*  See if the file exists.
               INQUIRE ( FILE = FILE, EXIST = THERE, IOSTAT = IOSTAT )
               IF( IOSTAT .NE. 0 ) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_FIOER( 'ERR', IOSTAT )
                  CALL MSG_SETC( 'F', FILE )
                  CALL ERR_REP( 'COMNDS_ERR5', 'Error inquiring ' //
     :                 'existence of file ''^F'': ^ERR', STATUS )
                  GO TO 999
               END IF

*  If it does, open it on unit 77 (first attempt to close it in case it
*  was left open last time).
               IF( THERE ) THEN
                  CLOSE( UNIT = 77, IOSTAT = IOSTAT )
                  OPEN ( UNIT = 77, FILE = FILE, STATUS = 'OLD',
     :                 IOSTAT = IOSTAT )
                  IF( IOSTAT .NE. 0 ) THEN
                     STATUS = SAI__ERROR
                     CALL ERR_FIOER( 'ERR', IOSTAT )
                     CALL MSG_SETC( 'F', FILE )
                     CALL ERR_REP( 'COMNDS_ERR6', 'Error opening file'//
     :                             ' ''^F'': ^ERR', STATUS )
                     GO TO 999
                  END IF

*  Tell the user where we are looking:
                  CALL PRPAGE( 'COMMANDS', ' ', .FALSE., STATUS )

                  IF( I .LT. 3 .OR. SOME ) THEN
                     CALL PRPAGE( 'COMMANDS', '  '//
     :                    TYPE( I )( : TLEN( I ) )//' commands:',
     :                    .FALSE., STATUS )
                  END IF

*  Abort if an error has occurred.
                  IF( STATUS .NE. SAI__OK ) GO TO 999

*  Indicate that a command file has been found.
                  SOME = .TRUE.

*  Initialise the number of commands which match the specified class
*  list.
                  NMATCH = 0

*  Read the next record. Jump to label 40 when the end of file is
*  reached.
 30               READ ( 77, '(A)', IOSTAT = IOSTAT, END = 40 ) RECORD
                  IF( IOSTAT .NE. 0 ) THEN
                     STATUS = SAI__ERROR
                     CALL ERR_FIOER( 'ERR', IOSTAT )
                     CALL MSG_SETC( 'F', FILE )
                     CALL ERR_REP( 'COMNDS_ERR7', 'Error reading file'//
     :                             ' ''^F'': ^ERR', STATUS )
                     GO TO 999
                  END IF

*  Convert tabs to spaces, and remove all unprintable characters.

                  CALL CHR_TRCHR( CHAR( 9 ), ' ', RECORD, STATUS )
                  CALL CHR_CLEAN( RECORD )

*  Skip blank records.
                  IF( RECORD .EQ. ' ' ) GO TO 30

*  Remove leading blanks from the record.
                  CALL CHR_LDBLK( RECORD )

*  Skip comment records.
                  IF( RECORD( 1 : 1 ) .EQ. '#' ) GO TO 30

*  Find the end of the first word (the command name).  Remove any bad
*  status value set by CHR_FIWE (CHR does not use ERR and so ERR_ANNUL
*  need not be called).
                  CMLEN = 1
                  CALL CHR_FIWE( RECORD, CMLEN, STATUS )
                  IF( STATUS .NE. SAI__OK ) STATUS = SAI__OK

*  Store the command name in upper case, and remove it from the record
*  buffer.
                  COMND = RECORD( : CMLEN )
                  CALL CHR_UCASE( COMND )
                  RECORD( : CMLEN ) = ' '
                  CALL CHR_LDBLK( RECORD )

*  Now extract the second word in the record (which has become the
*  first word in the buffer). This gives the list of classes to which
*  the command belongs. Remove the word from the buffer once it has
*  been stored. Any text then remaining in the buffer is the command
*  description. If there is no second word, use a null class list.
                  IF( RECORD .NE. ' ' ) THEN
                     CLLEN = 1
                     CALL CHR_FIWE( RECORD, CLLEN, STATUS )
                     IF( STATUS .NE. SAI__OK ) STATUS = SAI__OK

                     CLIST = RECORD( : CLLEN )
                     CALL CHR_UCASE( CLIST )
                     RECORD( : CLLEN ) = ' '
                     CALL CHR_LDBLK( RECORD )

                  ELSE
                     CLIST = '-'
                     CLLEN = 1
                  END IF

*  If a plain list of all commands is required...
                  IF( PARAMS .EQ. ' ' ) THEN

*  If there is insufficient room in the output buffer for the new
*  command name, write out the buffer (pausing after each page-full),
*  and reset the index of the next free cell.
                     IF( IOUT + CMLEN - 1 .GT. LEN( OUT ) ) THEN
                        CALL PRPAGE( 'COMMANDS', OUT, .FALSE., STATUS )
                        IF( STATUS .NE. SAI__OK ) GO TO 999
                        OUT = ' '
                        IOUT = 1
                     END IF

*  Append the command name to the output buffer, and increment the
*  index for the start of the next command to the start of the next
*  unoccupied column.
                     OUT( IOUT : ) = COMND
                     IOUT = FW*INT( ( IOUT + CMLEN - 1 )/FW ) + FW + 1

*  Increment the number of matching commands.
                     NMATCH = NMATCH + 1

*  If names and descriptions of commands matching the supplied class
*  lists are required...
                  ELSE

*  If a search for a given word is being performed see if the word is
*  contained in the command description (case insensitive).
                     IF( SEARCH ) THEN

                        RECUP = RECORD
                        CALL CHR_UCASE( RECUP )

                        IF( INDEX( RECUP, PARAMS( : WLEN ) ) .GT. 0 )
     :                                                              THEN
                           MATCH = .TRUE.

*  If the word was not found in the command description, compare it to
*  the command name.
                        ELSE
                           MATCH = ( INDEX( COMND( : CMLEN ),
     :                          PARAMS( : WLEN ) ) .GT. 0 )
                        END IF

*  Otherwise, see if this command matches the supplied class lists.
                     ELSE
                        CALL CMATCH( CLIST( : CLLEN ), PARAMS, MATCH )

                     END IF

*  If the command matches the given criteria...
                     IF( MATCH ) THEN

*  Construct the output buffer holding the command name followed by the
*  command's description, and print it.
                        OUT = ' '
                        OUT = COMND( : CMLEN )
                        IF( RECORD .NE. ' ' ) THEN
                           OUT( FW*INT( CMLEN/FW ) + FW + 1 : ) =
     :                          ' - '//RECORD
                        END IF

                        CALL PRPAGE( 'COMMANDS', OUT, .FALSE., STATUS )

*  Increment the number of matching commands.
                        NMATCH = NMATCH + 1

                     END IF

                  END IF

*  Close the file and abort if an error has occurred.
                  IF( STATUS .NE. SAI__OK ) THEN
                     CLOSE ( UNIT = 77 )
                     GO TO 999
                  END IF

*  Read the next record.
                  GO TO 30

*  Jump to to here when the end of file has been reached.
 40               CONTINUE

*  Close the file.
                  CLOSE ( UNIT = 77, IOSTAT = IOSTAT )
                  IF( IOSTAT .NE. 0 ) THEN
                     STATUS = SAI__ERROR
                     CALL ERR_FIOER( 'ERR', IOSTAT )
                     CALL MSG_SETC( 'F', FILE )
                     CALL ERR_REP( 'COMNDS_ERR8', 'Error closing file'//
     :                             ' ''^F'': ^ERR', STATUS )
                     GO TO 999
                  END IF

*  Flush the output buffer if required.
                  IF( IOUT .GT. 1 ) THEN
                     CALL PRPAGE( 'COMMANDS', OUT, .FALSE., STATUS )
                     IF( STATUS .NE. SAI__OK ) GO TO 999
                     OUT = ' '
                     IOUT = 1
                  END IF

*  Issue a message if no commands were found.
                  IF( NMATCH .EQ. 0 ) THEN
                     CALL PRPAGE( 'COMMANDS', '    <no commands found'//
     :                    ' in the given classes>', .FALSE.,
     :                    STATUS )
                     IF( STATUS .NE. SAI__OK ) GO TO 999
                  END IF

               END IF

            END IF

         END DO

*  If no command files were found, repoort an error.
         IF( .NOT. SOME .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'COMNDS_ERR9', 'No command classification ' //
     :           'information found.', STATUS )
         END IF

      END IF

*  Jump to here if an error has occurred.
 999  CONTINUE

      END
