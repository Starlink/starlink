      SUBROUTINE MONO( STATUS )
*+
*  Name:
*     SUBROUTINE MONO

*  Purpose:
*     IUEDR monolith.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MONO( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Parse command lines supplied by user and send to IUEDR for
*     processing.

*  Authors:
*     DMILLS: Dave Mills (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     ??-???-?? (DMILLS):
*       IUEDR Vn. 3.0
*     13-OCT-94 (MJC):
*       IUEDR Vn. 3.1-7
*       Modified to support lowercase in parameter values.
*     16-DEC-94 (MJC):
*       Script input support.
*     28-JAN-95 (MJC):
*       IUEDR Vn. 3.2
*       "Hidden" command to toggle parameter logging.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'CHR_ERR'

*  Global Variables:
      INCLUDE 'CMDYN'
      INCLUDE 'CMPSX'
      INCLUDE 'CMPRT'

      LOGICAL ACTIVE
      COMMON / REMSTATE / ACTIVE

*  Local Constants:
      INTEGER STDOUT                  ! Output logical unit.
      INTEGER BUF_LEN
      PARAMETER ( STDOUT = 6, BUF_LEN = 400 )

*  Status:
      INTEGER STATUS                  ! Global Status.

*  External References:
      INTEGER CHR_LEN

*  Local Variables:
      CHARACTER*80 ACTION
      CHARACTER*( BUF_LEN ) COMMAND   ! The users command input.
      CHARACTER*( BUF_LEN ) ACT_COMM  ! Un-processed command line.

      CHARACTER TAB

      INTEGER COMMLEN
      INTEGER I
      INTEGER J
      INTEGER CODE
      INTEGER FIOSTAT
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set-up TAB character
      TAB = CHAR( 9 )

      IF ( .NOT. ACTIVE ) THEN
         CALL INITDR( STATUS )
         ACTIVE = .TRUE.
         DO I = 1, 32
            DFREE( I ) = .TRUE.
         END DO
      END IF

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL LINE_WCONT( '%p Problems encountered during \\' )
         CALL LINE_WCONT( '%p initialisation, IUEDR aborting.\\' )
         CALL PRTBUF( STATUS )
         GO TO 999
      END IF

      CALL SUBPAR_DEACT( 'A', STATUS )
      ACTION = ' '
      DO WHILE ( ACTION.NE.'EXIT' .AND. ACTION.NE.'QUIT' )

*     Get a user input line or line from script input.
         IF ( PRTUSR ) THEN
            COMMAND = ' '
            CALL GETINP( 1, COMMAND, '> ', STATUS )

         ELSE
            READ( *, '( A )', END = 997 ) COMMAND
         END IF
         ACT_COMM = COMMAND
         COMMLEN = CHR_LEN( ACT_COMM )

*     Write line to the session log file.
         IF ( ISLOG ) THEN
            WRITE ( LOGFILE, '( A, A )' ) '> ', ACT_COMM( : COMMLEN )

         ELSE
            WRITE ( STDOUT, '( A )' ) ACT_COMM( : COMMLEN )
         END IF
         CALL CHR_UCASE( COMMAND )

*     Step over leading whitespace.
         J = 1
         DO WHILE ( COMMAND( J:J ).EQ.' ' .OR. COMMAND( J:J ).EQ.TAB )
            J = J + 1
         END DO

*     No command on line so ignore it.
         IF ( J .GE. BUF_LEN ) THEN
            GO TO 200
         END IF

*     Point into command line after the command name itself.
         I = J
         DO WHILE ( COMMAND( I : I ) .NE. ' ' )
            I = I + 1
         END DO

*     This is the name of the command given.
         ACTION = COMMAND( J:I - 1 )

*     Act on specified command.
	 IF ( ACTION .EQ. 'HELP' ) THEN
            CALL CMD_HLP( .FALSE., COMMAND( I + 1 : ), ' ' )

         ELSE IF ( ACTION .EQ. 'SPAWN' ) THEN
            CALL USR_SPAWN( ACT_COMM( I + 1 : ), STATUS )

         ELSE IF ( ACTION .EQ. 'CONFIGURE' ) THEN
            PARRED = ( .NOT. PARRED )

*     The IUEDR3 subroutine deals with acting on most commands.
         ELSE IF ( ACTION.NE.'EXIT' .AND. ACTION.NE.'QUIT' ) THEN
            CALL SUBPAR_FINDACT( ACTION, CODE, STATUS )
            CALL SUBPAR_CMDLINE( CODE, 3, ACT_COMM( I + 1 : ), STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_FLUSH( STATUS )

            ELSE
               CALL IUEDR3( ACTION, STATUS )
               CALL SUBPAR_DEACT( 'A', STATUS )
            END IF

         ELSE
            CALL SUBPAR_FINDACT( 'IUEDR3', CODE, STATUS )
            CALL SUBPAR_CMDLINE( CODE, 3, ACT_COMM( I + 1 : ), STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_FLUSH( STATUS )
            END IF
            GO TO 998
         END IF
  200    CONTINUE
      END DO

  997 CONTINUE
      CALL SUBPAR_FINDACT( 'IUEDR3', CODE, STATUS )
      CALL SUBPAR_CMDLINE( CODE, 3, ' ', STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_FLUSH( STATUS )
      END IF
      CALL LINE_WCONT( '%p \\' )
      CALL PRTBUF( STATUS )
      CALL LINE_WCONT( '%p!  End of Script\\' )
      CALL PRTBUF( STATUS )

  998 CONTINUE
      CALL CLOSDR
      IF ( ISLOG ) THEN
         CLOSE ( UNIT=LOGFILE )
         FIOSTAT = SAI__OK
         CALL FIO_PUNIT( LOGFILE, FIOSTAT )
      END IF

  999 CONTINUE

      END
