      SUBROUTINE INITDR( STATUS )
*+
*  Name:
*     SUBROUTINE INITDR

*  Purpose:
*     Initialise IUEDR.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL INITDR( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Method:
*     This sets up the basic initialisation for devices.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (Starlink)
*     {enter_new_authors_here}

*  History:
*     01-MAY-82 (JRG):
*       AT4 version.
*     05-JAN-88 (PCTR):
*       IUEDR Vn. 1.4
*     07-SEP-88 (PCTR):
*       IUEDR Vn. 2.0
*       Conversion to FORTRAN.  Conversion to GKS 7.2 graphics.
*     05-MAY-89 (PCTR):
*       IUEDR Vn. 2.1
*       Some restructuring and final conversion to SGP/16 style.
*     19-OCT-94 (MJC):
*       IUEDR Vn. 3.1-9
*     24-JAN-96 (MJC):
*       IUEDR Vn. 3.2-0
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Global Variables:
      INCLUDE 'CMGRAF'
      INCLUDE 'CMPSX'

*  Local Constants:
      INTEGER MAXNAME       ! Maximum device length.
      INTEGER STDLOG        ! Log file logical unit.
      INTEGER STDOUT        ! Output logical unit.
      PARAMETER ( MAXNAME = 16, STDLOG = 0, STDOUT = 6 )

*  Status:
      INTEGER STATUS        ! Global status.

*  Local Variables:
      CHARACTER*24 CTIME
      CHARACTER*9 TIMEC     ! Time in CHARACTER.
      CHARACTER*17 DATEC    ! Date in CHARACTER.

      BYTE TIMES( 9 )       ! Time in STRING.
      BYTE DATES( 17 )      ! Date in STRING.

      INTEGER I             ! Loop index.
      INTEGER JASF( 13 )    ! GKS aspect source flags.
      INTEGER NCHAR         ! Date length.
      INTEGER NTICKS        ! Time in Processor ticks.
      INTEGER IOSTAT        ! File I/O Status.

      LOGICAL USR           ! Whether there is an interactive user.
      LOGICAL NORED         ! Whether the output has been redirected.
*.

*  Get system details.
      CALL PSX_UNAME( SYSNAME, NODENAME, RELEASE, VERSION, MACHINE,
     :                STATUS )

*  Is there a user?
      CALL PSX_ISATTY( 0, USR, STATUS )

*  Has command output already been re-directed to a file?
      CALL PSX_ISATTY( 1, NORED, STATUS )

*  Set I/O system on a footing.
      CALL PRTSET( USR, (.NOT. NORED ) )

*  Error system comes second.
      CALL ERRSET

      IOSTAT = SAI__OK

*  Interactive Files.
      IF ( USR ) THEN

*      Open the log file if not VMS.
         IF ( SYSNAME.NE.'VMS' .AND. NORED ) THEN
            CALL FIO_GUNIT( LOGFILE, STATUS )
            OPEN ( UNIT = LOGFILE, FILE = 'session.lis',
     :             ACCESS = 'SEQUENTIAL', STATUS = 'UNKNOWN',
     :             IOSTAT = IOSTAT )
            IF ( IOSTAT .NE. SAI__OK ) THEN
               CALL ERROUT( 'Command output file: open error\\',
     :                      STATUS )

            ELSE
               ISLOG = .TRUE.
            END IF

         ELSE
            ISLOG = .FALSE.
         END IF

*  Redirect output to a file instead of the terminal if the input is
*  from a file rather than a terminal and the output has not already
*  been redirected to a file.
      ELSE IF ( NORED ) THEN
         ISLOG = .FALSE.
         OPEN( UNIT = STDOUT, NAME = 'session.lis',
     :         ACCESS = 'SEQUENTIAL', STATUS = 'UNKNOWN',
     :         IOSTAT = IOSTAT )
         CALL PRTSET( USR, .TRUE. )
         IF ( IOSTAT .NE. SAI__OK ) THEN
            CALL ERROUT( 'Command output file: open error\\', STATUS )
         END IF
      END IF

      IF ( STATUS .NE. SAI__OK ) THEN
         GO TO 999
      END IF

*  Set up program internals.

*  Set up graphics.
      CALL STR_TERM( 0, MAXNAME, DVNAME )

*  Initialise SGS.
      CALL SGS_INIT( STDOUT, STATUS )
      IF ( STATUS  .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: initialising SGS graphics\\', STATUS )
         GO TO 999

      ELSE

*     Set GKS aspect source flags to be individual.
         DO I = 1, 13
            JASF( I ) = 1
         END DO

         CALL GSASF( JASF )

*     Initialise IUEDR plotting flags.
         RESET = .TRUE.
         EMPTY = .TRUE.

         CALL GRF_RSGRAF( STATUS )

         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERROUT( 'Error: resetting graphics\\', STATUS )
            GO TO 999
         END IF
      END IF

*  CMFILE and all dataset contents.
      CALL CNDSN
      CALL CNCOMB

*  Print IUEDR header.
      CALL LINE_WCONT( '%p \\' )
      CALL PRTBUF( STATUS )
      CALL LINE_WCONT( '%pWelcome to IUEDR Version 3.2-0.\\' )
      CALL PRTBUF( STATUS )
      CALL LINE_WCONT( '%pRelease: 24th January 1996.\\' )
      CALL PRTBUF( STATUS )
      CALL LINE_WCONT( '%p \\' )
      CALL PRTBUF( STATUS )

*  Also put up the date and time (mainly useful in session.lis).
      CALL PSX_TIME( NTICKS, STATUS )
      CALL PSX_CTIME( NTICKS, CTIME, STATUS )

      TIMEC = CTIME( 12:19 )
      DATEC = CTIME( 1:11 ) // CTIME( 20: )
      CALL GEN_CTOS( TIMEC, 9, TIMES, NCHAR )
      CALL GEN_CTOS( DATEC, 17, DATES, NCHAR )
      CALL LINE_WRITS( '%pDATE=%s.\\', DATES )
      CALL LINE_WRITS( ' TIME=%s.\\', TIMES )
      CALL PRTBUF( STATUS )
      CALL LINE_WCONT( '%p \\' )
      CALL PRTBUF( STATUS )

 999  CONTINUE

      END
