      SUBROUTINE HISTORY( STATUS )
*+
*  Name:
*     HISTORY

*  Purpose:
*     Display, delete or add NDF history information.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL HISTORY( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*
*     IN UNIX THE USER CAN ONLY USE THIS APPLICATION BY TYPING
*     iras90_history TO AVIOD CONFLICT WITH UNIX history COMMAND
*
*     This routine allows several operations to be performed on the
*     history information stored within a group of NDFs...
*
*     1) Existing history records can be displayed (specify using
*     FUNCTION=DISPLAY). The history is displayed in pages on the
*     terminal screen, and optionally logged to a text file. The
*     history records to be displayed can be selected on the basis of
*     their creation dates (see parameters SINCE and BEFORE), or on the
*     basis of the command which created the history (see parameter
*     COMMAND).
*
*     2) All history information can be deleted (specify using
*     FUNCTION=ERASE). There is no option to delete selected history
*     records.
*
*     3) Additional history records can be appended. The text is
*     specified either by the contents of a text file or by typing at
*     the keyboard. The same text is used for each specified NDF.
*     (Specify using FUNCTION=ADD)
*
*     The required operation is performed on each of the NDFs specified
*     by parameter NDF in turn.  There is an option for the user to be
*     re-prompted for further operations once the first has been
*     completed (see parameter LOOP).  The default behaviour is to to
*     display all history records from the supplied NDFs and then exit.

*  Usage:
*     HISTORY NDF [FUNCTION] [LOGFILE]

*  ADAM Parameters:
*     BEFORE = LITERAL (Read)
*        Only those history records created before the date and time
*        specified by BEFORE are displayed. See help on
*        "Time_and_date_strings" for more information on specifying
*        times and dates within IRAS90. A null or blank value results
*        in BEFORE being ignored. If LOOP is false then the run time
*        default is a null value, otherwise there is no run time
*        default.                                                     []
*     COMMAND = LITERAL (Read)
*        The COMMAND parameter can be used to select which history
*        records are to be displayed on the basis of the application
*        which created them. If the string given for COMMAND can be
*        found as a sub-string within the name of the application which
*        created the history record, then the record is displayed. The
*        names of all IRAS90 applications are stored in the form
*        IRAS90:xxx, where xxx is the name of the application. Thus if
*        COMMAND was given the value "IRAS90:", then only those history
*        records which were created by IRAS90 applications would be
*        displayed. A null value causes all history records to be
*        displayed (subject to any other selection criteria). If LOOP
*        is false then the run time default is a null value, otherwise
*        there is no run time default.                                []
*     FUNCTION = LITERAL (Read)
*        The operation to be performed. Options are DISPLAY, ERASE or
*        ADD. If LOOP has a false value then the run time default is
*        'DISPLAY', otherwise there is no run time default.           []
*     LOGFILE = LITERAL (Write)
*        The name of a text file to receive a copy of any displayed
*        history records. The run time default is for no log file to be
*        produced.                                                   [!]
*     LOOP = _LOGICAL (Read)
*        If true then the user is re-prompted for a new value for
*        FUNCTION once the previously requested function has been
*        performed. If false, then the application exits once the first
*        function has been performed.                            [FALSE]
*     MSG_FILTER = LITERAL (Read)
*        The level of information displayed on the users screen. This
*        should take one of the values QUIET, NORMAL or VERBOSE (see
*        help on "Message_filtering"). Output to the log file is not
*        effected by the setting of this parameter.
*                                       [current message filter setting]
*     NDF = NDF (Update)
*        The group of NDFs to process. This should be in the form of a
*        group expression (see help on "Group_expressions").
*     SINCE = LITERAL (Read)
*        Only those history records which were created since the date
*        and time specified by SINCE are displayed. See help on
*        "Time_and_date_strings" for more information on specifying
*        times and dates within IRAS90. A null or blank value results
*        in SINCE being ignored. If LOOP is false then the run time
*        default is a null value, otherwise there is no run time
*        default.                                                     []
*     TEXT = LITERAL (Read)
*        Text to be used to form a new history record. If the supplied
*        text terminates with a minus sign, then the user is re-prompted
*        for more text (the minus sign is removed before the text is
*        stored in the NDF). The name of an existing text file can be
*        specified, preceeded with an up-arrow character (^), in which
*        case the text will be read from the text file. If more than
*        one NDF is being processed, the same text is used for each NDF.

*  Examples:
*     HISTORY CENA
*        This will display all the history information contained within
*        the NDF CENA on the terminal screen.
*     HISTORY CENA COMMAND=MAPCRDD
*        This will display all the history information within CENA
*        which was created by the MAPCRDD application.
*     HISTORY M51* ERASE
*        This will remove all history information from all NDFs with
*        names which start with the string "M51".
*     HISTORY "CENA,M82" LOGFILE=HISTORY MSG_FILTER=QUIET
*        This will write all the history information contained within
*        the two NDFs CENA and M82 to the text file HISTORY.DAT, but
*        will display nothing on the screen.
*     HISTORY M51 LOOP=Y SINCE=FEB-12 BEFORE=FEB-16
*        This will display all the history records from M51 which were
*        create between the 12th and 16th of february (this year), and
*        then reprompt the user for a new operation.
*     HISTORY M51 ADD TEXT="So far so good"
*        This will append a new history record to M51, containing the
*        text "So far so good".
*     HISTORY M51 ADD TEXT=^NEWHIST.LIS
*        This will append a new history record to M51, taking the
*        history text from the file NEWHIST.LIS.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-JAN-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ constants
      INCLUDE 'PAR_ERR'          ! PAR_ error values.
      INCLUDE 'PRM_PAR'          ! Standard Starlink constants.
      INCLUDE 'MSG_PAR'          ! MSG_ constants.
      INCLUDE 'GRP_PAR'          ! GRP_ constants.

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Returns used length of a string.

*  Local Variables:
      INTEGER ADDED              ! No. of items added to the group as a
                                 ! result of the last group expression.
      CHARACTER BEFORE*25        ! Value of parameter BEFORE.
      CHARACTER COMMND*80        ! The value of the COMMAND parameter.
      INTEGER EXTSIZ             ! Value of EXTEND_SIZE component.
      INTEGER FD                 ! File descriptor for log file.
      LOGICAL FLAG               ! True if a group expression was flagged.
      CHARACTER FUNC*7           ! Required function.
      CHARACTER HLOC*(DAT__SZLOC)! Locator to the HISTORY structure.
      INTEGER I                  ! Loop count.
      INTEGER IGRP1              ! Identifier for the NDF group.
      INTEGER IGRP2              ! Identifier for text group.
      INTEGER LCMD               ! Used length of COMMND.
      CHARACTER LFUNC*7          ! Lower case version of FUNC.
      INTEGER LNDFNM             ! Used length of NDFNAM.
      LOGICAL LOGPOS             ! True if displayed history islogged to
                                 ! a text file.
      LOGICAL LOOP               ! True if user is to be re-prompted for
                                 ! further operations.
      DOUBLE PRECISION MJDBEF    ! Modified Julian Date corresponding
                                 ! to BEFORE.
      DOUBLE PRECISION MJDSIN    ! Modified Julian Date corresponding
                                 ! to SINCE.
      INTEGER NDFIN              ! Identifier for input NDF.
      CHARACTER NDFLOC*(DAT__SZLOC)! Locator to top level NDF structure.
      CHARACTER NDFNAM*(GRP__SZNAM)! Name of current NDF.
      INTEGER NEXT               ! Index of next history record to be
                                 ! written.
      INTEGER NIN                ! No. of NDFs to be processed.
      INTEGER NRECS              ! The current size of the RECORDS
                                 ! structure.
      INTEGER NTREC              ! No. of lines of text stored in the
                                 ! text group.
      LOGICAL OK                 ! True if a record is to be displayed.
      LOGICAL ON                 ! True if screen output is enabled.
      INTEGER REC                ! The current record index to display
                                 ! or erase.
      CHARACTER RLOC*(DAT__SZLOC)! Locator to the RECORDS structure.
      CHARACTER SINCE*25         ! Value of parameter SINCE.
      LOGICAL SOME               ! True if any history records satified
                                 ! the selection criteria.
      LOGICAL THERE              ! True if the NDF has a defined HISTORY
                                 ! component.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      LCMD = 0

*  Establish the conditional message filter level using parameter
*  MSG_LEVEL.
      CALL MSG_IFGET( STATUS )

*  Get a value for parameter LOOP.
      CALL PAR_GET0L( 'LOOP', LOOP, STATUS )

*  Get a group containing the names of the NDFs to be processed.
      CALL IRM_RDNDF( 'NDF', 0, 1, '  Give more NDF names...', IGRP1,
     :                NIN, STATUS )

*  Abort if an error has been reported, or if there are no NDFs to
*  process.
      IF ( STATUS .NE. SAI__OK .OR. NIN .EQ. 0 ) GO TO 999

*  If required open a log file. The file descriptor returned in FD is
*  used to access this file.
      CALL IRM_ASFIO( 'LOGFILE', 'WRITE', 'LIST', 80, FD, LOGPOS,
     :                 STATUS )
      IF( STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )

*  Tell the user that output is being logged to the text file.
      IF( LOGPOS ) THEN
         CALL MSG_BLANKIF( MSG__NORM, STATUS )
         CALL MSG_OUTIF( MSG__NORM, 'HISTORY_MSG1',
     :               '  Logging displayed history to $LOGFILE', STATUS )
      END IF

*  Loop back to here if multiple operations are performed.
 10   CONTINUE

*  Get a value for parameter FUNCTION.  If only a single operation is
*  being performed, establish DISPLAY as a dynamic default for
*  parameter FUNCTION, otherwise don't specify any dynamic default.
      IF( LOOP ) THEN
         CALL PAR_CHOIC( 'FUNCTION', ' ', 'DISPLAY,ADD,ERASE', .FALSE.,
     :                   FUNC, STATUS )
         CALL PAR_CANCL( 'FUNCTION', STATUS )
      ELSE
         CALL PAR_CHOIC( 'FUNCTION', 'DISPLAY', 'DISPLAY,ADD,ERASE',
     :                   .FALSE., FUNC, STATUS )
      END IF

*  Abort if an error has occured.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  If history records are to be displayed,  get a value for parameter
*  SINCE, using a blank dynamic default if LOOP is false.
      IF( FUNC .EQ. 'DISPLAY' ) THEN
         IF( .NOT. LOOP ) CALL PAR_DEF0C( 'SINCE', ' ', STATUS )
         CALL IRM_GETTD( 'SINCE', SINCE, MJDSIN, STATUS )
         IF( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            SINCE = ' '
         END IF
         IF( LOOP ) CALL PAR_CANCL( 'SINCE', STATUS )
         IF( SINCE .EQ. ' ' ) MJDSIN = VAL__MIND

*  Do the same for parameter BEFORE.
         IF( .NOT. LOOP ) CALL PAR_DEF0C( 'BEFORE', ' ', STATUS )
         CALL IRM_GETTD( 'BEFORE', BEFORE, MJDBEF, STATUS )
         IF( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            BEFORE = ' '
         END IF
         IF( LOOP ) CALL PAR_CANCL( 'BEFORE', STATUS )
         IF( BEFORE .EQ. ' ' ) MJDBEF = VAL__MAXD

*  Do the same for parameter COMMAND. Convert the string to upper case.
         IF( .NOT. LOOP ) CALL PAR_DEF0C( 'COMMAND', ' ', STATUS )
         CALL PAR_GET0C( 'COMMAND', COMMND, STATUS )
         IF( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            COMMND = ' '
         END IF
         IF( LOOP ) CALL PAR_CANCL( 'COMMAND', STATUS )
         CALL CHR_UCASE( COMMND )
         LCMD = MAX( 1, CHR_LEN( COMMND ) )

*  If a record is to be added to the NDFs, create a group holding the
*  text.
      ELSE IF( FUNC .EQ. 'ADD' ) THEN

*  Create the group.
         CALL GRP_NEW( 'History text', IGRP2, STATUS )

*  Ensure the group is case sensitive.
         CALL GRP_SETCS( IGRP2, .TRUE., STATUS )

*  Suppress checks for the DELIMITER and COMMENT control characters by
*  setting them to the NULL value. This is in order to stop each line
*  of text being split up by any commas included in the text, and to
*  stop strings beginning with # being ignored.
         CALL GRP_SETCC( IGRP2, 'NUL,DEL,COM', '%%%', STATUS )

*  Get the text.
         FLAG = .TRUE.

         DO WHILE( FLAG .AND. STATUS .EQ. SAI__OK )
            CALL GRP_GROUP( 'TEXT', GRP__NOID, IGRP2, NTREC, ADDED,
     :                       FLAG, STATUS )
            CALL PAR_CANCL( 'TEXT', STATUS )
         END DO

         IF( STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )

*  Warn the user if no text has been supplied.
         IF( NTREC .EQ. 0 ) THEN
            CALL MSG_OUTIF( MSG__NORM, 'HISTORY_MSG2',
     :                     '    No new history text has been supplied',
     :                      STATUS )
            GO TO 998
         END IF

      END IF

*  Abort if an error has occured.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Indicate that a new screen page is to be started.
      CALL IRM_PAGE

*  Loop round each NDF to be processed.
      DO I = 1, NIN

*  Get an NDF identifier for the input NDF.
         CALL NDG_NDFAS( IGRP1, I, 'UPDATE', NDFIN, STATUS )

*  Tell the user which NDF is currently being procesed.
         CALL HISTC0( ' ', LOGPOS, FD, STATUS )

         CALL GRP_GET( IGRP1, I, 1, NDFNAM, STATUS )
         LNDFNM = MAX( 1, CHR_LEN( NDFNAM ) )
         CALL MSG_SETC( 'NDF', NDFNAM )
         CALL HISTC0( '  Processing ^NDF ...', LOGPOS, FD, STATUS )

*  Get a locator to the top level of the NDF.
         CALL NDF_LOC( NDFIN, 'UPDATE', NDFLOC, STATUS )

*  Get a locator to any existing HISTORY structure.
         CALL HISTA0( NDFLOC, THERE, HLOC, RLOC, EXTSIZ, NEXT, NRECS,
     :                STATUS )

*  If the NDF contains no  history records, the only allowed function
*  is ADD.
         IF( NEXT .EQ. 1 ) THEN
            IF( FUNC .EQ. 'ADD' ) THEN

*  If there is currently no HISTORY structure within the NDF, create
*  one.
               IF( .NOT. THERE ) THEN
                  CALL HISTB0( NDFLOC, HLOC, RLOC, EXTSIZ, NEXT, NRECS,
     :                         STATUS )
                  THERE = .TRUE.
               END IF

*  Append a history record to the end of the structure.
               CALL HISTA1( IGRP2, EXTSIZ, HLOC, RLOC, NRECS, NEXT,
     :                      STATUS )

*  Tell the user that the record has been created.
               CALL MSG_SETI( 'REC', NEXT - 1 )
               CALL HISTC0( '    Record ^REC created', LOGPOS, FD,
     :                      STATUS )

*  Give a warning if any other function was selected.
            ELSE
               LFUNC = FUNC
               CALL CHR_LCASE( LFUNC )
               CALL MSG_SETC( 'FUN', LFUNC )
               CALL HISTC0('    The NDF contains no history to ^FUN',
     :                     LOGPOS, FD, STATUS )
            END IF

*  If there are history records within the NDF...
         ELSE

*  If ADD was selected, append a history record to the end of the
*  structure.
            IF( FUNC .EQ. 'ADD' ) THEN
               CALL HISTA1( IGRP2, EXTSIZ, HLOC, RLOC, NRECS, NEXT,
     :                      STATUS )

*  Tell the user that the record has been created.
               CALL MSG_SETI( 'REC', NEXT - 1 )
               CALL HISTC0( '    Record ^REC created', LOGPOS, FD,
     :                      STATUS )

*  If ERASE was selected, erase the entire HISTORY structure and reset
*  default values.
            ELSE IF( FUNC .EQ. 'ERASE' ) THEN
               CALL DAT_ANNUL( RLOC, STATUS )
               CALL DAT_ANNUL( HLOC, STATUS )
               CALL DAT_ERASE( NDFLOC, 'HISTORY', STATUS )
               EXTSIZ = 5
               NEXT = 1
               NRECS = 0
               THERE = .FALSE.

               CALL HISTC0( '    All history information removed',
     :                      LOGPOS, FD, STATUS )

*  Otherwise, history records are to be displayed.
            ELSE

*  Indicate that no records have yet been displayed.
               SOME = .FALSE.

*  Process each record.
               DO REC = 1, NEXT - 1

*  See if this record was created by the application specified by
*  parameter COMMAND before the time specified by parameter BEFORE and
*  after the time specified by parameter SINCE.
                  CALL HISTB1( MJDSIN, MJDBEF, COMMND( : LCMD ), REC,
     :                         RLOC, OK, STATUS )

*  If it was, display the record, starting a new page for the first NDF.
                  IF( OK ) THEN
                     CALL HISTA2( NDFNAM( : LNDFNM ), LOGPOS, FD, REC,
     :                            RLOC, STATUS )
                     SOME = .TRUE.
                  END IF

               END DO

*  If no records satisfied the constraints, tell the user.
               IF( .NOT. SOME ) THEN
                  CALL HISTC0( '    No history information satisfied '//
     :                         'the selection criteria', LOGPOS, FD,
     :                          STATUS )
               END IF

            END IF

         END IF

*  If the NDF contains no history records, delete any existing HISTORY
*  structure.
         IF( NEXT .EQ. 1 .AND. THERE ) CALL DAT_ERASE( NDFLOC,
     :                                               'HISTORY', STATUS )

*  Annul the locators.
         IF( THERE ) THEN
            CALL DAT_ANNUL( RLOC, STATUS )
            CALL DAT_ANNUL( HLOC, STATUS )
         END IF
         CALL DAT_ANNUL( NDFLOC, STATUS )

*  Annul the NDF identifier.
         CALL NDF_ANNUL( NDFIN, STATUS )

*  If an error occured processing the current NDF, flush the error.
         IF( STATUS .NE. SAI__OK ) CALL ERR_FLUSH( STATUS )

*  If the user has switched off screen output, quit any DISPLAY task.
         CALL IRM_SPAGE( ON )
         IF( .NOT. ON .AND. FUNC .EQ. 'DISPLAY' ) GO TO 998

      END DO

*  Display a blank line.
 998  CONTINUE
      CALL HISTC0( ' ', LOGPOS, FD, STATUS )

*  Delete the group used to hold text.
      IF( FUNC .EQ. 'ADD' ) CALL GRP_DELET( IGRP2, STATUS )

*  If parameter LOOP is true, go round for another FUNCTION.
      IF( LOOP .AND. STATUS .EQ. SAI__OK ) GO TO 10

*  End the NDF context.
      CALL NDF_END( STATUS )

*  Store a list of the processed files for use by later applications.
      CALL IRM_LISTN( 'NDFLIST', IGRP1, 'HISTORY', STATUS )

*  Delete the group holding NDFs.
 999  CONTINUE
      CALL GRP_DELET( IGRP1, STATUS )

*  Close any log file.
      IF( LOGPOS ) CALL FIO_CANCL( 'LOGFILE', STATUS )

*  If a parameter null or abort value was given, annul the error.
      IF( STATUS .EQ. PAR__NULL .OR. STATUS .EQ. PAR__ABORT ) THEN
         CALL ERR_ANNUL( STATUS )
      END IF

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'HISTORY_ERR1',
     :   'HISTORY: Error displaying, erasing or adding NDF history.',
     :   STATUS )
      END IF

      END
