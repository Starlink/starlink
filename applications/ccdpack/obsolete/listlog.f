      SUBROUTINE LISTLOG( STATUS )
*+
*  Name:
*     LISTLOG

*  Purpose:
*     Lists an ADAM/ICL logfile.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL LISTLOG( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     LISTLOG lists the contents of a logfile created either by ICL or
*     by an ADAM task using the ICL log file system (e.g. CCDPACK).
*     The output can be directed into a printable file or to the 
*     terminal.

*  Usage:
*     LISTLOG LOGFILENAME OUTPUT [SINCE] [DTNS] [NAMES] [LABELS]

*  ADAM Parameters:
*     DTNS = LITERAL (Read)
*        A single string whose elements indicate which parts of the
*        logfile records (see notes) are to be listed. "D" stands for
*        Date, "T" stands for time, "N" stands for taskName and `S'
*        stands for String the actual entry. Thus the string "DTNS"
*        means that all the parts of each logfile record should be
*        listed. The default value "S" lists only the string entry part
*        of the logfile record. Illegal characters (i.e. non "D" "T"
*        "N" "S" ones) will be ignored.
*        ["S"]
*     LABELS( 1 to 5 ) = _CHAR (Read)
*        A list of up to five labels, separated by commas. Only records
*        whose entry strings (see notes) start with these labels (case
*        insensitive) are listed. This parameter defaults to "ALL"
*        which means list all entry strings irrespective of their
*        initial content.
*        ["ALL"]
*     LOGFILENAME = LITERAL (Read)
*        The name of the log file which is to be read. This defaults to
*        "ADAM_LOGFILE" an ADAM system logical name pointing the usual
*        system logfile.
*        ["ADAM_LOGFILE"]
*     NAMES( 1 to 5 ) = _CHAR (Read)
*        A list of up to five tasknames (see notes), separated by
*        commas. Only records with these tasknames (case insensitive)
*        are listed. The default "ALL" means list all records
*        irrespective of taskname.  ICL interprets the taskname as the
*        process name and uses this.  Other programs/packages which
*        write to the log file may use other indicators (e.g. CCDPACK
*        uses "CCDPACK" for this value so that its records may be
*        extracted by using this parameter, say if they are mixed with
*        other log information).
*        ["ALL"]
*     OUTPUT = LITERAL (Read)
*        Where the output from LISTLOG is to be directed. This defaults
*        to "S" which means list the output to the Screen (the 
*        terminal). If the output is required in a printable format then
*        a file name should be returned.
*        ["S"]
*     SINCE = LITERAL (Read)
*        A date and time specified in the format:
*           DD-MMM-YYYY HH:MM:SS.CC
*        e.g.
*           9-JAN-1991 10:30:22.5
*
*        Records whose time of entry into the log file are after this
*        will be selected. The default "S" (for Start) means use the
*        time found at the beginning of the logfile (this time is shown
*        immediately after the logfile is opened). If this return ("S")
*        is accepted then all records will be shown irrespective of
*        their time of entry. A value may be offset from the DATE
*        (note that the TIME part is ignored) at the start of the
*        logfile by using the format
*           - HH:MM:SS.CC
*        in this circumstance the "-" is replaced by the start date.
*        ["S"]

*  Examples:
*     LISTLOG \
*        This example just lists the logfile. If LISTLOG has not been
*        previously run then the logfile used is the system one
*        (assigned to the logical name "ADAM_LOGFILE") and the output
*        is listed to the terminal. If LISTLOG has been previously used
*        then the current values (i.e. those entered on the last use)
*        of LOGFILENAME and OUTPUT will be used.
*     LISTLOG CCDPACK.LOG CCDPACK.LIS
*        Lists the contents of the logfile CCDPACK.LOG to the printable
*        file CCDPACK.LIS.
*     LISTLOG MYREDUCE.LOG MYREDUCE.LIS SINCE="1-JAN-1991 00"
*        This examples writes the contents of logfile MYREDUCE.LOG to
*        MYREDUCE.LIS, selecting only the records whose dates are after
*        the shown date.
*     LISTLOG DTNS=DS \
*        In this example the date and string parts of the current
*        logfile are listed.
*     LISTLOG NAMES=[CCDPACK,AUSER] \
*        In this example only those logfile records whose tasknames are
*        "CCDPACK" or "AUSER" are listed. The comparison is case
*        insensitive.
*     LISTLOG LABELS=[Mean,Average,Total] \
*        In this example only those logfile records whose entry strings
*        start with the characters "Mean", "Average" or "Total" are
*        listed. The comparison is case insensitive.

*  Notes:
*       The format of a record in a logfile is
*
*         Date Time Taskname Entry
*
*       Date and Time are the time and date when the entry was made to
*       the logfile.  Taskname is the name of the task which made the
*       entry (usually the process or package name). Entry is the actual
*       string which is to be recorded.
*
*       Certain of the parameters are latched to use their current
*       value. These are LOGFILENAME, OUTPUT, DTNS, NAMES and LABELS.
*       LOGFILENAME and OUTPUT are prompted for and offer the current
*       value as a default. DTNS, NAMES and LABELS do not prompt and
*       hence always use the default or current value (if LISTLOG has
*       been previously run then the current values are those entered at
*       that time and may be inspected by using the utility TRACE on the
*       file ADAM_USER:LISTLOG) thus it is only necessary to enter your
*       desired defaults for these values once. To override the current
*       values use the keyword RESET on the command line or put the new
*       values on the command line.

*  Authors:
*     UNKNOWN
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     - (UNKNOWN):
*        Original version - known as the ADAM utility LISTLOG .
*     8-JAN-1992 (PDRAPER):
*        Added new prologue, changed behaviour of defaults and DTNS.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN
      EXTERNAL CHR_LEN           ! Length of string excluding trailing
                                 ! blanks

*  Local Variables:
      CHARACTER * ( 132 ) STRING ! message in a record
      CHARACTER * ( 156 ) OUTPUT ! output line
      CHARACTER * ( 16 ) NAME    ! taskname in a record
      CHARACTER * ( 20 ) LABELS( 5 ) ! list of labels
      CHARACTER * ( 80 ) LOGFILENAME ! Name of log file
      CHARACTER * ( 20 ) NAMES( 5 ) ! list of tasknames
      CHARACTER * ( 23 ) A_TIME1 ! time of first record converted to
                                  ! ASCII time
      CHARACTER * ( 4 ) DTNS     ! list of elements wanted in output
      CHARACTER * ( 8 ) SINCE    ! time(in reversed binary) at which
                                 ! listing is to start
      CHARACTER * ( 8 ) TIME     ! time(in reversed binary) of a record
      INTEGER I                  ! loop index
      INTEGER ISTART             ! index of first record whose time is
                                 ! later
      INTEGER ISTAT              ! system status
      INTEGER LEN_LABEL( 5 )     ! length of label strings
      INTEGER LEN_NAME( 5 )      ! length of taskname strings
      INTEGER NUMLAB             ! number of labels to be listed
      INTEGER NUMNAM             ! number of tasknames to be listed
      INTEGER OUTCHAN            ! i/o stream number for output or zero
                                 ! if output is to screen
      INTEGER POS                ! length of output line
      LOGICAL END                ! flags the end of the file
      LOGICAL LABEL_WANTED       ! flags that a label is to be output
      LOGICAL NODATA             ! flags that no records exist after
                                 ! the start time
      LOGICAL TASK_WANTED        ! flags that a taskname is to be output

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set channel number to 'not opened' before starting.
      OUTCHAN = -1

*  Get the log filename from user
      CALL PAR_GET0C( 'LOGFILENAME', LOGFILENAME, STATUS )
      CALL CHR_UCASE( LOGFILENAME )
      IF ( LOGFILENAME .EQ. 'A_L' ) LOGFILENAME = 'ADAM_LOGFILE'

*  Open the logfile
      CALL LOG_OPEN_READ( LOGFILENAME( :CHR_LEN( LOGFILENAME ) ),
     :                    ISTAT, STATUS )
      IF (ISTAT.NE.0) THEN

*  An error has occurred while opening the logfile report the error and
*  exit.
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'LOGNAME', LOGFILENAME )
         CALL ERR_FIOER( 'IOERR', ISTAT )
         CALL ERR_REP( 'OPNERR',
     :   '  Error opening logfile ^LOGNAME: ^IOERR', STATUS )
         GO TO 99
      END IF

*  Read the first record.
      CALL LOG_READ_SEQ( 1, NAME, TIME, STRING, END, STATUS )
      IF ( STRING .NE. 'ADAM_LOGFILE' .AND.
     :     STRING .NE. 'ADAMRM_LOGFILE') THEN

*  Illegal first record report an error and exit.
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'LOGNAME', LOGFILENAME )
         CALL ERR_REP( 'NOTLOGFILE',
     :   '  File ^LOGNAME is not an ADAM logfile', STATUS )
         GO TO 99
      END IF

*  Convert the time of the first record to ASCII time and write out to
*  the user.
      CALL LOG_CONV_TIME( TIME, A_TIME1, STATUS )
      CALL MSG_SETC( 'TIME1', A_TIME1 )
      IF ( STRING .EQ. 'ADAM_LOGFILE' ) THEN
         CALL MSG_OUT( 'READF1',
     :        '  Logfile initialised: ^TIME1', STATUS )
      ELSE
         CALL MSG_OUT( 'READF1',
     :        '  Remote logfile initialised: ^TIME1', STATUS )
      END IF

*  Get the output medium from user
      CALL GET_OUTPUT( OUTCHAN, STATUS )

*  Get the list of tasks to be listed from the user
      CALL GET_NAMES( NUMNAM, NAMES, LEN_NAME, STATUS )

*  Get a list of labels from the user
      CALL GET_LABELS( NUMLAB, LABELS, LEN_LABEL, STATUS )

*  Get which elements are required.
      CALL GET_DTNS( DTNS, STATUS )

*  Get the start time for output from user (defaulting to start of
*  file)
      CALL GET_SINCE( TIME, SINCE, STATUS )

*  Find the start record in the file, offsetting as appropriate for the
*  given start time.
      CALL LOG_FIND_START( SINCE, ISTART, NODATA, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  If no valid data is found in then report this and exit.
      IF ( NODATA ) THEN
         CALL MSG_SETC( 'LOGNAME', LOGFILENAME )
         CALL MSG_SETC( 'SINCE', SINCE )
         CALL MSG_OUT( 'NODATA',
     :   '  Logfile ^LOGNAME contains no valid records', STATUS )
         GO TO 99
      END IF

*  Now list the file starting at index ISTART
      I = ISTART
      END = .FALSE.
 1    CONTINUE               ! Start of 'DO WHILE' loop
         IF ( .NOT. END ) THEN

*  Still have records to read.
            CALL LOG_READ_SEQ( I, NAME, TIME, STRING, END, STATUS )
            IF ( STATUS .EQ. SAI__OK  .AND.  ( .NOT. END ) ) THEN

*  Another record read check that the tasknames are those required.
*  (NUMNAM = 0 indicates that all names are ok.)
               CALL LOG_CHECK_NAMES( NAME, NUMNAM, NAMES, LEN_NAME,
     :                               TASK_WANTED, STATUS )
               IF ( TASK_WANTED ) THEN

*  Taskname ok, check the label against the first characters of the
*  string. ( NUMLAB = 0 indicates that all strings are ok.)
                  CALL LOG_CHECK_LABEL( STRING, NUMLAB, LABELS,
     :                                  LEN_LABEL, LABEL_WANTED,
     :                                  STATUS )
                  IF ( LABEL_WANTED ) THEN

*  Create the output string.
                     CALL LOG_OUT( DTNS, TIME, NAME, STRING, POS,
     :                             OUTPUT, STATUS )

*  Write the output to the user or the printable file.
                     IF ( OUTCHAN .GT. -1 ) THEN
                        WRITE( OUTCHAN, '(1X,A)' ) OUTPUT( : POS )
                     ELSE

*  Use a token to stop message escape expansion.
                        CALL MSG_SETC( 'LINE', OUTPUT( :POS ) , STATUS )
                        CALL MSG_OUT( 'TEXT', '^LINE', STATUS )

*  Force immediate message output, ICL buffers the messages and can get
*  overloaded if not flushed.
                        CALL MSG_SYNC( STATUS )
                     END IF
                  END IF
               END IF
            ELSE

*  No more records - not an error reset status.
               STATUS = SAI__OK
            END IF

*  Increment record counter and return for next record.
            I = I + 1
            GO TO 1
         END IF                  ! End of 'DO WHILE' loop

*  Arrive directly here if aborting.
 99   CONTINUE

*    Close output channel,if it has been opened
      IF ( OUTCHAN .GT. -1 ) CLOSE( OUTCHAN )
      CALL LOG_CLOSE_READ( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'LISTLOG_ERR',
     :   'LISTLOG: Error while listing ADAM logfile.',
     :   STATUS )
      END IF

      END
* $Id$
