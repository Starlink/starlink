      SUBROUTINE READF( STATUS )
*+
*  Name:
*     READF

*  Purpose:
*     Read from a formatted data file.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Description:
*     Read input data from a formatted data file. READF attempts to
*     read data from columns in an sequential formatted file in the
*     most flexible manner possible. It is possible to specify the
*     following:
*
*        - What comment delimiter characters are used within the data
*        file -- if used, the comment delimiter must be the first
*        character of a line in the data file.
*        - What the column delimiters are (more than one character is
*        possible).
*        - Symbolic names for each of the data columns.
*
*     The application is intended to be very robust: if a read error
*     occurs within a line, READF will report an error and attempt to
*     continue.
*
*     The application has many parameters for controlling how data are
*     read, but the default values of these parameters are sufficient
*     for reading most data files.

*  Usage:
*     readf [data] [all=?]
*        {selcond=?
*        all

*  ADAM Parameters:
*     DATA = FILENAME (Read and Write)
*        The name of the formatted data file.
*
*        If the value is not specified on the command line, the value
*        of the global parameter PONGO_DATA is used. If PONGO_DATA is
*        not defined, the current value is used. If the current value
*        is not defined, the value is prompted for.
*     HARDCOM = _CHAR (Read and Write)
*        A character used to indicate a comment line in the data file.
*        The character must appear in the first column of a comment.
*
*        If the value is not specified on the command line, the current
*        value is used. The current value is initially set to "!".
*     SOFTCOM = _CHAR (Read and Write)
*        A character to indicate a comment line in the data file. This
*        parameter allows a second character to be used as a comment
*        delimiter.  The character must appear in the first column of a
*        comment.
*
*        If the value is not specified on the command line, the current
*        value is used. The current value is initially set to "!".
*     XCOL = _CHAR (Read and Write)
*        The column number (counting from 1), or the symbolic name of a
*        column, from which the X-axis data are read. The value "0"
*        means "do not read these data".
*
*        If the value is not specified on the command line, the value
*        of the global parameter PONGO_XCOL is used. If PONGO_XCOL is
*        not defined, the current value is used. The current value is
*        initially set to "0".
*     YCOL = _CHAR (Read and Write)
*        The column number (counting from 1), or the symbolic name of a
*        column, from which the Y-axis data are read. The value "0"
*        means "do not read these data".
*
*        If the value is not specified on the command line, the value
*        of the global parameter PONGO_YCOL is used. If PONGO_YCOL is
*        not defined, the current value is used. The current value is
*        initially set to "0".
*     ZCOL = _CHAR (Read and Write)
*        The column number (counting from 1), or the symbolic name of a
*        column, from which the Z-axis data are read. The value "0"
*        means "do not read these data".
*
*        If the value is not specified on the command line, the value
*        of the global parameter PONGO_ZCOL is used. If PONGO_ZCOL is
*        not defined, the current value is used. The current value is
*        initially set to "0".
*     EXCOL = _CHAR (Read and Write)
*        The column number (counting from 1), or the symbolic name of a
*        column, from which the X-axis error data are read. The value
*        "0" means "do not read these data".
*
*        If the value is not specified on the command line, the value
*        of the global parameter PONGO_EXCOL isused. If PONGO_EXCOL
*        is not defined, the current value is used. The current value
*        is initially set to "0".
*     EYCOL = _CHAR (Read and Write)
*        The column number (counting from 1), or the symbolic name of a
*        column, from which the Y-axis error data are read. The value
*        "0" means "do not read these data".
*
*        If the value is not specified on the command line, the value
*        of the global parameter PONGO_EYCOL is used.  If PONGO_EYCOL
*        is not defined, the current value is used. The current value
*        is initially set to "0".
*     LABCOL = _CHAR (Read and Write)
*        The column number (counting from 1), or the symbolic name of a
*        column, from which the symbolic name for each data point is
*        read. The value "0" means "do not read these data".
*
*        If the value is not specified on the command line, the value
*        of the global parameter PONGO_LABCOL is used. If PONGO_LABCOL
*        is not defined, the current value is used. The current value
*        is initially set to "0".
*     SYMCOL = _CHAR (Read and Write)
*        The column number (counting from 1), or the symbolic name of a
*        column, from which the PGPLOT symbol code for each data point
*        is read. The value "0" means "do not read these data".
*
*        If the value is not specified on the command line, the value
*        of the global parameter PONGO_SYMCOL is used. If PONGO_SYMCOL
*        is not defined, the current value is used. The current value
*        is initially set to "0".
*     DELIM = _CHAR (Read and Write)
*        The character string interpreted as a column delimitier when
*        reading the data file. For example, this can be used to read
*        LATEX format tables by setting DELIM="&".
*
*        If the value is not specified on the command line, the current
*        value is used. The current value is initially set to " ".
*     FROM = _INTEGER (Read and Write)
*        The first line of data to be read from the data file. The value
*        0 defaults to the beginning of the file.
*        [0]
*     TO = _INTEGER (Read and Write)
*        The last line of data to be read from the data file. The value
*        0 defaults to the end of the file.
*        [0]
*     SELCOND = _CHAR (Read and Write)
*        A condition (or criterion) upon which to select values from
*        the data file.  This condition has the form
*
*           [SELECT_COL] [COND] [VAL1{,VAL2, ...}]
*
*        where
*
*           - [SELECT_COL] is the data area used for the selection
*           test. This can be specified either by column number
*           (counting from 1) or by the symbolic name of a column.
*           There is no restriction on which column is used for
*           selection, i.e. it does not have to be one of the columns
*           from which data are being read.
*           - [COND] is the selection criterion. It may be one of the
*           following:
*
*              o "=" -- equals;
*              o "#" -- not equal;
*              o ">" -- greater than;
*              o "<" -- less than;
*              o "CE" -- equal to string;
*              o "C#" -- not equal to string;
*              o "RA" -- in the range VAL1 to VAL2;
*              o "LI" -- select if in the following list of values;
*              o "EX" -- exclude if in the following list of values;
*              o "IN" -- select if the substring is contained within the
*              value;
*              o "A>" -- absolute value greater than;
*              o "A<" -- absolute value less than.
*
*           - [VAL1{,VAL2, ...}] the value (or values) against which the
*           selection is made.
*
*        Note that there must be white space around the selection
*        criterion. A value of "0" means "read everything".
*
*        [The value is prompted for.]
*     XOPT = _CHAR (Read and Write)
*        A string that controls the style of the X-axis labelling and
*        tick marks. It consists of a series of letters, which are
*        described fully in the documentation for the BOXFRAME
*        application.
*
*        READF updates the value of the global parameters PONGO_XOPT.
*        The application will automatically remove any "L" characters
*        at the start of the options string, because it is assumed
*        that they have been inserted by the CLOG application -- any
*        new data will not have had logarithms taken.  If data are
*        given in logarithmic form, the "L" character should be
*        inserted into the options strings anywhere except at the
*        start.
*
*        [The value of the global parameter PONGO_XOPT is used. If
*        PONGO_XOPT is not defined, the default value "BCNST" is used.]
*     YOPT = _CHAR (Read and Write)
*        A string that controls the style of the Y-axis labelling and
*        tick marks. It consists of a series of letters, which are
*        described fully in the documentation for the BOXFRAME
*        application.
*
*        READF updates the value of the global parameters PONGO_YOPT.
*        The application will automatically remove any "L" characters
*        at the start of the options string, because it is assumed
*        that they have been inserted by the CLOG application -- any
*        new data will not have had logarithms taken.  If data are
*        given in logarithmic form, the "L" character should be
*        inserted into the options strings anywhere except at the
*        start.
*
*        [The value of the global parameter PONGO_YOPT is used. If
*        PONGO_XOPT is not defined, the default value "BCNST" is used.]
*     ERSCALE = _REAL (Read and Write)
*        The scale factor to be applied to the EXCOL and EYCOL data.
*
*        [The value of the global parameter PONGO_ERSCALE is used. If
*        PONGO_ERSCALE is not defined, the default value 1.0 is used.]
*     ADD = _LOGICAL (Read)
*        If FALSE, the data values already held will be cleared before
*        reading new data; if TRUE, the data read will be appended to
*        the existing data.
*        [FALSE]
*     ALL = _LOGICAL (Read and Write)
*        If TRUE, the whole data file will be read; if FALSE, a
*        selection condition will be prompted for.
*
*        If the value is not specified on the command line, the current
*        value is used. The current value is initially set to TRUE.
*     QUICK = _LOGICAL (Read and Write)
*        If TRUE, a "quick mode" read is performed. This mode can only
*        be used on files which exclusively contain numeric data. This
*        parameter can over-ride the action of the LABCOL and SELCOND
*        parameters.
*
*        If the value is not specified on the command line, the current
*        value is used. The current value is initially set to FALSE.
*     NDATA = _INTEGER (Write)
*        The number of data read from the data file.
*        [0]

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     PDRAPER: Peter W. Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     7-FEB-1990 (JBVAD::PAH):
*        Original version.
*     1-AUG-1990 (JBVAD::PAH):
*        The QUICK read option was added (faster than the equivalent
*        MONGO command).
*     19-OCT-1992 (PCTR):
*        Added contextual error report on exit.
*     1994 May 4 (MJC):
*        Give ERSCALE write access.
*     3-JUN-1994 (PDRAPER):
*        Removed status argument from MSG_SETI call.
*     6-JUN-1994 (PDRAPER):
*        Removed unused argument from PON_GETCOL calls.
*        Changed DCV_PAR to PRM_PAR.
*     14-APR-1997 (PDRAPER):
*        Changed FIO_ASSOC to PON_ASFIO. This changes the file type
*        from FILENAME to _CHAR. The advantage of this method is that
*        ICL variables no longer need an @ or '' prepending or appending
*        when referring to the file. INCOMPATIBLE with previous scripts.
*     11-JUL-1997 (PDRAPER):
*        Changed back so that it is possible to read data from
*        different files WITHOUT resetting the current contents.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PONGO_PAR'        ! PONGO global constants
      INCLUDE 'FIO_PAR'          ! FIO public global constants
      INCLUDE 'PAR_ERR'          ! PAR public global constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT public global constants

*  Global Variables:
      INCLUDE 'PONGO_CMN'        ! PONGO global variables

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( FIO__SZFNM ) FNAME ! Actual name of file
      CHARACTER * ( 1 ) HARDCOM  ! 'HARD' comment character
      CHARACTER * ( 10 ) DELIM   ! Column delimiter characters
      CHARACTER * ( LENLAB ) CHRSEL ! Character select value
      CHARACTER * ( LENLAB ) CLIST( MAXSELST ) ! List of character
                                               ! values for selection
      CHARACTER * ( 1 ) SOFTCOM  ! 'soft' comment character

      LOGICAL QUICK              ! Use quick mode for reading file
      LOGICAL CLRBUFF            ! Switch to specify clearing buffer
      LOGICAL OPEN               ! Input file is open

      INTEGER FD                 ! Descriptor for data file
      INTEGER SELCOL             ! select column
      INTEGER ISTLN, IFINLN      ! First and last line of file to be read
      INTEGER ICOND              ! index number of condition
      INTEGER NLIST              ! number of values in the select list

      REAL SELVAL1, SELVAL2      ! real select values
      REAL SIZESCALE             ! Scaling factor

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      NLIST = 1
      OPEN = .FALSE.
      CALL PON_ASFIO( 'DATA', 'READ', 'NONE', 0, FD, OPEN, STATUS )

      IF ( STATUS .EQ. SAI__OK ) THEN

*     Report the filename.
         CALL FIO_FNAME( FD, FNAME, STATUS )
         CALL MSG_SETC( 'FNAME', FNAME )
         CALL MSG_OUT( ' ', 'File: ^FNAME', STATUS )

*     Get the "hard" comment character.
         CALL PAR_GET0C( 'HARDCOM', HARDCOM, STATUS )

*     Get the column labels.
         CALL PON_GTCLAB( FD, MAXLAB, HARDCOM, NCOLS, COLLAB, STATUS )

*     Get column numbers for each of the data columns.
         CALL PON_GETCOL( 'XCOL', COLLAB, XCOL, STATUS )
         CALL PON_GETCOL( 'YCOL', COLLAB, YCOL, STATUS )
         CALL PON_GETCOL( 'ZCOL', COLLAB, ZCOL, STATUS )
         CALL PON_GETCOL( 'EXCOL', COLLAB, ERXCOL, STATUS )
         CALL PON_GETCOL( 'EYCOL', COLLAB, ERYCOL, STATUS )
         CALL PON_GETCOL( 'LABCOL', COLLAB, LABCOL, STATUS )
         CALL PON_GETCOL( 'SYMCOL', COLLAB, SYMCOL, STATUS )

*     Check that at least one valid column is to be read.
         IF ( STATUS .EQ. SAI__OK .AND. XCOL .EQ. 0 .AND. YCOL .EQ. 0
     :        .AND. ZCOL .EQ. 0 .AND.ERXCOL .EQ. 0 .AND. ERYCOL .EQ. 0
     :        .AND. LABCOL .EQ. 0 .AND. SYMCOL .EQ. 0 ) THEN

*  Ok issue error report and exit.
            STATUS = SAI__ERROR
            CALL ERR_REP( 'READF_NONEED',
     :'No data to be read (all columns 0)', STATUS )
            GO TO 999
         END IF

*     Get the delimiter(s) for the columns.
         CALL PAR_GET0C( 'DELIM', DELIM, STATUS )
         IF ( STATUS .EQ. PAR__NULL ) THEN
            DELIM = ' '
            CALL ERR_ANNUL( STATUS )
         END IF

*     Get the select CRITERIA.
         CALL PON_GETSEL( COLLAB, SELCOL, ICOND, CHRSEL, SELVAL1,
     :                    SELVAL2, NLIST, CLIST, STATUS )

*     Get the first and last lines to be read ...
         CALL PAR_GET0I( 'FROM', ISTLN, STATUS )
         CALL PAR_GET0I( 'TO', IFINLN, STATUS )

*     ... make the defaults sensible.
         IF ( IFINLN .EQ. 0 ) IFINLN = VAL__MAXI
         IF ( ISTLN .EQ. 0 ) ISTLN = 1

         IF( MAX( SELCOL, XCOL, ERXCOL, YCOL, ERYCOL, LABCOL, SYMCOL )
     :       .GT. MAXCOL ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'MAXCOL', MAXCOL )
            CALL ERR_REP( 'READF_BADCOL',
     :         'One of the columns exceeds the maximum column ' //
     :         'number allowed (^MAXCOL).', STATUS )
            GO TO 999
         END IF

*     These will be set by parameters.
         CALL PAR_GET0L( 'ADD', CLRBUFF, STATUS )
         CLRBUFF = ( .NOT. CLRBUFF )
         CALL PAR_GET0R( 'ERSCALE', SIZESCALE, STATUS )
         CALL PAR_GET0C( 'SOFTCOM', SOFTCOM, STATUS )

*     Read the file.
         CALL PAR_GET0L( 'QUICK', QUICK, STATUS )

         IF ( QUICK ) THEN

            IF ( ( ICOND .EQ. 5 ) .OR. ( ICOND .EQ. 6 )
     :           .OR. ( ICOND .EQ. 8 ) .OR. ( ICOND .EQ. 9 )
     :           .OR. ( ICOND .EQ. 12 ) ) THEN
               ICOND = 0
               SELCOL = 0
               CALL MSG_OUT( ' ', 'Non-numeric SELECT conditions ' //
     :                       'invalid in quick mode.', STATUS )
               CALL MSG_OUT( ' ', '*** SELECT condition ignored. ***',
     :                       STATUS )
            ENDIF

            IF ( LABCOL.NE.0 ) CALL MSG_OUT( ' ', 'No labels will ' //
     :                                       'be read in quick mode.',
     :                                       STATUS)

            CALL PON_QRDFIL( FD,
     :        SELCOL, ISTLN, IFINLN, ICOND, CHRSEL,
     :        SELVAL1, SELVAL2, NLIST, CLIST(3), SIZESCALE, CLRBUFF,
     :        HARDCOM, SOFTCOM, STATUS )
         ELSE
            CALL PON_RDFIL( FD,
     :        SELCOL, ISTLN, IFINLN, DELIM, ICOND, CHRSEL,
     :        SELVAL1, SELVAL2, NLIST, CLIST(3), SIZESCALE, CLRBUFF,
     :        HARDCOM, SOFTCOM, STATUS )
         END IF

*     Remove the logarithmic options from the axis option string.
         CALL PON_RMLOPT( 'XOPT', STATUS )
         CALL PON_RMLOPT ('YOPT', STATUS )
         CALL MSG_SETI( 'NDATA', NDAT )
         CALL MSG_OUT( ' ', '^NDATA data points read.', STATUS )
         CALL PAR_PUT0I( 'NDATA', NDAT, STATUS )

*  Reset the logarithmic flags to FALSE, unless data has just been
*  added (assume the user knows what they are doing in this case).
         IF ( CLRBUFF ) THEN
            IF ( XCOL .NE. 0 ) LXLOG = .FALSE.
            IF ( YCOL .NE. 0 ) LYLOG = .FALSE.
         END IF
      ELSE
         CALL ERR_REP( 'READF_BDFIL', 'Error opening the file.',
     :                 STATUS )
      END IF

*  Abort.
 999  CONTINUE

*  Close input file if opened.
      IF ( OPEN ) CALL FIO_CLOSE( FD, STATUS )

*  Check the reaturned status and report a contextual error message if
*  necessary.
      IF ( STATUS .NE. SAI__OK ) CALL ERR_REP( 'READF_END',
     :                              'READF: Data could not be read.',
     :                              STATUS )

      END
* $Id$
