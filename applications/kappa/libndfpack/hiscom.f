      SUBROUTINE HISCOM( STATUS )
*+
*  Name:
*     HISCOM

*  Purpose:
*     Adds commentary to the history of an NDF.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL HISCOM( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This task allows application-independent commentary to be added
*     to the history records of an NDF.  The text may be read from a
*     text file or obtained through a parameter.

*  Usage:
*     hiscom ndf [mode] { file=?
*                       { comment=?
*                      mode

*  ADAM Parameters:
*     APPNAME = LITERAL (Read)
*        The application name to be recorded in the new history record.
*        If a null (!) value is supplied, a default of "HISCOM" is used
*        and the new history record describes the parameter values supplied
*        when HISCOM was invoked. If a non-null value is supplied, the
*        new history record refers to the specified application name instead
*        of HISCOM and does not describe the HISCOM parameter values. [!]
*     COMMENT = LITERAL (Read)
*        A line of commentary limited to 72 characters.  If the value is
*        supplied on the command line only that line of commentary will
*        be written into the history.  Otherwise repeated prompting
*        enables a series of commentary lines to be supplied.  A null
*        value (!) terminates the loop.  Blank lines delimit
*        paragraphs.  Paragraph wrapping is enabled by parameter WRAP.
*        There is no suggested default to allow more room for entering
*        the value.
*     DATE =  LITERAL (Read)
*        The date and time to associated with the new history record.
*        Normally, a null (!) value should be supplied, in which case
*        the current UTC date and time will be used.  If a value is
*        supplied, it should be in one of the following forms.
*
*           Gregorian Calendar Date --- With the month expressed
*           either as an integer or a 3-character abbreviation, and with
*           optional decimal places to represent a fraction of a day
*           ("1996-10-2" or "1996-Oct-2.6" for example).  If no
*           fractional part of a day is given, the time refers to the
*           start of the day (zero hours).
*
*           Gregorian Date and Time --- Any calendar date (as above)
*           but with a fraction of a day expressed as hours, minutes and
*           seconds ("1996-Oct-2 12:13:56.985" for example).  The date
*           and time can be separated by a space or by a "T" (as used by
*           ISO 8601 format).
*
*           Modified Julian Date --- With or without decimal places
*           ("MJD 54321.4" for example).
*
*           Julian Date --- With or without decimal places
*           ("JD 2454321.9" for example).
*        [!]
*     FILE =  FILENAME (Read)
*        Name of the text file containing the commentary.  It is only
*        accessed if MODE="File".
*     MODE = LITERAL (Read)
*        The interaction mode.  The allowed values are described below.
*
*           "File"      ---  The commentary is to be read from a text
*                            file.  The formatting and layout of the
*                            text is preserved in the history unless
*                            WRAP=TRUE and there are lines longer than
*                            the width of the history records.
*           "Interface" ---  The commentary is to be supplied through a
*                            parameter.  See parameter COMMENT.
*
*        ["Interface"]
*     NDF = (Read and Write)
*        The NDF for which commentary is to be added to the history.
*     WRAP = _LOGICAL (Read)
*        WRAP=TRUE requests that the paragraphs of comments are wrapped
*        to make as much text fit on to each line of the history record
*        as possible.  WRAP=FALSE means that the commentary text beyond
*        the width of the history records (72 characters) is lost. If a
*        null (!) value is supplied, the value used is TRUE when
*        MODE="Interface" and FALSE if MODE="File".  [!]

*  Examples:
*     hiscom frame256 comment="This image has a non-uniform background"
*        This adds the comment "This image has a non-uniform background"
*        to the history records of the NDF called frame256.
*     hiscom ndf=eso146-g14 comment="This galaxy is retarded" mode=i
*        This adds the comment "This galaxy is retarded" to the history
*        records of the NDF called eso146-g14.
*     hiscom hh14_k file file=ircam_info.lis
*        This reads the file ircam_info.lis and places the text
*        contained therein into the history records of the NDF called
*        hh14_k.  Any lines longer than 72 characters are truncated to
*        that length.
*     hiscom hh14_k file file=ircam_info.lis wrap
*        As the previous example except the text in each paragraph is
*        wrapped to a width of 72 characters within the history
*        records.

*  Notes:
*     -  A HISTORY component is created if it does not exist within the
*     NDF.  The width of the history record is 72 characters.
*     -  An error will result if the current history update mode of the
*     NDF is "Disabled", and no commentary is written.  Otherwise the
*     commentary is written at the priority equal to the current
*     history update mode.
*     -  A warning messages (at the normal reporting level) is issued
*     if lines in the text file are too long for the history record and
*     WRAP=FALSE, though the first 72 characters are stored.
*     -  The maximum line length in the file is 200 characters.
*     -  Paragraphs should have fewer than 33 lines.  Longer ones will
*     be divided.

*  Related Applications:
*     KAPPA: HISLIST, HISSET, NDFTRACE.

*  Copyright:
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
*     Copyright (C) 2009 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David S Berry (JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     1995 June 28 (MJC):
*        Original version.
*     23-JAN-2008 (DSB):
*        Added DATE parameter.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'MSG_PAR'          ! MSG_ public constants
      INCLUDE 'PAR_PAR'          ! PAR_ constants
      INCLUDE 'PAR_ERR'          ! PAR_ error constants
      INCLUDE 'FIO_ERR'          ! FIO_ error constants

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! String length sans trailing blanks

*  Local Constants:
      INTEGER MXLINE
      PARAMETER ( MXLINE = 32 )  ! Maximum number of lines in a
                                 ! paragraph

*  Local Variables:
      CHARACTER * ( 80 ) APPNAM  ! Application name
      CHARACTER * ( NDF__SZHMX ) BUFFER ! Comment record
      CHARACTER * ( NDF__SZHMX ) COMENT( MXLINE ) ! Comment records
                                 ! (handling mapped char arrays is
                                 ! messy, so use hardwired buffer)
      INTEGER CONSTA             ! State of COMMENT parameter
      CHARACTER * ( 80 ) DATE    ! Date string
      INTEGER FD                 ! File descriptor
      CHARACTER * ( NDF__SZHUM ) HUMODE ! History update mode
      INTEGER INDF               ! NDF identifier
      LOGICAL LOOP               ! Loop for another comment?
      CHARACTER * ( 9 ) MODE     ! Mode for accessing the comments
      INTEGER NCHAR              ! Number of characters in comment
      INTEGER NLINES             ! Number of lines read from the file
      CHARACTER * ( 132 ) PARBUF ! Comment record (length limited by
                                 ! parameter system)
      INTEGER PLINES             ! Number of lines in current paragraph
      LOGICAL REPLAC             ! Replace the default history record?
      LOGICAL THERE              ! HISTORY component present?
      LOGICAL WRAP               ! Wrap history?

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain the NDF.
      CALL LPG_ASSOC( 'NDF', 'UPDATE', INDF, STATUS )

*  Get the application name to use. If no application name is supplied,
*  annul the error and indicate that the default history record created by
*  HISCOM should not be replaced (also set APPNAM blank to indicate that
*  NDF_HPUT should use the defaulty application name). Otherwise, indicate
*  the default history should be replaced.
      IF( STATUS .EQ. SAI__OK ) THEN
         CALL PAR_GET0C( 'APPNAME', APPNAM, STATUS )
         IF( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            REPLAC = .FALSE.
            APPNAM = ' '
         ELSE
            REPLAC = .TRUE.
         END IF
      END IF

*  Determine the mode to be used.
      CALL PAR_CHOIC( 'MODE', 'Interface', 'File,Interface', .FALSE.,
     :                MODE, STATUS )

*  Obtain the wrapping flag.  Use appropriate dynamic defaults depending
*  on the mode.
      IF ( MODE .EQ. 'FILE' ) THEN
         CALL PAR_GTD0L( 'WRAP', .FALSE., .TRUE., WRAP, STATUS )
      ELSE
         CALL PAR_GTD0L( 'WRAP', .TRUE., .TRUE., WRAP, STATUS )
      END IF

      IF ( STATUS .EQ. SAI__OK ) THEN

*  Get the date to use, and tell the NDF library to attach this date
*  to the next history record written to the NDF.
         CALL PAR_GET0C( 'DATE', DATE, STATUS )
         IF( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
         ELSE
            CALL NDF_HSDAT( DATE, INDF, STATUS )
         END IF

*  Check whether or not there is a HISTORY component present.
         CALL NDF_STATE( INDF, 'History', THERE, STATUS )

*  There can be no comment until the HISTORY component exists.
         IF ( .NOT. THERE ) CALL NDF_HCRE( INDF, STATUS )

*  Obtain the history update mode.
         CALL NDF_HINFO( INDF, 'MODE', 0, HUMODE, STATUS )

         IF ( HUMODE .EQ. 'DISABLED' ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'HISCOM_ERR1',
     :        'Unable to write commentary because the history update '/
     :        /'mode is DISABLED.', STATUS )
         ELSE

*  If we are not replacing the default history record created by HISCOM,
*  wWrite the default history information first, so that the commentary
*  will follow it.
            IF( .NOT. REPLAC ) CALL NDF_HDEF( INDF, ' ', STATUS )
         END IF
      END IF

      IF ( STATUS .EQ. SAI__OK ) THEN

*  File mode.
*  ==========
         IF ( MODE .EQ. 'FILE' ) THEN

*  Open the file.
            CALL FIO_ASSOC( 'FILE', 'READ', 'LIST', 0, FD, STATUS )

*  Initialise counters of the number of lines.
            NLINES = 0
            PLINES = 0

*  Read the file a line at a time.
            LOOP = .TRUE.
            DO WHILE ( STATUS .EQ. SAI__OK .AND. LOOP )
               CALL FIO_READ( FD, BUFFER, NCHAR, STATUS )

*  An end-of-file status is expected.
               IF ( STATUS .EQ. FIO__EOF ) THEN
                  CALL ERR_ANNUL( STATUS )
                  LOOP = .FALSE.
               END IF

*  Count the number of lines.
               IF ( STATUS .EQ. SAI__OK .AND. LOOP ) THEN
                  NLINES = NLINES + 1
                  PLINES = PLINES + 1

*  Issue a warning if the record is too long and cannot be wrapped.
                  IF ( NCHAR .GT. NDF__SZHIS .AND. .NOT. WRAP ) THEN
                     CALL MSG_SETI( 'N', NLINES )
                     CALL MSG_OUTIF( MSG__NORM, 'LONGLINE',
     :                 'Line ^N is longer than 72 characters and has '/
     :                 /'been truncated.', STATUS )

*  Truncate the comment.
                     COMENT( PLINES ) = BUFFER( 1:NDF__SZHIS )
                  ELSE

*  Just copy the comment line.
                     COMENT( PLINES ) = BUFFER
                  END IF
               END IF

*  Look for a paragraph or the end of the file or an exhausted buffer.
               IF ( BUFFER .EQ. ' ' .OR. .NOT. LOOP .OR.
     :              PLINES .EQ. MXLINE ) THEN

*  Append the line to the history, replacing the existing history if
*  requested, there are no message tokens to expand, and the right margin
*  is ragged.
                  CALL NDF_HPUT( HUMODE, APPNAM, REPLAC, PLINES, COMENT,
     :                           .FALSE., WRAP, .FALSE., INDF, STATUS )

*  Reset the paragraph line counter.
                  PLINES = 0
               END IF
            END DO

*  Close the file.
            CALL FIO_ANNUL( FD, STATUS )

*  Interface mode.
*  ===============
         ELSE IF ( MODE .EQ. 'INTERFACE' ) THEN

*  Find if the comment was given on the command line.
            CALL LPG_STATE( 'COMMENT', CONSTA, STATUS )

*  Read the comments a line at a time.
            LOOP = .TRUE.
            PLINES = 0
            DO WHILE ( STATUS .EQ. SAI__OK .AND. LOOP )
               CALL PAR_GET0C( 'COMMENT', PARBUF, STATUS )

*  A null status is expected so annul it, but indicate that the loop
*  is about to end.
               IF ( STATUS .EQ. PAR__NULL ) THEN
                  CALL ERR_ANNUL( STATUS )
                  LOOP = .FALSE.
               END IF

*  Increment the number of lines in the paragraph, if looping.
               IF ( LOOP .AND. STATUS .EQ. SAI__OK ) THEN
                  PLINES = PLINES + 1

*  Find the number of characters in the comment.
                  NCHAR = CHR_LEN( PARBUF )

*  Issue a warning if the comment is too long and cannot be wrapped.
                  IF ( NCHAR .GT. NDF__SZHIS .AND. .NOT. WRAP ) THEN
                     CALL MSG_SETI( 'N', NLINES )
                     CALL MSG_OUTIF( MSG__NORM, 'LONGLINE',
     :                 'The comment is longer than 72 characters and '/
     :                 /'has been truncated.', STATUS )

*  Truncate the comment.
                     COMENT( PLINES ) = PARBUF( 1:NDF__SZHIS )
                  ELSE

*  Just copy the comment line.
                     COMENT( PLINES ) = PARBUF
                  END IF
               END IF

*  End the loop if the value was on the command line.
               LOOP = .NOT. ( CONSTA .EQ. PAR__ACTIVE ) .AND. LOOP

*  Cancel the parameter if looping.
               IF( LOOP .AND. STATUS .EQ. SAI__OK ) THEN
                  CALL PAR_CANCL( 'COMMENT', STATUS )
               END IF

*  Look for a paragraph or the end of the file or an exhausted buffer.
               IF ( PARBUF .EQ. ' ' .OR. .NOT. LOOP .OR.
     :              PLINES .EQ. MXLINE ) THEN

*  Append the line to the history, replacing the existing history if
*  requested, there are no message tokens to expand, and the right margin
*  is ragged.
                  CALL NDF_HPUT( HUMODE, APPNAM, REPLAC, PLINES, COMENT,
     :                           .FALSE., WRAP, .FALSE., INDF, STATUS )

*  Reset the paragraph line counter.
                  PLINES = 0
               END IF

            END DO

         END IF
      END IF

*  Annul the NDF identifier.
      CALL NDF_ANNUL( INDF, STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'HISCOM_ERR',
     :     'HISCOM: Error adding commentary to the history of an '/
     :     /'NDF.', STATUS )
      END IF

      END
