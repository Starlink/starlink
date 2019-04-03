      SUBROUTINE NDF1_HFWRT( IDCB, APPN, NLINES, TEXT, TRANS, WRAP,
     :                       RJUST, STATUS )
*+
*  Name:
*     NDF1_HFWRT

*  Purpose:
*     Format text and write it to a history record.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_HFWRT( IDCB, APPN, NLINES, TEXT, TRANS, WRAP, RJUST,
*                      STATUS )

*  Description:
*     The routine writes a series of text lines to the current history
*     record, after first applying optional message token translation
*     and formatting operations.  If the history has not yet been
*     modified by the current application (and APPN is not "<APPEND>"),
*     it creates a new history record, initialises it, and inserts the
*     text suppled. If the history has already been modified (or if APPN
*     is "<APPEND>"), then the new text is simply appended to any already
*     present. The routine returns without action if the NDF does not
*     have a history component.

*  Arguments:
*     IDCB = INTEGER (Given)
*        DCB index identifying the NDF whose history is to be modified.
*     APPN = CHARACTER * ( * ) (Given)
*        Name of the application. This is only used (to initialise the
*        new history record) if the history has not yet been modified
*        by the current application, otherwise it is ignored. If a
*        blank value is given, then a suitable default will be used
*        instead. The special value "<APPEND>" may be supplied in order
*        to append the text lines to the current history text even if
*        the  history has not yet been modified by the current application.
*     NLINES = INTEGER (Given)
*        Number of new lines of text to be added to the history record.
*     TEXT( NLINES ) = CHARACTER * ( * ) (Given)
*        Array of text lines.
*     TRANS = LOGICAL (Given)
*        Whether message tokens embedded in the input text lines are to
*        be translated. If not, then all input text will be interpreted
*        literally.
*     WRAP = LOGICAL (Given)
*        Whether "wrapping" of paragraphs in the input text is to be
*        performed to adjust the text width to that of the history
*        record being written. If specified, wrapping takes place after
*        message token expansion.
*     RJUST = LOGICAL (Given)
*        Whether to pad lines of text so as to right justify them within
*        the width of the history record being written. If not, then
*        the right margin is left ragged.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     If a new history record is being initialised, then the text
*     length (characters per line) of the new record is determined by
*     the length of the elements of the TEXT array. If the history
*     record has already been written to, then the existing line text
*     length is not altered and formatting of new text takes place so
*     as to fit the existing text length.

*  Copyright:
*     Copyright (C) 1994 Particle Physics & Astronomy Research Council

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     DSB: David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     11-MAY-1993 (RFWS):
*        Original version.
*     16-JUN-1993 (RFWS):
*        Documented defaulting of application name.
*     21-JUN-1993 (RFWS):
*        Re-written to use new text wrapping routine and to perform
*        message token expansion before testing for blank lines.
*     17-NOV-1994 (RFWS):
*        Changed to preserve indentation on un-wrapped text and on the
*        first line of each wrapped paragraph.
*     16-OCT-2009 (DSB):
*        If APPN is "<APPEND>", always append text to the current
*        history record.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_HLOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Locator for NDF history component.
*        DCB_HTLEN( NDF__MXDCB ) = INTEGER (Read)
*           History current record text length.

*  Arguments Given:
      INTEGER IDCB
      CHARACTER * ( * ) APPN
      INTEGER NLINES
      CHARACTER * ( * ) TEXT( NLINES )
      LOGICAL TRANS
      LOGICAL WRAP
      LOGICAL RJUST

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Significant length of a string

*  Local Constants:
      INTEGER MXOUT              ! Output buffer size in characters
      PARAMETER ( MXOUT = 8 * NDF__SZHMX )

*  Local Variables:
      CHARACTER * ( 2 * NDF__SZHMX + 1 ) BUF ! Rolling text buffer
      CHARACTER * ( MXOUT ) OUT( 1 ) ! Formatted output buffer
      CHARACTER * ( NDF__SZHMX ) LINE ! Text for current input line
      CHARACTER * ( NDF__SZHMX ) LINENX ! Text for next input line
      INTEGER FP                 ! Formatting position in BUF
      INTEGER FPLAST             ! Last formatting position
      INTEGER I1                 ! Source character position
      INTEGER I2                 ! Destination character position
      INTEGER ILINE              ! Loop counter for input text lines
      INTEGER INDENT             ! Indent for next output line
      INTEGER L                  ! Length of output history text lines
      INTEGER LBUF               ! No. characters used in BUF
      INTEGER LOUT               ! No. output buffer characters used
      INTEGER NC                 ! Length of current input line
      INTEGER NCNX               ! Length of next input line
      LOGICAL APPEND             ! Append text to current history record?
      LOGICAL EMPTY              ! BUF empty?
      LOGICAL OK                 ! Output line formatted OK?

*.

*  Check inherited global status.
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Check that the length of the input text lines is not too large for
*  the internal buffers used for formatting.
         IF ( LEN( TEXT( 1 ) ) .GT. NDF__SZHMX ) THEN
            STATUS = NDF__HISTL
            CALL MSG_SETI( 'LEN', LEN( TEXT( 1 ) ) )
            CALL MSG_SETI( 'MAX', NDF__SZHMX )
            CALL ERR_REP( 'NDF1_HFWRT_LEN',
     :                    'The length of the supplied history text ' //
     :                    'lines (^LEN) exceeds the allowed maximum ' //
     :                    'NDF__SZHMX (^MAX) (possible programming ' //
     :                    'error).', STATUS )
         END IF

*  Ensure that history information is available in the DCB.
         CALL NDF1_DH( IDCB, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Check that a history component is present, otherwise there is nothing
*  to do.
            IF ( DCB_HLOC( IDCB ) .NE. DAT__NOLOC ) THEN

*  Set a flag if we are appending the new text to the end of the current
*  history record.
               APPEND = ( APPN .EQ. '<APPEND>' )

*  Obtain the text length for the current history record. If this is
*  zero (because there is not yet a current record), then use the
*  length of the input text lines instead, or if APPN is <APPEND> use
*  the length of the current history record.
               L = DCB_HTLEN( IDCB )
               IF ( L .EQ. 0 ) THEN
                  IF( APPEND ) THEN
                     CALL NDF1_HTLEN( IDCB, L, STATUS )
                     IF( L .EQ. 0 ) L = LEN( TEXT( 1 ) )
                  ELSE
                     L = LEN( TEXT( 1 ) )
                  END IF
               END IF

*  If we are appending text to the current history record, indicate that
*  the first line should not be indented. Otherwise, indicate that spaces
*  should be left unchanged (see NDF1_TWRAP).
               IF( APPEND ) THEN
                  INDENT = 0
               ELSE
                  INDENT = -1
               END IF

*  Initialise.
               LBUF = 0
               LOUT = 0

*  Determine the length of the first input line.
               NCNX = CHR_LEN( TEXT( 1 ) )

*  If the line is not blank and message token expansion is required,
*  then expand the text. Renew the message tokens for later use.
               IF ( NCNX .GT. 0 ) THEN
                  IF ( TRANS ) THEN
                     CALL MSG_LOAD( ' ', TEXT( 1 )( : NCNX ), LINENX,
     :                              NCNX, STATUS )
                     CALL MSG_RENEW

*  If no expansion is required, then simply copy the input text.
                  ELSE
                     LINENX( : NCNX ) = TEXT( 1 )( : NCNX )
                  END IF
               END IF

*  Convert any non-printing characters to blanks.
               IF ( STATUS .EQ. SAI__OK ) THEN
                  IF ( NCNX .GT. 0 ) THEN
                     CALL CHR_CLEAN( LINENX( : NCNX ) )

*  If performing paragraph-wrapping, also compress multiple embedded
*  blanks into single blanks. Leave leading blanks untouched, since the
*  indentation of the first line should be preserved.
                     IF ( WRAP ) THEN
                        CALL NDF1_CMPBL( .FALSE., LINENX( : NCNX ),
     :                                   NCNX )
                     END IF
                  END IF

*  Re-evaluate the text length.
                  IF ( NCNX .GT. 0 ) THEN
                     NCNX = CHR_LEN( LINENX( : NCNX ) )
                  END IF
               END IF

*  Loop to process each input line.
               IF ( STATUS .EQ. SAI__OK ) THEN
                  DO 3 ILINE = 1, NLINES

*  Obtain the text and length of the current line (previously found).
                     IF ( NCNX .GT. 0 )
     :                  LINE( : NCNX ) = LINENX( : NCNX )
                     NC = NCNX

*  Obtain the text of the next line, if it exists, by expanding or
*  copying it, as for the first line.
                     IF ( ILINE .LT. NLINES ) THEN
                        NCNX = CHR_LEN( TEXT( ILINE + 1 ) )
                        IF ( NCNX .GT. 0 ) THEN
                           IF ( TRANS ) THEN
                              CALL MSG_LOAD( ' ',
     :                                      TEXT( ILINE + 1 )( : NCNX ),
     :                                       LINENX, NCNX, STATUS )
                              CALL MSG_RENEW
                           ELSE
                              LINENX( : NCNX ) =
     :                           TEXT( ILINE + 1 )( : NCNX )
                           END IF
                        END IF

*  Convert non-printing characters, compress mulitple blanks and
*  re-evaluate the text length. Do not compress leading blanks if the
*  line before (i.e. the "current" line) was blank, since the
*  indentation of the first line of each paragraph should be preserved.
                        IF ( STATUS .EQ. SAI__OK ) THEN
                           IF ( NCNX .GT. 0 ) THEN
                              CALL CHR_CLEAN( LINENX( : NCNX ) )
                              IF ( WRAP ) THEN
                                 CALL NDF1_CMPBL( ( NC .NE. 0 ),
     :                                            LINENX( : NCNX ),
     :                                            NCNX )
                              END IF
                           END IF
                           IF ( NCNX .GT. 0 ) THEN
                              NCNX = CHR_LEN( LINENX( : NCNX ) )
                           END IF
                        END IF
                     END IF

*  If the current line is blank, then it does not need formatting (it
*  acts as a paragraph separator, if relevant).  Append a blank line
*  directly to the output buffer.
                     IF ( NC .EQ. 0 ) THEN
                        OUT( 1 )( LOUT + 1 : LOUT + L ) = ' '
                        LOUT = LOUT + L

*  If the output buffer is now full (it no longer contains space for
*  further lines), then flush it, writing its contents to the current
*  history record.
                        IF ( ( MXOUT - LOUT ) .LT. L ) THEN
                           CALL NDF1_HWRT( IDCB, APPN, LOUT / L,
     :                                     OUT( 1 )( : L ), STATUS )
                           LOUT = 0
                        END IF

*  If the current input line is not blank, then it must be appended to
*  the text in BUF prior to output. Append a separating blank to BUF
*  before adding it if necessary.
                     ELSE
                        IF ( ( LBUF .NE. 0 ) .AND.
     :                       ( LINE( 1 : 1 ) .NE. ' ' ) ) THEN
                           LBUF = LBUF + 1
                           BUF( LBUF : LBUF ) = ' '
                        END IF
                        BUF( LBUF + 1 : LBUF + NC ) = LINE( : NC )
                        LBUF = LBUF + NC

*  Loop to format the current contents of BUF into output lines.
                        FP = 1
 1                      CONTINUE ! Start of 'DO WHILE' loop
                        IF ( STATUS .EQ. SAI__OK ) THEN
                           FPLAST = FP

*  Format a line from BUF, appending the result to the output buffer,
*  and determine whether the contents of BUF have been exhausted.
                           CALL NDF1_TWRAP( BUF( : LBUF ), INDENT, FP,
     :                                 OUT( 1 )( LOUT + 1 : LOUT + L ) )
                           EMPTY = ( FP .EQ. 0 )

*  If we are appending text to the current history record, indicate that
*  the second and all subsequent lines should be indentned by 3 spaces.
                           IF( APPEND ) INDENT = 3

*  Determine if the resulting output line will be correctly formatted
*  (it may not be if BUF has been prematurely exhausted so that the
*  output line still requires text from the next input line). It will
*  be OK if no paragraph wrapping is required or if further text still
*  remains to be processed in BUF.
                           OK = ( ( .NOT. WRAP ) .OR. ( .NOT. EMPTY ) )

*  If necessary, check if the next input line is blank or if we are on
*  the last line. Either of these indicates the end of a paragraph, so
*  the text in BUF is complete.
                           IF ( .NOT. OK ) THEN
                              IF ( ILINE .LT. NLINES ) THEN
                                 OK = ( NCNX .EQ. 0 )
                              ELSE
                                 OK = .TRUE.
                              END IF
                           END IF

*  If the line is correctly formatted, then right-justify it if
*  required, so long as we know there is more text to follow in the
*  same input line or paragraph.
                           IF ( OK ) THEN
                              IF ( RJUST .AND. ( .NOT. EMPTY ) ) THEN
                                 CALL NDF1_RJUST(
     :                              OUT( 1 )( LOUT + 1 : LOUT + L ) )
                              END IF

*  Accept the line as part of the output.
                              LOUT = LOUT + L

*  If the output buffer is now full (there is no longer room for
*  further lines), then flush it by appending its contents to the
*  current history record.
                              IF ( ( MXOUT - LOUT ) .LT. L ) THEN
                                 CALL NDF1_HWRT( IDCB, APPN, LOUT / L,
     :                                         OUT( 1 )( : L ), STATUS )
                                 LOUT = 0
                              END IF
                           END IF

*  If the end of the text in BUF has been reached and it has all been
*  correctly formatted, then empty the buffer ready for more input.
                           IF ( EMPTY ) THEN
                              IF ( OK ) THEN
                                 LBUF = 0

*  If the last output line was not correctly formatted, then BUF
*  requires more input, so move its contents to the left, if necessary,
*  to eliminate leading characters which have so far been correctly
*  formatted. Then exit the formatting loop so as to process the next
*  input line.
                              ELSE
                                 IF ( FPLAST .NE. 1 ) THEN
                                    DO 2 I1 = FPLAST, LBUF
                                       I2 = I1 - FPLAST + 1
                                       BUF( I2 : I2 ) = BUF( I1 : I1 )
 2                                  CONTINUE
                                    LBUF = LBUF - FPLAST + 1
                                 END IF
                              END IF

*  If the end of BUF was not reached, then return to format another
*  output line using the text which remains in it.
                           ELSE
                              GO TO 1
                           END IF
                        END IF
                     END IF

*  Quit processing input lines if an error occurs.
                     IF ( STATUS .NE. SAI__OK ) GO TO 4
 3                CONTINUE
 4                CONTINUE
               END IF

*  If any lines remain in the output buffer, then write them to the
*  current history record.
               IF ( LOUT .GT. 0 ) CALL NDF1_HWRT( IDCB, APPN, LOUT / L,
     :                                            OUT( 1 )( : L ),
     :                                            STATUS )
            END IF
         END IF

*  Call error tracing routine.
         IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_HFWRT',
     :                                               STATUS )
      END IF

*  Before exiting, make a dummy call to MSG_LOAD to ensure that all
*  message tokens become undefined.
      CALL MSG_LOAD( ' ', ' ', BUF, NC, STATUS )

      END
