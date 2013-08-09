      SUBROUTINE COF_CHISR( FUNIT, NDF, NHEAD, RETAIN, STATUS )
*+
*  Name:
*     COF_CHISR

*  Purpose:
*     Creates a HISTORY structure within an NDF from FITS header
*     information.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_CHISR( FUNIT, NDF, NHEAD, RETAIN, STATUS )

*  Description:
*     The routine searches the FITS header for the HISTORY keywords that
*     were written by COF_WHISR, and so create the HISTORY structure
*     in an NDF.  It does assume that the HISTORY text has not been
*     tampered.  It also flags these HISTORY headers so they may be
*     excluded from the FITS airlock.

*  Arguments:
*     FUNIT = INTEGER (Given)
*        The FITSIO unit number for the FITS file.
*     NDF = INTEGER (Given)
*        The identifier for the NDF to contain the axis structure.
*     NHEAD = INTEGER (Given)
*        Number of FITS headers.
*     RETAIN( NHEAD ) = LOGICAL (Given and Returned)
*        Flags to indicate whether or not to propagate each FITS header
*        line to the NDF's FITS airlock.  Headers that are used to
*        restore NDF HISTORY records will be flagged .FALSE. on exit.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     The NDF and FITS files must be open, the former with write access.

*  Copyright:
*     Copyright (C) 1997, 1999, 2001, 2004 Central Laboratory of the
*     Research Councils. Copyright (C) 2009 Science & Technology
*     Facilities Council. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     AJC: Alan J. Chipperfield (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1997 March 5 (MJC):
*        Original version.
*     1997 July 30 (MJC):
*        Truncated the character strings in the HISTORY records.  Added
*        protection against long lines of additional history text.
*     1997 November 16 (MJC):
*        Added NHEAD and RETAIN arguments.  Flag NDF-style HISTORY
*        records by setting their corresponding RETAIN elements to
*        .FALSE..
*      8-OCT-1999 (DSB):
*        Terminate all DO WHILE loops if an error occurs within the loop.
*        Report an error if CHR conversion routines returns a bad status.
*        Flush any error at the end of this routine so that the output NDF
*        can continue to be built.
*      1-MAY-2001 (AJC):
*        Allow any non-HISTORY card to terminate a HISTORY record
*     2004 August 25 (MJC):
*        Allowed for the shift in alignment made by CFITSIO starting
*        in column 11 from 9, but also still valid for old data written
*        with the original alignment.
*     2004 September 10 (TIMJ):
*        Fix valgrind warning with uninitialised CARD on entry
*        to fitsio routine.
*     2009 June 26 (MJC):
*        UPDATE_MODE may now have leading blanks that must be trimmed to
*        avoid an error from NDF when NDF accesses this component.
*     2009 June 30 (MJC):
*        Indent paragraphs of text after the first line except for
*        Software, Arguments, and Parameters headings for which a new
*        line is created.
*     2009 October 16 (MJC):
*        Added Group heading to the list of records not indented.
*     2009 October 22 (MJC):
*        Allow for more than one Group heading, and for multiple
*        headings in the buffer extracted from a history paragraph.
*        There may still be other special cases.
*     15-OCT-2010 (DSB):
*        - Ensure CURREC is initialised even if an error has occurred.
*        - Delete any pre-existing HISTORY component before creating a
*          new one.
*     12-NOV-2010 (DSB):
*        Deleting the pre-existing history component using HDS confuses
*        NDF. So delete it using NDF instead.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'NDF_PAR'          ! NDF__ constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT public constants

*  Arguments Given:
      INTEGER FUNIT
      INTEGER NDF
      INTEGER NHEAD

*  Arguments Given and Returned:
      LOGICAL RETAIN( NHEAD )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_INDEX
      INTEGER CHR_INDEX          ! Character position of a substring

      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Length of a string less trailing
                                 ! blanks

*  Local Constants:
      INTEGER FITSOK             ! Good status for FITSIO library
      PARAMETER( FITSOK = 0 )

      INTEGER INDENT             ! Paragraph indentation in characters
      PARAMETER( INDENT = 3 )

      INTEGER MAXWID             ! Maximum number of characters in
                                 ! history additional text
      PARAMETER( MAXWID = NDF__SZHMX )

      INTEGER MAXWRD             ! Maximum number of words in line
      PARAMETER( MAXWRD = 7 )

*  Local Variables:
      INTEGER ALIGN              ! Alignment with respect to Column 9
      CHARACTER*( NDF__SZAPP ) APPN ! Application name
      INTEGER BCOL               ! Column position in indentation buffer
      CHARACTER*( MAXWID ) BUF   ! Buffer for paragraph indentation
      CHARACTER*80 CARD          ! FITS header card
      CHARACTER*( DAT__SZLOC ) CLOC ! Locator to a character component
      INTEGER CPHEAD             ! Character position of heading
      INTEGER CPHED2             ! Character position of second heading
      INTEGER CRCOL              ! Column where "Current record:" begins
      CHARACTER*( NDF__SZHDT ) CREATD ! History creation date
      LOGICAL CRETEX             ! Create TEXT component in a record?
      INTEGER CSIZE              ! Width of a character component
      INTEGER CURREC             ! Current record number
      CHARACTER*( NDF__SZHDT ) DATE ! History record date
      CHARACTER*( DAT__SZLOC ) ELOC ! Locator to element of RECORDS
      INTEGER END( MAXWRD )      ! End columns of words (not used)
      INTEGER FSTAT              ! FITSIO status
      LOGICAL HISPRE             ! HISTORY records may be present?
      CHARACTER*( DAT__SZLOC ) HLOC ! Locator to HISTORY component
      CHARACTER*4 IC             ! Record number
      INTEGER IARG               ! Index of "Arguments:" heading
      INTEGER IGRP               ! Index of "Group:" heading
      INTEGER IGRP2              ! Index of second "Group:" heading
      INTEGER IPAR               ! Index of "Parameters:" heading
      INTEGER IREC               ! Loop counter for history records
      INTEGER ISOF               ! Index of "Software:" heading
      INTEGER JREC               ! Loop counter for text lines
      INTEGER KINDEX             ! Keyword index
      CHARACTER*8 KEYWRD         ! FITS keyword
      CHARACTER*( DAT__SZLOC ) LOC ! Locator to NDF
      INTEGER LWIDTH             ! Width of line to write to history
      INTEGER LSTAT              ! Local status value
      LOGICAL MAKHIS             ! Make HISTORY structure?
      CHARACTER*( NDF__SZHUM ) MODE ! Update mode
      LOGICAL MORTEX             ! More text lines to process?
      INTEGER NC                 ! Number of characters
      INTEGER NEXREC             ! Number of existing HISTORY records
      INTEGER NWORD              ! Number of words in HISTORY card
      CHARACTER*2048 PARAGR      ! Paragraph of HISTORY text
      INTEGER PARCOL             ! Paragraph column where to append text
      LOGICAL PARSKP             ! There is a paragraph of text?
      CHARACTER*( NDF__SZREF ) REFER ! Reference dataset
      CHARACTER*( DAT__SZLOC ) RLOC ! Locator to RECORDS component
      INTEGER START( MAXWRD )    ! Start columns of words (not used)
      CHARACTER*( DAT__SZLOC ) TLOC ! Locator to TEXT component
      CHARACTER*( DAT__SZLOC ) TELOC ! Locator to element of TEXT
      INTEGER TEXCOL             ! Line number in HISTORY-text paragraph
      CHARACTER*( MAXWID ) TEXT ! HISTORY text
      LOGICAL THERE              ! Object exists?
      LOGICAL VALID              ! Locator valid?
      INTEGER WIDTH              ! Width in characters of history
                                 ! text
      CHARACTER*20 WORDS( MAXWRD ) ! Words in a HISTORY card

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise DO WHILE flag, and flag to indicate HISTORY structure
*  has yet to be written.
      HISPRE = .TRUE.
      MAKHIS = .TRUE.
      NEXREC = 0

*  Initialise the FITSIO status.  It's not the same as the Starlink
*  status, which is reset by the fixed part.
      FSTAT = FITSOK

*  Start the search from the first card.
      KINDEX = 1

*  Loop until all the HISTORY records have been found.  Only continue
*  search when there are untested headers remaining.
  100 CONTINUE                 ! Start of DO WHILE loop
      IF ( HISPRE .AND. STATUS .EQ. SAI__OK .AND.
     :     KINDEX .LE. NHEAD ) THEN

*  Search for a HISTORY card.  KINDEX is updated with the keyword index
*  number if a HISTORY record is found.
         CALL COF_FHIST( FUNIT, KINDEX, HISPRE, STATUS )

         IF ( HISPRE ) THEN

*  Obtain the header card.
            CARD = ' '
            CALL FTGREC( FUNIT, KINDEX, CARD, FSTAT )

*  Was the HISTORY card written by COF_WHISR?  Need to find the heading.
*  If this HISTORY card does not contain it, search for another HISTORY
*  card, starting from the next card.  Search from the first possible
*  location but allow for some formatting offset.
            ALIGN = INDEX( CARD( 9:80 ), 'History structure' )
            IF ( ALIGN .LT. 1 .OR. ALIGN .GT. 5 ) THEN
               KINDEX = KINDEX + 1
               GOTO 100
            END IF

*  Alignment correction is with respect to column 9.  ALIGN would be 1
*  for Column 9 when it should be zero.  The old alignment was to
*  Column 11, so ALIGN would be 2.
            ALIGN = ALIGN - 1

*  Create HISTORY structure.
*  =========================

*  If the NDF tuning parameter AUTO_HISTORY is set, the NDF will already
*  contain a HISTORY structure. So delete any pre-existing HISTORY structure
*  before creating the new one.
            CALL NDF_STATE( NDF, 'HISTORY', THERE, STATUS )
            IF( THERE ) CALL NDF_RESET( NDF, 'HISTORY', STATUS )

*  Obtain a locator for the NDF.
            CALL NDF_LOC( NDF, 'UPDATE', LOC, STATUS )

*  Create the HISTORY structure and obtain a locator to it.
            IF ( MAKHIS ) CALL DAT_NEW( LOC, 'HISTORY', 'HISTORY', 0, 0,
     :                                  STATUS )
            CALL DAT_FIND( LOC, 'HISTORY', HLOC, STATUS )

*  Annul the locator to the NDF.
            CALL DAT_ANNUL( LOC, STATUS )

*  Extract the creation date from the header card.
            CREATD = CARD( 35 + ALIGN:58 + ALIGN )

*  Convert the date from the KAPPA-style to the NDF format.
            CALL COF_DATEH( CREATD, STATUS )

*  Make the CREATED component and assign it the creation date via a
*  locator.
            IF ( MAKHIS )
     :        CALL DAT_NEW0C( HLOC, 'CREATED', NDF__SZHDT, STATUS )
            CALL DAT_FIND( HLOC, 'CREATED', CLOC, STATUS )
            CALL DAT_PUT0C( CLOC, CREATD, STATUS )
            CALL DAT_ANNUL( CLOC, STATUS )

*  Record that this card is not to be propagated to the FITS airlock.
            RETAIN( KINDEX ) = .FALSE.

*  Obtain the previous header card from the first HISTORY record (if a
*  previous card exists).  NDF HISTORY written by NDF2FITS is preceded
*  by a blank line.  This line should be removed to restore the former
*  appearance of the headers.
            IF ( KINDEX .GT. 1 .AND. MAKHIS ) THEN
               CALL FTGREC( FUNIT, KINDEX - 1, CARD, FSTAT )
               IF ( CARD .EQ. ' ' ) RETAIN( KINDEX - 1 ) = .FALSE.
            END IF

*  Skip to the next header card.  Here we assume that these headers have
*  not been tampered.
            KINDEX = KINDEX + 1
            CALL FTGREC( FUNIT, KINDEX, CARD, FSTAT )

*  Record that this card is not to be propagated to the FITS airlock.
            RETAIN( KINDEX ) = .FALSE.

*  Obtain the update mode.
            MODE = CARD( 22 + ALIGN:NDF__SZHUM + 21 + ALIGN )
            CALL CHR_LDBLK( MODE )

*  Make the UPDATE_MODE component and assign it the update mode via a
*  locator.
            IF ( MAKHIS )
     :        CALL DAT_NEW0C( HLOC, 'UPDATE_MODE', NDF__SZHUM, STATUS )
            CALL DAT_FIND( HLOC, 'UPDATE_MODE', CLOC, STATUS )
            CALL DAT_PUT0C( CLOC, MODE, STATUS )
            CALL DAT_ANNUL( CLOC, STATUS )

*  Obtain the current record.  Validate that it's an integer.
            CRCOL = INDEX( CARD, 'Current record:' )

            CURREC = 0
            IF ( STATUS .EQ. SAI__OK ) THEN
               CALL CHR_CTOI( CARD( CRCOL + 16: ), CURREC, STATUS )
               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL MSG_SETC( 'C', CARD )
                  CALL MSG_SETC( 'F', CARD( CRCOL + 16: ) )
                  CALL ERR_REP( 'COF_CHISR_ERR1', 'Bad integer field '//
     :                          '''^F'' in FITS card ''^C''.', STATUS )
               END IF
            END IF

*  Make the CURRENT_RECORD component and assign it the record number
*  via a locator.
            IF ( MAKHIS )
     :        CALL DAT_NEW0I( HLOC, 'CURRENT_RECORD', STATUS )
            CALL DAT_FIND( HLOC, 'CURRENT_RECORD', CLOC, STATUS )
            CALL DAT_PUT0I( CLOC, CURREC, STATUS )
            CALL DAT_ANNUL( CLOC, STATUS )

*  Create the RECORDS structure.
*  =============================
*
*  At this point we don't know how many records there are, so we create
*  a reasonable number to start with, which can be extended if needed,
*  and unused records truncated at the end.

*  Create the RECORDS structure and obtain a locator to it.
            IF ( MAKHIS ) THEN
               CALL DAT_NEW( HLOC, 'RECORDS', 'HIST_REC', 1, CURREC,
     :                       STATUS )
               CALL DAT_FIND( HLOC, 'RECORDS', RLOC, STATUS )

*  Find the current size of the HISTORY records.  Enlarge it as required
*  to store the additional records.
            ELSE
               CALL DAT_FIND( HLOC, 'RECORDS', RLOC, STATUS )
               CALL DAT_ALTER( RLOC, 1, NEXREC, STATUS )
            END IF

*  Skip over the expected blank line in the header.
            KINDEX = KINDEX + 1

*  Record that this card is not to be propagated to the FITS airlock.
            RETAIN( KINDEX ) = .FALSE.

*  Create a new RECORDS element.
*  =============================
            DO IREC = NEXREC + 1, NEXREC + CURREC

*  Skip to the next header card.  Here we assume that these headers have
*  not been tampered.
               KINDEX = KINDEX + 1
               CALL FTGREC( FUNIT, KINDEX, CARD, FSTAT )

*  Record that this card is not to be propagated to the FITS airlock.
               RETAIN( KINDEX ) = .FALSE.

*  Calculate the length of the record number.
               CALL CHR_ITOC( IREC, IC, NC )

*  Get a locator to the cell.
               CALL DAT_CELL( RLOC, 1, IREC, ELOC, STATUS )

*  Extract for the date and convert it from KAPPA-style to its NDF form.
               NC = NC + ALIGN
               DATE = CARD( 11 + NC: 34 + NC )
               CALL COF_DATEH( DATE, STATUS )

*  Make the DATE component and assign it the date via a locator.
               CALL DAT_NEW0C( ELOC, 'DATE', NDF__SZHDT, STATUS )
               CALL DAT_FIND( ELOC, 'DATE', CLOC, STATUS )
               CALL DAT_PUT0C( CLOC, DATE, STATUS )
               CALL DAT_ANNUL( CLOC, STATUS )

*  Extract for the command.
               APPN = CARD( 38 + NC: )
               CSIZE = MAX( 1, MIN( CHR_LEN( APPN ), NDF__SZAPP ) )

*  Make the COMMAND component and assign it the application name via a
*  locator.
               CALL DAT_NEW0C( ELOC, 'COMMAND', CSIZE, STATUS )
               CALL DAT_FIND( ELOC, 'COMMAND', CLOC, STATUS )
               CALL DAT_PUT0C( CLOC, APPN, STATUS )
               CALL DAT_ANNUL( CLOC, STATUS )

*  Skip to the next header card.  We assume that these headers have
*  not been tampered.
               KINDEX = KINDEX + 1
               CALL FTGREC( FUNIT, KINDEX, CARD, FSTAT )

*  Record that this card is not to be propagated to the FITS airlock.
               RETAIN( KINDEX ) = .FALSE.

*  Break the line into words.
               CALL CHR_DCWRD( CARD, MAXWRD, NWORD, START, END, WORDS,
     :                         LSTAT )

*  The username is the third word.  Make the USER component and assign
*  it the username via a locator.
               CSIZE = MIN( END( 3 ) - START( 3 ) + 1, NDF__SZUSR )
               CALL DAT_NEW0C( ELOC, 'USER', CSIZE, STATUS )
               CALL DAT_FIND( ELOC, 'USER', CLOC, STATUS )
               CALL DAT_PUT0C( CLOC, WORDS( 3 ), STATUS )
               CALL DAT_ANNUL( CLOC, STATUS )

*  The host machine is the fifth word.  Make the HOST component and assign
*  it the machine name via a locator.
               CSIZE = MIN( END( 5 ) - START( 5 ) + 1, NDF__SZHST )
               CALL DAT_NEW0C( ELOC, 'HOST', CSIZE, STATUS )
               CALL DAT_FIND( ELOC, 'HOST', CLOC, STATUS )
               CALL DAT_PUT0C( CLOC, WORDS( 5 ), STATUS )
               CALL DAT_ANNUL( CLOC, STATUS )

*  The width is the seventh word.
               IF ( STATUS .EQ. SAI__OK ) THEN
                  CALL CHR_CTOI( WORDS( 7 ), WIDTH, STATUS )
                  IF ( STATUS .NE. SAI__OK ) THEN
                     CALL MSG_SETC( 'C', CARD )
                     CALL MSG_SETC( 'F', WORDS( 7 ) )
                     CALL ERR_REP( 'COF_CHISR_ERR1', 'Bad integer '//
     :                             'field ''^F'' in FITS card ''^C''.',
     :                             STATUS )
                  END IF
               END IF

               WIDTH = MIN( MAXWID, WIDTH )

*  Skip to the next header card.  We assume that these headers have
*  not been tampered.
               KINDEX = KINDEX + 1
               CALL FTGREC( FUNIT, KINDEX, CARD, FSTAT )

*  Record that this card is not to be propagated to the FITS airlock.
               RETAIN( KINDEX ) = .FALSE.

*  Extract the dataset name.
               REFER = CARD( 18 + ALIGN: )
               CSIZE = MAX( 1, MIN( CHR_LEN( REFER ), NDF__SZREF ) )

*  Make the DATASET component and assign it the reference dataset via a
*  locator.
               CALL DAT_NEW0C( ELOC, 'DATASET', CSIZE, STATUS )
               CALL DAT_FIND( ELOC, 'DATASET', CLOC, STATUS )
               CALL DAT_PUT0C( CLOC, REFER, STATUS )
               CALL DAT_ANNUL( CLOC, STATUS )

*  Initialise the more-text and create text flags, and column and
*  record counters.
               MORTEX = .TRUE.
               CRETEX = .TRUE.
               PARCOL = 0
               JREC = 0
  140          CONTINUE
               IF ( MORTEX ) THEN

*  Skip to the next header card.  We assume that these headers have
*  not been tampered.
                  KINDEX = KINDEX + 1
                  CALL FTGREC( FUNIT, KINDEX, CARD, FSTAT )

*  Record that this card is not to be propagated to the FITS airlock,
*  provided it is HISTORY.
                  KEYWRD = CARD( 1:8 )
                  IF ( CARD .EQ. ' '  .OR.
     :                 KEYWRD .EQ. 'HISTORY' ) THEN
                     RETAIN( KINDEX ) = .FALSE.
                  END IF

*  See if this is the end of a history text paragraph i.e blank HISTORY text
*  or end of HISTORY record.
*  Any non-HISTORY card will denote end of HISTORY record.

*  Set PARSKP if end of paragraph within a history record
                  PARSKP = KEYWRD .EQ. 'HISTORY' .AND.
     :                     CARD( 9 + ALIGN: ) .EQ. ' '
                  IF ( KEYWRD .NE. 'HISTORY' .OR.
     :                 PARSKP ) THEN

*  The END card is the last header card so end the search for further
*  HISTORY cards.
                     IF ( KEYWRD .EQ. 'END' ) HISPRE = .FALSE.

*  Is there any text to write?
                     IF ( PARCOL .GT. 0 ) THEN

*  Create the TEXT component and get a locator to it.  This should only
*  be done for the first paragraph.
                        IF ( CRETEX ) THEN
                           CALL DAT_NEW1C( ELOC, 'TEXT', WIDTH, 1,
     :                                     STATUS )
                           CALL DAT_FIND( ELOC, 'TEXT', TLOC, STATUS )
                           CRETEX = .FALSE.
                        END IF

*  Convert the text into paragraphs of up to WIDTH characters.
                        TEXCOL = 1
                        PARAGR = ' '
  160                   CONTINUE     ! Start of DO WHILE loop

*  TEXCOL = 0 indicates that there are no more lines in the paragraph.
*  increment the number of text lines.
                        IF ( TEXCOL .NE. 0 .AND.
     :                       STATUS .EQ. SAI__OK ) THEN

*  Text in paragraph is indented, except for the first line of the
*  paragraph, or for lines starting with a heading.
                           BUF = PARAGR( TEXCOL:TEXCOL + WIDTH - 1 )
                           IARG = CHR_INDEX( BUF, 'Arguments:' )
                           IGRP = CHR_INDEX( BUF, 'Group:' )
                           IPAR = CHR_INDEX( BUF, 'Parameters:' )
                           ISOF = CHR_INDEX( BUF, 'Software:' )

*  Need to allow for short text after a heading, where more than
*  one heading exists within the allowed paragraph width.  First find
*  the first heading, so want to find the minimum positive offset.  If
*  we add more headings make IARG, IGRP etc. an array.
                            CPHEAD = WIDTH
                            IF ( IARG .GT. 0 )
     :                        CPHEAD = MIN( CPHEAD, IARG )
                            IF ( IGRP .GT. 0 )
     :                        CPHEAD = MIN( CPHEAD, IGRP )
                            IF ( IPAR .GT. 0 )
     :                        CPHEAD = MIN( CPHEAD, IPAR )
                            IF ( ISOF .GT. 0 )
     :                        CPHEAD = MIN( CPHEAD, ISOF )

*  If the heading is short there may be a later heading within the
*  allowed width.  Only want to extract up to but not including the
*  second heading.  Note successive Group headings may appear.
                            CPHED2 = MAX( IARG, IGRP, IPAR, ISOF )
                            IGRP2 = CHR_INDEX( BUF( IGRP + 1: ),
     :                                         'Group:' )
                            IF ( IGRP2 .GT. 0 ) THEN
                               LWIDTH = IGRP2 - 1
                            ELSE IF ( CPHED2 .GT. CPHEAD ) THEN
                               LWIDTH = MIN( CPHED2 - 1, WIDTH )
                            ELSE
                               LWIDTH = WIDTH
                            END IF

*  First line of paragraph, hence no indentation.
                           IF ( IREC .EQ. 0 ) THEN
                              CALL CHR_PFORM( 1, PARAGR, .FALSE.,
     :                                        TEXCOL, TEXT( :LWIDTH ) )

*  A heading would fit in the next output line.
                           ELSE IF ( IARG .GT. 0 .OR. IGRP .GT. 0 .OR.
     :                               IPAR .GT. 0 .OR. ISOF .GT. 0 ) THEN

*  The heading is at the current paragraph position.  So treat it like
*  the first line.
                              IF ( CPHEAD .EQ. 1 ) THEN
                                 CALL CHR_PFORM( 1, PARAGR, .FALSE.,
     :                                           TEXCOL,
     :                                           TEXT( :LWIDTH ) )

*  It seems with Group that a leading space appears that prevents the
*  Group heading from not being indented.  So shift the current index
*  within the paragraph pointer by one to skip over the space.  Then
*  index of the heading will be one and indented correctly.  It has
*  no affect when it's a continuation (indented) line, as leading spaces
*  are expected and removed.  Note we must not change TEXCOL, when
*  CHR_PFORM returns it as zero, since this indicates that the
*  paragraph has ended, and it is time to move on to the next HISTORY
*  record.
                                 IF ( TEXCOL .NE. 0 )
     :                             TEXCOL = TEXCOL + 1

*  Just transfer up to but excluding the heading, indenting the regulation
*  three characters.  Shift the position within paragraph to be at the
*  heading.  There may still be leading blanks present in the paragraph,
*  so remove those first to give consistent alignment.  Note we need to
*  set the paragraph position before this shift.
                              ELSE
                                 BCOL = INDENT
                                 TEXT = ' '
                                 TEXCOL = TEXCOL + CPHEAD - 1
                                 CALL CHR_LDBLK( BUF )

*  Obtain the new position of the heading.
                                 IARG = CHR_INDEX( BUF, 'Arguments:' )
                                 IGRP = CHR_INDEX( BUF, 'Group:' )
                                 IPAR = CHR_INDEX( BUF, 'Parameters:' )
                                 ISOF = CHR_INDEX( BUF, 'Software:' )

*  Again find the first heading.
                                 CPHEAD = WIDTH
                                 IF ( IARG .GT. 0 )
     :                             CPHEAD = MIN( CPHEAD, IARG )
                                 IF ( IGRP .GT. 0 )
     :                             CPHEAD = MIN( CPHEAD, IGRP )
                                 IF ( IPAR .GT. 0 )
     :                             CPHEAD = MIN( CPHEAD, IPAR )
                                 IF ( ISOF .GT. 0 )
     :                             CPHEAD = MIN( CPHEAD, ISOF )

                                 CALL CHR_APPND( BUF( :CPHEAD - 1 ),
     :                                           TEXT, BCOL )
                              END IF

*  Paragraph lines not containing a heading are indented.
                           ELSE
                              TEXT = ' '
                              CALL CHR_PFORM( 1, PARAGR, .FALSE.,
     :                                        TEXCOL, TEXT( 4:LWIDTH ) )
                           END IF

*  Increment the text line counter, and extend the TEXT component.
                           JREC = JREC + 1
                           CALL DAT_ALTER( TLOC, 1, JREC, STATUS )

*  Get a locator to the cell and write the history record.
                           CALL DAT_CELL( TLOC, 1, JREC, TELOC,
     :                                    STATUS )
                           CALL DAT_PUT0C( TELOC, TEXT( :LWIDTH ),
     :                                     STATUS )

*  Release the locator to the TEXT element.
                           CALL DAT_ANNUL( TELOC, STATUS )

*  Need to add a blank line if there is another paragraph.
                           IF ( PARSKP ) THEN

*  Increment the text line counter, and extend the TEXT component.
                              JREC = JREC + 1
                              CALL DAT_ALTER( TLOC, 1, JREC, STATUS )

*  Get a locator to the cell and write the blank history record.
                              CALL DAT_CELL( TLOC, 1, JREC, TELOC,
     :                                       STATUS )
                              CALL DAT_PUT0C( TELOC, ' ', STATUS )

*  Release the locator to the TEXT element.
                              CALL DAT_ANNUL( TELOC, STATUS )
                           END IF

*  Return to start of DO WHILE loop.
                           GOTO 160
                        END IF
                     END IF

*  We can start a new paragraph.
                     IF ( PARSKP ) PARCOL = 0
                  ELSE

*  Append the current record to the paragraph buffer.
                     CALL CHR_APPND( CARD( 9 + ALIGN: ), PARAGR,
     :                               PARCOL )
                     IF ( CARD( 80:80 ) .EQ. ' ' ) PARCOL = PARCOL + 1
                  END IF

*  Are there more text records?
                  MORTEX = KEYWRD .EQ. 'HISTORY'

*  Return to start of DO WHILE loop.
                  GOTO 140

*  Annul the locator to the TEXT component.
               ELSE
                  CALL DAT_VALID( TLOC, VALID, STATUS )
                  IF ( VALID ) CALL DAT_ANNUL( TLOC, STATUS )
               END IF

*  Annul the locator to the element of the RECORDS structure.
               CALL DAT_ANNUL( ELOC, STATUS )
            END DO

*  Annul the locator to the RECORD and HISTORY structures.
            CALL DAT_ANNUL( RLOC, STATUS )
            CALL DAT_ANNUL( HLOC, STATUS )

         END IF

*  HISTORY structure created.
         MAKHIS = .FALSE.

*  Update the count of the number of records.
         NEXREC = NEXREC + CURREC

*  For the moment, assume there is only one set of NDF-style HISTORY
*  headers.  This should only fail if the headers have been tampered,
*  i.e. divided.  In normal circumstances this loop shouldn't be
*  required anyway.
*
*  Return to start of DO WHILE loop.
*        GOTO 100
      END IF

*  Check for a FITSIO error.  Handle a bad status.  Negative values are
*  reserved for non-fatal warnings.  203 is trying to read beyond the
*  last header.
      IF ( FSTAT .GT. FITSOK .AND. FSTAT .NE. 203 ) THEN
         CALL COF_FIOER( FSTAT, 'COF_CHISR_ERR', 'FTGREC',
     :                   'Error converting HISTORY header card to NDF '/
     :                   /'HISTORY component.', STATUS )
      END IF

*  If an error has occurred, issue a warning and flush the error, so that
*  we can continue to build the rest of the NDF.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL NDF_MSG( 'NDF', NDF )
         CALL ERR_REP( 'COF_CHISR_ERR', 'The history information in '/
     :                 /'the output NDF ''^NDF'' may be corrupt.',
     :                 STATUS )
         CALL ERR_FLUSH( STATUS )
      END IF

      END
