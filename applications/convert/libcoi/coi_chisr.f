      SUBROUTINE COI_CHISR( IMDESC, NDF, NHEAD, RETAIN, STATUS )
*+
*  Name:
*     COI_CHISR

*  Purpose:
*     Creates a HISTORY structure within an NDF from IRAF header
*     information.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COI_CHISR( IMDESC, NDF, NHEAD, RETAIN, STATUS )

*  Description:
*     The routine searches the IRAF header for the HISTORY keywords that
*     were written by COI_WHISR, and so create the HISTORY structure
*     in an NDF.  It does assume that the HISTORY text has not been
*     tampered.  It also flags these HISTORY headers so they may be
*     excluded from the FITS airlock.

*  Arguments:
*     IMDESC = INTEGER (Given)
*        The IMFORT file descriptor.
*     NDF = INTEGER (Given)
*        The identifier for the NDF to contain the axis structure.
*     NHEAD = INTEGER (Given)
*        Number of IRAF headers.
*     RETAIN( NHEAD ) = LOGICAL (Given and Returned)
*        Flags to indicate whether or not to propagate each IRAF header
*        line to the NDF's FITS airlock.  Headers that are used to
*        restore NDF HISTORY records will be flagged .FALSE. on exit.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     The NDF and IRAF files must be open, with the former open for
*     write access.

*  Copyright:
*     Copyright (C) 1997 Central Laboratory of the Research Councils.
*     Copyright (C) 2009 Science & Technology Facilities Council. All
*     Rights Reserved.

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
*     {enter_new_authors_here}

*  History:
*     1997 July 23 (MJC):
*        Original version.
*     2009 June 26 (MJC):
*        UPDATE_MODE may now have leading blanks that must be trimmed to
*        avoid an error from NDF when NDF accesses this component.
*        Merged in most of the modifications made to its sister routine
*        COF_CHISR.
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
      INTEGER IMDESC
      INTEGER NDF
      INTEGER NHEAD

*  Arguments Given and Returned:
      LOGICAL RETAIN( NHEAD )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Used length of a string

*  Local Constants:
      INTEGER MAXWID             ! Maximum number of characters in
                                 ! history additional text
      PARAMETER( MAXWID = 80 )

      INTEGER MAXWRD             ! Maximum number of words in line
      PARAMETER( MAXWRD = 7 )

*  Local Variables:
      INTEGER ALIGN              ! Alignment with respect to Column 9
      CHARACTER*( NDF__SZAPP ) APPN ! Application name
      CHARACTER*80 CARD          ! IRAF header card
      CHARACTER*( DAT__SZLOC ) CLOC ! Locator to a character component
      INTEGER CRCOL              ! Column where "Current record:" begins
      CHARACTER*( NDF__SZHDT ) CREATD ! History creation date
      LOGICAL CRETEX             ! Create TEXT component in a record?
      INTEGER CSIZE              ! Width of a character component
      INTEGER CURREC             ! Current record number
      CHARACTER*( NDF__SZHDT ) DATE ! History record date
      CHARACTER*( DAT__SZLOC ) ELOC ! Locator to element of RECORDS
      INTEGER END( MAXWRD )      ! End columns of words (not used)
      LOGICAL HISPRE             ! HISTORY records may be present?
      CHARACTER*( DAT__SZLOC ) HLOC ! Locator to HISTORY component
      CHARACTER*4 IC             ! Record number
      INTEGER IREC               ! Loop counter for history records
      INTEGER JREC               ! Loop counter for text lines
      INTEGER KINDEX             ! Keyword index
      CHARACTER*8 KEYWRD         ! IRAF keyword
      CHARACTER*( DAT__SZLOC ) LOC ! Locator to NDF
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
      CHARACTER*( MAXWID ) TEXT  ! HISTORY text
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

*  Start the search from the first card.
      KINDEX = 1

*  Loop until all the HISTORY records have been found.  Only continue
*  search when there are untested headers remaining.
  100 CONTINUE                 ! Start of DO WHILE loop
      IF ( HISPRE .AND. STATUS .EQ. SAI__OK .AND.
     :     KINDEX .LE. NHEAD ) THEN

*  Search for a HISTORY card.  KINDEX is updated with the keyword index
*  number if a HISTORY record is found.
         CALL COI_FHIST( IMDESC, NHEAD, KINDEX, HISPRE, STATUS )

         IF ( HISPRE ) THEN

*  Obtain the header card.
            CARD = ' '
            CALL GETLIN( IMDESC, KINDEX, CARD )

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

*  Obtain a locator for the NDF.
            CALL NDF_LOC( NDF, 'UPDATE', LOC, STATUS )

*  Create the HISTORY structure and obtain a locator to it.
            IF ( MAKHIS )
     :        CALL DAT_NEW( LOC, 'HISTORY', 'HISTORY', 0, 0, STATUS )
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
               CALL GETLIN( IMDESC, KINDEX - 1, CARD )
               IF ( CARD .EQ. ' ' ) RETAIN( KINDEX - 1 ) = .FALSE.
            END IF

*  Skip to the next header card.  Here we assume that these headers have
*  not been tampered.
            KINDEX = KINDEX + 1
            CALL GETLIN( IMDESC, KINDEX, CARD )

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

            IF ( STATUS .EQ. SAI__OK ) THEN
               CALL CHR_CTOI( CARD( CRCOL + 16: ), CURREC, STATUS )
               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL MSG_SETC( 'C', CARD )
                  CALL MSG_SETC( 'F', CARD( CRCOL + 16: ) )
                  CALL ERR_REP( 'COI_CHISR_ERR1', 'Bad integer field '//
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
               CALL GETLIN( IMDESC, KINDEX, CARD )

*  Record that this card is not to be propagated to the FITS airlock.
               RETAIN( KINDEX ) = .FALSE.

*  Calculate the length of the record number.
               CALL CHR_ITOC( IREC, IC, NC )

*  Get a locator to the cell.
               CALL DAT_CELL( RLOC, 1, IREC, ELOC, STATUS )

*  Extract for the date and convert it from KAPPA-style to its NDF form.
               DATE = CARD( 13 + NC: 36 + NC )
               CALL COF_DATEH( DATE, STATUS )

*  Make the DATE component and assign it the date via a locator.
               CALL DAT_NEW0C( ELOC, 'DATE', NDF__SZHDT, STATUS )
               CALL DAT_FIND( ELOC, 'DATE', CLOC, STATUS )
               CALL DAT_PUT0C( CLOC, DATE, STATUS )
               CALL DAT_ANNUL( CLOC, STATUS )

*  Extract for the command.
               APPN = CARD( 40 + NC: )
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
               CALL GETLIN( IMDESC, KINDEX, CARD )

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
               CALL GETLIN( IMDESC, KINDEX, CARD )

*  Record that this card is not to be propagated to the FITS airlock.
               RETAIN( KINDEX ) = .FALSE.

*  Extract the dataset name.
               REFER = CARD( 20: )
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
                  CALL GETLIN( IMDESC, KINDEX, CARD )

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
                  IF ( KEYWRD .NE. 'HISTORY' .OR. PARSKP ) THEN

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

*  Convert the text into paragraphs of WIDTH characters.
                        TEXCOL = 1
  160                   CONTINUE     ! Start of DO WHILE loop

*  TEXCOL = 0 indicates that there are no more lines in the paragraph.
*  increment the number of text lines.
                        IF ( TEXCOL .NE. 0 .AND.
     :                       STATUS .EQ. SAI__OK ) THEN
                           CALL CHR_PFORM( 1, PARAGR, .FALSE., TEXCOL,
     :                                     TEXT( :WIDTH ) )

*  Increment the text line counter, and extend the TEXT component.
                           JREC = JREC + 1
                           CALL DAT_ALTER( TLOC, 1, JREC, STATUS )

*  Get a locator to the cell and write the history record.
                           CALL DAT_CELL( TLOC, 1, JREC, TELOC,
     :                                    STATUS )
                           CALL DAT_PUT0C( TELOC, TEXT( :WIDTH ),
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
*         GOTO 100
      END IF

*  If an error has occurred, issue a warning and flush the error, so that
*  we can continue to build the rest of the NDF.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL NDF_MSG( 'NDF', NDF )
         CALL ERR_REP( 'COI_CHISR_ERR', 'The history information in '/
     :                 /'the output NDF ''^NDF'' may be corrupt.',
     :                 STATUS )
         CALL ERR_FLUSH( STATUS )
      END IF

      END
