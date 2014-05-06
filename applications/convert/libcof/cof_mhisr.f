      SUBROUTINE COF_MHISR( FUNIT, ALIGN, NHEAD, RETAIN, KINDEX, HISREC,
     :                      STATUS )
*+
*  Name:
*     COF_MHISR

*  Purpose:
*     Extracts history information from FITS header with comntinuations

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_MHISR( FUNIT, ALIGN, NHEAD, RETAIN, KINDEX, HISREC
*                     STATUS )

*  Description:
*     The routine extracts NDF2FITS-generated (written by CVG_WHISR)
*     history text from a FITS file for the current heading and returns
*     the associated text.  The text may span more than one header
*     record and as much of the text that can fit within the supplied
*     HISREC argument is returned.  The HISTORY headers for the heading,
*     however, are read completely, so that the current location is
*     ready to parse the next heading.
*
*     This routine is intended for records comprising the epoch and
*     command, and the Dataset and Software records.  These can all
*     be wrapped to multiple lines now by FITSIO, whereas it previously
*     had truncated to 72 characters.  It should not be used for the
*     Arguments, Group, Parameters, and Software written by NDF_HOUT
*     into paragraphs; these are handled by COF_CHISR.

*     This subroutine  does assume that the HISTORY text has not been
*     tampered.  It also flags the HISTORY headers for the current
*     heading so they may be excluded from the FITS airlock.

*  Arguments:
*     FUNIT = INTEGER (Given)
*        The FITSIO unit number for the FITS file.
*     ALIGN = INTEGER (Given)
*        Alignment of the history text with respect to Column 9 of the
*        FITS headers.
*     NHEAD = INTEGER (Given)
*        Number of FITS headers.
*     RETAIN( NHEAD ) = LOGICAL (Given and Returned)
*        Flags to indicate whether or not to propagate each FITS header
*        line to the NDF's FITS airlock.  Headers that are used to
*        restore NDF HISTORY records will be flagged .FALSE. on exit.
*     KINDEX = INTEGER (Given and Returned)
*        The index of the current FITS header.
*     HISREC = CHARACTER * ( * ) (Returned)
*        The extracted text from the FITS headers.  This should be
*        at least 144 characters long to accommodate two records.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     The FITS files must be open.

*  Copyright:
*     Copyright (C) 2014 Science & Technology Facilities Council.
*     All Rights Reserved.

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
*     2014 May 3 (MJC):
*        Original version.
*     {enter_changes_here}

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
      INTEGER ALIGN
      INTEGER NHEAD

*  Arguments Given and Returned:
      LOGICAL RETAIN( NHEAD )
      INTEGER KINDEX

*  Arguments Returned:
       CHARACTER * ( * ) HISREC

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

      INTEGER MAXWID             ! Maximum number of characters in
                                 ! history additional text
      PARAMETER( MAXWID = NDF__SZHMX )

      INTEGER MAXWRD             ! Maximum number of words in line
      PARAMETER( MAXWRD = 20 )

*  Local Variables:
      CHARACTER*( MAXWID ) BUF   ! Buffer for paragraph indentation
      CHARACTER*80 CARD          ! FITS header card
      INTEGER END( MAXWRD )      ! End columns of words (not used)
      INTEGER FSTAT              ! FITSIO status
      INTEGER IARG               ! Index of "Arguments:" heading
      INTEGER IGRP               ! Index of "Group:" heading
      INTEGER IPAR               ! Index of "Parameters:" heading
      INTEGER ISOF               ! Index of "Software:" heading
      CHARACTER*8 KEYWRD         ! FITS keyword
      INTEGER LSTAT              ! Local status value
      LOGICAL MORTEX             ! More text lines to process?
      INTEGER NWORD              ! Number of words in HISTORY card
      INTEGER PARCOL             ! Paragraph column where to append text
      INTEGER START( MAXWRD )    ! Start columns of words (not used)
      CHARACTER*( MAXWID ) TEXT ! HISTORY text
      CHARACTER*72 WORDS( MAXWRD ) ! Words in a HISTORY card

*.
      HISREC = ' '

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the FITSIO status.  It's not the same as the Starlink
*  status, which is reset by the fixed part.
      FSTAT = FITSOK

*  Skip to the next header card.  We assume that these headers have
*  not been tampered.
      KINDEX = KINDEX + 1
      CALL FTGREC( FUNIT, KINDEX, CARD, FSTAT )

*  Record that this card is not to be propagated to the FITS airlock,
*  provided it is HISTORY.
      KEYWRD = CARD( 1:8 )
      IF ( CARD .EQ. ' '  .OR. KEYWRD .EQ. 'HISTORY' ) THEN
         RETAIN( KINDEX ) = .FALSE.
      END IF

*  The long-line continuation is not appropriate for certain headings.
      BUF = CARD( 9: )
      IARG = CHR_INDEX( BUF, 'Arguments:' )
      IGRP = CHR_INDEX( BUF, 'Group:' )
      IPAR = CHR_INDEX( BUF, 'Parameters:' )
      ISOF = CHR_INDEX( BUF, 'Software:' )
      MORTEX = .NOT. ( IARG .NE. 0 .OR. IGRP .NE. 0 .OR.
     :                 IPAR .NE. 0 .OR. ISOF .NE. 0 )

      IF ( .NOT. MORTEX .AND. HISREC .EQ. ' ' ) THEN
         STATUS = SAI__ERROR
         IF ( IARG .NE. 0 ) CALL MSG_SETC( 'C', 'Arguments' )
         IF ( IGRP .NE. 0 ) CALL MSG_SETC( 'C', 'Group' )
         IF ( IPAR .NE. 0 ) CALL MSG_SETC( 'C', 'Parameters' )
         IF ( ISOF .NE. 0 ) CALL MSG_SETC( 'C', 'Software' )
         CALL ERR_REP( 'COF_MHISR_WRHEAD',
     :                 'COF_MHISR: called for a ^C heading derived '//
     :                 'from NDF HISTORY records and hence has '//
     :                 'neatly indented paragraphs rather than '//
     :                 'continuation lines (probable programming '//
     :                 'error).' )
         GO TO 999
      END IF

*  Break the header text into words.
      CALL CHR_DCWRD( CARD, MAXWRD, NWORD, START, END, WORDS, LSTAT )

*  Test that the urrent line is a heading.  This assumes that headings
*  comprise only a single word.
      IF ( WORDS( 2 )( END( 2 ):END( 2 ) ) .EQ. ':' ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'COF_MHISR_NOHEAD',
     :                 'COF_MHISR: Currently not located at a '//
     :                 'heading in HISTORY (probable programming '//
     :                 'error).' )
         GO TO 999
      END IF

*  The HISTORY header read is the start of the paragraph.  The next
*  question asks whether there are continuation lines made by FTPHIS.
      HISREC = BUF

*  Initialise the more-text and create text flags, and column and
*  record counters.
      MORTEX = .TRUE.
      PARCOL = END( NWORD ) - 8
  100 CONTINUE
      IF ( MORTEX ) THEN

*  Skip to the next header card.  We assume that these headers have
*  not been tampered.
         KINDEX = KINDEX + 1
         CALL FTGREC( FUNIT, KINDEX, CARD, FSTAT )

*  Record that this card is not to be propagated to the FITS airlock,
*  provided it is HISTORY.
         KEYWRD = CARD( 1:8 )
         IF ( CARD .EQ. ' '  .OR. KEYWRD .EQ. 'HISTORY' ) THEN
            RETAIN( KINDEX ) = .FALSE.
         END IF

*  Break the header text into words.
         CALL CHR_DCWRD( CARD, MAXWRD, NWORD, START, END, WORDS, LSTAT )

*  If it's a new heading, we have come too far.
         MORTEX = CARD( END( 2 ):END( 2 ) ) .NE. ':'

*  Any non-HISTORY card will denote end of HISTORY record.
         IF ( KEYWRD .EQ. 'HISTORY' .AND. MORTEX ) THEN

*  Append the current record to the paragraph buffer.
            CALL CHR_APPND( CARD( 9 + ALIGN: ), HISREC, PARCOL )
            IF ( CARD( 80:80 ) .EQ. ' ' ) PARCOL = PARCOL + 1
         END IF

*  Return to start of DO WHILE loop.
         GOTO 100
      END IF

*  Count back a HISTORY header, as the above search for continuation
*  lines will always overshoot one header beyond the information
*  required.
      KINDEX = KINDEX - 1

  999 CONTINUE

      END
