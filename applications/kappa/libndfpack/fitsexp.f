      SUBROUTINE FITSEXP( STATUS )
*+
*  Name:
*     FITSEXP

*  Purpose:
*     Exports NDF-extension information into an NDF FITS extension.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL FITSEXP( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application places the values of components of an NDF
*     extension into the FITS extension within the same NDF.  This
*     operation is needed if auxiliary data are to appear in the header
*     of a FITS file converted from the NDF.  The list of extension
*     components whose values are to be copied, their corresponding
*     FITS keyword names, optional FITS inline comments, and the
*     location of the new FITS header are specified in a "keyword
*     translation table" held in a separate text file.

*  Usage:
*     fitsexp ndf table

*  ADAM Parameters:
*     NDF = NDF (Read and Write)
*        The NDF in which the extension data are to be exported to
*        the FITS extension.
*     TABLE = FILE (Read)
*        The text file containing the keyword translation table. The
*        format of this file is described under "Table Format".

*  Examples:
*     fitsexp datafile fitstable.txt
*        This writes new FITS-extension elements for the NDF called
*        datafile, creating the FITS extension if it does not exist.
*        The selection of auxiliary components to export to the FITS
*        extension, their keyword names, locations, and comments
*        are under the control of a keyword translation table held in
*        the file fitstable.txt.

*  Notes:
*     -  Requests to assign values to the following reserved keywords
*     in the FITS extension are ignored: SIMPLE, BITPIX, NAXIS, NAXISn,
*     EXTEND, PCOUNT, GCOUNT, XTENSION, BLOCKED, and END.
*     -  Only scalar or one-element vector components may be
*     transferred to the FITS extension.
*     -  The data type of the component selects the type of the FITS
*     value.
*     -  If the destination keyword exists, the existing value and
*     comment are replaced with the new values.
*     -  If an error is found within a line, processing continues
*     to the next line and the error reported.
*     -  To be sure that the resultant FITS extension is what you
*     desired, you should inspect it using the command fitslist before
*     exporting the data.  If there is something wrong, you may find it
*     convenient to use command FITSEDIT to make minor corrections.

*  References:
*     "A User's Guide for the Flexible Image Transport System (FITS)",
*     NASA/Science Office of Science and Technology (1994).

*  Related Applications:
*     KAPPA: FITSEDIT, FITSHEAD, FITSLIST, FITSMOD; CONVERT: NDF2FITS.

*  Implementation Status:
*     -  The replacements are made in blocks of 32 to reduce the number
*     of time-consuming shuffles of the FITS extension.  Thus it is
*     possible to locate a new keyword before another keyword, provided
*     the latter keyword appears in an earlier block, though reliance
*     on this feature is discouraged; instead run the application
*     twice.
*     -  For each block the application inserts new cards or relocates
*     old ones, marking each with different tokens, and then sorts the
*     FITS extension into the requested order, removing the relocated
*     cards.  It then inserts the new values.  If there are multiple
*     occurrences of a keyword, this process can leave behind cards
*     having the token value '{undefined}'.

*  Table Format:
*     The keyword translation table should be held in a text file, with
*     one extension component specified per line.  Each line should
*     contain two or three fields, separated by spaces and/or tabs, as
*     follows.
*
*     -  Field 1:
*        The name of the input extension component whose value is to be
*        copied to the FITS extension.  For example, CCDPACK.FILTER
*        would copy the value of the component called FILTER in the
*        extension called CCDPACK; and IRAS90.ASTROMETRY.EQUINOX would
*        copy the value of component EQUINOX in the structure
*        ASTROMETRY in the extension IRAS90.  The extension may not be
*        FITS.
*
*     -  Field 2:
*        The name of the FITS keyword to which the value is to be
*        copied.  Hierarchical keywords are not permissible.  The
*        keyword name may be followed by a further keyword name in
*        parentheses (and no spaces).  This second keyword defines the
*        card before which the new keyword is to be placed.  If this
*        second keyword is not present in the FITS extension or is not
*        supplied, the new header card is placed at the end of the
*        existing cards, but immediately before any END card.  For
*        example, EQUINOX(EPOCH) would write the keyword EQUINOX
*        immediately before the existing card with keyword EPOCH.  FITS
*        keywords are limited to 8 characters and may only comprise
*        uppercase alphabetic characters, digits, underscore, and
*        hyphen.  While it is possible to have multiple occurrences of
*        the same keyword in a FITS header, it is regarded as bad
*        practice.  For this and efficiency reasons, this programme
*        only looks for the first appearance of a keyword when
*        substituting the values, and so only the last value inserted
*        appears in the final FITS extension.  (See "Implementation
*        Status".)
*
*     -  Field 3:
*        The comment to appear in the FITS header card for the chosen
*        keyword.  This field is optional.  As much of the comment will
*        appear in the header card as the value permits up to a maximum
*        of 47 characters.
*
*     Comments may appear at any point in the table and should begin
*     with an exclamation mark.  The remainder of the line will then be
*     ignored.

*  Timing:
*     Approximately proportional to the number of FITS keywords to be
*     translated.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
*     Copyright (C) 2004 Central Laboratory of the Research Councils.
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
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1994 July 15 (MJC):
*        Original version.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL.
*     2012 May 10 (MJC):
*        Add _INT64 support.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Data-system constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'FIO_ERR'          ! FIO_ error codes
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Significant length of a string

*  Local Constants:
      INTEGER COMLN              ! Maximum number of characters in an
                                 ! inline comment in a FITS header card
      PARAMETER ( COMLN = 47 )

      INTEGER CVALLN             ! Maximum number of characters in a
                                 ! string value in a FITS header card
      PARAMETER ( CVALLN = 68 )

      INTEGER FITSLN             ! No. of characters in a FITS header
      PARAMETER ( FITSLN = 80 )  ! card

      INTEGER KEYLN              ! Maximum number of characters in a
                                 ! FITS keyword
      PARAMETER ( KEYLN = 8 )

      INTEGER MXWRIT             ! Maximum number of sets of component
                                 ! information that can be stored
      PARAMETER ( MXWRIT = 32 )  ! card


*  Local Variables:
      INTEGER CARD               ! Fits header element (card) found
      INTEGER CDELIM             ! Character pointer to a delimiter
      CHARACTER * ( DAT__SZLOC ) CELLOC ! Locator to first element of
                                 ! FITS extension locator
      INTEGER CLAST              ! Previous character pointer to a
                                 ! delimiter
      CHARACTER * ( DAT__SZLOC ) CLOC ! Locator to a component or
                                 ! extension, and finally the export
                                 ! component
      CHARACTER * ( DAT__SZLOC ) CLOC2 ! Locator to a component
      CHARACTER * ( DAT__SZLOC ) CLOCS( MXWRIT ) ! Stored locators
      CHARACTER * ( COMLN ) COMENT ! Current inline comments
      CHARACTER * ( COMLN ) COMNTS( MXWRIT ) ! Stored comments
      CHARACTER * ( DAT__SZNAM ) COMP ! Component name in NDF extension
      CHARACTER * ( CVALLN ) CVAL ! Character FITS value
      DOUBLE PRECISION DVAL      ! Double precision FITS value
      INTEGER EL                 ! Number of FITS header records mapped
      CHARACTER * ( DAT__SZNAM ) EXTNAM ! NDF extension name
      INTEGER FDIM( 1 )          ! Initial length of the FITS extension
      LOGICAL FIRST              ! The component is first in the path?
      LOGICAL FOUND              ! Was a value found?
      INTEGER HIC                ! Position of comment character
      INTEGER I1( 3 )            ! Pointer to start of tokens
      INTEGER I2( 3 )            ! Pointer to end of tokens
      INTEGER ICOMP              ! Loop counter of stored components
      INTEGER IFIL               ! File descriptor
      INTEGER ILINE              ! Input line counter
      INTEGER INDF               ! NDF identifier
      INTEGER IVAL               ! Integer FITS value
      CHARACTER * ( KEYLN ) KEYIND ! Current position keyword
      CHARACTER * ( KEYLN ) KEYPOS( MXWRIT ) ! Stored position keywords
      CHARACTER * ( KEYLN ) KEYWRD ! Current keyword
      CHARACTER * ( KEYLN ) KEYWDS( MXWRIT ) ! Stored keywords
      INTEGER*8 KVAL             ! 64-bit integer FITS value
      INTEGER KWCPOS             ! Secondary keyword character pointer
      INTEGER L( 3 )             ! Lengths of the words in a line of the
                                 ! translation file
      LOGICAL LAST               ! The component is last in the path
      INTEGER LENGTH             ! Length of a character being mapped
      CHARACTER * ( 132 ) LINE   ! Buffer for file reading
      CHARACTER * ( DAT__SZLOC ) LOC ! FITS extension locator
      INTEGER LSTAT              ! Local status variable
      LOGICAL LVAL               ! Logical FITS value
      INTEGER NC                 ! Number of characters
      INTEGER NEL                ! Number of HDS values obtained
      INTEGER NTOK               ! Number of tokens on line
      INTEGER NWRITE             ! Number of names, comments, and
                                 ! locators stored
      INTEGER PNTR( 1 )          ! Pointer to mapped FITS headers
      REAL RVAL                  ! Real FITS value
      INTEGER SIZE               ! Number of values in the NDF component
      LOGICAL THERE              ! Whether extension is already there
      CHARACTER * ( 1 ) TOK( 2 ) ! Buffer for parsing lines
      CHARACTER * ( DAT__SZTYP ) TYPE ! Component's data type
      LOGICAL VALKEY             ! Whether keyword contains only valid
                                 ! chaaracters

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Access the FITS extension.
*  ==========================

*  Obtain the NDF to be updated.
      CALL LPG_ASSOC( 'NDF', 'UPDATE', INDF, STATUS )

*  See whether or not there is a FITS extension.
      CALL NDF_XSTAT( INDF, 'FITS', THERE, STATUS )
      IF ( .NOT. THERE ) THEN

*  Create the FITS extension.  Use an arbitrary size.  It will be
*  increased, if required, as new keywords are created.
         FDIM( 1 ) = 1
         CALL NDF_XNEW( INDF, 'FITS', '_CHAR*80', 1, FDIM, LOC, STATUS )

*  Since we want to map the FITS array in update mode, it must first
*  have a value to read.  Therefore write an END card to the first cell
*  of the array.
         CALL DAT_CELL ( LOC, 1, 1, CELLOC, STATUS )
         CALL DAT_PUT0C ( CELLOC, 'END   ', STATUS )
         CALL DAT_ANNUL ( CELLOC, STATUS )
      ELSE

*  Find the FITS extension.
         CALL NDF_XLOC( INDF, 'FITS', 'UPDATE', LOC, STATUS )
      END IF

*  Open the translation table file.
*  ================================
      CALL FIO_ASSOC( 'TABLE', 'READ', 'LIST', 0, IFIL, STATUS )

*  Defer error reporting.
      CALL ERR_MARK

*  Parse each line of the translation table.
*  =========================================

*  Initialise the count of header cards to write.
      NWRITE = 0

*  Loop to read from the file, counting the lines read.  The continue is
*  the start of a 'DO WHILE' loop.
      ILINE = 0
   10 CONTINUE
      CALL FIO_READ( IFIL, LINE, NC, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
         ILINE = ILINE + 1

*  If the line is not blank, then look for a comment delimiter. If
*  found, adjust the line length to omit the comment and remove any
*  resulting trailing blanks.
         IF ( NC. NE. 0 ) THEN
            HIC = INDEX( LINE( : NC ), '!' )
            IF ( HIC .NE. 0 ) THEN
               NC = HIC - 1
               IF ( NC .GT. 0 ) NC = CHR_LEN( LINE( : NC ) )
            END IF
         END IF

*  Ignore lines which are now blank.  Remove non-printable characters
*  (e.g. tabs) and leading blanks from the rest.  Decompose the line
*  into tokens.  Notice that since the comments are optional only two
*  tokens are searched for.
         IF ( NC .NE. 0 ) THEN
            CALL CHR_CLEAN( LINE( : NC ) )
            CALL CHR_LDBLK( LINE( : NC ) )
            CALL CHR_DCWRD( LINE( : NC ), 2, NTOK, I1, I2, TOK, LSTAT )

*  Report an error if fewer than two tokens were found.
            IF ( NTOK .LT. 2 ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'FITSEXP_2FEW',
     :           'Insufficient information on line in translation '/
     :           /'table.', STATUS )
               GOTO 30
            END IF

*  Ignore the bad status if more than two tokens were found; the
*  trailing text is the comment for the FITS card.  Skip over the space
*  after the keyword (hence the plus two).  Store the length of
*  the comment.  Set the null comment when there are only two items in
*  the list.
            IF ( LSTAT .NE. 0 ) THEN
               COMENT = LINE( I2( 2 ) + 2:NC )
               L( 3 ) = NC - I2( 2 ) - 1
            ELSE
               COMENT = ' '
            END IF

*  Work down the hierarchy to the object.
*  ======================================

*  Use the equivalent of a 'DO WHILE loop' to follow the path.

*  Initialise some variables for the loop, including the length of the
*  first field, namely the component path.
            L( 1 ) = I2( 1 ) - I1( 1 ) + 1
            CDELIM = 0
            CLOC = DAT__NOLOC
            FIRST = .TRUE.

*  Come here to search the next structure level for the component to
*  export.
   20       CONTINUE

*  Move through the path to find each component, searching in the
*  forward direction.  The character pointer is incremented by one
*  so as not to obtain the same delimiter as the previous call.
            CDELIM = CDELIM + 1
            CALL CHR_TOCHR( '.', LINE( I1( 1 ):I2( 1 ) ), .TRUE.,
     :                      CDELIM )

*  See whether or not this is the last component.
            LAST = CDELIM .GE. L( 1 )

*  The first name is the extension.  Check that there is a component
*  to copy.  If not report the error but continue on to the next
*  line of the translation table.
            IF ( FIRST ) THEN
               FIRST = .FALSE.
               IF ( INDEX( LINE( I1( 1 ):I2( 1 ) ), '.' ) .EQ. 0 ) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETC( 'TOK', LINE( I1( 1 ):I2( 1 ) ) )
                  CALL ERR_REP( 'FITSEXP_NOCOMP',
     :              'The line ^TOK does not specify an extension '/
     :              /'component.', STATUS )
                  GOTO 30
               END IF

*  Extract the name of the NDF extension.  If all has gone as expected
*  I1( 1 ) should be 1 (which if substituted might help to follow the
*  code).
               EXTNAM = LINE( I1( 1 ):I1( 1 ) + CDELIM - 2 )

*  Look to see if it exists.  If not report the error but continue on
*  to the next line of the translation table.
               CALL NDF_XSTAT( INDF, EXTNAM, THERE, STATUS )

               IF ( .NOT. THERE ) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETC( 'EXT', EXTNAM )
                  CALL ERR_REP( 'FITSEXP_NOEXT',
     :              'The extension ^EXT does not exist.', STATUS )
                  GOTO 30
               END IF

*  Get a locator to the extension.
               CALL NDF_XLOC( INDF, EXTNAM, 'READ', CLOC, STATUS )

            ELSE

*  Extract the component.
               COMP = LINE( I1( 1 ) + CLAST:I1( 1 ) + CDELIM - 2 )

*  Look to see if it exists.  If not report the error but continue on
*  to the next line of the translation table.
               CALL DAT_THERE( CLOC, COMP, THERE, STATUS )

               IF ( .NOT. THERE ) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETC( 'CMP',
     :                           LINE( I1( 1 ):I1( 1 ) + CDELIM - 2 ) )
                  CALL ERR_REP( 'FITSEXP_NOCMP',
     :              'The component ^CMP does not exist.', STATUS )
                  GOTO 30
               END IF

*  Get a locator to the component.
               CALL DAT_FIND( CLOC, COMP, CLOC2, STATUS )

*  Get the size of the object.
               CALL DAT_SIZE( CLOC2, SIZE, STATUS )

*  Only scalars (or one-element vectors) can be written to a FITS
*  header card.  Write an appropriate error message depending on whether
*  this is a structure or a component.
               IF ( SIZE .GT. 1 ) THEN
                  STATUS = SAI__ERROR
                  IF ( LAST ) THEN
                     CALL MSG_SETC( 'CMP', LINE( I1( 1 ):I2( 1 ) ) )
                     CALL ERR_REP( 'FITSEXP_ARRAYCMP',
     :                 'The component ^CMP is a vector and therefore '/
     :                 /'cannot be written to a FITS header card.',
     :                 STATUS )
                  ELSE
                     CALL MSG_SETC( 'CMP', LINE( I1( 1 ):I1( 1 ) +
     :                              CDELIM - 2 ) )
                     CALL ERR_REP( 'FITSEXP_ARRAYSTR',
     :                 'The alleged structure ^CMP is a vector and '/
     :                 /'therefore the choice of component within it '/
     :                 /'is ambiguous.', STATUS )
                  END IF
                  GOTO 30
               END IF

*  Annul the former extension component and reset the component locator
*  to point to the new component.
               IF ( CLOC .NE. DAT__NOLOC ) THEN
                  CALL DAT_ANNUL( CLOC, STATUS )
               END IF
               CLOC = CLOC2
            END IF

*  Record the position of the last delimiter.
            CLAST = CDELIM

*  Continue to work down the hierarchy unless the desired component has
*  been found.
            IF ( .NOT. LAST ) GOTO 20

*  Obtain the name of the keyword.
*  ===============================

*  Search for parentheses.
            KWCPOS = INDEX( LINE( I1( 2 ):I2( 2 ) ), '(' )

*  Test for no parentheses.  Sert the column limtis for the keyword.
            IF ( KWCPOS .EQ. 0 ) THEN
               L( 2 ) = I2( 2 ) - I1( 2 ) + 1
            ELSE
               L( 2 ) = KWCPOS - 1
            END IF

*  Keywords are limited to eight characters.
            IF ( L( 2 ) .GT. KEYLN ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'FITSEXP_KEYLEN',
     :           'The keyword is longer than the eight characters '/
     :           /'permitted within FITS.', STATUS )
               GOTO 30
            END IF

*  Extract the keyword.  Convert to uppercase.
            KEYWRD = LINE( I1( 2 ):L( 2 ) + I1( 2 ) - 1 )
            CALL CHR_UCASE( KEYWRD )

*  Validate the keyword.  Report the error and go to the next record of
*  the translation table.
            CALL FTS1_ISKEY( KEYWRD, VALKEY, STATUS )
            IF ( .NOT. VALKEY ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETC( 'KEY', KEYWRD )
               CALL ERR_REP( 'FITSEXP_INVKEY',
     :           'The keyword ^KEY includes characters other than '/
     :           /'numbers, uppercase letters, hyphen, and underscore '/
     :           /'thereby violating the FITS standard.', STATUS )
               GOTO 30
            END IF

*  Look for the secondary keyword before which the new card is to be
*  placed.
            IF ( KWCPOS .NE. 0 ) THEN

*  Look for the right-hand parenthesis searching forwards from the
*  left-hand parenthesis.
               CDELIM = KWCPOS + 1
               CALL CHR_TOCHR( ')', LINE( I1( 2 ):I2( 2 ) ), .TRUE.,
     :                         CDELIM )

*  It does not matter if the right-hand parenthesis is omitted as the
*  character pointer is one character beyond the second keyword.

*  Keywords are limited to eight characters.
               IF ( CDELIM - KWCPOS - 1 .GT. KEYLN ) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP( 'FITSEXP_KEYPLEN',
     :              'The positional keyword is longer than the eight '/
     :              /'characters permitted within FITS.', STATUS )
                  GOTO 30
               END IF

*  Extract the position keyword, and convert it to uppercase.
               KEYIND = LINE( I1( 2 ) + KWCPOS: I1( 2 ) + CDELIM - 2 )
               CALL CHR_UCASE( KEYIND )

*  Validate the position keyword.  Report the error and go to the next
*  record of the translation table.
               CALL FTS1_ISKEY( KEYIND, VALKEY, STATUS )
               IF ( .NOT. VALKEY ) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETC( 'KEY', KEYIND )
                  CALL ERR_REP( 'FITSEXP_INVKEY',
     :              'The keyword ^KEY includes characters other than '/
     :              /'numbers, uppercase letters, hyphen, and '/
     :              /'underscore thereby violating the FITS standard.',
     :              STATUS )
                  GOTO 30
               END IF

            ELSE
               KEYIND = ' '
            END IF

*  Store the information to write to the NDF's FITS extension so that
*  that several headers may be processed at one time, thus improving
*  the efficiency.
            NWRITE = NWRITE + 1
            KEYWDS( NWRITE ) = KEYWRD
            KEYPOS( NWRITE ) = KEYIND
            COMNTS( NWRITE ) = COMENT
            CLOCS( NWRITE ) = CLOC
         END IF

*  Come here to give additional information about the line in which an
*  error was detected.
   30    CONTINUE

*  If a fatal error has occurred, report which line of the translation
*  table it occurred in and display the line contents.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL MSG_SETI( 'ILINE', ILINE )
            CALL ERR_REP( 'FITSEXP_ILINE',
     :        'Error occurred in line ^ILINE of the '/
     :           /'keyword translation table $TABLE.', STATUS )
            CALL MSG_SETC( 'LINE', LINE )
            CALL ERR_REP( 'FITSEXP_LINE',
     :        'Line read was: ''^LINE''.', STATUS )

*  Flush the error to let processing continue.
            CALL ERR_FLUSH( STATUS )
            CALL MSG_BLANK( STATUS )
         END IF

*  Return to attempt to read the next line of the translation table,
*  unless the store is full and therefore the FITS extension must be
*  updated.
         IF ( NWRITE .LT. MXWRIT ) GOTO 10

*  An end-of-file condition is expected.  So annul it in the current
*  error context.
      ELSE IF ( STATUS .EQ. FIO__EOF ) THEN
         CALL ERR_ANNUL( STATUS )
      END IF

*  Write FITS-extension records for all the stored objects.
*  ========================================================

*  See if there are any headers to write.
      IF ( STATUS .EQ. SAI__OK .AND. NWRITE .GT. 0 ) THEN

*  Created the cards in the FITS extension, though their values are not
*  assigned.
         CALL FTS1_PTKEY( LOC, NWRITE, KEYWDS, KEYPOS, STATUS )

*  Map the FITS extension.
         CALL DAT_MAPV( LOC, '_CHAR*80', 'UPDATE', PNTR( 1 ), EL,
     :                  STATUS )
         LENGTH = FITSLN

*  Loop through all the stored objects.
         DO ICOMP = 1, NWRITE

*  Obtain its data type.
            CALL DAT_TYPE( CLOCS( ICOMP ), TYPE, STATUS )

*  Handle each data type separately....

*  Character:
*  =========

*  Obtain a value for the keyword from the component in the NDF
*  extension.  A vector get enables both scalar and one-element-array
*  values to be stored in the FITS header.  The stored locator points
*  to this object.  Its value is then written to the FITS extension
*  within the card of the desired keyword, this card having been
*  written with a token value by FTS1_PTKEY.  The ".FALSE." instructs
*  the routine that this is not a comment card.  Note the length of the
*  character-array elements is passed by value after the last genuine
*  argument.  This is for UNIX and does not harm on VMS.  The second
*  character argument is no problem since it is not passed by pointer.
*  Ditto for extract data of other types from the FITS extension.
            IF ( TYPE( 1:5 ) .EQ. '_CHAR' ) THEN
               CALL DAT_GETVC( CLOCS( ICOMP ), 1, CVAL, NEL, STATUS )
               CALL FTS1_UKEYC( EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ), 1,
     :                          KEYWDS( ICOMP ), CVAL, '/',
     :                          COMNTS( ICOMP ), .FALSE., FOUND, CARD,
     :                          STATUS, %VAL( CNF_CVAL( LENGTH ) ) )

*  Double precision:
*  ================
*  Obtain a value for the keyword from the NDF-extension component and
*  update the card.
            ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
               CALL DAT_GETVD( CLOCS( ICOMP ), 1, DVAL, NEL, STATUS )
               CALL FTS1_UKEYD( EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ), 1,
     :                          KEYWDS( ICOMP ), DVAL, '/',
     :                          COMNTS( ICOMP ), FOUND, CARD, STATUS,
     :                          %VAL( CNF_CVAL( LENGTH ) ) )

*  Integer, signed and unsigned word and byte:
*  ===========================================
*  Obtain a value for the keyword from the NDF-extension component and
*  update the card.
            ELSE IF ( TYPE .EQ. '_INTEGER' .OR. TYPE .EQ. '_BYTE' .OR.
     :                TYPE .EQ. '_UBYTE' .OR. TYPE .EQ. '_WORD' .OR.
     :                TYPE .EQ. '_UWORD' ) THEN
               CALL DAT_GETVI( CLOCS( ICOMP ), 1, IVAL, NEL, STATUS )
               CALL FTS1_UKEYI( EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ), 1,
     :                          KEYWDS( ICOMP ), IVAL, '/',
     :                          COMNTS( ICOMP ), FOUND, CARD, STATUS,
     :                          %VAL( CNF_CVAL( LENGTH ) ) )

*  64-bit integer:
*  ===============
*  Obtain a value for the keyword from the NDF-extension component and
*  update the card.
            ELSE IF ( TYPE .EQ. '_INT64' ) THEN
               CALL DAT_GETVK( CLOCS( ICOMP ), 1, KVAL, NEL, STATUS )
               CALL FTS1_UKEYK( EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ), 1,
     :                          KEYWDS( ICOMP ), KVAL, '/',
     :                          COMNTS( ICOMP ), FOUND, CARD, STATUS,
     :                          %VAL( CNF_CVAL( LENGTH ) ) )

*  Logical:
*  ========
*  Obtain a value for the keyword from the NDF-extension component and
*  update the card.
            ELSE IF ( TYPE .EQ. '_LOGICAL' ) THEN
               CALL DAT_GETVL( CLOCS( ICOMP ), 1, LVAL, NEL, STATUS )
               CALL FTS1_UKEYL( EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ), 1,
     :                          KEYWDS( ICOMP ), LVAL, '/',
     :                          COMNTS( ICOMP ), FOUND, CARD, STATUS,
     :                          %VAL( CNF_CVAL( LENGTH ) ) )

*  Real:
*  =====
*  Obtain a value for the keyword from the NDF-extension component and
*  update the card.
            ELSE IF ( TYPE .EQ. '_REAL' ) THEN
               CALL DAT_GETVR( CLOCS( ICOMP ), 1, RVAL, NEL, STATUS )
               CALL FTS1_UKEYR( EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ), 1,
     :                          KEYWDS( ICOMP ), RVAL, '/',
     :                          COMNTS( ICOMP ), FOUND, CARD, STATUS,
     :                          %VAL( CNF_CVAL( LENGTH ) ) )

            END IF

*  If there has been no other error, then check if the keyword was
*  found.  This should happen, but include it just in case of keyword
*  truncation or something weird has happened or there is a bug in the
*  placement of keywords.  Report an error if it was not, but carry on
*  anyway.
            IF ( STATUS .EQ. SAI__OK ) THEN
               IF ( .NOT. FOUND ) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETC( 'KEYWORD', KEYWDS( ICOMP ) )
                  CALL ERR_REP( 'FITSEXP_ABSENT',
     :              'The FITS keyword ''^KEYWORD'' could not be found '/
     :              /'to store the value.', STATUS )
                  CALL ERR_FLUSH( STATUS )
               END IF
            END IF

*  Annul the locator to the component to free resources.
            CALL DAT_ANNUL( CLOCS( ICOMP ), STATUS )

*  If a fatal error has occurred, report which line of the translation
*  table it occurred in and display the line contents.
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL MSG_SETI( 'ILINE', ILINE )
               CALL ERR_REP( 'FITSEXP_ILINE',
     :           'Error occurred in line ^ILINE of the '/
     :           /'keyword translation table $TABLE.', STATUS )
               CALL MSG_SETC( 'LINE', LINE )
               CALL ERR_REP( 'FITSEXP_LINE',
     :           'Line read was: ''^LINE''.', STATUS )

*  Flush the error to let processing continue.
               CALL ERR_FLUSH( STATUS )
               CALL MSG_BLANK( STATUS )
            END IF
         END DO

*  Annul (thereby unmapping) locator to the FITS extension.
      CALL DAT_ANNUL( LOC, STATUS )

*  Reset the number of stored values back to none, and return to reading
*  from the translation table.
         NWRITE = 0
         GOTO 10
      END IF

*  End the deferral of error reporting and close the translation table
*  file.
      CALL ERR_RLSE
      CALL FIO_CLOSE( IFIL, STATUS )

*  Annul the NDF identifier.
      CALL NDF_ANNUL( INDF, STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'FITSEXP_ERR',
     :     'FITSEXP: Error importing FITS information into an NDF '/
     :     /'extension.', STATUS )
      END IF

      END
