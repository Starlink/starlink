      SUBROUTINE FTS1_RFMOD( FD, MAXMOD, NWRITE, EDITS, KEYWDS, KEYPOS,
     :                       KOCCUR, POCCUR, VALUES, COMNTS, TYPES,
     :                       STATUS )
*+
*  Name:
*     FTS1_RFMOD

*  Purpose:
*     Reads a text file containing instructions for editing an NDF's
*     FITS extension.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FTS1_RFMOD( FD, MAXMOD, NWRITE, EDITS, KEYWDS, KEYPOS,
*                      KOCCUR, POCCUR, VALUES, COMNTS, TYPES, STATUS )

*  Description:
*     This routines opens a text file and parses it to determine how to
*     modify an NDF's FITS extension.  Details of the format and its
*     interpretation is given in the item called "File Format".  The
*     routine returns the editing command, keyword, position, values,
*     comment, and data type in arrays.

*  Arguments:
*     FD = INTEGER (Given)
*        The FIO identifier of the text file containing the editing
*        instructions.
*     MAXMOD = INTEGER (Given)
*        The maximum number of modifications.
*     NWRITE = INTEGER (Returned)
*        The number of modifications actually made.
*     EDITS( MAXMOD ) = CHARACTER * ( * ) (Returned)
*        The editing commands.  Thus need only be one character per
*        element. The first character of each returned value will be
*        upper case, all others will be lower case (as required by
*        FTS1_EDFEX).
*     KEYWDS( MAXMOD ) = CHARACTER * ( * ) (Returned)
*        The FITS keywords to be modified into FITS card array.  The
*        length should be at least 48 characters to allow for
*        hierarchical keywords.
*     KEYPOS( MAXMOD ) = CHARACTER * ( * ) (Returned)
*        The position keyword names.  The new keywords are inserted
*        immediately before each corresponding position keyword.
*        The length should be at least 48 characters to allow for
*        hierarchical keywords.
*     KOCCUR( MAXMOD ) = INTEGER (Returned)
*        The occurrences of the KEYWDS keywords to use.
*     POCCUR( MAXMOD ) = INTEGER (Returned)
*        The occurrences of the KEYPOS keywords to use.
*     VALUES( MAXMOD ) = CHARACTER * ( * ) (Returned)
*        The values to be given to the KEYWDS keywords.  The length
*        should be at least 68 characters to allow for the maximum
*        length of a value.
*     COMNTS( MAXMOD ) = CHARACTER * ( * ) (Returned)
*        The comments of the NAME keywords to use.  The length should
*        be at least 68 characters to allow for the maximum length of a
*        comment, but normally 50 should be adequate.
*     TYPES( MAXMOD ) = CHARACTER * ( * ) (Returned)
*        The data types of the values to write.  These will be one of
*        the following: '_CHAR',  '_DOUBLE', '_INTEGER', '_LOGICAL',
*        '_REAL'.  In addition there are two special values: 'COMMENT'
*        to indicate that the card is a comment (so strictly it has no
*        type), and ' ' to indicate that the data type is unknown, as
*        occurs for a value defined by a reference keyword.  The length
*        should be at least 8 characters.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  File Format:
*     The file consists of a series of lines, one per editing
*     instruction, although blank lines and lines beginning with a ! or
*     # are treated as comments.  Note that the order does matter, as
*     the edits are performed in the order given.
*
*     The format is summarised below:
*
*       command keyword{[occur]}{(keyword{[occur]})} {value {comment}}
*
*     where braces indicate optional values, and occur is the
*     occurrence of the keyword.  In effect there are four fields
*     delineated by spaces that define the edit operation, keyword,
*     value and comment.
*
*     Field 1:  This specifies the editing operation.  Allowed values
*     are Amend, Delete, Exist, Move, Null, Read, Write, and Update, and
*     can be abbreviated to the initial letter.  Delete removes a named
*     keyword.  Read causes the value of a named keyword to be
*     displayed to standard output.  Exist reports TRUE to standard
*     output if the named keyword exists in the header, and FALSE if
*     the keyword is not present.  Move relocates a named keyword to be
*     immediately before a second keyword.  When this positional
*     keyword is not supplied, it defaults to the END card, and if the
*     END card is absent, the new location is at the end of the
*     headers.  Write creates a new card given a value and an optional
*     comment.  Its location uses the same rules as for the Move
*     command.  Update revises the value and/or the comment.  If a
*     secondary keyword is defined explicitly, the card may be
*     relocated at the same time.  Update requires that the keyword
*     exists.  Amend behaves as Write if the keyword in Field 2 is
*     not already present, or as Update if the keyword exists.  Null
*     replaces the value of a named keyword with blanks.

*     Field 2:  This specifies the keyword to edit, and optionally the
*     position of that keyword in the header after the edit (for Move,
*     Write, Update, and Amend edits).  The new position in the header
*     is immediately before a positional keyword, whose name is given in
*     parentheses concatenated to the edit keyword.  See "Field 1" for
*     defaulting when the position parameter is not defined or is null.
*
*     Both the editing keyword and position keyword may be compound to
*     handle hierarchical keywords.  In this case the form is
*     keyword1.keyword2.keyword3 etc.  All keywords must be valid FITS
*     keywords.  This means they must be no more than 8 characters
*     long, and the only permitted characters are uppercase alphabetic,
*     numbers, hyphen, and underscore.  Invalid keywords will be
*     rejected.
*
*     Both the edit and position keyword may have an occurrence
*     specified in brackets [].  This enables editing of a keyword that
*     is not the first occurrence of that keyword, or locate a edited
*     keyword not at the first occurrence of the positional keyword.
*     Note that it is not normal to have multiple occurrences of a
*     keyword in a FITS header, unless it is blank, COMMENT or HISTORY.
*     Any text other than a positive integer is interpreted as the
*     first occurrence.

*     Use a null value ('' or "") if you want the card to be a comment
*     with keyword other than COMMENT or HISTORY.  As blank keywords are
*     used for hierarchical keywords, to write a comment in a blank
*     keyword you must give a null edit keyword.  These have no keyword
*     appears before the left parenthesis or bracket, such as (), [],
*     [2], or (EPOCH).

*     Field 3:
*     This specifies the value to assign to the edited keyword in the
*     the Amend, Write, and Update operations, or the name of the new
*     keyword in the Rename modification.  If the keyword exists, the
*     existing value or keyword is replaced, as appropriate.  The data
*     type used to store the value is inferred from the value itself.
*     See topic "Value Data Types".
*
*     For the Update and Write modifications there is a special value,
*     $V, which means use the current value of the edited keyword,
*     provided that keyword exists.  This makes it possible to modify a
*     comment, leaving the value unaltered.  In addition $V(keyword)
*     requests that the value of the keyword given between the
*     parentheses be assigned to the keyword being edited.
*
*     The value field is ignored when the keyword is COMMENT, HISTORY
*     or blank and the modification is an Update or Write.
*
*     Field 4:
*     This specifies the comment to assign to the edited keyword for the
*     Amend, Write, and Update operations.  A leading '/' should not be
*     supplied.

*     There is a special value, $C, which means use the current comment
*     of the edited keyword, provided that keyword exists.  This makes
*     it possible to modify a value, leaving the comment unaltered.  In
*     addition $C(keyword) requests that the comment of the keyword
*     given between the parentheses be assigned to the edited keyword.
*
*     To obtain leading spaces before some commentary, use a quote (')
*     or double quote (") as the first character of the comment.  There
*     is no need to terminate the comment with a trailing and matching
*     quotation character.  Also do not double quotes should one form
*     part of the comment.

*  Value Data Types:
*     The data type of the value is determined as follows:
*        - Values enclosed in quotes (') or doubled quotes (") are
*        strings.  Note that numeric or logical string values must
*        be quoted to prevent them being converted to a numeric or
*        logical value in the FITS extension.
*        - Otherwise type conversions of the first word after the
*        keywords are made to integer, double precision, and logical
*        types in turn.  If a conversion is successful, that becomes the
*        data type.  In the case of double precision, the type is set
*        to real when the number of significant digits only warrants
*        single precision.  If all the conversions failed the value
*        is deemed to be a string.

*  Examples of the File Format:
*     The best way to illustrate the options is by listing some example
*     lines.
*
*         P AIRMASS
*     This reports the value of keyword AIRMASS to standard output.
*
*         E FILTER
*     This determines whether keyword FILTER exists and reports TRUE or
*     FALSE to standard output.
*
*         D OFFSET
*     This deletes the keyword OFFSET.
*
*         Delete OFFSET[2]
*     This deletes any second occurrence of keyword OFFSET.
*
*         Rename OFFSET1[2] OFFSET2
*     This renames the second occurrence of keyword OFFSET1 to have
*     keyword OFFSET2.
*
*         W AIRMASS 1.379
*     This writes a real value to new keyword AIRMASS, which will be
*     located at the end of the FITS extension.
*
*         A AIRMASS 1.379
*     This writes a real value to keyword AIRMASS if it exists,
*     otherwise it writes a real value to new keyword AIRMASS located
*     at the end of the FITS extension.
*
*         N AIRMASS
*     This blanks the value of the AIRMASS keyword, if it exists.
*
*         W FILTER(AIRMASS) Y
*     This writes a logical true value to new keyword FILTER, which
*     will be located just before the AIRMASS keyword, if it exists.
*
*         Write FILTER(AIRMASS) 'Y'
*     As the preceding example except that this writes a character
*     value "Y".
*
*         W COMMENT(AIRMASS) . Following values apply to mid-observation
*     This writes a COMMENT card immediately before the AIRMASS card,
*     the comment being "Following values apply to mid-observation".
*
*         W DROCOM(AIRMASS) '' Following values apply to mid-observation
*     As the preceding example but this writes to a non-standard
*     comment keyword called DROCOM.  Note the need to supply a null
*     value.
*
*         W (AIRMASS) '' Following values apply to mid-observation
*     As the preceding example but this writes to a blank-keyword
*     comment.
*
*         U OBSERVER "Dr. Peter O'Leary" Name of principal observer
*     This updates the OBSERVER keyword with the string value
*     "Dr. Peter O'Leary", and comment "Name of principal observer".
*     Note that had the value been enclosed in single quotes ('), the
*     apostrophe would need to be doubled.
*
*         M OFFSET
*     This moves the keyword OFFSET to just before the END card.
*
*         Move OFFSET(SCALE)
*     This moves the keyword OFFSET to just before the SCALE card.
*
*         Move OFFSET[2](COMMENT[3])
*     This moves the second occurrence of keyword OFFSET to just
*     before the third COMMENT card.
*
*  Copyright:
*     Copyright (C) 1996 Central Laboratory of the Research Councils.
*     Copyright (C) 2008, 2009 Science and Technology Facilties Council.
*     All Rights Reserved.

*  Licence:
*     This programme is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This programme is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE.  See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this programme; if not, write to the Free Software
*     Foundation, Inc., 51, Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David S. Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     1996 November 4 (MJC):
*        Original version.
*     2008 June 14 (MJC):
*        Add Amend command.
*     2009 January 11 (MJC):
*        Add Null command.
*     2009 January 19 (MJC):
*        Description modified for Null option retaining the Value
*        Indicator.
*     4-OCT-2018 (DSB):
*        Ensure returned EDIT strings have form expected by FTS1_EDFEX
*        - first character upper case, all others lower case, with no
*        leading spaces.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'FIO_ERR'          ! FIO__ error constants

*  Arguments Given:
      INTEGER FD
      INTEGER MAXMOD

*  Arguments Returned:
      INTEGER NWRITE
      CHARACTER * ( * ) EDITS( MAXMOD )
      CHARACTER * ( * ) KEYWDS( MAXMOD )
      CHARACTER * ( * ) KEYPOS( MAXMOD )
      INTEGER KOCCUR( MAXMOD )
      INTEGER POCCUR( MAXMOD )
      CHARACTER * ( * ) VALUES( MAXMOD )
      CHARACTER * ( * ) COMNTS( MAXMOD )
      CHARACTER * ( * ) TYPES( MAXMOD )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL CHR_INSET          ! A string is a member of a given set?
      INTEGER CHR_LEN            ! Used length of a string
      CHARACTER*1 CHR_UPPER      ! Returned upper case character

*  Local Constants:
      INTEGER HKEYLN             ! Maximum number of characters in a
                                 ! FITS header card hierarchical keyword
      PARAMETER ( HKEYLN = 48 )

      INTEGER KEYLN              ! Maximum number of characters in a
                                 ! FITS keyword
      PARAMETER ( KEYLN = 8 )

*  Local Variables:
      INTEGER CDELIM             ! Character pointer to a delimiter
      INTEGER CLPPOS             ! Column of comment reference keyword
                                 ! left parenthesis
      LOGICAL COMCRD             ! Keyword is a standard comment?
      CHARACTER * ( 70 ) COMENT  ! Comment string
      INTEGER CSTAT              ! Local CHR status
      LOGICAL DOUBLE             ! Value starts with a double-quote?
      CHARACTER * ( 2 ) DQ       ! Doubled-quotes search string
      INTEGER DQPOS              ! Position to start search for
                                 ! doubled-quotes
      LOGICAL DQPRES             ! Doubled quote present?
      DOUBLE PRECISION DVAL      ! FITS-card value
      CHARACTER * ( 1 ) EDIT     ! Edit command
      INTEGER ENDCOL             ! Column of end of string section
      INTEGER I1( 4 )            ! Pointer to start of tokens
      INTEGER I2( 4 )            ! Pointer to end of tokens
      INTEGER ILINE              ! Input line counter
      INTEGER IVAL               ! FITS-card value
      CHARACTER * ( HKEYLN ) KEYIND ! FITS positional keyword (can be
                                 ! hierarchical)
      CHARACTER * ( 68 ) KEYS    ! Comment string
      CHARACTER * ( HKEYLN ) KEYREC ! FITS comment reference keyword
                                 ! (can be hierarchical)
      CHARACTER * ( HKEYLN ) KEYREV ! FITS value reference keyword (can
                                 ! be hierarchical)
      CHARACTER * ( HKEYLN ) KEYWRD  ! FITS keyword to modify (can be
                                 ! hierarchical)
      INTEGER KLPPOS             ! Column of keyword left parenthesis
      INTEGER KOC                ! Keyword occurrence number
      CHARACTER * ( 132 ) LINE   ! Buffer for file reading
      INTEGER LKEY               ! Length of the keyword
      INTEGER LPOSK              ! Length of the positional keyword
      INTEGER LREFCK             ! Length of the comment reference
                                 ! keyword
      INTEGER LREFVK             ! Length of the value reference keyword
      LOGICAL LVAL               ! FITS-card value
      INTEGER NC                 ! Number of characters in a line
      INTEGER NCDQ               ! Position of doubled quote (in
                                 ! substring)
      INTEGER NDIGIT             ! Number of significant digits in value
      LOGICAL NEEDVC             ! Edit needs a value and possibly a
                                 ! comment?
      INTEGER NTOK               ! Number of tokens on line
      INTEGER POC                ! Position-keyword occurrence number
      INTEGER RECOC              ! Reference-comment-keyword occurrence
                                 ! number
      LOGICAL REFCOM             ! Value contains comment reference
                                 ! keyword?
      LOGICAL REFVAL             ! Value contains value reference
                                 ! keyword?
      INTEGER REVOC              ! Reference-value-keyword occurrence
                                 ! number
      INTEGER RQPOS              ! Position of value's right quote
      LOGICAL SINGLE             ! Value starts with a quote?
      CHARACTER * ( 1 ) SQ       ! Single-quotes search string
      INTEGER STACOL             ! Column of start of string section
      CHARACTER * ( 1 ) TOK( 4 ) ! Buffer for parsing lines
      INTEGER TQCOL              ! Column of end of string
      CHARACTER * ( 8 ) TYPE     ! Value data type
      CHARACTER * ( 70 ) VAL     ! Value string sans doubled quotes
      CHARACTER * ( 70 ) VALUE   ! Value string
      INTEGER VLPPOS             ! Column of value reference keyword
                                 ! left parenthesis

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

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
      CALL FIO_READ( FD, LINE, NC, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
         ILINE = ILINE + 1

*  Identify comment lines.
*  =======================

*  If the line is not blank, then look for an initial comment delimiter.
*  If found, adjust the line length so that the next line of the file is
*  read.
         IF ( NC. NE. 0 ) THEN
            CALL CHR_LDBLK( LINE( :NC ) )
            IF ( LINE( 1:1 ) .EQ. '!' .OR. LINE( 1:1 ) .EQ. '#' ) NC = 0
         END IF

*  Ignore lines which are now blank.  Read another line from the file.
         IF ( NC .EQ. 0 ) GOTO 10

*  Decompose the line into words.
*  ==============================

*  Remove non-printable characters (e.g. tabs) and leading blanks from
*  the rest.  Decompose the line into tokens.
         CALL CHR_CLEAN( LINE( :NC ) )
         CALL CHR_LDBLK( LINE( :NC ) )
         CALL CHR_DCWRD( LINE( :NC ), 4, NTOK, I1, I2, TOK, CSTAT )

*  Go to the next line if there are no words.  A bad status on its own
*  means that there were more tokens on the line than four, which is
*  likely given multi-word values and/or comments, so ignore it.
         IF ( NTOK .LT. 1 .AND. CSTAT .EQ. 0 ) GO TO 10

*  Extract and validate the mandatory fields.
*  ==========================================

*  Extract the edit command, and ensure it is lower case except for the
*  first character, which is upper case. Also remove leading blanks.
         EDIT = LINE( I1( 1 ):I1( 1 ) )
         CALL CHR_LDBLK( EDIT )
         CALL CHR_LCASE( EDIT )
         EDIT( 1:1 ) = CHR_UPPER( EDIT( 1:1 ) )

*  Validate the edit command.  Just use the first character, as
*  subsequent characters are superfluous (but might help the human
*  reader).
         IF ( .NOT. CHR_INSET( 'A,D,E,M,N,P,R,U,W', EDIT ) ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'EDIT', LINE( I1( 1 ):I2( 1 ) ) )
            CALL ERR_REP( 'FTS1_RFMOD_BADEDIT',
     :        'The edit command ^EDIT is not one of Amend, Delete, '/
     :        /'Exist, Move, NUll, Print, Rename, Update, or Write.',
     :        STATUS )
            GOTO 50
         END IF

*  Extract the keyword.
         IF ( NTOK .GE. 2 ) THEN
            KEYS = LINE( I1( 2 ):I2( 2 ) )
            LKEY = I2( 2 ) - I1( 2 ) + 1
         ELSE
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'EDIT', LINE( I1( 1 ):I2( 1 ) ) )
            CALL ERR_REP( 'FTS1_RFMOD_NOKEYWORD',
     :        'There is no keyword to which to apply the edit.',
     :        STATUS )
            GOTO 50
         END IF

*  Values and/or comments are only relevant to the U(pdate), W(rite),
*  and A(mend) options.  A new keyword is required for R(ename).
         NEEDVC = EDIT .EQ. 'W' .OR. EDIT .EQ. 'U' .OR.
     :            EDIT .EQ. 'A' .OR. EDIT .EQ. 'R'

*  Initialise null default values (for other edit commands and null
*  comment).
         COMENT = ' '
         VALUE = ' '

*  Ignore the bad status if more than four tokens were found; the
*  trailing text is the comment for the FITS card.  Extract the keyword,
*  value and comment fields, and store the string lengths.  Note that
*  comment extends to the end of the line.  Set the null comment and
*  value when the list contains only two items or one item respectively.
*  These may be revised later if the value is a multi-word string.
         IF ( NEEDVC ) THEN

            IF ( NTOK .GE. 3 ) VALUE = LINE( I1( 3 ):I2( 3 ) )

            IF ( CSTAT .NE. 0 .OR. NTOK .GE. 4 )
     :        COMENT = LINE( I1( 4 ):NC )

         END IF

*  Obtain the edit keyword and occurrence.
*  =======================================

*  Search for left parenthesis.
         KLPPOS = INDEX( KEYS, '(' )

*  Test for a parenthesis.  Reset the column limits for the keyword.
         IF ( KLPPOS .NE. 0 ) LKEY = KLPPOS - 1

*  Watch for the special case when the keyword is not specified, i.e.
*  it is a blank keyword.  The subroutine below will take care of a
*  blank followed by brackets.
         IF ( LKEY .EQ. 0 ) THEN
            KEYWRD = ' '
            KOC = 1
            LKEY = KEYLN

*  Just the keyword was supplied.
         ELSE IF ( KLPPOS .EQ. 0 ) THEN

*  Extract the uppercase keyword and any occurrence, and find the
*  length of the keyword.  Also validate the keyword.
            CALL FTS1_EVKEY( KEYS( :LKEY ), KEYWRD, LKEY, KOC, STATUS )

         ELSE

*  Extract the uppercase keyword and any occurrence, and find the
*  length of the keyword.  Also validate the keyword.
            CALL FTS1_EVKEY( KEYS( :KLPPOS - 1 ), KEYWRD, LKEY, KOC,
     :                       STATUS )
         END IF

*  Give more context of the error and then go to the next line of the
*  file.
         IF ( STATUS .NE. SAI__OK ) GOTO 50

*  Record if the card is a comment.
         COMCRD = KEYWRD .EQ. 'COMMENT' .OR. KEYWRD .EQ. 'HISTORY' .OR.
     :            KEYWRD .EQ. ' '

*  Obtain the positional keyword and occurrence.
*  =============================================

*  Look for the secondary keyword before which the new card is to be
*  placed.  Assume initally that there is no positional keyword, except
*  for writing or moving where a positional keyword must be specified,
*  and so default it to the END card.  For Amend this is a provisional
*  position, if in fact the keyword already exists.  The default
*  occurrence is the first one.
         IF ( EDIT .EQ. 'W' .OR. EDIT .EQ. 'A' .OR. EDIT .EQ. 'M' ) THEN
            KEYIND = 'END'
         ELSE
            KEYIND = ' '
         END IF
         POC = 1
         IF ( KLPPOS .NE. 0 ) THEN

*  Look for the right-hand parenthesis searching forwards from the
*  left-hand parenthesis.
            CDELIM = KLPPOS + 1
            CALL CHR_TOCHR( ')', KEYS, .TRUE., CDELIM )

*  Derive the provisional length of the positional keyword.  It does
*  not matter if the right-hand parenthesis is omitted as the character
*  pointer is one character beyond the second keyword.
            LPOSK = CDELIM - KLPPOS - 1

*  Note the special case when the length is 0, meaning the END card.
            IF ( LPOSK .EQ. 0 ) THEN
               KEYIND = 'END'
            ELSE

*  Extract the uppercase positional keyword and any occurrence, and
*  find the length of the keyword.  Also validate the keyword.
               CALL FTS1_EVKEY( KEYS( KLPPOS + 1:CDELIM - 1 ), KEYIND,
     :                          LPOSK, POC, STATUS )

*  Give more context of the error and then go to the next line of the
*  file.
               IF ( STATUS .NE. SAI__OK ) GOTO 50
            END IF

         END IF

*  Interpret special characters in the value.
*  ==========================================

         IF ( NEEDVC .AND. .NOT. COMCRD ) THEN

*  At present the only special values are $V meaning use the current
*  value of the current keyword, and $V(keyword{[occurrence]}) meaning
*  use the value of the occurrence of a named keyword.
            REFVAL = VALUE( 1:2 ) .EQ. '$V'

*  Initialise as the current editing keyword.
            KEYREV = KEYWRD

*  Search for left parenthesis.
            IF ( REFVAL ) THEN
               VLPPOS = INDEX( VALUE, '(' )
               IF ( VLPPOS .NE. 0 ) THEN

*  Look for the right-hand parenthesis searching forwards from the
*  left-hand parenthesis.
                  CDELIM = VLPPOS + 1
                  CALL CHR_TOCHR( ')', VALUE, .TRUE., CDELIM )

*  Derive the provisional length of the value reference keyword.  It
*  does not matter if the right-hand parenthesis is omitted as the
*  character pointer is one character beyond the second keyword.
                  LREFVK = CDELIM - VLPPOS - 1

*  Exclude the special case when the length is 0 (already defaults to
*  current editing keyword).
                  IF ( LREFVK .GT. 0 ) THEN

*  Extract the uppercase positional keyword and any occurrence, and
*  find the length of the keyword.  Also validate the keyword.
                     CALL FTS1_EVKEY( VALUE( VLPPOS + 1:CDELIM - 1 ),
     :                                KEYREV, LREFVK, REVOC, STATUS )

*  Give more context of the error and then go to the next line of the
*  file.
                     IF ( STATUS .NE. SAI__OK ) GOTO 50
                  END IF
               END IF
            END IF

*  Deal with quoted multi-word string values.
*  ==========================================

*  Above we assumed that the value comprised a single word, and
*  subsequent words are comments for the FITS card.  Multi-word
*  values must be enclosed in quotes.
            SINGLE = VALUE( 1:1 ) .EQ. ''''
            DOUBLE = VALUE( 1:1 ) .EQ. '"'
            DQPRES = .FALSE.

            IF ( SINGLE .OR. DOUBLE ) THEN

*  Assign the strings to search for, depending on the type of the outer
*  delimiter.
               IF ( SINGLE ) THEN
                  DQ = ''''''
                  SQ = ''''
               ELSE
                  DQ = '""'
                  SQ = '"'
               END IF

*  Look out for doubled quotes, as they would otherwise indicate the
*  end of the string.  Search from the start of the value, and then for
*  each doubled quote.
               NCDQ = 1
               DQPOS = I1( 3 ) + 1
   40          CONTINUE        ! Start of 'DO WHILE' loop
               IF ( NCDQ .NE. 0 ) THEN
                  NCDQ = INDEX( LINE( DQPOS: ), DQ )
                  DQPRES = DQPRES .OR. NCDQ .GT. 0
                  IF ( NCDQ .GT. 0 ) DQPOS = DQPOS + NCDQ + 1
                 GO TO 40
               END IF

*  Need to locate the corresponding quote character ('), starting after
*  any doubled quote.  The same quote appearing as part of the value
*  must then be doubled, so it is better to use the alternative double
*  quote (").
               RQPOS = INDEX( LINE( DQPOS: ), SQ )
               IF ( RQPOS .EQ. 0 ) THEN

*  There is no second quote.  Assume that the rest of the line is the
*  value.  The comment string is therefore blank.
                  VALUE = LINE( I1( 3 ) + 1: )
                  COMENT = ' '

*  The string is null.  Set the type to be a comment, i.e. no value.
               ELSE IF ( RQPOS .EQ. 1 ) THEN
                  VALUE = ' '
                  COMCRD = .TRUE.

*  Set the position of end of the string to avoid the last quote (hence
*  minus 2 rather than the normal minus 1).  Note the offset is
*  immediately following the last double quote.  Extract the revised
*  comment.  This may be null if the
               ELSE
                  VALUE = LINE( I1( 3 ) + 1:DQPOS + RQPOS - 2 )
                  IF ( DQPOS + RQPOS .LT. NC ) THEN
                     COMENT = LINE( DQPOS + RQPOS + 1: )
                  ELSE
                     COMENT = ' '
                  END IF

               END IF

*  Replace any doubled quotes with single quotes.  This is achieved by
*  repeatedly searching forwards for each doubled quote in the FITS
*  card, extracting from the start to the first of the quotes,
*  appending this to the value string.  When the last of the doubled
*  quotes has been located, just append any remainder of the string
*  from the FITS card.  The start position is immediately following the
*  double quote, except initially when it's the first string character.
               IF ( DQPRES ) THEN
                  STACOL = 1
                  ENDCOL = 1
                  NC = 0
                  TQCOL = CHR_LEN( VALUE )

   30             CONTINUE      ! Start of 'DO WHILE' loop
                  IF ( ENDCOL .LE. TQCOL ) THEN
                     CALL CHR_FIND( VALUE, DQ, .TRUE., ENDCOL )

                     IF ( ENDCOL .LE. TQCOL ) THEN
                        CALL CHR_APPND( VALUE( STACOL:ENDCOL ), VAL,
     :                                  NC )

                        ENDCOL = ENDCOL + 2
                        STACOL = ENDCOL
                     END IF
                     GO TO 30
                  END IF

                  IF ( STACOL .LE. TQCOL ) THEN
                     CALL CHR_APPND( VALUE( STACOL:TQCOL ), VAL, NC )
                  END IF

                  VALUE = VAL
               END IF
            END IF
         END IF

*  Validate a value that is the keyword for a rename operation.
*  ============================================================
         IF ( EDIT .EQ. 'R' ) THEN

*  Validate the keyword.  Reuse some variables as workspace.  Extract
*  the uppercase new keyword.  The occurrence is ignored as it would
*  imply a move operation too.
            KEYREV = VALUE
            CALL FTS1_EVKEY( KEYREV, VALUE, LKEY, REVOC, STATUS )

*  Give more context of the error and then go to the next line of the
*  file.
            IF ( STATUS .NE. SAI__OK ) GOTO 50
         END IF

*  Interpret special characters in the comment.
*  ============================================
         IF ( NEEDVC ) THEN

*  At present the only special values are $C meaning use the current
*  comment of the current keyword, and $C(keyword{[occurrence]}) meaning
*  use the comment of the occurrence of a named keyword.
            REFCOM = COMENT( 1:2 ) .EQ. '$C'
            IF ( REFCOM ) THEN

*  Initialise as the current editing keyword.
               KEYREC = KEYWRD

*  Search for left parenthesis.
               CLPPOS = INDEX( COMENT, '(' )
               IF ( CLPPOS .NE. 0 ) THEN

*  Look for the right-hand parenthesis searching forwards from the
*  left-hand parenthesis.
                  CDELIM = CLPPOS + 1
                  CALL CHR_TOCHR( ')', COMENT, .TRUE., CDELIM )

*  Derive the provisional length of the comment reference keyword.  It
*  does not matter if the right-hand parenthesis is omitted as the
*  character pointer is one character beyond the second keyword.
                  LREFCK = CDELIM - CLPPOS - 1

*  Exclude the special case when the length is 0 (already defaults to
*  current editing keyword).
                  IF ( LREFCK .GT. 0 ) THEN

*  Extract the uppercase positional keyword and any occurrence, and
*  find the length of the keyword.  Also validate the keyword.
                     CALL FTS1_EVKEY( COMENT( CLPPOS + 1:CDELIM - 1 ),
     :                                KEYREC, LREFCK, RECOC, STATUS )

*  Give more context of the error and then go to the next line of the
*  file.
                     IF ( STATUS .NE. SAI__OK ) GOTO 50
                  END IF
               END IF

*  The other special case is if the string begins with a quote or double
*  quote to allow for leading spaces.
            ELSE IF ( COMENT( 1:1 ) .EQ. '''' .OR.
     :                COMENT( 1:1 ) .EQ. '"' ) THEN

*  Specify the search delimiter.
               IF ( COMENT( 1:1 ) .EQ. '''' ) THEN
                  SQ = ''''
               ELSE
                  SQ = '"'
               END IF

*  Find the trailing quote searching from the end of the comment.
               RQPOS = CHR_LEN( COMENT )
               IF ( COMENT( RQPOS:RQPOS ) .EQ. SQ ) THEN
                  COMENT = COMENT( 2:RQPOS-1 )
               ELSE
                  COMENT = COMENT( 2: )
               END IF

            END IF

*  Determine the type of the value.
*  ================================

*  This is a comment so there is no type.  By convention we call it
*  'COMMENT'.
            IF ( COMCRD ) THEN
               TYPE = 'COMMENT'

*  The data type of value must be character if it is quoted.
            ELSE IF ( SINGLE .OR. DOUBLE ) THEN
               TYPE = '_CHAR'

*  The data type is unknown the $V special value is used.
            ELSE IF ( REFVAL ) THEN
               TYPE = ' '

            ELSE

*  Proceed to test for each data type in turn.  Attempt a conversion
*  and look for an error.

*  Check for an integer.
               CSTAT = 0
               CALL CHR_CTOI( VALUE, IVAL, CSTAT )
               IF ( CSTAT .EQ. 0 ) THEN
                  TYPE = '_INTEGER'

*  Check for a floating point.
               ELSE

                  CSTAT = 0
                  CALL CHR_CTOD( VALUE, DVAL, CSTAT )
                  IF ( CSTAT .EQ. 0 ) THEN

*  Determine how many significant digits it has.
                     CALL KPG1_SGDIG( VALUE, NDIGIT, CSTAT )
                     IF ( NDIGIT .GT. -INT( LOG10( VAL__EPSR ) ) ) THEN
                        TYPE = '_DOUBLE'
                     ELSE
                        TYPE = '_REAL'
                     END IF

*  Check for a logical.  Note a literal string Y, N, YES, NO, T, F etc.
*  should be in quotes.
                  ELSE
                     CSTAT = 0
                     CALL CHR_CTOL( VALUE, LVAL, CSTAT )
                     IF ( CSTAT .EQ. 0 ) THEN
                        TYPE = '_LOGICAL'

                     ELSE
                        TYPE = '_CHAR'
                     END IF
                  END IF
               END IF
            END IF
         END IF

*  Store the validated and interpreted data into arrays.
*  =====================================================
*
*  Store the information to write to the NDF's FITS extension so that
*  that several headers may be processed at one time, thus improving
*  the efficiency.
         NWRITE = NWRITE + 1
         EDITS( NWRITE ) = EDIT
         KEYWDS( NWRITE ) = KEYWRD
         KEYPOS( NWRITE ) = KEYIND
         KOCCUR( NWRITE ) = KOC
         POCCUR( NWRITE ) = POC
         VALUES( NWRITE ) = VALUE
         COMNTS( NWRITE ) = COMENT
         TYPES( NWRITE ) = TYPE

*  Come here to give additional information about the line in which an
*  error was detected.
   50    CONTINUE

*  If a fatal error has occurred, report which line of the translation
*  table it occurred in and display the line contents.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL MSG_SETI( 'ILINE', ILINE )
            CALL ERR_REP( 'FTS1_RFMOD_ILINE',
     :        'Error occurred in line ^ILINE of the keyword '/
     :        /'translation table $TABLE.', STATUS )
            CALL MSG_SETC( 'LINE', LINE )
            CALL ERR_REP( 'FTS1_RFMOD_LINE',
     :        'Line read was: ''^LINE''.', STATUS )

*  Flush the error to let processing continue.
            CALL ERR_FLUSH( STATUS )
            CALL MSG_BLANK( STATUS )
         END IF

*  Return to attempt to read the next line of the translation table,
*  unless the store is full and therefore the FITS extension must be
*  updated.
         IF ( NWRITE .LT. MAXMOD ) GOTO 10

*  An end-of-file condition is expected.  So annul it in the current
*  error context.
      ELSE IF ( STATUS .EQ. FIO__EOF ) THEN
         CALL ERR_ANNUL( STATUS )
      END IF

      END
