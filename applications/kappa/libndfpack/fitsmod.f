      SUBROUTINE FITSMOD( STATUS )
*+
*  Name:
*     FITSMOD

*  Purpose:
*     Edits an NDF FITS extension via a text file or parameters.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL FITSMOD( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application edits the FITS extension of an NDF file in a
*     variety of ways.  It permits insersion of new keywords, including
*     comment lines; revision of existing keyword, values, and inline
*     comments; relocation of keywords; deletion of keywords; printing
*     of keyword values; and it can test whether or not a keyword
*     exists.  The occurrence of keywords may be defined, when there
*     are more than one cards of the same name.  The location of each
*     insertion or move is immediately before some occurrence of a
*     corresponding keyword.
*
*     Control of the editing can be through parameters, or from a text
*     file whose format is described in topic "File Format".

*  Usage:
*     fitsmod ndf { keyword edit value comment position
*                 { table=?
*                mode

*  ADAM Parameters:
*     COMMENT = LITERAL (Read)
*        The comments to be written to the KEYWORD keyword for the
*        "Update", "Write", and "Amend" editing commands.  A null value
*        (!) gives a blank comment.  The special value "$C" means use
*        the current comment.  In addition "$C(keyword)" requests that
*        the comment of the keyword given between the parentheses be
*        assigned to the keyword being edited.  If this positional
*        keyword does not exist, the comment is unchanged for "Update"
*        and is blank for a "Write" edit.  The same applies to the
*        "Amend" edit, the choice depending on whether or not the
*        KEYWORD keyword exists.
*     EDIT = LITERAL (Read)
*        The editing command to apply to the keyword.  The allowed
*        options are listed below.
*
*        "Amend' acts as option "Update" if the keyword exists, but
*        as the "Write" option should the keyword be absent.

*        "Delete" removes a named keyword.
*
*        "Exist" reports TRUE to standard output if the named keyword
*        exists in the header, and FALSE if the keyword is not present.
*        The same value is also stored in output parameter EXISTS.
*
*        "Move" relocates a named keyword to be immediately before a
*        second keyword (see parameter POSITION).  When this positional
*        keyword is not supplied, it defaults to the END card, and if
*        the END card is absent, the new location is at the end of the
*        headers.
*
*        'Null' nullifies the value of the named keyword.  Spaces
*        substitute the keyword's value.
*
*        "Print" causes the value of a named keyword to be displayed to
*        standard output.  This will be a blank for a comment card.
*
*        "Rename" renames a keyword, using parameter NEWKEY to obtain
*        the new keyword.
*
*        "Update" revises the value and/or the comment.  If a secondary
*        keyword is defined explicitly (parameter POSITION), the card
*        may be relocated at the same time.  If the secondary keyword
*        does not exist, the card being edited is not moved.  "Update"
*        requires that the keyword being edited exists.
*
*        "Write" creates a new card given a value and an optional
*        comment.  Its location uses the same rules as for the "Move"
*        command.  The FITS extension is created first should it not
*        exist.
*     EXISTS = _LOGICAL (Write)
*        An output parameter which is set to the result of the final
*        "Exist" operation (see parameter EDIT).
*     KEYWORD = LITERAL (Read)
*        The name of the keyword to be edited in the FITS extension.  A
*        name may be compound to handle hierarchical keywords, and it
*        has the form keyword1.keyword2.keyword3 etc.  The maximum
*        number of keywords per FITS card is twenty.  Each keyword must
*        be no longer than eight characters, and be a valid FITS keyword
*        comprising only alphanumeric characters, hyphen, and
*        underscore.  Any lowercase letters are converted to uppercase
*        and blanks are removed before insertion, or comparison with the
*        existing keywords.
*
*        The keywords " ", "COMMENT", and "HISTORY" are comment cards
*        and do not have a value.
*
*        The keyword must exist except for the "Amend", "Write", and
*        "Exist" commands.
*
*        Both KEYWORD and POSITION keywords may have an occurrence
*        specified in brackets [] following the name.  This enables
*        editing of a keyword that is not the first occurrence of that
*        keyword, or locate a edited keyword not at the first occurrence
*        of the positional keyword.  Note that it is not normal to have
*        multiple occurrences of a keyword in a FITS header, unless it
*        is blank, COMMENT or HISTORY.  Any text between the brackets
*        other than a positive integer is interpreted as the first
*        occurrence.
*     MODE = LITERAL (Read)
*        The mode by which the editing instructions are supplied.  The
*        alternatives are "File", which uses a text file; and
*        "Interface" which uses parameters. ["Interface"]
*     NDF = NDF (Read and Write)
*        The NDF in which the FITS extension is to be edited.
*     NEWKEY = LITERAL (Read)
*        The name of the keyword to replace the KEYWORD keyword.  It is
*        only accessed when EDIT="Rename".  A name may be compound to
*        handle hierarchical keywords, and it has the form
*        keyword1.keyword2.keyword3 etc.  The maximum number of
*        keywords per FITS card is twenty.  Each keyword must be no
*        longer than eight characters, and be a valid FITS keyword
*        comprising only alphanumeric characters, hyphen, and
*        underscore.
*     POSITION = LITERAL (Read)
*        The position keyword name.  A position name may be compound to
*        handle hierarchical keywords, and it has the form
*        keyword1.keyword2.keyword3 etc.  The maximum number of
*        keywords per FITS card is twenty.  Each keyword must be no
*        longer than eight characters.  When locating the position card,
*        comparisons are made in uppercase and with the blanks removed.
*        An occurrence may be specified (see parameter KEYWORD for
*        details).
*
*        The new keywords are inserted immediately before each
*        corresponding position keyword.  If any name in it does not
*        exist in FITS array, or the null value (!) is supplied the
*        consequences will be as follows.  For a "Write", "Amend" (new
*        keyword), or "Move" edit, the KEYWORD keyword will be inserted
*        just before the END card or appended to FITS array when the END
*        card does not exist; for an "Update" or "Amend" (with an
*        existing keyword) edit, the edit keyword is not relocated.
*
*        A positional keyword is only accessed by the "Move", "Write",
*        "Amend", and "Update" editing commands.
*     READONLY = _LOGICAL (Read)
*        Determines if read or write access is requested for the NDF.
*        If a TRUE value is supplied for READONLY, the NDF is opened
*        for reading only. An error will then be reported if any of the
*        requested editing operations would change the contents of the
*        NDF. If a FALSE value is supplied for READONLY, the NDF is
*        opened for both reading and writing, but an error will be
*        reported if the NDF file is write-protected on disk. If the
*        MODE parameter is set to "File", the dynamic default value for
*        READONLY is FALSE. If MODE is set to "Interface", the dynamic
*        default value for READONLY depends on the value of the EDIT
*        parameter: TRUE for "Print" and "Exist", and FALSE for all other
*        editing commands. []
*     STRING = _LOGICAL (Read)
*        When STRING is FALSE, inferred data typing is used for the
*        "Write", "Update", and "Amend" editing commands.  So for
*        instance, if parameter VALUE = "Y", it would appears as
*        logical TRUE rather than the string 'Y       ' in the FITS
*        header.  See topic "Value Data Type".  When STRING is TRUE, the
*        value will be treated as a string for the purpose of writing
*        the FITS header.  [FALSE]
*     TABLE = FILENAME (Read)
*        The text file containing the keyword translation table.  The
*        format of this file is described under "File Format".  For
*        illustrations, see under "Examples of the File Format".
*     VALUE = LITERAL (Read)
*        The new value of the KEYWORD keyword for the "Update", "Write"
*        and "Amend" editing commands.  The special value "$V" means use
*        the current value of the KEYWORD keyword.  This makes it
*        possible to modify a comment, leaving the value unaltered.  In
*        addition "$V(keyword)" requests that the value of the reference
*        keyword given between the parentheses be assigned to the
*        keyword being edited.  This reference keyword must exist and
*        have a value for a "Write" or "Amend" (new keyword) edit;
*        whereas the FITS-header value is unchanged for "Update" or
*        "Amend" (keyword exists) if there are problems with this
*        reference keyword.

*  Examples:
*     fitsmod dro42 bscale exist
*        This reports TRUE or FALSE depending on whether or not the
*        FITS keyword BSCALE exists in the FITS extension of the NDF
*        called dro42.
*     fitsmod dro42 bscale p
*        This reports the value of the keyword BSCALE stored in the
*        FITS extension of the NDF called dro42.  An error message will
*        appear if BSCALE does not exist.
*     fitsmod abc edit=move keyword=bscale position=bzero
*        This moves the keyword BSCALE to lie immediately before keyword
*        BZERO in the FITS extension of the NDF called abc.  An error
*        will result if either BSCALE or BZERO does not exist.
*     fitsmod dro42 airmass dele
*        This deletes the keyword AIRMASS, if it exists, in the FITS
*        extension of the NDF called dro42.
*     fitsmod ndf=dro42 edit=d keyword=airmass[2]
*        This deletes the second occurrence of keyword AIRMASS, if it
*        exists, in the FITS extension of the NDF called dro42.
*     fitsmod @100 airmass w 1.456 "Airmass at mid-observation"
*        This creates the keyword AIRMASS in the FITS extension of the
*        NDF called 100, assigning the keyword the real value 1.456 and
*        comment "Airmass at mid-observation".  The header is
*        located just before the end.  The FITS extension is created if
*        it does not exist.
*     fitsmod @100 airmass w 1.456 "Airmass at mid-observation" phase
*        As the previous example except that the new keyword is written
*        immediately before keyword PHASE.
*     fitsmod obe observer u value="O'Leary" comment=$C
*        This updates the keyword OBSERVER with value "O'Leary",
*        retaining its old comment.  The modified FITS extension lies
*        within the NDF called obe.
*     fitsmod test filter w position=end value=27 comment=! string
*        This creates the keyword FILTER in the FITS extension of the
*        NDF called test, assigning the keyword the string value "27".
*        There is no comment.  The keyword is located at the end of the
*        headers, but before any END card.  The FITS extension is
*        created if it does not exist.
*     fitsmod test edit=w keyword=detector value=$V(ing.dethead)
*             comment="   Detector name" accept
*        This creates the keyword DETECTOR in the FITS extension of the
*        NDF called test, assigning the keyword the value of the
*        existing hierarchical keyword ING.DETHEAD.  The comment is
*        "   Detector name", the leading spaces are significant.  The
*        keyword is located at the current position keyword.  The FITS
*        extension is created if it does not exist.
*     fitsmod datafile mode=file table=fitstable.txt
*        This edits the FITS-extension of the NDF called
*        datafile, creating the FITS extension if it does not exist.
*        The editing instructions are stored in the text file called
*        fitstable.txt.

*  Notes:
*     -  Requests to move, assign values or comments, the following
*     reserved keywords in the FITS extension are ignored: SIMPLE,
*     BITPIX, NAXIS, NAXISn, EXTEND, PCOUNT, GCOUNT, XTENSION, BLOCKED,
*     and END.
*     -  When an error occurs during editing, warning messages are sent
*     at the normal reporting level, and processing continues to the
*     next editing command.
*     -  The FITS fixed format is used for writing or updating
*     headers, except for double-precision values requiring more space.
*     The comment is delineated from the value by the string " / ".
*     -  The comments in comment cards begin one space following the
*     keyword or from column 10 whichever is greater.
*     -  To be sure that the resultant FITS extension is what you
*     desired, you should inspect it using the command FITSLIST before
*     exporting the data.  If there is something wrong, you may find it
*     convenient to use command FITSEDIT to make minor corrections.

*  References:
*     "A User's Guide for the Flexible Image Transport System (FITS)",
*     NASA/Science Office of Science and Technology (1994).

*  Related Applications:
*     KAPPA: FITSEDIT, FITSEXIST, FITSEXP, FITSHEAD, FITSIMP, FITSLIST,
*     FITSVAL, FITSWRITE.

*  Examples of The File Format:
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
*     Note the full stop.
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
*     -  Field 1:
*        This specifies the editing operation.  Allowed values are
*        Amend, Delete, Exist, Move, Null, Print, Rename, Write, and
*        Update, and can be abbreviated to the initial upper-case
*        letter.  It is not case insensitive to afford some protection
*        against typing errors.
*        -  Delete removes a named keyword.
*        -  Print causes the value of a named keyword to be
*        displayed to standard output.
*        -  Exist reports TRUE to standard output if the named keyword
*        exists in the header, and FALSE if the keyword is not present.
*        -  Move relocates a named keyword to be immediately before a
*        second keyword.  When this positional keyword is not supplied,
*        it defaults to the END card, and if the END card is absent, the
*        new location is at the end of the headers.
*        -  Write creates a new card given a value and an optional
*        comment.  Its location uses the same rules as for the
*        Move command.
*        -  Update revises the value and/or the comment. If a secondary
*        keyword is defined explicitly, the card may be relocated at the
*        same time.  Update requires that the keyword exists.
*        -  Amend acts like Update if the keyword supplied in
*        "Field 2" exists, and like Write otherwise.
*        -  Null replaces the value of a named keyword with blanks.
*
*     -  Field 2:
*        This specifies the keyword to edit, and optionally the
*        position of that keyword in the header after the edit (for
*        Move, Write, Update, and Amend edits).  The new position in
*        the header is immediately before a positional keyword, whose
*        name is given in parentheses concatenated to the edit keyword.
*        See "Field 1" for defaulting when the position parameter is not
*        defined or is null.
*
*        Both the editing keyword and position keyword may be compound
*        to handle hierarchical keywords.  In this case the form is
*        keyword1.keyword2.keyword3 etc.  All keywords must be valid
*        FITS keywords.  This means they must be no more than eight
*        characters long, and the only permitted characters are
*        uppercase alphabetic, numbers, hyphen, and underscore.
*        Invalid keywords will be rejected.
*
*        Both the edit and position keyword may have an occurrence
*        specified in brackets [].  This enables editing of a keyword
*        that is not the first occurrence of that keyword, or locate a
*        edited keyword not at the first occurrence of the positional
*        keyword.  Note that it is not normal to have multiple
*        occurrences of a keyword in a FITS header, unless it is blank,
*        COMMENT or HISTORY.  Any text between the brackets other than
*        a positive integer is interpreted as the first occurrence.
*
*        Use a null value ('' or "") if you want the card to be a
*        comment with keyword other than COMMENT or HISTORY.  As blank
*        keywords are used for hierarchical keywords, to write a
*        comment in a blank keyword you must give a null edit keyword.
*        These have no keyword appearing before the left parenthesis
*        or bracket, such as (), [], [2], or (EPOCH).
*
*     -  Field 3:
*        This specifies the value to assign to the edited keyword in
*        the Write, Update, and Amend operations, or the name of the
*        new keyword in the Rename modification.  If the keyword
*        exists, its current value or keyword is replaced, as
*        appropriate.  The data type used to store the value is
*        inferred from the value itself.  See topic "Value Data Type".
*
*        For the Update, Write, and Amend modifications there is a
*        special value, $V, which means use the current value of the
*        edited keyword, provided that keyword exists.  This makes it
*        possible to modify a comment, leaving the value unaltered.  In
*        addition $V(keyword) requests that the value of the keyword
*        given between the parentheses be assigned to the keyword being
*        edited.
*
*        The value field is ignored when the keyword is COMMENT,
*        HISTORY or blank, and the modification is to Update, Write, or
*        Amend.
*
*     -  Field 4:
*        This specifies the comment to assign to the edited keyword for
*        the Write, Update, and Amend operations.  A leading "/"
*        delimiter should not be supplied.
*
*        There is a special value, $C, which means use the current
*        comment of the edited keyword, provided that keyword exists.
*        This makes it possible to modify a value, leaving the comment
*        unaltered.  In addition $C(keyword) requests that the comment
*        of the keyword given between the parentheses be assigned to
*        the edited keyword.
*
*        To obtain leading spaces before some commentary, use a quote
*        (') or double quote (") as the first character of the comment.
*        There is no need to terminate the comment with a trailing and
*        matching quotation character.  Also do not double quotes
*        should one form part of the comment.

*  Timing:
*     Approximately proportional to the number of FITS keywords to be
*     edited.  "Update" and "Write" edits require the most time.

*  Value Data Type:
*     The data type of a value is determined as follows:
*        -  For the text-file, values enclosed in quotes (') or doubled
*        quotes (") are strings.  Note that numeric or logical string
*        values must be quoted to prevent them being converted to a
*        numeric or logical value in the FITS extension.
*        -  For prompting the value is a string when parameter STRING
*        is TRUE.
*        -  Otherwise type conversions of the first word after the
*        keywords are made to integer, double precision, and logical
*        types in turn.  If a conversion is successful, that becomes the
*        data type.  In the case of double precision, the type is set
*        to real when the number of significant digits only warrants
*        single precision.  If all the conversions failed the value
*        is deemed to be a string.

*  Copyright:
*     Copyright (C) 1996, 1999-2000, 2004 Central Laboratory of the
*     Research Councils.
*     Copyright (C) 2008, 2009, 2011, 2015 Science and Technology
*     Facilties Council.
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
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1996 November 8 (MJC):
*        Original version.
*     22-SEP-1999 (DSB):
*        Added EXISTS parameter.
*     2000 May 8 (MJC):
*        Increased comment length to 70, allowing for comment or blank
*        lines.  Removed unused variables.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL.
*     2008 June 14 (MJC):
*        Add Amend editing option, and make Field 1 options easier to
*        read.
*     2009 January 11 (MJC):
*        Add Null editing option.
*     22-JUN-2011 (DSB):
*        Added READONLY parameter.
*     2015 April 22 (MJC):
*        Allow an END header to be written after the other headers if
*        one does not exist.  It has no associated value or comment.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Data-system constants
      INCLUDE 'NDF_PAR'          ! NDF__ public constants
      INCLUDE 'PRM_PAR'          ! PRM__ public constants
      INCLUDE 'PAR_ERR'          ! PAR__ error codes
      INCLUDE 'FIO_ERR'          ! FIO__ error codes
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Significant length of a string

*  Local Constants:
      INTEGER COMLN              ! Maximum number of characters in an
                                 ! comment in a FITS header card
      PARAMETER ( COMLN = 70 )

      INTEGER CVALLN             ! Maximum number of characters in a
                                 ! string value in a FITS header card
      PARAMETER ( CVALLN = 68 )

      INTEGER FITSLN             ! No. of characters in a FITS header
      PARAMETER ( FITSLN = 80 )  ! card

      INTEGER HKEYLN             ! Maximum number of characters in a
                                 ! hierarchical FITS keyword
      PARAMETER ( HKEYLN = 48 )

      INTEGER KEYLN              ! Maximum number of characters in a
                                 ! FITS keyword
      PARAMETER ( KEYLN = 8 )


*  Local Variables:
      CHARACTER * 6 AMODE         ! NDF access mode
      INTEGER ATEMPT             ! Number of attempts to obtain a
                                 ! valid keyword
      CHARACTER * ( DAT__SZLOC ) CELLOC ! Locator to first element of
                                 ! FITS extension locator
      CHARACTER * ( COMLN ) COMENT ! Comments
      INTEGER COPNTR             ! Pointer to mapped values
      INTEGER CSTAT              ! Local status variable
      DOUBLE PRECISION DVAL      ! Double precision FITS value
      CHARACTER * 6 EDIT         ! Edit command
      INTEGER EDPNTR             ! Pointer to mapped edit commands
      INTEGER EKPNTR             ! Pointer to mapped edit keywords
      LOGICAL EXISTS             ! Result of final "Exist" operation
      INTEGER FD                 ! Descriptor of the text file
      INTEGER FDIM( 1 )          ! Initial length of the FITS extension
      INTEGER INDF               ! NDF identifier
      INTEGER IVAL               ! Integer FITS value
      CHARACTER * ( HKEYLN ) KEYIND ! Position keyword
      CHARACTER * ( HKEYLN ) KEYS ! Keyword string
      CHARACTER * ( HKEYLN ) KEYWRD ! Edit keyword
      INTEGER KOCCUR             ! Occurrence of edit keyword
      INTEGER KOPNTR             ! Pointer to mapped keyword occurrences
      INTEGER LKEY               ! Length of a keyword
      CHARACTER * ( DAT__SZLOC ) LOC ! FITS extension locator
      LOGICAL LVAL               ! Logical FITS value
      CHARACTER * 9 MODE         ! Mode for accessing edit instructions
      INTEGER NBLANK             ! Number of blank lines in file
      INTEGER NCOMS              ! Number of comment lines in file
      INTEGER NDIGIT             ! Number of significant digits
      INTEGER NEDIT              ! Number of editing commands
      INTEGER NWRITE             ! Number of editing commands
                                 ! locators stored
      LOGICAL OPEN               ! Is text file open?
      INTEGER PKPNTR             ! Pointer to mapped positional keywords
      INTEGER POCCUR             ! Occurrence of positional keyword
      INTEGER POPNTR             ! Pointer to mapped poistional-keyword
                                 ! occurrences
      LOGICAL RDONLY             ! Will all the editing be read-only?
      LOGICAL STRING             ! Is value a string?
      LOGICAL THERE              ! FITS extension already exists?
      CHARACTER * ( DAT__SZTYP ) TYPE ! Component's data type
      INTEGER TYPNTR             ! Pointer to mapped types
      LOGICAL VALID              ! Keyword is valid?
      CHARACTER * ( CVALLN ) VALUE ! Character FITS value
      INTEGER VAPNTR             ! Pointer to mapped values

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain the mode of operation.
*  =============================
      CALL PAR_CHOIC( 'MODE', 'Interface', 'Interface,File',
     :                .TRUE., MODE, STATUS )

*  Access the FITS extension.
*  ==========================

*  Set the default for the READONLY parameter. For FILE mode, this is
*  always FALSE (since we do not know what commands will be performed
*  until the file is read we have to assume that they may require write
*  access). For INTERFACE mode, the default depends on the requested
*  editing command.
      IF( MODE .EQ. 'FILE' ) THEN
         RDONLY = .FALSE.

*  For INTERFACE mode, the default depends on the requested editing
*  command.
      ELSE

*  Obtain the edit command
         CALL PAR_CHOIC( 'EDIT', 'Read', 'Amend,Delete,Exist,Move,'/
     :                    /'Null,Print,Rename,Update,Write',
     :                   .FALSE., EDIT, STATUS )

*  If the editing command does not change the file, use a default of TRUE
*  for READONLY.
         RDONLY = ( EDIT .EQ. 'EXIST' .OR. EDIT .EQ. 'PRINT' )
      END IF

*  Set the default for READONLY and get a value from the environment.
      CALL PAR_DEF0L( 'READONLY', RDONLY, STATUS )
      CALL PAR_GET0L( 'READONLY', RDONLY, STATUS )

*  Choose the appropriate NDF access mode.
      IF( RDONLY ) THEN
         AMODE = 'READ'
      ELSE
         AMODE = 'UPDATE'
      END IF

*  Obtain the NDF to be updated.
      CALL LPG_ASSOC( 'NDF', AMODE, INDF, STATUS )

*  See whether or not there is a FITS extension.
      CALL NDF_XSTAT( INDF, 'FITS', THERE, STATUS )
      IF ( .NOT. THERE ) THEN

*  Create the FITS extension.  Use an arbitrary size.  It will be
*  increased, if required, as new keywords are created.
         FDIM( 1 ) = 36
         CALL NDF_XNEW( INDF, 'FITS', '_CHAR*80', 1, FDIM, LOC, STATUS )

*  Since we want to map the FITS array in update mode, it must first
*  have a value to read.  Therefore write an END card to the first cell
*  of the array.
         CALL DAT_CELL ( LOC, 1, 1, CELLOC, STATUS )
         CALL DAT_PUT0C ( CELLOC, 'END   ', STATUS )
         CALL DAT_ANNUL ( CELLOC, STATUS )
      ELSE

*  Find the FITS extension.
         CALL NDF_XLOC( INDF, 'FITS', AMODE, LOC, STATUS )
      END IF

*    Abort if the answer is illegal.
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Determine the maximum increase in size of the FITS extension.
*  =============================================================
      IF ( MODE .EQ. 'FILE' ) THEN

*  Open the file of editing instructions.
         OPEN = .FALSE.
         CALL FIO_ASSOC( 'TABLE', 'READ', 'LIST', 0, FD, STATUS )
         IF ( STATUS .EQ. SAI__OK ) OPEN = .TRUE.

*  Count the number of data lines in the file.
         CALL KPG1_NUMFL( FD, '!,#', NWRITE, NCOMS, NBLANK, STATUS )

*  Obtain and perform the editing instructions for the file mode.
*  ==============================================================

*  Create and map workspace to hold the edit commands, edit and
*  positional keywords, values, comments, types, and edit and positional
*  occurrences.
         CALL PSX_MALLOC( NWRITE, EDPNTR, STATUS )
         CALL PSX_MALLOC( NWRITE * HKEYLN, EKPNTR, STATUS )
         CALL PSX_MALLOC( NWRITE * HKEYLN, PKPNTR, STATUS )
         CALL PSX_CALLOC( NWRITE, '_INTEGER', KOPNTR, STATUS )
         CALL PSX_CALLOC( NWRITE, '_INTEGER', POPNTR, STATUS )
         CALL PSX_MALLOC( NWRITE * CVALLN, VAPNTR, STATUS )
         CALL PSX_MALLOC( NWRITE * COMLN, COPNTR, STATUS )
         CALL PSX_MALLOC( NWRITE * 8, TYPNTR, STATUS )

*  Read and parse the lines of the file of editing instructions.
*  Validate the headers, syntax, and values.  Store the information in
*  the work arrays.  Notice that the lengths must be passed in order
*  following the last true argument: STATUS.
         CALL FTS1_RFMOD( FD, NWRITE, NEDIT, %VAL( CNF_PVAL( EDPNTR ) ),
     :                    %VAL( CNF_PVAL( EKPNTR ) ),
     :                    %VAL( CNF_PVAL( PKPNTR ) ),
     :                    %VAL( CNF_PVAL( KOPNTR ) ),
     :                    %VAL( CNF_PVAL( POPNTR ) ),
     :                    %VAL( CNF_PVAL( VAPNTR ) ),
     :                    %VAL( CNF_PVAL( COPNTR ) ),
     :                    %VAL( CNF_PVAL( TYPNTR ) ), STATUS,
     :                    %VAL( CNF_CVAL( 1 ) ),
     :                    %VAL( CNF_CVAL( HKEYLN ) ),
     :                    %VAL( CNF_CVAL( HKEYLN ) ),
     :                    %VAL( CNF_CVAL( CVALLN ) ),
     :                    %VAL( CNF_CVAL( COMLN ) ),
     :                    %VAL( CNF_CVAL( 8 ) ) )

*  Perform the edits on the FITS extension.
         CALL FTS1_EDFEX( NEDIT, %VAL( CNF_PVAL( EDPNTR ) ),
     :                    %VAL( CNF_PVAL( EKPNTR ) ),
     :                    %VAL( CNF_PVAL( PKPNTR ) ),
     :                    %VAL( CNF_PVAL( KOPNTR ) ),
     :                    %VAL( CNF_PVAL( POPNTR ) ),
     :                    %VAL( CNF_PVAL( VAPNTR ) ),
     :                    %VAL( CNF_PVAL( COPNTR ) ),
     :                    %VAL( CNF_PVAL( TYPNTR ) ),
     :                    LOC, EXISTS, STATUS, %VAL( CNF_CVAL( 1 ) ),
     :                    %VAL( CNF_CVAL( HKEYLN ) ),
     :                    %VAL( CNF_CVAL( HKEYLN ) ),
     :                    %VAL( CNF_CVAL( CVALLN ) ),
     :                    %VAL( CNF_CVAL( COMLN ) ),
     :                    %VAL( CNF_CVAL( 8 ) ) )

*  Free the workspace.
         CALL PSX_FREE( EDPNTR, STATUS )
         CALL PSX_FREE( EKPNTR, STATUS )
         CALL PSX_FREE( PKPNTR, STATUS )
         CALL PSX_FREE( KOPNTR, STATUS )
         CALL PSX_FREE( POPNTR, STATUS )
         CALL PSX_FREE( VAPNTR, STATUS )
         CALL PSX_FREE( COPNTR, STATUS )
         CALL PSX_FREE( TYPNTR, STATUS )

*  Close the text file.
         IF ( OPEN ) CALL FIO_CLOSE( FD, STATUS )


*  Obtain the editing instructions via the parameter system.
*  =========================================================
      ELSE

*  Interface mode only allows one edit per invocation.
         NEDIT = 1

*  Obtain the edit keyword and occurrence.
*  ---------------------------------------

*  The edit keyword has to be validated and the occurrence extracted.
*  So perform a DO WHILE loop, but give up after 5 failed attempts.
         ATEMPT = 1
         VALID = .FALSE.
   10    CONTINUE       ! Start of 'DO WHILE' loop.
         IF ( ATEMPT .LT. 5 .AND. .NOT. VALID ) THEN

*  Obtain the keyword definition and its length.
            CALL PAR_GET0C( 'KEYWORD', KEYS, STATUS )
            IF ( STATUS .NE. SAI__OK ) GOTO 999
            LKEY = CHR_LEN( KEYS )

*  Watch for the special case when the keyword is blank.  The
*  subroutine below will take care of a blank followed by brackets.
            IF ( LKEY .EQ. 0 ) THEN
               KEYWRD = ' '
               KOCCUR = 1

            ELSE

*  Extract the uppercase keyword and any occurrence, and find the
*  length of the keyword.  Also validate the keyword.  Use a new error
*  context so that the error can be flushed.
               CALL ERR_MARK
               CALL FTS1_EVKEY( KEYS( :LKEY ), KEYWRD, LKEY, KOCCUR,
     :                          STATUS )

*  Report what went wrong.  Increment the attempt count.
               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL ERR_FLUSH( STATUS )
                  ATEMPT = ATEMPT + 1
               ELSE
                  VALID = .TRUE.
               END IF
               CALL ERR_RLSE
            END IF

*  Return to the head of the 'DO WHILE' loop.
            IF ( .NOT. VALID ) GOTO 10
         END IF

*  Give up.
         IF ( .NOT. VALID ) GOTO 999

*  Obtain the position keyword and occurrence.
*  -------------------------------------------

*  This is only relevant for certain editing instructions, but
*  initialise a value otherwise to be on the safe side.
         KEYIND = ' '
         IF ( EDIT .EQ. 'MOVE' .OR. EDIT .EQ. 'UPDATE' .OR.
     :        EDIT .EQ. 'WRITE' .OR. EDIT .EQ. 'AMEND' ) THEN

*  The positional keyword has to be validated and the occurrence
*  extracted.  So perform a DO WHILE loop, but give up after 5 failed
*  attempts.
            ATEMPT = 1
            VALID = .FALSE.
   20       CONTINUE       ! Start of 'DO WHILE' loop.
            IF ( ATEMPT .LT. 5 .AND. .NOT. VALID ) THEN

*  Obtain the position keyword definition.
               CALL ERR_MARK
               CALL PAR_GET0C( 'POSITION', KEYS, STATUS )

*  A null value means insert before at the END card or at the end for
*  Write and Move edits.  It is null for Update, meaning leave the card
*  unmoved.  It may also be null for Amend too, but we do not know
*  whether the header exists at this point, so treat as Write.
               IF ( STATUS .EQ. PAR__NULL ) THEN
                  CALL ERR_ANNUL( STATUS )
                  VALID = .TRUE.
                  IF ( EDIT .EQ. 'MOVE' .OR. EDIT .EQ. 'AMEND' .OR.
     :                 EDIT .EQ. 'WRITE' ) KEYIND = 'END'
                  POCCUR = 1

               ELSE IF ( STATUS .NE. SAI__OK ) THEN
                  CALL ERR_RLSE
                  GOTO 999

               ELSE

*  Watch for the special case when the keyword is blank.  The
*  subroutine below will take care of a blank followed by brackets.
                  LKEY = CHR_LEN( KEYS )
                  IF ( LKEY .EQ. 0 ) THEN
                     KEYIND = ' '
                     POCCUR = 1

                  ELSE

*  Extract the uppercase keyword and any occurrence, and find the
*  length of the keyword.  Also validate the keyword.  Use a new error
*  context so that the error can be flushed.
                     CALL ERR_MARK
                     CALL FTS1_EVKEY( KEYS( :LKEY ), KEYIND, LKEY,
     :                                POCCUR, STATUS )

*  Report what went wrong.  Increment the attempt count.
                     IF ( STATUS .NE. SAI__OK ) THEN
                        CALL ERR_FLUSH( STATUS )
                        ATEMPT = ATEMPT + 1
                     ELSE
                        VALID = .TRUE.
                     END IF
                     CALL ERR_RLSE
                  END IF
               END IF
               CALL ERR_RLSE

*  Return to the head of the 'DO WHILE' loop.
               IF ( .NOT. VALID ) GOTO 20
            END IF

*  Give up.
            IF ( .NOT. VALID ) GOTO 999
         END IF

*  Obtain the value.
*  -----------------

*  This is only relevant for certain editing instructions, but
*  initialise a value otherwise to be on the safe side.
         VALUE = ' '
         TYPE = ' '
         IF ( EDIT .EQ. 'UPDATE' .OR. EDIT .EQ. 'WRITE' .OR.
     :        EDIT .EQ. 'AMEND' ) THEN

*  The value is not needed for certain named comment cards or the END
*  card.
            IF ( KEYWRD .NE. 'COMMENT' .AND. KEYWRD .NE. 'HISTORY' .AND.
     :           KEYWRD .NE. ' ' .AND. KEYWRD .NE. 'END' ) THEN

*  Obtain the value.
               CALL ERR_MARK
               CALL PAR_GET0C( 'VALUE', VALUE, STATUS )

*  A null value means the string is blank, posssibly for a comment card.
*  insert before at the END card or at the end.
               IF ( STATUS .EQ. PAR__NULL ) THEN
                  CALL ERR_ANNUL( STATUS )
                  VALUE = ' '

               ELSE IF ( STATUS .NE. SAI__OK ) THEN
                  CALL ERR_RLSE
                  GOTO 999
               END IF
               CALL ERR_RLSE

*  Determine the type of the value.
*  --------------------------------

*  Is the value a string?
               CALL ERR_MARK
               CALL PAR_GET0L( 'STRING', STRING, STATUS )
               IF ( STATUS .EQ. PAR__NULL ) THEN
                  CALL ERR_ANNUL( STATUS )
                  STRING = .FALSE.
               END IF
               CALL ERR_RLSE

*  The data type of value must be character if it is quoted.
               IF ( STRING .OR. VALUE( 1:1 ) .EQ. '''' .OR.
     :              VALUE( 1:1 ) .EQ. '"' ) THEN
                  TYPE = '_CHAR'

*  The data type is unknown the $V special value is used.
               ELSE IF ( VALUE( 1:2 ) .EQ. '$V' ) THEN
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
                        IF ( NDIGIT .GT.
     :                       -INT( LOG10( VAL__EPSR ) ) ) THEN
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

*  There is no value.  Use the special type of 'COMMENT'.
            ELSE
               TYPE = 'COMMENT'
            END IF

*  Obtain the comment.
*  -------------------

*  Initialise a comment as a value is always passed to the editing
*  routine.
            COMENT = ' '

*  The comment is not needed for the END card.
            IF ( KEYWRD .NE. 'END' ) THEN

*  Obtain the comment string.
               CALL ERR_MARK
               CALL PAR_GET0C( 'COMMENT', COMENT, STATUS )

*  A null value means the string is blank, posssibly for a comment card.
*  insert before at the END card or at the end.
               IF ( STATUS .EQ. PAR__NULL ) THEN
                  CALL ERR_ANNUL( STATUS )
                  COMENT = ' '

               ELSE IF ( STATUS .NE. SAI__OK ) THEN
                  CALL ERR_RLSE
                  GOTO 999
               END IF
               CALL ERR_RLSE

*   Preserved the comment when making a keyword's value null.
            ELSE IF ( EDIT .EQ. 'NULL' ) THEN
               COMENT = '$C'
            END IF
         END IF

*  Obtain the new keyword.
*  -----------------------

*  This is only relevant for the rename operation.
         IF ( EDIT .EQ. 'RENAME' ) THEN

*  The new keyword has to be validated and any erroneous occurrence
*  removed.  So perform a DO WHILE loop, but give up after 5 failed
*  attempts.
            ATEMPT = 1
            VALID = .FALSE.
   30       CONTINUE       ! Start of 'DO WHILE' loop.
            IF ( ATEMPT .LT. 5 .AND. .NOT. VALID ) THEN

*  Obtain the keyword definition and its length.
               CALL PAR_GET0C( 'NEWKEY', KEYS, STATUS )
               IF ( STATUS .NE. SAI__OK ) GOTO 999
               LKEY = CHR_LEN( KEYS )

*  Watch for the special case when the keyword is blank.  The
*  subroutine below will take care of a blank followed by brackets.
               IF ( LKEY .EQ. 0 ) THEN
                  VALUE = ' '

               ELSE

*  Extract the uppercase keyword and any occurrence, and find the
*  length of the keyword.  Also validate the keyword.  Use a new error
*  context so that the error can be flushed.  Note the FTS1_EDKEY uses
*  the value for the renamed keyword.  Use POCCUR as a dummy
                  CALL ERR_MARK
                  CALL FTS1_EVKEY( KEYS( :LKEY ), VALUE, LKEY, POCCUR,
     :                             STATUS )

*  Report what went wrong.  Increment the attempt count.
                  IF ( STATUS .NE. SAI__OK ) THEN
                     CALL ERR_FLUSH( STATUS )
                     ATEMPT = ATEMPT + 1
                  ELSE
                     VALID = .TRUE.
                  END IF
                  CALL ERR_RLSE
               END IF

*  Return to the head of the 'DO WHILE' loop.
               IF ( .NOT. VALID ) GOTO 30
            END IF

*  Give up.
            IF ( .NOT. VALID ) GOTO 999
         END IF

*  Perform the editing operation.
*  ==============================
         CALL FTS1_EDFEX( NEDIT, EDIT, KEYWRD, KEYIND, KOCCUR,
     :                    POCCUR, VALUE, COMENT, TYPE, LOC, EXISTS,
     :                    STATUS )
      END IF

* Store the result of the final "Exist" operation in the EXISTS output
* parameter.
      CALL PAR_PUT0L( 'EXISTS', EXISTS, STATUS )

  999 CONTINUE

*  Annul (thereby unmapping) locator to the FITS extension.
      CALL DAT_ANNUL( LOC, STATUS )

*  Annul the NDF identifier.
      CALL NDF_ANNUL( INDF, STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'FITSMOD_ERR',
     :     'FITSMOD: Error editing FITS extension in an NDF.', STATUS )
      END IF

      END
