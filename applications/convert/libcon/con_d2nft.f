      SUBROUTINE CON_D2NFT( PATH, NAME, TYPE, COMENT, OUTNDF, NPC,
     :                      STRUCT, STNAME, NFITS, STATUS )
*+
*  Name:
*     CON_D2NFT

*  Purpose:
*     Write an NDF FITS extension card from a DST FITS component.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CON_D2NFT( PATH, NAME, TYPE, COMENT, OUTNDF, NPC, STRUCT,
*                     STNAME, NFITS, STATUS )

*  Description:
*     This routine serves CON_DST2N.  It creates or extends an NDF
*     FITS extension, by transferring to it information stored in a DST
*     file's FITS structure (and substructures).  FITS cards are written
*     using the fixed format.  Substructure objects are stored in blank
*     keyword headers that have the keyword=value syntax from column 10.
*     The start of a block of such headers can have a heading preceded
*     by a blank line when STNAME is not blank (and STRUCT is .TRUE.).

*  Arguments:
*     PATH = CHARACTER * ( * ) (Given)
*        The path of the FITS component in the DST FITS structure.
*     NAME = CHARACTER * ( * ) (Given)
*        The name of the FITS keyword to be written (also usually the
*        name of the component in the DST FITS structure).  If this is
*        blank, a blank header card is written.
*     TYPE = CHARACTER * ( * ) (Given)
*        The data type of the FITS structure primitive component.
*     COMENT = CHARACTER * ( * ) (Given)
*        The comment associated with the FITS value.
*     OUTNDF = CHARACTER * ( * ) (Given)
*        The name of the output NDF including the path in the container
*        file.
*     NPC = INTEGER (Given)
*        Number of significant characters in OUTNDF.
*     STRUCT = LOGICAL (Given)
*        Whether or not the component was stored in a structure and so
*        is output in a blank comment card in La Palma
*        hierarchical-keyword style (since this is why this feature is
*        present).
*     STNAME = CHARACTER * ( * ) (Given)
*        The name of the structure in the DST FITS structure containing
*        a block of associated keywords.  It is used to write a heading
*        before the keywords are written as blank comments, provided
*        it is not null.  STNAME is ignored when STRUCT is .FALSE..
*     NFITS = INTEGER (Given and Returned)
*        The number of FITS headers in the output NDF FITS extension.
*     STATUS = INTEGER ({status_access_mode})
*        The global status.

*  Copyright:
*     Copyright (C) 1996 Central Laboratory of the Research Councils.
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
*     1996 September 20 (MJC):
*        Original version, based on code originally in CON_DST2N.
*     {enter_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) PATH
      CHARACTER * ( * ) NAME
      CHARACTER * ( * ) TYPE
      CHARACTER * ( * ) COMENT
      CHARACTER * ( * ) OUTNDF
      INTEGER NPC
      LOGICAL STRUCT
      CHARACTER * ( * ) STNAME

*  Arguments Given and Returned:
      INTEGER NFITS              ! Number of FITS items

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Length of a character string ignoring
                                 ! trailing blanks
*  Local Constants:
      INTEGER FITKEY             ! Length of a FITS keyword
      PARAMETER ( FITKEY = 8 )
      INTEGER FITLEN             ! Length of a FITS header card
      PARAMETER ( FITLEN = 80 )
      INTEGER VALLEN             ! Maximum length of a FITS value
      PARAMETER ( VALLEN = FITLEN - FITKEY )

*  Local Variables:
      BYTE BARRAY( 100 )         ! Used to read in BYTE type data items
      CHARACTER * ( 256 ) BUFFER ! Used to form error messages and paths
      DOUBLE PRECISION DARRAY( 100 ) ! Used to read in DP type data items
      INTEGER DSTAT              ! DTA_ routine returned status
      REAL FARRAY( 100 )         ! Used to read in FLOAT type data items
      INTEGER FDIMS( 2 )         ! Dimensions of FITS extension
      CHARACTER * ( 90 ) FITNAM  ! Name of FITS-structure item
      CHARACTER * ( FITLEN ) FITSTR ! Contains FITS string
      CHARACTER * ( VALLEN ) FITVAL ! Contains FITS value
      INTEGER IARRAY( 100 )      ! Used to read in INTEGER type data
                                 ! items
      INTEGER MINLEN             ! Minimum length of input FITS string
      CHARACTER * ( 80 ) NAMOUT  ! Name in output structure
      INTEGER NC                 ! Number of characters
      INTEGER NCC                ! Column from where the comment
                                 ! appears in the FITS card image
      INTEGER NCI                ! Column in input FITS character value
      INTEGER NCO                ! Column in output FITS character value
      INTEGER NCOM               ! Length of FITS comment
      INTEGER NCQ                ! Position of a quote in an input FITS
                                 ! value
      INTEGER NDATA              ! No. of data values
      INTEGER NF                 ! Length of string
      INTEGER NOFF               ! Offset for start of keyword
      INTEGER NSTR               ! Effective length of string
      INTEGER NSTRT              ! True length of string
      INTEGER*2 SARRAY(100)      ! Used to read in SHORT type data items

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM declarations for conversions
      INCLUDE 'NUM_DEF_CVT'      ! NUM definitions for conversions

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Validate the number of FITS headers.
      NFITS = MAX ( NFITS, 0 )

*  Test for a structured component of the DST FITS structure.
      BUFFER = OUTNDF( :NPC )//'.MORE'
      IF ( STRUCT .AND. STNAME .NE. ' ' ) THEN

*  Write a blank line and comment containing the name of the hierarchy.
*  Obtain the name of the FITS extension.
         NFITS = NFITS + 1
         FDIMS( 1 ) = 1
         FDIMS( 2 ) = NFITS
         CALL DTA_CRNAM( BUFFER, 'FITS', 2, FDIMS, NAMOUT, DSTAT )

*  Write the FITS blank card image to the FITS extension.
         FITSTR = ' '
         CALL DTA_WRVARC( NAMOUT, 80, FITSTR, DSTAT )

*  Select the next FITS header, and write out the heading in a comment.
         NFITS = NFITS + 1
         FDIMS( 1 ) = 1
         FDIMS( 2 ) = NFITS
         CALL DTA_CRNAM( BUFFER, 'FITS', 2, FDIMS, NAMOUT, DSTAT )
         FITSTR = 'COMMENT   Keywords for structure: '//STNAME
         CALL DTA_WRVARC( NAMOUT, 80, FITSTR, DSTAT )

*  Test for a blank keyword name.
      ELSE IF ( NAME .EQ. ' ' ) THEN

*  Write a blank line and comment containing the name of the hierarchy.
*  Obtain the name of the FITS extension.
         NFITS = NFITS + 1
         FDIMS( 1 ) = 1
         FDIMS( 2 ) = NFITS
         CALL DTA_CRNAM( BUFFER, 'FITS', 2, FDIMS, NAMOUT, DSTAT )

*  Write the FITS blank card image to the FITS extension, and return.
         FITSTR = ' '
         CALL DTA_WRVARC( NAMOUT, 80, FITSTR, DSTAT )
         GOTO 500
      END IF

*  Initialise the FITS value as part of it may only be overwritten
*  otherwise.
      FITVAL = ' '

*  Read the FITS item value. Each type possibility must be catered for
*  separately. The item value is then converted into a character
*  string.  Note that numeric and logical types are right justified to
*  20 characters.  Character values may be longer, and are left
*  justified.
      NDATA = 1
      IF ( TYPE .EQ. 'BYTE' ) THEN
         CALL DTA_RDVARB( PATH, NDATA, BARRAY, DSTAT )
         IF ( DSTAT .NE. 0 ) GOTO 450
         IARRAY( 1 ) = NUM_BTOI( BARRAY( 1 ) )
         CALL CHR_ITOC( IARRAY( 1 ), FITVAL( :20 ), NC )

      ELSE IF ( TYPE .EQ. 'CHAR' ) THEN
         CALL DTA_RDVARC( PATH, FDIMS( 1 ), FITVAL, DSTAT )
         IF ( DSTAT .NE. 0 ) GOTO 450

      ELSE IF ( TYPE .EQ. 'DOUBLE' ) THEN
         CALL DTA_RDVARD( PATH, NDATA, DARRAY, DSTAT )
         IF ( DSTAT .NE. 0 ) GOTO 450
         CALL CHR_DTOC( DARRAY( 1 ), FITVAL( :20 ), NC )

      ELSE IF ( TYPE .EQ. 'FLOAT' ) THEN
         CALL DTA_RDVARF( PATH, NDATA, FARRAY, DSTAT )
         IF ( DSTAT .NE. 0 ) GOTO 450
         CALL CHR_RTOC( FARRAY( 1 ), FITVAL( :20 ), NC )

      ELSE IF ( TYPE .EQ. 'INT' ) THEN
         CALL DTA_RDVARI( PATH, NDATA, IARRAY, DSTAT )
         IF ( DSTAT .NE. 0 ) GOTO 450
         CALL CHR_ITOC( IARRAY( 1 ), FITVAL( :20 ), NC )

      ELSE IF ( TYPE .EQ. 'SHORT' ) THEN
         CALL DTA_RDVARS( PATH, NDATA, SARRAY, DSTAT )
         IF ( DSTAT .NE. 0 ) GOTO 450
         IARRAY( 1 ) = NUM_WTOI( SARRAY( 1 ) )
         CALL CHR_ITOC( IARRAY( 1 ), FITVAL( :20 ), NC )
      END IF

*  Initialise the FITS card-image character string.
      FITSTR = ' '

*  Format the FITS character string or "card image".  It is composed of
*  the following items.
*    o  A keyword occupies the first eight columns.
*    o  An equals sign and a following blank go into spaces 8 and 9.
*    o  The remaining spaces up to space 31 are used for
*       the value of the FITS item.  Hence the length of FITVAL is 20
*       (it used to be variable STRING*64), except for character
*       strings which may fill the card.
*       -  Character type items are left justified and other types are
*          right justified.
*       -  Character items are enclosed in quotes and must be at least
*          8 characters long.  Given the comments, it is limited to 18
*          characters.
*    o  The comment delimiter is in column 32, and comments start at
*       column 34.
*    o  Columns 31 and 33 are spaces.
*
*    The above are shifted right by 9 characters for a structured FITS
*    component.

*  Find the lengths of the value and keyword.
      NSTRT = CHR_LEN( FITVAL )
      NSTR = NSTRT
      NF = CHR_LEN( NAME )

*  Start to build the FITS card image.  Allow for the offset for
*  structured keywords.  The equals sign is aligned in column 22 or
*  further right if the name is longer than 13 characters in an
*  hierarchical keyword (13 was chosen because this is the length used
*  by the ING group), but must appear in column 9 for regulation FITS
*  keyword=value headers.
      IF ( STRUCT ) THEN
         NOFF = 9
         FITSTR( NOFF + 1: NF + NOFF ) = NAME( 1: NF )
         NOFF = MAX( NF + NOFF - 8, 13 )
      ELSE
         FITSTR( 1:NF ) = NAME( 1:NF )
         NOFF = 0
      END IF
      FITSTR( NOFF + 9: NOFF + 10 ) = '= '

*  Insert the value strings.
      IF ( TYPE .EQ. 'CHAR' ) THEN

*  Constrain the length of the character value.
         NSTR = MIN( 68 - NOFF, MAX( 8, NSTR ) )

*  Insert the leading quote, the value, and then the trailing quote.
         FITSTR( 11 + NOFF:11 + NOFF ) = ''''

*  The valued can be extracted verbatim when it does not contain any
*  quotes.  If it does include quotes these must be doubled in the FITS
*  character value.  So first look for a quote.
         NCQ = INDEX( FITVAL, '''' )
         IF ( NCQ .EQ. 0 ) THEN
            FITSTR( 12 + NOFF:11 + NSTR + NOFF ) = FITVAL( 1:NSTR )

*  Insert the trailing quote.
            FITSTR( NSTR + 12 + NOFF:NSTR + 12 + NOFF ) = ''''

*  Search for the quotes, and form the FITS value string piecemeal,
*  adding an extra quote and appending the text between the quotes.
         ELSE

*  Start the search at the first column of the input value, but in
*  column 12 of the FITS card image.
            NCI = 1
            NCO = 12 + NOFF
            MINLEN = 8

*  Loop until there are no more quotes in the string.
            DO WHILE ( NCQ .NE. 0 )

*  Extract the portion of the value up to and including the quote.
               FITSTR( NCO:NCO + NCQ - 1 ) = FITVAL( NCI:NCI + NCQ - 1 )

*  Move the counters to the character after the quote.
               NCO = NCO + NCQ
               NCI = NCI + NCQ

*  Add the extra quote to the FITS value and moving the character
*  counter along.
               FITSTR( NCO:NCO ) = ''''
               NCO = NCO + 1

*  Look for the next quote.
               NCQ = INDEX( FITVAL( NCI: ), '''' )

*  We do not want trailing blanks when the original value had less than
*  eight characters.  So decrement the effective length of the value so
*  we have replaced trailing blanks with second quotes.
               IF ( NSTRT .LT. MINLEN ) THEN
                  NSTR = NSTR - 1
                  MINLEN = MINLEN - 1
               END IF
            END DO

*  Append the remainder of the text that follows the last quote.
*  Increment the character counter.
            FITSTR( NCO:NCO + NSTR - NCI ) = FITVAL( NCI: )
            NCO = NCO + NSTR - NCI + 1

*  Insert the trailing quote.
            FITSTR( NCO:NCO ) = ''''

*  Revise the length, which will be used to specify the location of the
*  comment field.
            NSTR = NCO - 12
         END IF

      ELSE

*  Insert the non-character value, right justified.
         FITSTR( 31 + NOFF - NSTR:30 + NOFF ) = FITVAL( 1:NSTR )

*  By definition the length of the value must be 20.
         NSTR = 20
      END IF

*  The backslash to separate the item value from the comment and a
*  following blank are inserted after the value if there is room. It
*  must have capacity to write at least four characters of comment plus
*  3 for the comment delimiter and the spaces bracketing it.
      IF ( NSTR .LE. 61 ) THEN

*  For numeric data the delimiter will occur in column 32.
         IF ( TYPE .NE. 'CHAR' ) THEN
            NCC = 32 + NOFF

*  The furthest left the delimiter can go is column 32, and therefore
*  the comment must come after column 33.
         ELSE
            NCC = MAX( 32 + NOFF, NSTR + 14 )
         END IF

*  Write the delimiter to the card image.
         FITSTR( NCC:NCC+1 ) = '/ '

*  The comment itself is copied into the remaining space, provided
*  there is a comment.
         NCOM = CHR_LEN( COMENT )
         IF ( NCOM .GT. 0 ) THEN
            NCOM = MAX( CHR_LEN( COMENT ), 79 - NCC )
            FITSTR( NCC + 2:NCC + 1 + NCOM ) = COMENT( 1:NCOM )
         END IF
      END IF

*  Obtain the name of the FITS extension.  This extends the object if it
*  already exists.(?)
      NFITS = NFITS + 1
      FDIMS( 1 ) = 1
      FDIMS( 2 ) = NFITS
      CALL DTA_CRNAM( BUFFER, 'FITS', 2, FDIMS, NAMOUT, DSTAT )

*  Write the FITS card image to the FITS extension.
      CALL DTA_WRVARC( NAMOUT, 80, FITSTR, DSTAT )

      GOTO 500
  450 CONTINUE
      STATUS = DSTAT
      BUFFER = 'Error reading '//PATH//'.'

      CALL ERR_REP( 'CON_D2NFT_READER', BUFFER, STATUS )
  500 CONTINUE

      END
