      SUBROUTINE FTS1_PRVAL( NCARD, HEADER, KEYWRD, OCCUR, CARD,
     :                       SVALUE, COMENT, STATUS )
*+
*  Name:
*     FTS1_PRVAL

*  Purpose:
*     Prints the value of a specified card in an array of FITS headers.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FTS1_PRVAL( NCARD, HEADER, KEYWRD, OCCUR, CARD, SVALUE,
*                      COMENT, STATUS )

*  Description:
*     This routine reports the value of a keyword in an array of FITS
*     headers via the Messaging system.  It also returns the value and
*     comment assoiciated with the header.  Hierarchical keywords are
*     allowed.  A bad status is returned if the desired keyword is not
*     present in the header array, or the card index is out of bounds.

*  Arguments:
*     NCARD = INTEGER (Given)
*        The number of cards in the FITS header array.
*     HEADER( NCARD ) = CHARACTER * ( 80 ) (Given)
*        The array of FITS headers.
*     KEYWRD = CHARACTER * ( * ) (Given)
*        The keyword to search for in the array.  This may be a
*        compound name to handle hierarchical keywords, and it has the
*        form keyword1.keyword2.keyword3 etc.  The maximum number of
*        keywords per FITS card is 20.  Comparisons are performed in
*        uppercase and blanks are removed.  Each keyword must be no
*        longer than 8 characters.
*     OCCUR = INTEGER (Given)
*        The occurrence of the keyword to locate.  Values less than 1
*        obtain the first occurrence.
*     CARD = INTEGER (Given)
*        The index number of card containing the desired keyword.
*        Must be between 1 and NCARD.
*     SVALUE =  CHARACTER * ( * ) (Returned)
*        The value.  This should have length of at least 68 characters.
*     COMENT =  CHARACTER * ( * ) (Returned)
*        The value.  This should have length of at least 60 characters,
*        although normally 47 characters suffice.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     The comments and values are extracted from a single card.

*  Copyright:
*     Copyright (C) 2005 Particle Physics & Astronomy Research Council.
*     Copyright (C) 2011 Science & Technology Facilities Council.
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
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2005 October 6 (MJC):
*        Original version.
*     2011 May 10 (MJC):
*        Set mandatory bad status before calling ERR_REP.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'MSG_PAR'          ! MSG__ constants

*  Arguments Given:
      INTEGER NCARD
      CHARACTER * ( * ) HEADER( NCARD )
      CHARACTER * ( * ) KEYWRD
      INTEGER OCCUR
      INTEGER CARD

*  Arguments Returned:
      CHARACTER * ( * ) SVALUE
      CHARACTER * ( * ) COMENT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER VALLN              ! Maximum number of characters in a
                                 ! FITS header card value
      PARAMETER ( VALLN = 68 )

*  Local Variables:
      INTEGER COCCUR             ! Local positive copy of the count
      INTEGER DCARD              ! Dummy card number
      DOUBLE PRECISION DVALUE    ! FITS value
      INTEGER IVALUE             ! FITS value
      LOGICAL LVALUE             ! FITS value
      INTEGER NC                 ! Number of characters in value
      LOGICAL PRESNT             ! Named card is present? (dummy)
      REAL RVALUE                ! FITS value
      CHARACTER * ( DAT__SZTYP ) TYPE ! Data type of the value to be
                                 ! written
      CHARACTER * ( VALLN ) VALUE ! String value

*.

*  Initialise the returned value.
      SVALUE = ' '

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Validate the input dimensions.
      IF ( CARD .LT. 1 .OR. CARD .GT. NCARD ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'M', NCARD )
         CALL MSG_SETI( 'C', CARD )
         CALL ERR_REP( 'FTS1_PRVAL_INVCARD',
     :     'The FITS header whose value is required has an index (^C)' /
     :     /'not between 1 and the number of headers (^M). '/
     :     /'(Probable programmming error.)', STATUS )

         RETURN
      END IF

*  Ensure that the occurrence is valid.
      COCCUR = MAX( 1, OCCUR )

*  Determine the type of the value to read.
      CALL FTS1_QTYPE( HEADER( CARD ), TYPE, STATUS )

*  Obtain the value of the reference card, using the appropriate data
*  type.   Convert it to a token for output, and to a string where
*  necessary.
      IF ( TYPE .EQ. '_CHAR' ) THEN
         CALL FTS1_GKEYC( 1, HEADER( CARD ), 1, KEYWRD, COCCUR,
     :                    PRESNT, VALUE, COMENT, DCARD, STATUS )
         CALL MSG_SETC( 'VALUE', VALUE )
         SVALUE = VALUE

      ELSE IF ( TYPE .EQ. '_REAL' ) THEN
         CALL FTS1_GKEYR( 1, HEADER( CARD ), 1, KEYWRD, COCCUR,
     :                    PRESNT, RVALUE, COMENT, DCARD, STATUS )
         CALL MSG_SETR( 'VALUE', RVALUE )
         CALL CHR_RTOC( RVALUE, SVALUE, NC )

      ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
         CALL FTS1_GKEYI( 1, HEADER( CARD ), 1, KEYWRD, COCCUR,
     :                    PRESNT, IVALUE, COMENT, DCARD, STATUS )
         CALL MSG_SETI( 'VALUE', IVALUE )
         CALL CHR_ITOC( IVALUE, SVALUE, NC )

      ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
         CALL FTS1_GKEYD( 1, HEADER( CARD ), 1, KEYWRD, COCCUR,
     :                    PRESNT, DVALUE, COMENT, DCARD, STATUS )
         CALL MSG_SETD( 'VALUE', DVALUE )
         CALL CHR_DTOC( DVALUE, SVALUE, NC )

      ELSE IF ( TYPE .EQ. '_LOGICAL' ) THEN
         CALL FTS1_GKEYL( 1, HEADER( CARD ), 1, KEYWRD, COCCUR,
     :                    PRESNT, LVALUE, COMENT, DCARD, STATUS )
         CALL MSG_SETL( 'VALUE', LVALUE )
         CALL CHR_LTOC( LVALUE, SVALUE, NC )

      ELSE IF ( TYPE .EQ. 'COMMENT' ) THEN
         CALL MSG_SETC( 'VALUE', ' ' )

      END IF

*  Report the value.
      CALL MSG_OUTIF( MSG__QUIET, 'PRINT', '^VALUE', STATUS )

*  Report if something went wrong.
      IF ( .NOT. PRESNT ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'KEY', KEYWRD )
         IF ( COCCUR .EQ. 1 ) THEN
            CALL ERR_REP( 'FTS1_PRVAL_NOTPRESENT',
     :        'FTS1_PRVAL: Supplied keyword ^KEY is not present in '/
     :        /'the FITS header.', STATUS )
         ELSE
            CALL MSG_SETI( 'N', COCCUR )
            CALL ERR_REP( 'FTS1_PRVAL_FEWEROCC',
     :        'FTS1_PRVAL: There are fewer than ^N occurrences of '/
     :        /'the supplied keyword ^KEY in the FITS header.', STATUS )
         END IF
      END IF

      END
