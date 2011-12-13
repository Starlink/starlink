      SUBROUTINE CCD1_FTGET( NCARD, IPFITS, SCARD, NAME, VALUE, ICARD,
     :                       STATUS )
*+
*  Name:
*     CCD1_FTGET

*  Purpose:
*     Get FITS value as a character string.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL CCD1_FTGET( NCARD, IPFITS, SCARD, NAME, VALUE, ICARD, STATUS )

*  Description:
*     This routine attempts to retrieve from a buffer containing a
*     mapped FITS extension the value of a header card with a given
*     keyword.  If no matching header card is found then an error
*     message is output and STATUS is set.
*
*     The value returned is a string representation of the value
*     requested.

*  Arguments:
*     NCARD = INTEGER (Given)
*        The number of card images in the buffer.
*     IPFITS = INTEGER (Given)
*        Pointer to a buffer containing the header card images.
*     SCARD = INTEGER (Given)
*        The number of the card from where the search will begin.  This
*        is needed because the headers make contain a dummy header
*        prior to an extension.
*     NAME = CHARACTER * ( * ) (Given)
*        The name of the keyword whose value is required.  This may be
*        a compound name to handle hierarchical keywords, and it has
*        the form keyword1.keyword2.keyword3 etc.  The maximum number of
*        keywords per FITS card is 20.  Comparisons are performed in
*        uppercase and blanks are removed.  Each keyword must be no
*        longer than 8 characters.
*     VALUE = CHARACTER * ( * ) (Returned)
*        The value of the keyword.
*     CARD = INTEGER (Returned)
*        The number of the card containing the named keyword.
*        If the card could not be found this is set to zero.
*     STATUS = INTEGER (Given)
*        Global status value.

*  Notes:
*     This routine is passed a pointer to the FITS buffer rather than
*     the pointer itself.  This is so that the calling routine does not
*     need to worry about the rather ugly business of passing character
*     lengths by value to the routine (this routine does that instead).

*  Copyright:
*     Copyright (C) 2000 Central Laboratory of the Research Councils

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
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     6-SEP-2000 (MBT):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER NCARD
      INTEGER SCARD
      INTEGER IPFITS
      CHARACTER * ( * ) NAME

*  Arguments Returned:
      CHARACTER * ( * ) VALUE
      INTEGER ICARD

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Length of string

*  Local Variables:
      CHARACTER * ( 80 ) CVAL     ! Character value of selected FITS card
      INTEGER NCHAR               ! Number of characters in conversion
      LOGICAL LVAL                ! Logical value of selected FITS card
      LOGICAL THERE               ! Whether requested item is present
      REAL RVAL                   ! Real value of selected FITS card

*  Local Constants:
      INTEGER LENGTH
      PARAMETER ( LENGTH = 80 )

*.

*  Set default return value.
      VALUE = ' '
      ICARD = 0

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If there are no mapped cards, we will not find an answer.
      IF ( NCARD .LE. 0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'CCD1_FTGET_NOFITS',
     :                 '  No FITS extension found', STATUS )
         GO TO 99
      END IF

*  Defer delivery of error messages.
      CALL ERR_MARK

*  In the following calls to FTS1_GKEY<T> look out for fortran magic
*  getting the FITS character array lengths passed.

*  Attempt to get numerical value for named keyword.
      CALL FTS1_GKEYR( NCARD, %VAL( CNF_PVAL( IPFITS ) ),
     :                 1, NAME, THERE, RVAL,
     :                 ICARD, STATUS, %VAL( CNF_CVAL( LENGTH ) ) )
      IF ( STATUS .EQ. SAI__OK .AND. THERE ) THEN
         CALL CHR_RTOC( RVAL, VALUE, NCHAR )
         GO TO 1
      ELSE IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_ANNUL( STATUS )
      END IF

*  Attempt to get logical value for named keyword.
      CALL FTS1_GKEYL( NCARD, %VAL( CNF_PVAL( IPFITS ) ),
     :                 1, NAME, THERE, LVAL,
     :                 ICARD, STATUS, %VAL( CNF_CVAL( LENGTH ) ) )
      IF ( STATUS .EQ. SAI__OK .AND. THERE ) THEN
         CALL CHR_LTOC( LVAL, VALUE, NCHAR )
         GO TO 1
      ELSE IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_ANNUL( STATUS )
      END IF

*  Attempt to get character value for named keyword.
      CALL FTS1_GKEYC( NCARD, %VAL( CNF_PVAL( IPFITS ) ),
     :                 1, NAME, THERE, CVAL,
     :                 ICARD, STATUS, %VAL( CNF_CVAL( LENGTH ) ) )
      IF ( STATUS .EQ. SAI__OK .AND. THERE ) THEN
         VALUE = '''' // CVAL
         VALUE( CHR_LEN( VALUE ) + 1: ) = ''''
         GO TO 1
      ELSE IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_ANNUL( STATUS )
      END IF

*  No conversion could be made.
      STATUS = SAI__ERROR
      CALL MSG_SETC( 'HEAD', NAME )
      CALL ERR_REP( 'CCD1_GTVAL',
     :'  Failed to find value for FITS header ^HEAD', STATUS )

*  Release error context.
 1    CONTINUE
      CALL ERR_RLSE

*  Tidy up and exit.
 99   CONTINUE

      END
* $Id$
