      SUBROUTINE FTS1_EVKEY( STRING, KEYWRD, LENGTH, OCCUR, STATUS )
*+
*  Name:
*     FTS1_EVKEY

*  Purpose:
*     Extracts a keyword and occurrence, and validates the keyword.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FTS1_EVKEY( STRING, KEYWRD, LENGTH, OCCUR, STATUS )

*  Description:
*     This routine serves FTS1_RFMOD.  It takes a string containing a
*     keyword and an optional occurrence in brackets, and extracts the
*     keyword and any occurrence.  It validates the keyword or the
*     hierarchical keywords.  It also returns the length of the keyword.

*  Arguments:
*     STRING = CHARACTER * ( * ) (Given)
*        The string containing the keyword or hierarchical keyword in
*        the form keyword1.keyword2.keyword3 etc.  There may be a
*        trailing [number] string which defines the occcurence of the
*        keyword.
*     KEYWRD = CHARACTER * ( * ) (Returned)
*        The extracted keyword in uppercase.  The supplied length is
*        recommended to be at least 48 to allow for six full-length
*        keywords.
*     LENGTH = INTEGER (Returned)
*        The length in characters of the keyword, so the keyword is
*        KEYWRD( :LENGTH ).
*     OCCUR = INTEGER (Returned)
*        The occurrence of the keyword to use.  If none is supplied in
*        the STRING or the value is not a positive integer, OCCUR is
*        assigned the value 1.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  [optional_subroutine_items]...
*  Copyright:
*     Copyright (C) 1996 Central Laboratory of the Research Councils.
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
*     {enter_new_authors_here}

*  History:
*     1996 November 5 (MJC):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) STRING

*  Arguments Returned:
      CHARACTER * ( * ) KEYWRD
      INTEGER LENGTH
      INTEGER OCCUR

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Constants:
      INTEGER KEYLN              ! Maximum number of characters in a
                                 ! FITS keyword
      PARAMETER ( KEYLN = 8 )

*  Local Variables:
      INTEGER CDELIM             ! Character pointer to a delimiter
      LOGICAL CMPKEY             ! Compound keyword?
      INTEGER FSPOS              ! Column containing fullstop in
                                 ! hierarchical keyword
      INTEGER HKPOS              ! Start column of an hierarchical
                                 ! keyword
      INTEGER LBPOS              ! Column of keyword left bracket
      INTEGER LOCC               ! Length of the occurrence string
      INTEGER LSTAT              ! Local status
      LOGICAL VALHKE             ! Hierarchical keyword element is
                                 ! valid?
      LOGICAL VALKEY             ! Keyword is valid?

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      LSTAT = SAI__OK

*  Extract the keyword.
*  ====================

*  Initialise the occcurence.
      OCCUR = 1

*  Obtain the length of the string.
      CALL CHR_LDBLK( STRING )
      LENGTH = CHR_LEN( STRING )

*  Search for a left bracket which, if present, would enclose the
*  occurrence of the keyword.
      IF ( LENGTH .GT. 0 ) THEN
         LBPOS = INDEX( STRING( 1:LENGTH ), '[' )
      ELSE
         LBPOS = 0
      END IF

*  Test for a left bracket.  Reset the length of the keyword such that
*  the bracketed string is not part of it.
      IF ( LBPOS .NE. 0 ) LENGTH = LBPOS - 1

*  Extract the position keyword, and convert it to uppercase.
      IF ( LENGTH .GT. 0 ) THEN
         KEYWRD = STRING( 1:LENGTH )
         CALL CHR_UCASE( KEYWRD )

*  Validate the positional keyword.
*  ================================

*  Determine whether or not the keyword is compound.
         CMPKEY = INDEX( KEYWRD( :LENGTH ), '.' ) .NE. 0

         IF ( CMPKEY ) THEN

*  Initialise the valid flag and the column pointers used to isolate
*  each hierarchical keyword for validation.
            VALKEY = .TRUE.
            HKPOS = 1
            FSPOS = HKPOS

   10       CONTINUE      !  Start of 'DO WHILE' loop
            IF ( FSPOS .LT. LENGTH ) THEN

*  Locate the next fullstop in the compound keyword.
               CALL CHR_TOCHR( '.', KEYWRD, .TRUE., FSPOS )

*  Keywords are limited to eight characters.  Report an error but
*  continue.
               IF ( FSPOS - HKPOS .GT. KEYLN ) THEN
                  LSTAT = SAI__ERROR
                  CALL MSG_SETC( 'KEY', STRING( HKPOS:FSPOS - 1 ) )
                  CALL ERR_REP( 'FTS1_EVKEY_KEYLEN',
     :              'The keyword ^KEY is longer than the eight '/
     :              /'characters permitted within FITS.', LSTAT )
               END IF

*  Validate the keyword.  Update the overall validity of the keyword.
*  the translation table.
               CALL FTS1_ISKEY( KEYWRD( HKPOS:FSPOS - 1 ), VALHKE,
     :                          STATUS )
               VALKEY = VALKEY .AND. VALHKE

*  Move the columns to the start of the next element of the compound
*  name.
               HKPOS = FSPOS + 1
               FSPOS = HKPOS

*  There are more hierarchical keywords to validate so return to the
*  start of the 'DO WHILE' loop, unless the hierarchical keyword is
*  already invalid.
               IF ( HKPOS .LT. KEYLN .AND. VALKEY ) GO TO 10
            END IF
         ELSE

*  Keywords are limited to eight characters.
            IF ( LENGTH .GT. KEYLN ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETC( 'KEY', STRING( 1:LENGTH ) )
               CALL ERR_REP( 'FTS1_EVKEY_KEYHLEN',
     :           'The hierarchical keyword element ^KEY is longer '/
     :           /'than the eight characters permitted within FITS.',
     :           STATUS )
               GOTO 999
            END IF

*  Validate the simple keyword.
            CALL FTS1_ISKEY( KEYWRD, VALKEY, STATUS )
         END IF

*  Report the error giving full context.
         IF ( .NOT. VALKEY ) THEN
            STATUS = SAI__ERROR
            IF ( CMPKEY ) THEN
               CALL MSG_SETC( 'K', 'hierarchical keyword' )
            ELSE
               CALL MSG_SETC( 'K', 'keyword' )
            END IF
            CALL MSG_SETC( 'KEY', KEYWRD )
            CALL ERR_REP( 'FTS1_EVKEY_INVKEY',
     :        'The ^K ^KEY includes characters other than '/
     :        /'numbers, uppercase letters, hyphen, and underscore '/
     :        /'thereby violating the FITS standard.', STATUS )
            GOTO 999

*  Reinstate the bad global status, if keyword length was the only
*  error.
         ELSE IF ( LSTAT .NE. SAI__OK ) THEN
            STATUS = LSTAT
         END IF

*  Although the keyword is blank, its length is still the full keyword
*  length.  Return the blank keyword.
      ELSE
         KEYWRD = ' '
         LENGTH = KEYLN

      END IF

*  Determine the occurrence of the positional keyword.
*  ===================================================

*  Identify the occurrence of the keyword, first by searching for a
*  right bracket.
      IF ( LBPOS .NE. 0 ) THEN

*  Look for the right-hand parenthesis searching forwards from the
*  left-hand parenthesis.
         CDELIM = LBPOS + 1
         CALL CHR_TOCHR( ']', STRING, .TRUE., CDELIM )

*  It does not matter if the right-hand bracket is omitted as the
*  character pointer is one character beyond the second keyword.

*  Find the number of characters in the occurrence.
         LOCC = CDELIM - LBPOS - 1

*  Test whether or not the occurrence string is null.
         IF ( LOCC .GT. 0 ) THEN

*  Convert the string between the brackets to an integer.
            CALL ERR_MARK
            CALL CHR_CTOI( STRING( LBPOS + 1:CDELIM - 1 ), OCCUR,
     :                     STATUS )

*  Treat a bad status as the first occurrence, and annul any conversion
*  error within a new error context.
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERR_ANNUL( STATUS )
               OCCUR = 1
            END IF
            OCCUR = MAX( OCCUR, 1 )
            CALL ERR_RLSE
         END IF
      END IF

  999 CONTINUE

      END
