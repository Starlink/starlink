      SUBROUTINE IMG1_RKEY( NCARD, BLOCK, WHOLE, N, ITEM, INRANG,
     :                      STATUS )
*+
* Name:
*    IMG1_RKEY

*  Purpose:
*     Returns a keyword or record from a FITS block.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IMG1_RKEY( NCARD, BLOCK, WHOLE, N, ITEM, INRANG,
*                     STATUS )

*  Description:
*     This routine reads the Nth record from the FITS character array
*     and returns the keyword or record as requested. The record
*     organization of the FITS block is taken into account when
*     determining which record is really the Nth, this is necessary
*     when records may be blank, which indicates that they have been
*     deleted and should not be counted and also because the position of
*     the 'END' keyword is not known.
*
*     Hierarchical keywords are returned as period separated.

*  Arguments:
*     NCARD = INTEGER (Given)
*        Number of real entries in FITS block (declared size).
*     BLOCK( NCARD ) = CHARACTER * ( * ) (Given)
*        The FITS block.
*     WHOLE = LOGICAL (Given)
*        Whether the whole record is required or just the keyword.
*     N = INTEGER (Given)
*        The index of the required record.
*     ITEM = CHARACTER * ( * ) (Return)
*        The extracted record or keyword.
*     INRANG = LOGICAL (Returned)
*        If the requested record is not within the range of indices this
*        is set .FALSE. on exit.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     28-JUL-1994 (PDRAPER):
*        Original version.
*     31-AUG-1994 (PDRAPER):
*        Now returns a blank value if index is not in range.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'IMG_ERR'          ! IMG error codes

*  Arguments Given:
      INTEGER NCARD
      CHARACTER * ( * ) BLOCK( NCARD )
      LOGICAL WHOLE
      INTEGER N

*  Arguments Returned:
      CHARACTER * ( * ) ITEM
      LOGICAL INRANG

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Used length of string

*  Local Variables:
      INTEGER EQUALS             ! Position of equals sign
      INTEGER FIRST              ! First character to copy
      INTEGER I                  ! Loop variable
      INTEGER IAT                ! Current character position
      INTEGER INDX               ! Real index of record
      INTEGER LAST               ! Last character to copy
      INTEGER LENOUT             ! Real length of output string
      INTEGER LSTAT              ! Local status
      LOGICAL REPEAT             ! Next space might be repeat
      LOGICAL TRUNC              ! String is truncated
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  First check that the index is sensible.
      INRANG = .FALSE.
      IF ( N .LE. NCARD ) THEN

*  Look at each record and count only non-blank ones.
         INDX = 0
         DO 1 I = 1, NCARD
            IF ( BLOCK( I ) .NE. ' ' ) THEN
               IF ( BLOCK( I )( 1:3 ) .NE. 'END' ) THEN
                  INDX = INDX + 1
                  IF ( INDX .EQ. N ) THEN
                     INDX = I
                     INRANG = .TRUE.
                     GO TO 2
                  END IF
               ELSE

*  'END' card encountered. Stop at this point as the required record
*  obviously isn't in range.
                  GO TO 2
               END IF
            END IF
 1       CONTINUE
 2       CONTINUE
      END IF

*  If a record has been found and we require it complete then just copy
*  the record (checking for truncation).
      IF ( INRANG .AND. WHOLE ) THEN
         CALL CHR_COPY( BLOCK( INDX ), .FALSE., ITEM, LSTAT )
         IF ( LSTAT .NE. 0 ) THEN

*  Truncation has occurred.
            STATUS = IMG__TRUNC
            CALL MSG_SETC( 'REC', BLOCK( INDX ) )
            CALL MSG_SETI( 'LEN', LEN( ITEM ) )
            CALL ERR_REP( 'IMG1_RKEY_TRUNC', 'Failed to copy ' //
     :           'FITS record (^REC) into character string of ' //
     :           'length ^LEN (possible programming error).', STATUS )
         END IF
      ELSE IF ( INRANG ) THEN

*  Just need the keyword. This is either the first 8 characters or all
*  characters up to the first '=' sign. Check for '=' first as this is
*  the most flexible constraint.
         EQUALS = INDEX( BLOCK( INDX ), '=' )
         IF ( EQUALS .EQ. 0 ) THEN

*  Use first 8 characters.
            CALL CHR_COPY( BLOCK( INDX )( 1: 8 ), .FALSE., ITEM, LSTAT )
            IF ( LSTAT .NE. 0 ) THEN

*  Truncation has occurred.
               STATUS = IMG__TRUNC
               CALL MSG_SETC( 'REC', BLOCK( INDX )( 1: 8 ) )
               CALL MSG_SETI( 'LEN', LEN( ITEM ) )
               CALL ERR_REP( 'IMG1_RKEY_TRUNC', 'Failed to copy ' //
     :              'FITS keyword (^REC) into character string ' //
     :              'of length ^LEN (possible programming error).',
     :              STATUS )
            END IF
         ELSE

*  Use all characters up to equals. Need to remove leading blanks and
*  replace any inter-word spaces with periods.
            DO 3 I = 1, EQUALS
               IF ( BLOCK( INDX )( I: I ) .NE. ' ' ) THEN
                  FIRST = I
                  GO TO 4
               END IF
 3          CONTINUE
 4          CONTINUE

*  Find the length of the target string and initialize the insertion
*  position.
            LENOUT = LEN( ITEM )
            IAT = 0
            LAST = CHR_LEN( BLOCK( INDX )( :EQUALS - 1 ) )
            TRUNC = .FALSE.
            REPEAT = .FALSE.
            DO 5 I = FIRST, LAST
               IF ( BLOCK( INDX )( I: I ) .NE. ' ' ) THEN
                  IAT = IAT + 1
                  REPEAT = .FALSE.
                  IF ( IAT .LE. LENOUT ) THEN
                     ITEM( IAT: IAT ) = BLOCK( INDX )( I: I )
                  ELSE

*  Truncation problem.
                     TRUNC = .TRUE.
                  END IF
               ELSE

*  Blank character, must be an inter-word separator so replace it with a
*  period if this is the first and not a repeat.
                  IF ( .NOT. REPEAT ) THEN
                     IAT = IAT + 1
                     ITEM( IAT: IAT ) = '.'
                     REPEAT = .TRUE.
                  END IF
               END IF
 5          CONTINUE

*  Check that all went well. If not report the error.
            IF ( TRUNC ) THEN
               STATUS = IMG__TRUNC
               CALL MSG_SETC( 'REC', BLOCK( INDX )( 1: LAST ) )
               CALL MSG_SETI( 'LEN', LEN( ITEM ) )
               CALL ERR_REP( 'IMG1_RKEY_TRUNC', 'Failed to copy ' //
     :              'FITS keyword (^REC) into character string ' //
     :              'of length ^LEN (possible programming error).',
     :              STATUS )
            END IF
         END IF

      ELSE

*  Out of range, return a blank value.
         ITEM = ' '
      END IF
      END
* $Id$
