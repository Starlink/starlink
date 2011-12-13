      SUBROUTINE CCD1_DLCAT( WORDS, NWORD, DELIM, LIST, STATUS )
*+
*  Name:
*     CCD1_DLCAT

*  Purpose:
*     Construct a delimiter-separated list of strings from an array.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL CCD1_DLCAT( WORDS, NWORD, DELIM, LIST, STATUS  )

*  Description:
*     This routine concatenates a list of strings in an array of
*     strings to form a list separated by the delimiter.  Trailing
*     blanks in the word (though not in the delimiter) are ignored.
*
*     If any of the words contains the delimiter, or is blank, then
*     STATUS is set and an error is reported.

*  Arguments:
*     WORDS( NWORD ) = CHARACTER * ( * ) (Given)
*        The array of strings to be concatenated.
*     NWORD = INTEGER (Given)
*        The number of elements in WORDS.
*     DELIM = CHARACTER * ( * ) (Given)
*        The delimiter to separate the words.
*     LIST = CHARACTER * ( * ) (Returned)
*        A delimiter separated list formed by joining
*        WORDS( 1 ) // DELIM // WORDS( 2 ) // DELIM // .. // WORDS( NWORD ).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils

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
*     13-APR-1999 (MBT):
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
      INTEGER NWORD
      CHARACTER * ( * ) WORDS( NWORD )
      CHARACTER * ( * ) DELIM

*  Arguments Returned:
      CHARACTER * ( * ) LIST

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IAT                ! Position in string
      INTEGER I                  ! Loop variable
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise returned variables.
      LIST = ' '

*  Initialise position in output string.
      IAT = 0

*  Loop over elements in list.
      DO 1 I = 1, NWORD
         IF ( INDEX( WORDS( I ), DELIM ) .GT. 0 ) THEN

*  Word contains the delimiter - set error status and exit.
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'WORD', WORDS( I ) )
            CALL MSG_SETC( 'DELIM', DELIM )
            CALL ERR_REP( 'CCD1_DLCAT_DELIM',
     :      'Word "^WORD" contains delimiter "^DELIM"', STATUS )
            GO TO 99
         ELSE IF ( WORDS( I ) .EQ. ' ' ) THEN

*  Word is blank - set error status and exit.
            STATUS = SAI__ERROR
            CALL ERR_REP( 'CCD1_DLCAT_BLANKWORD',
     :      'Blank word in list is not allowed', STATUS )
            GO TO 99
         ELSE

*  Word is suitable for inclusion in list.
            CALL CHR_APPND( WORDS( I ), LIST, IAT )
            LIST( IAT + 1: ) = DELIM
            IAT = IAT + LEN( DELIM )
         END IF
 1    CONTINUE

*  Remove final delimiter.
      IF ( IAT .GT. 0 ) THEN
         LIST( IAT - LEN( DELIM ) + 1: ) = ' '
      END IF

*  Error exit label.
 99   CONTINUE

      END
* $Id$
