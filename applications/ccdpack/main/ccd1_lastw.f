      SUBROUTINE CCD1_LASTW( STRING, INDEX, NOTFND, STATUS )
*+
*  Name:
*     CCD1_LASTW

*  Purpose:
*     Finds the start of the current word.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_LASTW( STRING, INDEX, NOTFND, STATUS )

*  Description:
*     Finds the start of the current word. Starts of words are
*     indicated by the presence of a space delimeter. INDEX is updated
*     on exit to the start of the word.

*  Arguments:
*     STRING = CHARACTER * ( * ) (Given)
*        The string to search for the beginning of the word.
*     INDEX = INTEGER (Given and Returned)
*        Position within string to start looking for start of word,
*        updated to the start of the word on exit.
*     NOTFND = LOGICAL (Returned)
*        Set true if start of word is not located before start of string
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council. All
*     Rights Reserved.

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
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     5-FEB-1992 (PDRAPER):
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

*  Arguments Given and Returned:
      INTEGER INDEX

*  Arguments Returned:
      LOGICAL NOTFND

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start looking for end of word.
      NOTFND = .FALSE.
 1    CONTINUE                   ! Start of 'DO WHILE' loop
      IF ( STRING( INDEX - 1 : INDEX - 1 ) .NE. ' '  ) THEN

*  No word delimeter, decrement index.
         INDEX = INDEX - 1
         IF ( INDEX .LT. 1 ) THEN

*  Moved past start of string. No start of word found.
            NOTFND = .TRUE.
            GO TO 99
         END IF
         GO TO 1
      END IF

99    END
* $Id$
