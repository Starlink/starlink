      SUBROUTINE SST_RMCHR( CHARS, STR )
*+
*  Name:
*     SST_RMCHR

*  Purpose:
*     Remove specified characters from a string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SST_RMCHR( CHARS, STR )

*  Description:
*     The routine removes a specified set of characters from a string,
*     moving remaining characters to the left to eliminate the resulting
*     space(s). Extra spaces on the right are filled with space
*     characters ' '.

*  Arguments:
*     CHARS = CHARACTER * ( * ) (Given)
*        A string consisting of all the characters which are to be
*        removed.
*     STR = CHARACTER * ( * ) (Given and Returned)
*        String from which characters are to be removed.

*  Copyright:
*     Copyright (C) 1990 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or modify it under
*     the terms of the GNU General Public License as published by the Free Software
*     Foundation; either version 2 of the License, or (at your option) any later
*     version.
*
*     This program is distributed in the hope that it will be useful,but WITHOUT ANY
*     WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
*     PARTICULAR PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License along with
*     this program; if not, write to the Free Software Foundation, Inc., 59 Temple
*     Place,Suite 330, Boston, MA  02111-1307, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     6-SEP-1990 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER * ( * ) CHARS

*  Arguments Given and Returned:
      CHARACTER * ( * ) STR

*  Local Variables:
      INTEGER I                  ! Loop counter for input characters
      INTEGER J                  ! Number of output characters

*.

*  Initialise count of characters remaining.
      J = 0

*  Loop to test each character.
      DO 1 I = 1, LEN( STR )

*  If it is not to be removed, then copy it to its new location in the
*  string.
         IF ( INDEX( CHARS, STR( I : I ) ) .EQ. 0 ) THEN
            J = J + 1
            STR( J : J ) = STR( I : I )
         END IF
1     CONTINUE

*  Fill extra spaces on the right with spaces.
      IF ( J .LT. LEN( STR ) ) STR( J + 1 : ) = ' '

      END
* @(#)sst_rmchr.f   1.1   94/12/05 11:31:34   96/07/05 10:27:29
