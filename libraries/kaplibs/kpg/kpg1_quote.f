      SUBROUTINE KPG1_QUOTE( IN, OUT, STATUS )
*+
*  Name:
*     KPG1_QUOTE

*  Purpose:
*     Quote a supplied string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_QUOTE( IN, OUT, STATUS )

*  Description:
*     This routine returns a new string holding a copy of the supplied
*     string within single quotes. Any single quotes within the supplied
*     string are escaped using a backslash.

*  Arguments:
*     IN = CHARACTER*(*) (Given)
*        The input string. Any trailing spaces are included in the
*        returned quoted string.
*     OUT = CHARACTER*(*) (Returned)
*        The quoted string.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  An error is reported if the output string is not large enough to
*     hold the quoted string.

*  Copyright:
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
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     9-OCT-2012 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Local Constants:
      CHARACTER BCKSLH*1         ! A single backslash
*  Some compilers need '\\' to get '\', which isn't a problem as Fortran
*  will truncate the string '\\' to '\' on the occasions when that isn't
*  needed.
      PARAMETER( BCKSLH = '\\' )

*  Arguments Given:
      CHARACTER IN*(*)

*  Arguments Returned:
      CHARACTER OUT*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER C*1              ! Current character
      INTEGER I                  ! Index into input string
      INTEGER INLEN              ! Length of input string
      INTEGER J                  ! Index into output string
      INTEGER OUTLEN             ! Length of output string
      LOGICAL OK                 ! Output string is large enough?
*.

*  Initialise.
      OUT = IN

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the lengths of the strings.
      INLEN = LEN( IN )
      OUTLEN = LEN( OUT )

*  Initialise the output string to hold the starting quote.
      OUT( 1 : 1 ) = ''''

*  Loop round all input characters, keeping the index of the next output
*  characters to be written in "J".
      OK = .TRUE.
      J = 2
      DO I = 1, INLEN
         C = IN( I : I )

*  If the input character is a single quote, append a backslash to the
*  output string so long as there is room for two more characters (the
*  single quote being escaped and the terminating single quote).
         IF( C .EQ. '''' ) THEN
            IF( J .LE. OUTLEN - 2 ) THEN
               OUT( J : J ) = BCKSLH
               J = J + 1
            ELSE
               OK = .FALSE.
               GO TO 10
            END IF
         END IF

*  Copy the next input character to the output.
         OUT( J : J ) = C

*  Move on to the next output character.
         J = J + 1

*  If the output string is too small, abort.
         IF( J .EQ. OUTLEN .AND. I .LT. INLEN ) THEN
            OK = .FALSE.
            GO TO 10
         END IF

      END DO

 10   CONTINUE

*  Add the terminating quote.
      OUT( J : J ) = ''''

*  Report an error if the output string is too small.
      IF( .NOT. OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'KPG1_QUOTE: Output string truncated.',
     :                 STATUS )
      END IF

      END
