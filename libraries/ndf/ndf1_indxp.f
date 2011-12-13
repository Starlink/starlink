      INTEGER FUNCTION NDF1_INDXP( STR, CH )
*+
*  Name:
*     NDF1_INDXP

*  Purpose:
*     Find a character in a string, ignoring characters in parentheses.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = NDF1_INDXP( STR, CH )

*  Description:
*     The function returns the position of the first occurrence of the
*     character CH in the string STR, omitting any occurrences which
*     lie within parentheses '(...)'. Account is taken of nested
*     parentheses.

*  Arguments:
*     STR = CHARACTER * ( * ) (Given)
*        String to be searched.
*     CH = CHARACTER * ( 1 ) (Given)
*        Character to be found.

*  Returned Value:
*     NDF1_INDXP = INTEGER
*        The character position of the first un-parenthesised
*        occurrence of the character CH in the string STR. A value of
*        zero is returned if no such occurrence exists.

*  Algorithm:
*     -  Initialise.
*     -  Inspect each character in STR.
*     -  If the target character is found when not inside parentheses,
*     then return its position.
*     -  Count entries into each level of parenthesis.
*     -  Decrement the count when leaving each level of parenthesis.

*  Copyright:
*     Copyright (C) 1989 Science & Engineering Research Council.
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
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-OCT-1989 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER * ( * ) STR
      CHARACTER * ( 1 ) CH

*  Local Variables:
      INTEGER I                  ! Loop counter for characters
      INTEGER PAR                ! Depth of nested parentheses

*.

*  Initialise.
      PAR = 0
      NDF1_INDXP = 0

*  Inspect each character in STR.
      DO 1 I = 1, LEN( STR )

*  If the target character is found when not inside parentheses, then
*  return its position.
         IF ( ( STR( I : I ) .EQ. CH ) .AND. ( PAR .EQ. 0 ) ) THEN
            NDF1_INDXP = I
            GO TO 2

*  Count entries into each level of nested parenthesis.
         ELSE IF ( STR( I : I ) .EQ. '(' ) THEN
            PAR = PAR + 1

*  Decrement the count when leaving each level of parenthesis. Ignore
*  missing left parentheses.
         ELSE IF ( ( STR( I : I ) .EQ. ')' ) .AND. ( PAR .GT. 0 ) ) THEN
            PAR = PAR - 1
         END IF
1     CONTINUE
2     CONTINUE

      END
