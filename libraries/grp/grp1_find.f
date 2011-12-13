      SUBROUTINE GRP1_FIND( UPPER, SIZE, START, ARRAY, TEXT, INDEX,
     :                      STATUS )
*+
*  Name:
*     GRP1_FIND

*  Purpose:
*     Find a name in a character array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP1_FIND( UPPER, SIZE, START, ARRAY, TEXT, INDEX, STATUS )

*  Description:
*     The array is searched (starying at START) for the given text, and
*     the index of the first occurrence returned. If the text is not
*     found an index of zero is returned. The search is case
*     insensitive if argument UPPER is supplied true.

*  Arguments:
*     UPPER = LOGICAL (Given)
*        If true, then strings comparisons are case insensitive.
*     SIZE = INTEGER (Given)
*        The size of the array specified by argument ARRAY.
*     START = INTEGER (Given)
*        The lowest index to be checked.
*     ARRAY( SIZE ) = CHARACTER * ( * ) (Given)
*        The array.
*     TEXT = CHARACTER * ( * ) (Given)
*        The text to be searched for.
*     INDEX = INTEGER (Returned)
*        The index at which the text was found within ARRAY. A value of
*        zero is returned if the text is not found.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-AUG-1992 (DSB):
*        Original version
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      LOGICAL UPPER
      INTEGER SIZE
      INTEGER START
      CHARACTER ARRAY( SIZE )*(*)
      CHARACTER TEXT*(*)

*  Arguments Returned:
      INTEGER INDEX

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_SIMLR
      LOGICAL CHR_SIMLR          ! True if two strings are the same,
                                 ! ignoring case differences.

*  Local Variables:
      INTEGER I                  ! Loop count.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the returned index to indicate that the given text was not
*  found.
      INDEX = 0

*  Loop through the array until the text is found.
      DO I = MAX( 1, START ), SIZE
         IF( INDEX .EQ. 0 ) THEN

*  If any characters remain in the array text, compare them with the
*  supplied text -- only ignore case if enabled.
            IF ( UPPER ) THEN
               IF( CHR_SIMLR( ARRAY( I ), TEXT ) ) INDEX = I

*  Otherwise perform case sensitive comparison.
            ELSE
               IF ( ARRAY( I ) .EQ. TEXT ) INDEX = I

            END IF

         END IF

      END DO

 999  CONTINUE

      END
