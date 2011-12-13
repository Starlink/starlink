      SUBROUTINE CCD1_INSER( STR1, STR2, START, RESULT, STATUS )
*+
*  Name:
*     CCD1_INSER

*  Purpose:
*     To insert a string into another string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_INSER( STR1, STR2, START, RESULT, STATUS )

*  Description:
*     The routine inserts STR1 into STR2 starting at the position START.
*     After the elements of STR1 have been the copied the rest of STR2
*     is copied. The result is returned in the RESULT string. Any excess
*     characters are not copied.

*  Arguments:
*     STR1 = CHARACTER * ( * ) (Given)
*        The string to be inserted.
*     STR2 = CHARACTER * ( * ) (Given)
*        The string into which STR1 is to be inserted, note that the
*        result is returned in RESULT and this variable is left
*        unmodified.
*     START = INTEGER (Given)
*        The index position of string STR2 at which to start inserting
*        STR1.
*     RESULT = CHARACTER * ( * ) (Returned)
*        The string containing the result of inserting STR1 into STR2.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council. All
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
*     21-JUN-1991 (PDRAPER):
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
      CHARACTER * ( * ) STR1
      CHARACTER * ( * ) STR2
      INTEGER START

*  Arguments Returned:
      CHARACTER * ( * ) RESULT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER LENRES             ! Length of output string
      INTEGER IAT                ! Current position in result buffer

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check for start.
      IF ( START .GT. 0 ) THEN

*  Find the lengths of the input strings
         LENRES = LEN( RESULT )

*  Copy STR2 ( 1: START-1) to result string
         IAT = 0
         CALL CCD1_APPND( STR2( : START - 1 ), RESULT, IAT ,STATUS )

*  If IAT is less than LENRES then have room to continue.
         IF ( IAT .LT. LENRES ) THEN
            CALL CCD1_APPND( STR1, RESULT, IAT, STATUS )

*  If IAT is less than LENRES then have room to continue. Final part of
*  STR2.
            IF ( IAT .LT. LENRES ) THEN
               CALL CCD1_APPND( STR2( START : ) , RESULT, IAT,STATUS )
            END IF
         END IF
      END IF

      END
* $Id$
