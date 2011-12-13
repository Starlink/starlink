      SUBROUTINE CHR_LDBLK( STRING )
*+
*  Name:
*     CHR_LDBLK

*  Purpose:
*     Remove any leading blanks from a string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHR_LDBLK( STRING )

*  Description:
*     Remove any leading blanks from the character string.
*     The remaining characters are moved to the left to eliminate the
*     resulting empty space, and the end of the string is filled with
*     blanks.

*  Arguments:
*     STRING = CHARACTER * ( * ) (Given and Returned)
*        The string from which the leading blanks are to be removed.

*  Algorithm:
*     Find the position of the first non-blank character.
*     If there are some blanks to be removed then
*        Shunt up the string to remove the leading blanks.
*        Copy extra blanks into the end of the string.
*     end if

*  Copyright:
*     Copyright (C) 1984, 1988, 1989 Science & Engineering Research Council.
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
*     ACD: A.C. Davenhall (ROE)
*     AJC: A.J. Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     20-JUN-1984 (ACD):
*        Original version.
*     30-JUN-1984 (ACD):
*        Modified to update a single string rather than generating a
*        new one. This was done to be consistent with the other
*        CHR_ routines.
*     16-NOV-1984 (ACD):
*        Name changed from CHR_LDBLNK to CHR_LDBLK to conform to SSE
*        convention.
*     3-OCT-1988 (AJC):
*        Improve documentation.
*     13-JUN-1989 (AJC):
*        Revise for speed. Use of the declared length of string as
*        CHR_LEN is expensive.
*        Also remove initial check for all spaces; it costs every
*        time and only if the string is blank and .GT. about 20 chars.
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given and Returned:
      CHARACTER STRING * ( * )

*  Local Variables:
      INTEGER CURORG             ! Original current position in string
      INTEGER CURSHF             ! Shifted current position in the string
      INTEGER LENCPY             ! Number of characters to be shifted
      INTEGER START              ! Position of first non-blank character
      INTEGER STOP               ! Declared length of string

*.

*  Find the first non-space.
      STOP = LEN( STRING )

      DO 10 START = 1, STOP
         IF ( STRING( START : START ) .NE. ' ' ) GO TO 20
 10   CONTINUE

      START = STOP + 1
 20   CONTINUE

*  START now points to the start of the string. Check if there are any
*  leading blanks to be removed.
      IF ( ( START .GT. 1 ) .AND. ( START .LE. STOP ) ) THEN

*     Shunt up the non-blank characters to the start of the string.
*     Do the whole string because it is just as expensive to find the
*     used length.
         LENCPY = STOP - START + 1

         DO 30 CURSHF = 1, LENCPY
            CURORG = CURSHF + START - 1
            STRING( CURSHF : CURSHF ) = STRING( CURORG : CURORG )
 30      CONTINUE

*     Remove the characters at the end of the string that are now
*     duplicated.
         STRING( LENCPY+1 : ) = ' '
      END IF

      END
