      SUBROUTINE STRING_STRIPQUOT ( ENTRY, VALUE, STATUS )
*+
*  Name:
*     STRING_STRIPQUOT

*  Purpose:
*     Remove surrounding quotes and escaped quotes

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL STRING_STRIPQUOT ( ENTRY, VALUE, STATUS )

*  Description:
*     Produces the 'value' of a character string formatted following
*     FORTRAN 77 syntax - ie. It is contained within single quotes, but
*     the single quote character can be included as part of the string
*     by doubling it.
*     If the given string contains no quotes, then it is simply copied
*     to the output string.

*  Arguments:
*     ENTRY=CHARACTER*(*) (given)
*           The value-string to be interpreted
*     VALUE=CHARACTER*(*) (returned)
*           The processed string
*     STATUS=INTEGER (returned)

*  Algorithm:
*     The quotes surrounding the input string are found. The characters
*     between them are copied to the output string, pairs of quotes
*     being contracted to single quotes.

*  Copyright:
*     Copyright (C) 1984 Science & Engineering Research Council.
*     Copyright (C) 1996 Central Laboratory of the Research Councils.
*     All Rights Reserved.

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
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     19-SEP-1984 (REVAD::BDK):
*        Original
*     21-NOV-1996: Change CHAR(39) in PARAMETER statement to ''''
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      CHARACTER*(*) ENTRY       ! The value-string to be interpreted

*  Arguments Returned:
      CHARACTER*(*) VALUE       ! The processed character string

*  Status:
      INTEGER STATUS

*    External references :
      INTEGER STRING_IANYR
      EXTERNAL STRING_IANYR

*  Local Constants:
      CHARACTER*(*) QUOTE
      PARAMETER ( QUOTE = '''' )

*  Local Variables:
      INTEGER FIRSTQ                     ! position of first quote in
                                         ! ENTRY

      INTEGER LASTQ                      ! position of last quote in
                                         ! ENTRY

      INTEGER I
      INTEGER J

*.

      IF ( STATUS .NE. SAI__OK ) RETURN

*
*   Find the surrounding quotes
*
      FIRSTQ = INDEX ( ENTRY, QUOTE )
      LASTQ = STRING_IANYR ( ENTRY, QUOTE )
*
*   Copy the characters handling 'escaped' quotes
*
      IF ( FIRSTQ .EQ. 0 ) THEN
*
*      No quotes in string
*
         VALUE = ENTRY

      ELSE IF ( LASTQ .LE. FIRSTQ+1 ) THEN

         VALUE = ' '

      ELSE

         VALUE = ' '
         J = FIRSTQ + 1
         I = 1

         DO WHILE ( ( J .LT. LASTQ ) .AND. ( I .LE. LEN(VALUE) ) )

            VALUE(I:I) = ENTRY(J:J)
            I = I + 1
            IF ( ENTRY(J:J+1) .EQ. QUOTE // QUOTE ) THEN
               J = J + 2
            ELSE
               J = J + 1
            ENDIF

         ENDDO

      ENDIF

      END
