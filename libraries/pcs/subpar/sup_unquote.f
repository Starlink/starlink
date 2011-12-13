      SUBROUTINE SUBPAR_UNQUOTE ( ENTRY, VALUE, STATUS )
*+
*  Name:
*     SUBPAR_UNQUOTE

*  Purpose:
*     Remove surrounding quotes and escaped quotes.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL SUBPAR_UNQUOTE ( ENTRY, VALUE, STATUS )

*  Description:
*     Produces the 'value' of a character string formatted following
*     ADAM syntax - ie. It is contained within quotes, and quotes can be
*     included as part of the string by doubling them. Single and double
*     quotes are equivalent.
*     If the given string does not start and end with a quote, it is
*     simply copied to the output string.

*  Arguments:
*     ENTRY=CHARACTER*(*) (given)
*        The value-string to be interpreted
*     VALUE=CHARACTER*(*) (returned)
*        The processed string
*     STATUS=INTEGER (returned)

*  Copyright:
*     Copyright (C) 1997 Central Laboratory of the Research Councils.
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
*     A.J.Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     28-OCT-1997 (AJC):
*        Original
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'

      CHARACTER*(*) ENTRY       ! The value-string to be interpreted

      CHARACTER*(*) VALUE       ! The processed character string

      INTEGER STATUS

      INTEGER CHR_LEN
      EXTERNAL CHR_LEN

      INTEGER ENTLEN            ! Used length of given string
      INTEGER I
      INTEGER J
      LOGICAL QUOTE             ! Quote encountered?
*.


      IF ( STATUS .NE. SAI__OK ) RETURN

      ENTLEN = CHR_LEN( ENTRY )

* Check if string starts and ends with quote
      IF ( ( ( ENTRY(1:1) .EQ. '''' ) .OR.
     :       ( ENTRY(1:1) .EQ. '"' ) ) .AND.
     :     ( ( ENTRY(ENTLEN:ENTLEN) .EQ. '''' ) .OR.
     :       ( ENTRY(ENTLEN:ENTLEN) .EQ. '"' ) ) ) THEN
* It is a quoted string
         J = 1
         VALUE = ' '
         QUOTE = .FALSE.

         DO I = 2, ENTLEN
            IF ( ( ENTRY(I:I) .EQ. '''' ) .OR.
     :           ( ENTRY(I:I) .EQ. '"' ) ) THEN
               IF ( QUOTE ) THEN
                  QUOTE = .FALSE.
                  VALUE(J:J) = ENTRY(I:I)
                  J = J + 1
               ELSE
                  QUOTE = .TRUE.
               ENDIF
            ELSE
               VALUE(J:J) = ENTRY(I:I)
               J = J + 1
            ENDIF
            IF ( J .GT. LEN( VALUE ) ) GOTO 99
         ENDDO

      ELSE
*  Not a quoted string
         VALUE = ENTRY

      ENDIF

99    CONTINUE

      END
