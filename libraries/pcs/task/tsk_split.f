      SUBROUTINE TASK_SPLIT ( VALUE, MAXVALS, NVALS, STRINGS, STATUS )
*+
*  Name:
*     TASK_SPLIT

*  Purpose:
*     Split an argument list into an array of strings

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL TASK_SPLIT ( VALUE, MAXVALS, NVALS, STRINGS, STATUS )

*  Description:
*     Given an argument list consisting of space-separated arguments,
*     split it into an array of strings. Account for arrays by not regarding
*     spaces within [] brackets as being separators.

*  Arguments:
*     VALUE=CHARACTER*(*) (given)
*           the argument list
*     MAXVALS=INTEGER (given)
*           the maximum number of strings that can be returned
*     NVALS=INTEGER (returned)
*           the actual number of strings in the argument list
*     STRINGS=CHARACTER*(*) (returned)
*           the array of strings
*     STATUS=INTEGER

*  Algorithm:
*     Replace [] with () and use STRING_ARRCHAR to split the string into
*     space-separated sub-strings (it regards the parentheses as tokens in
*     their own right in these circumstances). Re-concatenate as necessary
*     to preserve array specifications.

*  Copyright:
*     Copyright (C) 1989 Science & Engineering Research Council. All
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
*     W.F.Lupton (AAOEPP::WFL)
*     {enter_new_authors_here}

*  History:
*     01-MAY-1989 (AAOEPP::WFL):
*        Original
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'TASK_ERR'

*  Arguments Given:
      CHARACTER*(*) VALUE              ! argument list

      INTEGER MAXVALS                  ! max num of strings that can be returned

*  Arguments Returned:
      INTEGER NVALS                    ! actual num of strings in the arg list

      CHARACTER*(*) STRINGS(1:*)       ! array of strings

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER I                        ! loop counter

      CHARACTER*200 COPY               ! copy of VALUE with [] -> ()

      CHARACTER*200 TEMP               ! temporary string

      INTEGER COUNT                    ! number of sub-strings

      CHARACTER CARRAY(20)*40          ! sub-strings

      INTEGER CLENGTHS(20)             ! lengths of sub-strings

      INTEGER PTR                      ! next char posn to use in TEMP

      INTEGER LEVEL                    ! parenthesis level

*.
      IF ( STATUS .NE. SAI__OK ) RETURN

      COPY = VALUE
      DO I = 1, LEN ( COPY )
         IF ( COPY(I:I) .EQ. '[' ) THEN
            COPY(I:I) = '('
         ELSE IF ( COPY(I:I) .EQ. ']' ) THEN
            COPY(I:I) = ')'
         ENDIF
      ENDDO
      CALL STRING_ARRCHAR ( COPY, 20, COUNT, CARRAY, CLENGTHS, STATUS )

      I = 0
      NVALS = 0
      DO WHILE ( I .LT. COUNT )
         I = I + 1
         TEMP = CARRAY(I)
         IF ( TEMP .EQ. '(' ) THEN
            PTR = 1
            TEMP(PTR:) = '[ '
            PTR = PTR + 2
            LEVEL = 1
            DO WHILE ( I .LT. COUNT .AND. LEVEL .GT. 0 )
               I = I + 1
               IF ( CARRAY(I) .EQ. '(' ) THEN
                  TEMP(PTR:) = '[ '
                  PTR = PTR + 2
                  LEVEL = LEVEL + 1
               ELSE IF ( CARRAY(I) .EQ. ')' ) THEN
                  TEMP(PTR:) = '] '
                  PTR = PTR + 2
                  LEVEL = LEVEL - 1
               ELSE
                  TEMP(PTR:) = CARRAY(I)
                  PTR = PTR + CLENGTHS(I) + 1
               ENDIF
            ENDDO
         ENDIF
         NVALS = NVALS + 1
         IF ( NVALS .LE. MAXVALS ) THEN
            STRINGS(NVALS) = TEMP
         ENDIF
      ENDDO

      END
