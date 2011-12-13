      SUBROUTINE TASK_CNCAT ( NVALS, STRINGS, VALUE, STATUS )
*+
*  Name:
*     TASK_CNCAT

*  Purpose:
*     Concatenate an array of strings into an argument list

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL TASK_CNCAT ( NVALS, STRINGS, VALUE, STATUS )

*  Description:
*     Given an array of strings, concatenate them with separating single
*     spaces, having removed leading and trailing spaces. This generates
*     an argument list suitable for passing through the ADAM message
*     system.

*  Arguments:
*     NVALS=INTEGER (given)
*           number of strings in the array
*     STRINGS=CHARACTER*(*) (given)
*           the array of strings
*     VALUE=CHARACTER*(*) (returned)
*           the concatenated string
*     STATUS=INTEGER

*  Algorithm:
*     For each of the NVALS elements of STRINGS, use LIB$SKPC to skip
*     leading spaces and STR$TRIM to remove trailing spaces, then append
*     the result onto VALUE with a single trailing space.

*  Copyright:
*     Copyright (C) 1985, 1990, 1992-1993 Science & Engineering
*     Research Council. All Rights Reserved.

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
*     13-JUN-1985 (REVAD::BDK):
*        Original
*     01-MAR-1990: fix one-off error in FIRSTSPACE and initialise VALUE
*                  to spaces (REVAD::BDK)
*     04-OCT-1992 (RLVAD::AJC):
*        Use CHR_FANDL not STR$SKPC and STR$TRIM
*     24-AUG-1993 (RLVAD::AJC):
*        Remove istat - not used
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
      INTEGER NVALS                    ! the number of strings

      CHARACTER*(*) STRINGS(*)         ! the array of strings

*  Arguments Returned:
      CHARACTER*(*) VALUE              ! the concatenated line

*  Status:
      INTEGER STATUS

*    External references :

*  Local Variables:
      INTEGER FIRSTSPACE               ! first free space in VALUE

      INTEGER MAXLEN                   ! LEN(VALUE)

      INTEGER J                        ! loop counter

      INTEGER START                    ! position of start of a string

      INTEGER ENDSTR                   ! position of end of a string
*.

      IF ( STATUS .NE. SAI__OK ) RETURN

      FIRSTSPACE = 1
      MAXLEN = LEN ( VALUE )
      VALUE = ' '

      J = 0
      DO WHILE ( ( STATUS .EQ. SAI__OK ) .AND. ( J .LT. NVALS ) )

         J = J + 1
         CALL CHR_FANDL( STRINGS(J), START, ENDSTR )
         IF ( FIRSTSPACE + ENDSTR +1 .LE. MAXLEN ) THEN
            VALUE(FIRSTSPACE:) = STRINGS(J)(START:)
            FIRSTSPACE = FIRSTSPACE + ENDSTR + 1
         ELSE
            STATUS = TASK__STRFL
         ENDIF

      ENDDO

      END
