      SUBROUTINE PARSECON_DECVAL ( ENTRY, VALUE, CLASS, STATUS )

*+
*  Name:
*     PARSECON_DECVAL

*  Purpose:
*     determine the data type of a value-string.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL PARSECON_DECVAL ( ENTRY, VALUE, CLASS, STATUS )

*  Description:
*     Analyses the type of a constant value as implied by its syntax,
*     and if necessary performs some pre-processing on it.

*  Arguments:
*     ENTRY=CHARACTER*(*) (given)
*        The value-string to be interpreted
*     VALUE=CHARACTER*(*) (returned)
*        If ENTRY has the syntax of a character string, a structure
*        name, or a logical value, then it is subjected to any
*        necessary preprocessing and returned in VALUE.
*     CLASS=INTEGER (returned)
*        A type-code determined from the syntax of ENTRY.
*        PARSE__CHAR - character string
*                      (first character is quote)
*        PARSE__NUMBER - integer or real number
*                      (first character is digit,+,- or .)
*        PARSE__STRUC - data structure name
*                      (first char is ",[ or letter)
*        PARSE__LOGTRUE - logical true
*                      (T, TRUE, Y or YES regardless of case)
*        PARSE__LOGFALSE - logical false
*                      (F, FALSE, N or NO regardless of case)
*        PARSE__NONE - none of the above
*     STATUS=INTEGER (returned)

*  Algorithm:
*     If the first character is a quote, then ENTRY is a quoted string.
*     Otherwise try for a logical constant. Failing that, if the first
*     character is a number, the class is NUMBER. The only remaining
*     valid possibility is a data structure/file name, check that it
*     starts with a letter, ~ or /. Otherwise return class = PARSE__NONE.

*  Copyright:
*     Copyright (C) 1984, 1985, 1986, 1990, 1992 Science & Engineering Research Council.
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
*     W.F.Lupton (RGO)
*     {enter_new_authors_here}

*  History:
*     31.08.1984:  VAX version (REVAD::BDK)
*     10.10.1984:  Optimised character matching (REVAD::BDK)
*     21.10.1984:  Change syntax of logicals to match SSE (REVAD::BDK)
*     27.02.1985:  add class PARSE__NONE (REVAD::BDK)
*     18.04.1985:  copy numbers into VALUE
*     20.08.1985:  remove case-sensitivity of logicals and names
*        (REVAD::BDK)
*     10.09.1986:  allow '.' to be class NUMBER (REVAD::BDK)
*     16.10.1990:  use CHR and make portable (RLVAD::AJC)
*     26.02.1992:  allow names to start ~, / or $ (RLVAD::AJC)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'PARSECON_ERR'
      INCLUDE 'PARSECON_PAR'


*  Arguments Given:
      CHARACTER*(*) ENTRY       ! The value-string to be interpreted


*  Arguments Returned:
      CHARACTER*(*) VALUE       ! If ENTRY has the syntax of a
                                ! character string, or a structure
                                ! name, then it is subjected to any
                                ! necessary preprocessing and
                                ! returned in VALUE.

      INTEGER CLASS             ! A type-code determined from the
                                ! syntax of ENTRY.


*  Status:
      INTEGER STATUS


*  Local Constants:
      CHARACTER*(*) QUOTE
      PARAMETER ( QUOTE = '''' )
      CHARACTER*(*) STRUCC
      PARAMETER ( STRUCC = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ~/$["' )
      CHARACTER*(*) NUMBER
      PARAMETER ( NUMBER = '+-.0123456789' )

*.


      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check syntax of the given character string
*  First for quoted string.
      IF ( ENTRY(1:1) .EQ. QUOTE ) THEN
*     Character string constant. Should remove superfluous
*     quotes from string.
         CLASS = PARSE__CHAR
         CALL STRING_STRIPQUOT ( ENTRY, VALUE, STATUS )

*  Else check for number
      ELSE IF ( INDEX( NUMBER, ENTRY(1:1) ) .NE. 0 ) THEN
          CLASS = PARSE__NUMBER
          VALUE = ENTRY

*  Else check for logical constants, or name, or other.
      ELSE
*      Force to uppercase
         VALUE = ENTRY
         CALL CHR_UCASE( VALUE )

*      Check for logical TRUE.
         IF ( ( VALUE .EQ. 'T' ) .OR. ( VALUE .EQ. 'TRUE' ) .OR.
     :     ( VALUE .EQ. 'Y' ) .OR. ( VALUE .EQ. 'YES' ) ) THEN

            CLASS = PARSE__LOGTRUE

*     Else check for logical FALSE.
         ELSE IF ( ( VALUE .EQ. 'F' ) .OR. ( VALUE .EQ. 'FALSE' ) .OR.
     :     ( VALUE .EQ. 'N' ) .OR. ( VALUE .EQ. 'NO' ) ) THEN

            CLASS = PARSE__LOGFALSE

*     Else check for name.
         ELSE IF ( INDEX( STRUCC, VALUE(1:1) ) .NE. 0 ) THEN
            CLASS = PARSE__STRUC

*     Else it's something else - possible '(' or ')'.
         ELSE
            CLASS = PARSE__NONE

         ENDIF

      ENDIF

      END
