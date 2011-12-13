      SUBROUTINE STRING_SPLIT( STRING, CH, MAXST, SUBSTS, NSTR, STATUS )
*+
*  Name:
*     STRING_SPLIT

*  Purpose:
*     To split a string into substrings separated by a specified
*     character. The number of substrings returned will be one
*     more than the number of separators in the string. Null
*     substrings will be returned as spaces.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL STRING_SPLIT( STRING, CH, MAXST, SUBSTS, NSTR, STATUS )

*  Description:
*     The used length of the input string is found.
*     If it is zero, the number of substrings is set to 1 and the
*     first element of the substring array set to blank.
*     Otherwise the string is searched for occurrences of the
*     separator and intervening characters saved in consecutive
*     elements of the substring array. A check is made to ensure
*     that the substring array does not overflow.
*
*     There is currently no check for truncation of substrings.

*  Arguments:
*     STRING = CHARACTER*(*) (Given)
*        The string to be split
*     CH = CHARACTER*1 (Given)
*        The separating character
*     MAXST = INTEGER (Given)
*        The maximum number of strings (size of SUBSTS)
*     SUBSTS( MAXST ) = CHARACTER*(*) (Returned)
*        An array to receive the substrings
*     NSTR = INTEGER (Returned)
*        Number of strings found
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
*     AJC: A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-JUL-1991 (AJC):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      CHARACTER*(*) STRING
      CHARACTER*(1) CH
      INTEGER MAXST

*  Arguments Returned:
      CHARACTER*(*) SUBSTS( MAXST )
      INTEGER NSTR

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Used legth of string

*  Local Variables:
      INTEGER I                  ! STRING index
      INTEGER J                  ! substring index
      INTEGER N                  ! SUBSTS element
      INTEGER STRLEN             ! STRING length
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      I = 1
      N = 1

*  Find the used length of the string
      STRLEN = CHR_LEN( STRING )

*  If it is a blank string, set the first substring to blank
*  the following loop will not be executed
      IF ( STRLEN .EQ. 0 ) THEN
         SUBSTS(1) = ' '
      ENDIF

*  Now look for substrings
      DOWHILE ( I .LE. STRLEN )

*      If there is still room in substring array
         IF ( N .LE. MAXST ) THEN

*        Look for the separator
            J = INDEX( STRING( I:STRLEN ), CH )

*        If the separator is found
            IF ( J .NE. 0 ) THEN

*           If it is the first character put a blank substring
               IF ( J .EQ. 1 ) THEN
                  SUBSTS( N ) = ' '
*           Otherwise copy the substring to the substring array
               ELSE
                  SUBSTS( N ) = STRING( I:I+J-2 )
               ENDIF

*           and increment the pointers
               I = I + J
               N = N + 1

*        Otherwise the separator was not found -
*        put all of remainder into next SUBSTS
*        and force loop exit
            ELSE
               SUBSTS( N ) = STRING( I:STRLEN )
               I = STRLEN + 1

            ENDIF

*     MAXST exceeded - too many substrings for SUBSTS
         ELSE

*        Report the error
            STATUS = SAI__ERROR
            CALL EMS_REP( 'STR_SPLIT1',
     :      'STRING_SPLIT: Too many substrings', STATUS )
            CALL EMS_SETC( 'STRING', STRING )
            CALL EMS_REP( 'STR_SPLIT2',
     :      'STRING is: ^STRING', STATUS )

*        Force loop exit
            I = STRLEN + 1

         ENDIF

      ENDDO

      NSTR = N

      END
