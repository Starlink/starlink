      SUBROUTINE SST_CLEAN( STR )
*+
*  Name:
*     SST_CLEAN

*  Purpose:
*     Convert Fortran to upper case, removing multiple spaces.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SST_CLEAN( STR )

*  Description:
*     The routine "cleans" a Fortran 77 code line by converting all
*     unquoted lower case characters to upper case and changing all
*     unquoted multiple spaces to single spaces.

*  Arguments:
*     STR = CHARACTER * ( * ) (Given and Returned)
*        Character string containing code line.

*  Algorithm:
*     -  Initialise.
*     -  Loop to process each character.
*     -  Convert unquoted lowercase characters to upper case.
*     -  If unquoted spaces are encountered, only retain the first one,
*     setting a "multiple space" flag to prevent subsequent ones being
*     used.
*     -  Transfer other characters to their new position in the string.
*     -  Note when quote characters are encountered and toggle a quote
*     flag.
*     -  Reset the "multiple space" flag after a character which is not
*     an unquoted space.
*     -  Eliminate any characters that remain at the end of the
*     processed string.

*  Copyright:
*     Copyright (C) 1989, 1990 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or modify it under
*     the terms of the GNU General Public License as published by the Free Software
*     Foundation; either version 2 of the License, or (at your option) any later
*     version.
*
*     This program is distributed in the hope that it will be useful,but WITHOUT ANY
*     WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
*     PARTICULAR PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License along with
*     this program; if not, write to the Free Software Foundation, Inc., 59 Temple
*     Place,Suite 330, Boston, MA  02111-1307, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     4-AUG-1989 (RFWS):
*        Original version.
*     28-FEB-1990 (RFWS):
*        Renamed from CLEAN to SST_CLEAN.
*     8-AUG-1990 (RFWS):
*        Changed to use a lookup table for case conversion.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given and Returned:
      CHARACTER * ( * ) STR

*  Local Variables:
      CHARACTER * ( 1 ) CH       ! Current character
      CHARACTER * ( 26 ) UCASE   ! Upper case alphabet
      INTEGER I                  ! Loop counter for characters
      INTEGER IC                 ! Alphabet position of character
      INTEGER IOUT               ! Position of output character
      LOGICAL QUOTE              ! Whether current character is quoted
      LOGICAL SPACE              ! Unquoted space output last?

*  Local Data:
      DATA UCASE /'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/

*.

*  Initialise.
      IOUT = 0
      QUOTE = .FALSE.
      SPACE = .FALSE.

*  Loop to process each input character.
      DO 1 I = 1, LEN( STR )
         CH = STR( I : I )

*  Convert unquoted lower case characters to upper case using a lookup
*  table.
         IF ( .NOT. QUOTE ) THEN
            IC = INDEX( 'abcdefghijklmnopqrstuvwxyz', CH )
            IF ( IC .NE. 0 ) THEN
               CH = UCASE( IC : IC )
            END IF
         END IF

*  Only keep the first space in an series of unquoted spaces.
         IF ( ( CH .EQ. ' ' ) .AND. ( .NOT. QUOTE ) ) THEN
            IF ( .NOT. SPACE ) THEN
               IOUT = IOUT + 1
               STR( IOUT : IOUT ) = ' '
            ENDIF

*  Note that such a space has been encountered to prevent any more
*  being retained until another character which is not an unquoted
*  space is found.
            SPACE = .TRUE.

*  All other characters are retained, moving forward in the string if
*  necessary because of eliminated spaces.
         ELSE
            IOUT = IOUT + 1
            STR( IOUT : IOUT ) = CH

*  Note if the current character position is quoted.
            IF ( CH .EQ. '''' ) QUOTE = .NOT. QUOTE

*  Note the last character was not an unquoted space.
            SPACE = .FALSE.
         END IF
1     CONTINUE

*  Eliminate any characters remaining at the end of the string.
      IF ( IOUT .LT. LEN( STR ) ) THEN
         STR( IOUT + 1 : ) = ' '
      END IF

      END
* @(#)sst_clean.f   1.1   94/12/05 11:31:22   96/07/05 10:27:25
