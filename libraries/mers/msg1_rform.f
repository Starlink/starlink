      SUBROUTINE MSG1_RFORM( TEXT, IPOSN, STRING, STRLEN )
*+
*  Name:
*     MSG1_RFORM

*  Purpose:
*     Reformat the given text to a new width.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MSG1_RFORM( TEXT, IPOSN, STRING )

*  Description:
*     This subroutine is called repeatedly to reformat the given 
*     text string to a new width (given by the declared length of the 
*     returned character variable). The returned line always has a 
*     ragged right margin. The text in the returned string is formatted
*     to end at a word end. A word in this context is a contiguous
*     string of non-blank characters.

*  Arguments:
*     TEXT = CHARACTER * ( * ) (Given)
*        The character variable which contains the text to be
*        reformatted. Leading blanks are preserved.
*     IPOSN = INTEGER (Given and Returned)
*        On entry, this argument specifies the character position in
*        TEXT from which to start generating the next returned line. 
*        It is given as the number of characters from the first
*        character in TEXT. If a value less than 1 is used, then 1 will
*        be used. If a value greater than the declared length of the
*        returned string is given, the returned string is initialised to
*        blank space and IPOSN is reset to zero.
*
*        On exit, this argument is set to one more than the position
*        in TEXT of the last blank character which appears in the
*        returned line STRING (i.e. the position at which the
*        generation of the next output line should start). When the end
*        of the given string is reached, IPOSN is returned set to zero.
*     STRING = CHARACTER * ( * ) (Returned)
*        The returned line of text, left justified. The length of this
*        argument defines the maximum length of the returned line.
*     STRLEN = INTEGER (Returned)
*        The used length of STRING.

*  Notes:
*     -  This routine should be called repeatedly to generate
*     successive returned lines from the given text.  Initially, the
*     pointer IPOSN should be set to unity; it will be updated after
*     each call, ready to generate the next returned line. A value of
*     zero is returned for IPOSN when there is no more text to
*     process. Trailing blanks in the given text are ignored, multiple
*     blanks between words are maintained, a single blank is dropped in
*     multiple blanks which occur at a new returned line.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     PCTR: P.C.T. Rees (STARLINK)
*     AJC: A.J. Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26-FEB-2001 (AJC):
*        Original version - copy of EMS1_RFORM
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER * ( * ) TEXT

*  Arguments Given and Returned:
      INTEGER IPOSN

*  Arguments Returned:
      CHARACTER * ( * ) STRING

      INTEGER STRLEN

*  External References:
      INTEGER CHR_LEN            ! String length

*  Local Variables:
      INTEGER ILAST              ! Last allowed index of the substring
      INTEGER IPLEN              ! Declared length of the given text
      INTEGER ISTART             ! Start index of substring
      INTEGER ISUB               ! Substring pointer
      INTEGER OPLEN              ! Declared length of the returned line

*.

*  Get the declared lengths of the given and returned character
*  variables.
      IPLEN = CHR_LEN( TEXT )
      OPLEN = LEN( STRING )

*  If the given string is not empty and the starting position does not
*  lie beyond the end of the given text, then there is potentially
*  something to return.
      IF ( ( IPLEN .GT. 0 ) .AND. ( IPOSN .LE. IPLEN ) ) THEN

*     If the starting position is before the beginning of the string,
*     then advance it to the first character position.
         IF ( IPOSN .LT. 1 ) IPOSN = 1

*     Initialise the start index, ISTART, and the allowed length,
*     ILAST, of the given string.
         ISTART = IPOSN
         ILAST = MIN( ISTART+OPLEN-1, IPLEN )

*     Check whether the entire given substring will fit into the
*     returned string.
         IF ( OPLEN .GE. IPLEN-ISTART+1 ) THEN

*        The given substring can fit into the returned string, assign
*        the returned string and update the returned pointer.
            STRING = TEXT( ISTART : )
            STRLEN = IPLEN - ISTART + 1
            IPOSN = 0
         ELSE

*        Loop backwards through the given substring to find the last
*        blank space that will fit into the returned string.
            DO 10 ISUB = ILAST+1, ISTART, -1
               IF ( TEXT( ISUB : ISUB ) .EQ. ' ' ) GO TO 20
 10         CONTINUE

            ISUB = ILAST
 20         CONTINUE

*        Assign the returned string and update the returned string 
*        length and character pointer.
            IF ( ISUB .GT. ILAST ) THEN
               STRING = TEXT( ISTART : )
               STRLEN = OPLEN
            ELSE
               STRING = TEXT( ISTART : ISUB )
               STRLEN = ISUB - ISTART + 1
            END IF

            IPOSN = ISUB + 1
         END IF
      END IF
      
      END
