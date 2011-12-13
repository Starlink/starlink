      SUBROUTINE CHR_DCWRD( STRING, MXWRD, NWRD, START, STOP, WORDS,
     :                      LSTAT )
*+
*  Name:
*     CHR_DCWRD

*  Purpose:
*     Split a string into its component words.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHR_DCWRD( STRING, MXWRD, NWRD, START, STOP, WORDS, LSTAT )

*  Description:
*     All the words in the given character string are detected and
*     returned as individual elements of a character array. In this
*     context, a word is defined as a continuous string of non-blank
*     characters. Hence words must be separated from each other by
*     one or more blanks.

*  Arguments:
*     STRING = CHARACTER * ( * ) (Given)
*        The string to be split into its constituent words.
*     MXWRD = INTEGER (Given)
*        The maximum number of words that can be extracted from the
*        given string: if there are more than MXWRD words in the
*        string, only the first MXWRD will be returned.
*     NWRD = INTEGER (Returned)
*        The number of words located in the string.
*     START( MXWRD ) = INTEGER (Returned)
*        The Ith element contains the position of the first element
*        of the Ith word in the given string.
*     STOP( MXWRD ) = INTEGER (Returned)
*        The Ith element contains the position of the last element of
*        the Ith word in the given string.
*     WORDS( MXWRD ) = CHARACTER * ( * ) (Returned)
*        The Ith element contains the Ith word located in the given
*        string.
*     LSTAT = INTEGER (Returned)
*        The local status. This is a return status only: the routine
*        is not affected by the value on input. It has the following
*        values: SAI__OK for successful completion, SAI__ERROR if the
*        number of words exceeds MXWRD.

*  Algorithm:
*     Set the local status to SAI__OK.
*     Get the length of the given string, ignoring trailing blanks.
*     If the length is greater than 0 then
*       set the word counter to 0.
*       If the first element is not equal to ' ' then
*         increment the word counter.
*         Set the start position of the first word to 1.
*       end if
*       For all string elements from the second to the last:
*         If the previous element is blank and this element is
*         non-blank then
*           If there is still space for more words then
*             increment the word counter.
*             Set start position of the current word.
*           else
*             Set the local status.
*           end if
*         else if the previous element was non-blank and this element
*         is blank then
*           If the status is not set then
*             set the current stop position.
*           end if
*         end if
*       end for
*       If the status is not set then
*         If the last element non-blank then
*           set the current stop position equal to the last element.
*         end if
*       end if
*       Get the length of each element of the output array.
*       For all the words located:
*         Copy into appropriate element of the output array.
*       end for
*     else
*       Set the number of words to 0.
*     end if

*  Copyright:
*     Copyright (C) 1984, 1988, 1991 Science & Engineering Research Council.
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
*     PCTR: P.C.T. Rees (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-MAR-1984 (ACD):
*        Original version.
*     30-JUN-1984 (ACD):
*        Modified for inclusion in the CHR_ library.
*     16-NOV-1984 (ACD):
*        Name changed from CHR_STRDEC to CHR_DCWRD in order to meet
*        the naming convention.
*     3-OCT-1988 (AJC):
*        Improved documentation.
*     13-FEB-1991 (PCTR):
*        Improved speed.
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER STRING * ( * )

      INTEGER MXWRD

*  Arguments Returned:
      INTEGER NWRD
      INTEGER START( MXWRD )
      INTEGER STOP( MXWRD )

      CHARACTER WORDS( MXWRD ) * ( * )

*  Status:
      INTEGER LSTAT

*  External References:
      INTEGER CHR_LEN            ! String length (ignoring trailing blanks)

*  Local Variables:
      CHARACTER CURCHR           ! Current character value
      CHARACTER LSTCHR           ! Last character value

      INTEGER CURWRD             ! Current word
      INTEGER CINDEX             ! Current element of the input string
      INTEGER MXWLN              ! Maximum length of each element of WORDS
      INTEGER STRGLN             ! Length of input string

*.

*  Initialise the local status.
      LSTAT = SAI__OK

*  Get the length of the non-blank characters in the input string,
*  and proceed if the input string is non-blank.
      STRGLN = CHR_LEN( STRING )

      IF ( STRGLN .GT. 0 ) THEN
         NWRD = 0

*     Check whether the first element is blank or not. If non-blank
*     increment the number of words and set the starting point.
         IF ( STRING( 1 : 1 ) .NE. ' ' ) THEN
            NWRD = NWRD + 1
            START( 1 ) = 1
         END IF

*      Assign the last character value.
         LSTCHR = STRING( 1 : 1 )

*      Examine all the remaining elements to check for the starting and
*      stopping positions of words.
         DO 10 CINDEX = 2, STRGLN

*        Assign the current character value.
            CURCHR = STRING( CINDEX : CINDEX )

*        Check for the start of a new word.
            IF ( ( CURCHR .NE. ' ' ) .AND. ( LSTCHR .EQ. ' ' ) ) THEN

               IF ( NWRD .LT. MXWRD ) THEN
                  NWRD = NWRD + 1
                  START( NWRD ) = CINDEX
               ELSE
                  LSTAT = SAI__ERROR
                  GO TO 20
               END IF

*        Check for the end of a word.
            ELSE IF ( ( CURCHR .EQ. ' ' )
     :                .AND. ( LSTCHR .NE. ' ' ) ) THEN
               STOP( NWRD ) = CINDEX - 1
            END IF

*        Update the last character value.
            LSTCHR = CURCHR
 10      CONTINUE
 20      CONTINUE

*     Check whether the last element of 'STRING' is non-blank.
*     If so this terminates the last word.
         IF ( LSTAT .EQ. SAI__OK ) THEN
            IF ( STRING( STRGLN : STRGLN ) .NE. ' ' )
     :      STOP( NWRD ) = STRGLN
         END IF

*     Find the maximum length of the output words.
         MXWLN = LEN( WORDS( 1 ) )

*     Copy the words found into the output array. The words are
*     copied left justified.
         DO 30 CURWRD = 1, NWRD
            WORDS( CURWRD ) = STRING( START( CURWRD ) : STOP( CURWRD ) )
 30      CONTINUE
      ELSE

*     The input string is blank.
         NWRD = 0
      END IF

      END
