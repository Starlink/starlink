      SUBROUTINE CHR_FIND( STRING, SUBSTR, FORWD, IPOSN )
*+
*  Name:
*     CHR_FIND

*  Purpose:
*     Find the next occurrence of given substring within a string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHR_FIND( STRING, SUBSTR, FORWD, IPOSN )

*  Description:
*     Increments a pointer to a character position within the given
*     string and checks if the following sequence of characters matches
*     the specified substring, ignoring differences in case. The search
*     may be performed either forwards or backwards. If a match is found,
*     the position of the substring is returned. If no match exists,
*     the pointer is set to one more than the length of the string
*     if the search is forwards, zero if the search is backwards.

*  Arguments:
*     STRING = CHARACTER * ( * ) (Given)
*        The string to be searched.
*     SUBSTR = CHARACTER * ( * ) (Given)
*        The substring to be searched for, ignoring case.
*     FORWD = LOGICAL (Given)
*        The search direction: if .TRUE., proceed through the string
*        in a forward direction, otherwise work backwards.
*     IPOSN = INTEGER (Given and Returned)
*        The starting position for the search. If the initial value of
*        IPOSN does not point at a character within the string, the
*        routine returns without action.

*  Copyright:
*     Copyright (C) 1990, 1991, 1994 Science & Engineering Research Council.
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
*     PCTR: P.C.T. Rees (STARLINK)
*     ACC:  A.C. Charles (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-DEC-1990 (PCTR):
*        Original version.
*     21-FEB-1991 (PCTR):
*        Added FORWD argument and behaviour.
*     10-MAR-1994 (ACC for PCTR):
*        Modifications to prologue.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER * ( * ) STRING
      CHARACTER * ( * ) SUBSTR

      LOGICAL FORWD

*  Arguments Given and Returned:
      INTEGER IPOSN

*  External References:
      CHARACTER CHR_LOWER        ! Lowercase character conversion
      CHARACTER CHR_UPPER        ! Uppercase character conversion

      LOGICAL CHR_SIMLR          ! Case independent string equality

*  Local Variables:
      LOGICAL ISALPH             ! Whether LCHAR/UCHAR is alphabetic

      INTEGER END                ! End loop index
      INTEGER INCR               ! Loop increment: 1=forward, -1=reverse
      INTEGER LINDX              ! Lowercase index
      INTEGER SINDX              ! Substring index
      INTEGER STRLEN             ! Declared length of string
      INTEGER SUBLEN             ! Declared length of substring
      INTEGER UINDX              ! Uppercase index

      CHARACTER LCHAR            ! Lowercase start of substring
      CHARACTER UCHAR            ! Uppercase start of substring

*.

*  Initialise STRLEN and SUBLEN.
      STRLEN = LEN( STRING )
      SUBLEN = LEN( SUBSTR )

*  Trap bad start position values.
      IF ( ( IPOSN .GT. 0 ) .AND. ( IPOSN .LE. STRLEN-SUBLEN ) ) THEN

*     Initialise loop indices.
         IF ( FORWD ) THEN
            INCR = 1
            END = STRLEN
         ELSE
            INCR = -1
            END = 1
         END IF

*     Initialise the lower and uppercase substring start.
         LCHAR = SUBSTR( 1 : 1 )
         LCHAR = CHR_LOWER( LCHAR )
         UCHAR = CHR_UPPER( LCHAR )

*     Initialise ISALPH.
         ISALPH = ( LCHAR .NE. UCHAR )

*     Loop for all instances of LCHAR/UCHAR in the given string until
*     either a match is found ot the end of the string is reached.
*     DO WHILE loop.
 10      CONTINUE
         IF ( ( IPOSN .GT. 0 ) .AND. ( IPOSN .LE. STRLEN-SUBLEN ) ) THEN

*        Find the indices of the lower and uppercase substring start
*        in the string - first the lowercase.
            DO 20 LINDX = IPOSN, END, INCR
               IF ( LCHAR .EQ. STRING( LINDX : LINDX ) ) GO TO 30
 20         CONTINUE

            LINDX = 0
 30         CONTINUE

*        Find the index to be used in the string comparison.
            IF ( ISALPH ) THEN

*           Find the index of the uppercase substring start.
               DO 40 UINDX = IPOSN, END, INCR
                  IF ( UCHAR .EQ. STRING( UINDX : UINDX ) ) GO TO 50
 40            CONTINUE

               UINDX = 0
 50            CONTINUE

               IF ( LINDX .EQ. 0 ) THEN
                  SINDX = UINDX
               ELSE IF ( UINDX .EQ. 0 ) THEN
                  SINDX = LINDX
               ELSE IF ( FORWD ) THEN
                  SINDX = MIN( LINDX, UINDX )
               ELSE
                  SINDX = MAX( LINDX, UINDX )
               END IF
            ELSE
               SINDX = LINDX
            END IF

*        Check if a substring with start LCHAR/UCHAR exists.
            IF ( SINDX .NE. 0 ) THEN

*           A substring with start LCHAR/UCHAR does exist: is the
*           substring similar?
               IF ( CHR_SIMLR( STRING( SINDX : SINDX+SUBLEN-1 ),
     :                                 SUBSTR ) ) THEN

*              If so, then set IPOSN and return.
                  IPOSN = SINDX
                  GO TO 999
               ELSE

*              No match, so increment IPOSN and try again.
                  IPOSN = SINDX + INCR
               END IF
            ELSE

*           No match, so abort.
               IPOSN = END + INCR
            END IF
         GO TO 10
         END IF
      END IF

 999  CONTINUE

      END
