      SUBROUTINE MSG1_GESC( ESCCHR, STRING, IPOSN )
*+
*  Name:
*     MSG1_GESC

*  Purpose:
*     Get the next occurrence of a set of specified escape characters.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MSG1_GESC( ESCCHR, STRING, IPOSN )

*  Description:
*     The given string is searched forward from the index IPOSN+1 for the 
*     next occurrence of any of the characters given in the escape string.
*     The character pointer IPOSN is returned pointing to this next escape 
*     character in the given string. If no escape character is found, 
*     IPOSN is returned set to zero.

*  Arguments:
*     ESCCHR = CHARACTER * ( * ) (Given)
*        The set of given escape characters.
*     STRING = CHARACTER * ( * ) (Given)
*        The string to be searched.
*     IPOSN = INTEGER (Given and Returned)
*        Given as the pointer to the previous escape character, returned
*        as the pointer to the next escape character. IPOSN is given as
*        0 to indicate the start of the search and is returned set to
*        the index of the next escape character: when no more escape
*        characters can be found in the given string, IPOSN is returned
*        as 0.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
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
*     {enter_new_authors_here}

*  History:
*     1-AUG-1991 (PCTR):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER * ( * ) ESCCHR
      CHARACTER * ( * ) STRING

*  Arguments Given and Returned:
      INTEGER IPOSN

*  Local Variables:
      INTEGER ESCLEN             ! Length of the escape character string
      INTEGER ICHR               ! Loop index
      INTEGER IDX                ! Character index
      INTEGER NEWPOS             ! Index of the next escape character
      INTEGER STRLEN             ! Declared length of STRING

*.

*  Get the length of the given strings.
      ESCLEN = LEN( ESCCHR )
      STRLEN = LEN( STRING )

*  Initialise NEWPOS.
      NEWPOS = 0

*  Check that IPOSN points to somewhere within the string and that 
*  the length of the escape character string is non-zero.
      IF ( IPOSN .LT. STRLEN .AND. ESCLEN .GT. 0 ) THEN

*     Loop to get the index of the next escape character.
         DO 10 ICHR = 1, ESCLEN
            IDX = INDEX( STRING( IPOSN+1 : ), ESCCHR( ICHR : ICHR ) )

            IF ( IDX .GT. 0 ) THEN

               IF ( NEWPOS .EQ. 0 ) THEN
                  NEWPOS = IDX
               ELSE IF ( NEWPOS .GT. 0 ) THEN
                  NEWPOS = MIN( IDX, NEWPOS )
               END IF
            END IF
 10      CONTINUE
      END IF

*  Update IPOSN.
      IF ( NEWPOS .GT. 0 ) THEN
         IPOSN = IPOSN + NEWPOS 
      ELSE
         IPOSN = 0
      END IF

      END
