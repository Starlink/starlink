      SUBROUTINE MSG1_GNAM( STRING, IPOSN, NAME, NAMLEN, STATUS )
*+
*  Name:
*     MSG1_GNAM

*  Purpose:
*     Get the next name in a string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MSG1_GNAM( STRING, IPOSN, NAME, NAMLEN, STATUS )

*  Description:
*     The given string is searched from index IPOSN+1 for the next name.
*     A name is defined as a contiguous string of alphanumeric and 
*     underscore characters. The end of the string is given by the next 
*     character which is not alphanumeric or an underscore (normally white
*     space). If the name string overflows the declared length of the
*     returned name argument, NAME, then the name string is returned 
*     truncated to the length of the NAME argument and the status 
*     argument is returned set. The character pointer IPOSN is returned 
*     pointing to the last character of the name in the given string.

*  Arguments:
*     STRING = CHARACTER * ( * ) (Given)
*        The string to be searched for a name.
*     IPOSN = INTEGER (Given and Returned)
*        Given as the pointer to the immediately before the first
*        element to be used in the name string search. Returned as the
*        last element of the name string.
*     NAME = CHARACTER * ( * ) (Returned)
*        The returned name string.
*     NAMLEN = INTEGER (Returned)
*        The length of the returned name string.
*     STATUS = INTEGER (Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
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
*     {enter_new_authors_here}

*  History:
*      2-AUG-1991 (PCTR):
*        Original version.
*     21-FRB-2001 (AJC):
*        Copied and renamed from EMS1_GNAM
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) STRING

*  Arguments Given and Returned:
      INTEGER IPOSN

*  Arguments Returned:
      CHARACTER * ( * ) NAME
      INTEGER NAMLEN

*  Status:
      INTEGER STATUS

*  External References:
      LOGICAL CHR_ISALM          ! Whether an alphanumeric character
      LOGICAL CHR_ISALF          ! Whether an alphabetic character

*  Local Variables:
      LOGICAL BEGIN              ! Whether beginning the search

      INTEGER ICHR               ! Loop index
      INTEGER MXLEN              ! Declared length of NAME
      INTEGER STRLEN             ! Declared length of STRING

      CHARACTER * 1 CVALUE       ! Character value

*.

*  Initialise STATUS.
      STATUS = SAI__OK

*  Initialize NAME, NAMLEN and STRLEN.
      NAME = ' '
      NAMLEN = 0
      STRLEN = LEN( STRING )

*  Check that IPOSN points somewhere within the given string.
      IF ( IPOSN .LT. STRLEN ) THEN

*     Get the declared length of the name string.
         MXLEN = LEN( NAME )

*     Initialize BEGIN.
         BEGIN = .TRUE.

*     Loop to get the name string.
         DO 10 ICHR = IPOSN + 1, STRLEN

*        Get the next character from the given string.
            CVALUE = STRING( ICHR : ICHR )

*        Check for an end of name.
            IF ( BEGIN ) THEN

*           The first character of a name must be alphabetic.
               IF ( CHR_ISALF( CVALUE ) ) THEN
                  BEGIN = .FALSE.
               ELSE
                  GO TO 20
               END IF

*        All remaining characters must be alphanumeric.
            ELSE IF ( .NOT.CHR_ISALM( CVALUE ) ) THEN
               GO TO 20
            END IF

*        Increment the length of the name string and add the next
*        letter to the name string.
            IF ( NAMLEN .EQ. MXLEN ) THEN
               STATUS = SAI__ERROR
               GO TO 20
            ELSE
               NAMLEN = NAMLEN + 1
               NAME( NAMLEN : NAMLEN ) = CVALUE
            END IF
 10      CONTINUE
 20      CONTINUE

*     Update IPOSN.
         IF ( .NOT. BEGIN .AND. STATUS .EQ. SAI__OK ) IPOSN = ICHR - 1
      END IF

      END
