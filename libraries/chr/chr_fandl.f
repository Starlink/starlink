      SUBROUTINE CHR_FANDL( STRING, INDEX1, INDEX2 )
*+
*  Name:
*     CHR_FANDL

*  Purpose:
*     Find the first and last non-blank characters in a string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHR_FANDL( STRING, INDEX1, INDEX2 )

*  Description:
*     Find the indices of the first and last non-blank characters in
*     the given string. If the string is all blank, the first index 
*     is returned set to the end of the string and the last index is 
*     returned set to zero, i.e. INDEX1 is greater than INDEX2.
*     If the string has no length, i.e. it is a substring with the first 
*     index greater than the second, both indices are returned set to 
*     zero.

*  Arguments:
*     STRING = CHARACTER * ( * ) (Given)
*        The character string.
*     INDEX1 = INTEGER (Returned)
*        The position of first non-blank character.
*     INDEX2 = INTEGER (Returned)
*        The position of last non-blank character.

*  Authors:
*     NE: Nick Eaton (Durham)
*     AJC: A.J. Chipperfield (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     ACC:  A.C. Charles (STARLINK)
*     {enter_new_authors_here}

*  History:
*     AUG-1988 (NE):
*        Original version.
*     27-FEB-1989 (AJC):
*        Revise and use CHR_LEN - improved speed.
*     14-FEB-1991 (PCTR):
*        Improved speed.
*     10-MAR-1994 (ACC for PCTR):
*        Modifications to prologue.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER * ( * ) STRING

*  Arguments Returned:
      INTEGER INDEX1
      INTEGER INDEX2

*  External References:
      INTEGER CHR_LEN            ! String length (ignoring trailing blanks)

*.

*  Find the used length of the input string and check it is not zero.
      INDEX2 = CHR_LEN( STRING )

      IF ( INDEX2 .LE. 0 ) THEN

*     The string is blank.
         INDEX2 = 0

*     Set INDEX1 to declared length of string.
         INDEX1 = LEN( STRING )

*     Trap for null strings.
         IF ( INDEX1 .LE. 0 ) THEN
            INDEX1 = 0
         END IF
      ELSE

*     The string contains non-blank characters. Find the position of 
*     the first non-blank character.
         DO 10 INDEX1 = 1, INDEX2
            IF ( STRING( INDEX1 : INDEX1 ) .NE. ' ' ) GO TO 20
 10      CONTINUE
 20      CONTINUE
      END IF

      END
