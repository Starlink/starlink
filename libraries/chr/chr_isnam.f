      LOGICAL FUNCTION CHR_ISNAM( STRING )
*+
*  Name:
*     CHR_ISNAM

*  Purpose:
*     Return whether a string is a valid name.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = CHR_ISNAM( STRING )

*  Description:
*     Determine whether the given string is a valid name: i.e. 
*     whether it starts with an alphabetic character and continues
*     with alphanumeric or underscore characters.

*  Arguments:
*     STRING = CHARACTER * ( * ) (Given)
*        The string to be tested.

*  Returned Value:
*     CHR_ISNAM = LOGICAL
*        Returns .TRUE. if the given string is a valid name, returns 
*        .FALSE. otherwise.

*  Algorithm:
*     Use CHR_LEN to determine the string length, ignoring trailing 
*     blanks.
*     Check that the first character is alphabetic using CHR_ISALF.
*     Check any remaining characters using CHR_ISALM.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     ACD: A.C. Davenhall (ROE)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1983 (JRG):
*        Original version.
*     16-NOV-1984 (ACD):
*        Documentation improved and method simplified.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER STRING * ( * )

*  External References:
      LOGICAL CHR_ISALF          ! Whether alphabetic
      LOGICAL CHR_ISALM          ! Whether alphanumeric

      INTEGER CHR_LEN            ! String length (ignoring trailing blanks)

*  Local Variables:
      INTEGER IPOSN              ! Current position in STRING
      INTEGER SIZE               ! String length

*.

*  Find the length of the given string, ignoring trailing blanks.
      SIZE = CHR_LEN( STRING )

*  Trap zero-length strings.
      IF ( SIZE .GT. 0 ) THEN

*     The string is non-blank, so initialise the returned value and 
*     the position index.
         CHR_ISNAM = CHR_ISALF( STRING( 1 : 1 ) )
         IPOSN = 1

*     Loop to check each character in the given string.
*     DO WHILE loop.
 10      CONTINUE
         IF ( CHR_ISNAM .AND. ( IPOSN .LT. SIZE) ) THEN
            IPOSN = IPOSN + 1
            CHR_ISNAM = CHR_ISALM( STRING( IPOSN : IPOSN ) )
         GO TO 10
         END IF
      ELSE

*     String is of 0 length or totally blank. Set the returned 
*     value to false.
         CHR_ISNAM = .FALSE.
      END IF

      END
