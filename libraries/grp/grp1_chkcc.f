      LOGICAL FUNCTION GRP1_CHKCC( STRING, INDEX, CC, ESC, OK )
*+
*  Name:
*     GRP1_CHKCC

*  Purpose:
*     Check if a sub-string begins with a non-escaped control character.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     MATCH = GRP1_CHKCC( STRING, INDEX, CC, ESC, OK )

*  Description:
*     This subroutine should be used to check if a specified character in
*     a string is a GRP control character. If the character matches the
*     control character but is preceeded by a single escape character then
*     is is not considered to be a control character.

*  Arguments:
*     STRING = CHARACTER * ( * ) (Given)
*        The string to search.
*     INDEX = INTEGER (Given)
*        The index of the character to check.
*     CC = CHARACTER * 1 (Given)
*        The control character to check for.
*     ESC = CHARACTER * 1 (Given)
*        The escape character.
*     OK = LOGICAL (Given)
*        Should the escape character be used?

*  Returned Value:
*     GRP1_CHKCC = LOGICAL 
*        Should the character with index INDEX within STRING be treated
*        as a GRP control character of type CC?

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     27-AUG-1999 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER STRING*(*)
      INTEGER INDEX
      CHARACTER CC*(*)
      CHARACTER ESC*1
      LOGICAL OK

*  Local Variables:
      INTEGER P                  ! Index of previous character
      INTEGER NESC               ! No. of escape characters found
*.

*  Compare the specified character.
      GRP1_CHKCC = ( STRING( INDEX: INDEX ) .EQ. CC ) 

*  Return immediately if the character does not match, or if it is the
*  first character in the string, or if escape characters are not being
*  used.
      IF( GRP1_CHKCC .AND. INDEX .GT. 1 .AND. OK ) THEN

*  Count the number of escape characters occuring before the control
*  character.
         P = INDEX - 1
         DO WHILE( P .GT. 0 .AND. STRING( P : P ) .EQ. ESC )
            P = P - 1
         END DO

         NESC = INDEX - 1 - P

*  The control character is escaped if it is preceeded by an odd number
*  of escape characters.
         GRP1_CHKCC = ( MOD( NESC, 2 ) .EQ. 0 ) 

      END IF

      END
