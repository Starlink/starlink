      SUBROUTINE CCD1_REPC( STRING, CHAR1, CHAR2, STATUS )
*+
*  Name:
*     CCD1_REPC

*  Purpose:
*     Replaces all occurances of a character with another character.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_REPC( STRING, CHAR1, CHAR2, STATUS )

*  Description:
*     The routine replaces all occurances of CHAR1 in STRING with CHAR2.
*     The replacement is performed insitu.

*  Arguments:
*     STRING = CHARACTER * ( * ) (Given and Returned)
*        The string which contains the characters which are to be
*        checked for replacement.
*     CHAR1 = CHARACTER * ( * ) (Given)
*        The character whose occurences are to be replaced.
*     CHAR2 = CHARACTER * ( * ) (Given)
*        The character which is to replace the occurences of CHAR1.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils
 
*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1-OCT-1991 (PDRAPER):
*        Original version.
*     6-FEB-1992 (PDRAPER):
*        Changed from ARD original.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( 1 ) CHAR1
      CHARACTER * ( 1 ) CHAR2

*  Arguments Given and Returned:
      CHARACTER * ( * ) STRING

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IAT                ! Current position of CHAR1

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Look for first occurence of character, if not found then terminate.
      IAT = INDEX( STRING , CHAR1 )

*  Loop while further occurences of CHAR1 are located.
1     CONTINUE              ! Start of 'DO WHILE' loop
         IF ( IAT .NE. 0 ) THEN
            STRING( IAT : IAT ) = CHAR2
            IAT = INDEX( STRING, CHAR1 )
            GO TO 1
         END IF
      END
