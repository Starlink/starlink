      SUBROUTINE CHR_SWAP( CHAR1, CHAR2 )
*+
*  Name:
*     CHR_SWAP

*  Purpose:
*     Swap two single-character variables.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHR_SWAP( CHAR1, CHAR2 )

*  Description:
*     Exchange the values of two single-character variables.

*  Arguments:
*     CHAR1 = CHARACTER * 1 (Given and Returned)
*        The first character.
*     CHAR2 = CHARACTER * 1 (Given and Returned)
*        The second character.

*  Algorithm:
*     Use a third (temporary) character variable during the swap.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     ACD: A.C. Davenhall (ROE)
*     AJC: A.J. Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1983 (JRG):
*        Original version.
*     16-NOV-1984 (ACD):
*        Documentation improved.
*     3-OCT-1988 (AJC):
*        Documentation improved.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given and Returned:
      CHARACTER * 1 CHAR1
      CHARACTER * 1 CHAR2

*  Local Variables:
      CHARACTER * 1 TEMP         ! Temporary copy

*.

*  Swap the two characters.
      TEMP = CHAR1
      CHAR1 = CHAR2
      CHAR2 = TEMP

      END
