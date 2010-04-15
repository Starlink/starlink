      SUBROUTINE JCMT_SWAPXY (IN, NX, NY, OUT, STATUS)
*+
*  Name:
*     JCMT_SWAPXY

*  Purpose:
*     Swaps indexing of input array in output array; OUT(I,J) = IN(J,I)

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL JCMT_SWAPXY (IN, NX, NY, OUT, STATUS)

*  Description:

*  Arguments:
*     IN (NX, NY) = REAL (Given)
*        The input array
*     NX = INTEGER (Given)
*        the x dimension of the input array
*     NY = INTEGER (Given)
*        the y dimension of the input array
*     OUT (NY, NX) = REAL (Returned)
*        The swapped array
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  [optional_subroutine_items]...
*  Authors:
*     REVAD::JFL: J.Lightfoot
*     {enter_new_authors_here}

*  History:
*     22-MAY-1991 (REVAD::JFL):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NX
      INTEGER NY
      REAL IN (NX, NY)

*  Arguments Returned:
      REAL OUT (NY, NX)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:

*  Local Variables:
      INTEGER IY                 ! counter
      INTEGER IX                 ! counter
*.

*  Check inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

*  Do the swap

      DO IX = 1, NX
         DO IY = 1, NY
            OUT(IY, IX) = IN(IX, IY)
         END DO
      END DO

      END


