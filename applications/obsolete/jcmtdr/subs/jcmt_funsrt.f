      SUBROUTINE JCMT_FUNSORT (INDEX, N, IN, OUT)
*+
*  Name:
*     JCMT_FUNSORT

*  Purpose:
*     This routine unsorts the IN array that has previously been sorted using
*     the Figaro GEN_FVSORT routine working with the index array INDEX.
*     The output is placed in OUT. IN and OUT must NOT be the same array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL JCMT_FUNSORT (INDEX, N, IN, OUT)

*  Description:

*  [optional_subroutine_items]...
*  Authors:
*     J.Lightfoot (REVAD::JFL)
*     {enter_new_authors_here}

*  History:
*     12-MAY-1991: (REVAD::JFL) - original version
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      IMPLICIT NONE

*  Global variables:
      INTEGER N
      INTEGER INDEX(N)
      REAL IN(N)
      REAL OUT(N)

*  Local constants:

*  Functions:

*  Local variables:
      INTEGER I
*.

      DO I = 1, N
         OUT(INDEX(I)) = IN(I)
      END DO

      END

