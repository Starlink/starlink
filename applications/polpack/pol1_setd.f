      SUBROUTINE POL1_SETD( N, VAL, DATA, STATUS )
*+
*  Name:
*     POL1_SETD

*  Purpose:
*     Square the supplied array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_SETD( N, VAL, DATA, STATUS )

*  Description:
*     This routine stores a given constant value in every element of an
*     array.

*  Arguments:
*     N = INTEGER (Given)
*        The number of points in DATA.
*     VAL = DOUBLE PRECISION (Given)
*        The value to store.
*     DATA( N ) = DOUBLE PRECISION (Returned)
*        The array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     31-MAR-1998 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER N
      DOUBLE PRECISION VAL

*  Arguments Given and Returned:
      DOUBLE PRECISION DATA( N )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! loop index
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      DO I = 1, N
         DATA( I ) = VAL
      END DO

      END
