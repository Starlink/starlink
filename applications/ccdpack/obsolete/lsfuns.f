
      SUBROUTINE LSFUNS( X, Y, PXY, N, DIFF, STATUS )
*+
*  Name:
*     LSFUNS

*  Purpose:
*     Part of LSFUN1, forms difference in positions.

*  Language:
*     Starlink Fortran 77

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-JUL-1992 (PDRAPER):
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
      DOUBLE PRECISION X( N )
      DOUBLE PRECISION Y( N )
      DOUBLE PRECISION PXY( N, 2 )

*  Arguments Returned:
      DOUBLE PRECISION DIFF( N * 2 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop variable
      INTEGER J                  ! Counter for sums
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop over A and B forming sum of the squared differences.
      DO 1 I = 1, N
         DIFF( I ) = X( I ) - PXY( I, 1 )
 1    CONTINUE
      J = N
      DO 2 I = 1, N
         J = J + 1
         DIFF( J ) = Y( I ) - PXY( I, 2 )
 2    CONTINUE
      END
* $Id$
