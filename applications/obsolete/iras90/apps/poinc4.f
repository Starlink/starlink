      SUBROUTINE POINC4( YP, Y, YL, POS, VAL, STATUS )
*+
*  Name:
*     POINC4

*  Purpose:
*     Parabola interpolate three known points.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POINC4( YP, Y, YL, POS, VAL, STATUS )

*  Description:
*     This routine fits a parabola to three known points at -1, 0 and 1.
*     And then finds the value at the given position.

*  Arguments:
*     YP, Y, YL = REAL (Given)
*        The known points at -1, 0 and 1, respectively.
*     POS = REAL (Given)
*        The position at which the value of the parabola is to be found.
*     VAL = REAL (Returned)
*        The found value at POS
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     5-MAR-1993 (WG):
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
      REAL YP, Y, YL
      REAL POS

*  Arguments Returned:
      REAL VAL

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      REAL A, B, C               ! Parabola parameters

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the values of the parabola parameters from the known points.
      C = Y
      A = 0.5 * ( YP + YL ) - C
      B = 0.5 * ( YL - YP )

*  Find the value of the given position.
      VAL = A * POS * POS + B * POS + C

      END
