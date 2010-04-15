
      SUBROUTINE SCLLIM( PERCEN, DOWN, UP, SDOWN, SUP, STATUS )
*+
*  Name:
*     SCLLIM

*  Purpose:
*     To scale upper and lower limits by a given percentage.
*     For use in ensuring that graphs have sufficient room to show all
*     points clearly.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SCLLIM( PERCEN, DOWN, UP, SDOWN, SUP )

*  Description:
*     The routine finds the real difference between the limits (
*     checking that the minimum and maximum values are so ) and scales
*     them to be larger and smaller by PERCEN percent.

*  Arguments:
*     PERCEN = REAL (Given)
*        The percentage by which to scale limits.
*     DOWN = REAL (Given)
*        Lower limit.
*     UP = REAL (Given)
*        Upper limit.
*     SDOWN = REAL (Returned)
*        Scaled lower limit.
*     SUP = REAL (Returned)
*        Scaled upper limit.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-OCT-1990 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Status:
      INTEGER STATUS             ! Global status

*  Arguments Given:
      REAL PERCEN
      REAL DOWN, UP

*  Arguments returned
      REAL SDOWN, SUP

*  Local Variables:
      REAL CHANGE, DIFF
*.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Find the true upper and lower limits
      SUP = MAX ( UP, DOWN )
      SDOWN = MIN( UP, DOWN )

*  If not the same values
      IF ( SUP .NE. SDOWN ) THEN
        CHANGE = PERCEN / 100.0
        DIFF = ( UP - DOWN ) * PERCEN / 100.0
        SUP = SUP + DIFF
        SDOWN = SDOWN - DIFF

*  Else an error
      ELSE
         CALL ERR_REP( 'SAME_VALUES','SCLLIM - (plotting) limits have'
     :                //' the same value', STATUS )
      END IF
      END
* $Id$
