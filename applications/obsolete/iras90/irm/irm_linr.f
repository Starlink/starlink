      SUBROUTINE IRM_LINR( DIS1, DIS2, VAL1, VAL2, OUT, STATUS  )
*+
*  Name:
*     IRM_LINR

*  Purpose:
*     Performe a linear interpolation.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRM_LINR( DIS1, DIS2, VAL1, VAL2, OUT, STATUS )

*  Description:
*     This subroutine performes a linear interpolation between two known
*     points. If one of the points has bad value, the output will take
*     the value of the other point. If both points are bad, the output
*     will be bad as well.

*  Arguments:
*     DIS1, DIS2 = REAL (Given)
*        Distances of the position to the first point and second point,
*        respectively.
*     VAL1, VAL2 = REAL (Given)
*        Values on the first point and second point, respectively.
*     OUT = REAL (Returned)
*        The value at the position found by linear interpolation.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     13-AUG-1992 (WG):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Primitive constants

*  Arguments Given:
      REAL VAL1, VAL2
      REAL DIS1, DIS2

*  Arguments Returned:
      REAL OUT

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the distances between two ends is 0, set status, report and exit.
      IF ( DIS1 + DIS2 .LE. VAL__SMLR ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'IRM_LINR_ERR1', 'IRM_LINR: Distances from the '/
     :                /'position to be interpolated to both ends are '/
     :                /'0 (possibly programming error ).', STATUS )
         GOTO 999
      END IF

*  If value at the first point is bad, assign the value of the second
*  point to the output.
      IF ( VAL1 .EQ. VAL__BADR ) THEN
         OUT = VAL2

*  If value at the second point is bad, assign the value of the first
*  point to the output.
      ELSE IF ( VAL2 .EQ. VAL__BADR ) THEN
         OUT = VAL1

*  If both values are not bad, find the output value by linear
*  interpolation.
      ELSE
         OUT = ( VAL1 * DIS2 + VAL2 * DIS1 ) / ( DIS1 + DIS2 )
      END IF

 999  CONTINUE

      END
