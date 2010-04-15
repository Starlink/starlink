      SUBROUTINE POINC3( YP, Y, YL, MAXVAL, MAXPOS, STATUS )
*+
*  Name:
*     POINC3

*  Purpose:
*     Find max by parabola interpolation of three known points.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POINC3( YP, Y, YL, MAXVAL, MAXPOS, STATUS )

*  Description:
*     This subroutine fits three known points with a parabola:
*              Y = A * X^2 + B * X + C
*     and find the max. value and position of this parabola with in the
*     range of these three know points. It assumes that the known point
*     YP, Y and YL have X coordinates -1, 0 and 1, respectively. The
*     position of the max. value is returned in this coordinate.

*  Arguments:
*     YP, Y, YL = REAL (Given)
*        The known values at position -1, 0 and 1, respectively.
*     MAXVAL = REAL (Returned)
*        The max. of the parabola.
*     MAXPOS = REAL (Returned)
*        The position of the min.
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
      INCLUDE 'PRM_PAR'          ! Primitive constants

*  Arguments Given:
      REAL YP, Y, YL

*  Arguments Returned:
      REAL MAXVAL, MAXPOS

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      REAL A, B, C               ! Parabola parameters
      LOGICAL FOUND              ! Flag shows found a max. in the
                                 ! know points range.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the values of the parabola parameters from the known points.
*  Let X = 0, we have:
      C = Y

*  Let X = -1 and 1, we have:
      A = 0.5 * ( YP + YL ) - C
      B = 0.5 * ( YL - YP )

*  If A is negative, there is a max. find its position.
      FOUND = .FALSE.
      IF ( A .LT. -VAL__SMLR ) THEN
         MAXPOS = -B / ( 2.0 * A )

*  If it is inside the range of known points, find the max. value.
         IF ( MAXPOS .GE. -1.0 .AND. MAXPOS .LE. 1.0 ) THEN
            MAXVAL = - ( B * B ) / ( 4.0 * A ) + C
            FOUND = .TRUE.
         END IF
      END IF

*  If no max. is found within the range of the known points,
*  the max is attanded at one end of the section.
      IF ( .NOT. FOUND ) THEN
         IF ( YL .GT. YP ) THEN
            MAXVAL = YL
            MAXPOS = 1.0
         ELSE
            MAXVAL = YP
            MAXPOS = -1.0
         END IF
      END IF

      END
