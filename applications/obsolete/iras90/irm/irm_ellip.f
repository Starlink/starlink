      SUBROUTINE IRM_ELLIP( X, Y, XSIZE, YSIZE, STATUS )
*+
*  Name:
*     IRM_ELLIP

*  Purpose:
*     Draw an ellipse

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRM_ELLIP( X, Y, XSIZE, YSIZE, STATUS )

*  Description:
*     An ellipse is drawn using the current SGS pen. Nothing is drawn
*     if any of the supplied argument values are bad (but no error is
*     reported).

*  Arguments:
*     X = REAL (Given)
*        World X coordinate of ellipse centre
*     Y = REAL (Given)
*        World Y coordinate of ellipse centre
*     XSIZE = REAL (Given)
*        Half-axis size in world X coordinate
*     YSIZE = REAL (Given)
*        Half-axis size in world Y coordinate
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-JUL-1993 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'IRA_PAR'          ! IRA_ constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants

*  Arguments Given:
      REAL X
      REAL Y
      REAL XSIZE
      REAL YSIZE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MXCHRD             ! No. of sections in ellipse
      PARAMETER ( MXCHRD = 50 )

*  Local Variables:
      INTEGER I                  ! Loop index
      REAL T                     ! Ellipse angular parameter value

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If any of the input values are bad, return without drawing anything.
      IF( X .NE. VAL__BADR .AND. Y .NE. VAL__BADR .AND.
     :    XSIZE .NE. VAL__BADR .AND. YSIZE .NE. VAL__BADR ) THEN

*  Start drawing the poly-line
         CALL SGS_BPOLY( XSIZE + X, Y )

*  Loop round each chord of the ellipse
         DO I = 1, MXCHRD
            T = (IRA__TWOPI/MXCHRD)*REAL( I )
            CALL SGS_APOLY( X + XSIZE*COS( T ), Y + YSIZE*SIN( T ) )
         END DO

*  Output the poly-line.
         CALL SGS_OPOLY

*  See if an error has been reported by GKS or SGS
         CALL GKS_GSTAT( STATUS )

      END IF

*  If an error occurred, give a contextual message.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRM_ELLIP_ERR1',
     :                 'IRM_ELLIP: Unable to draw an ellipse',
     :                 STATUS )
      END IF

      END
