      SUBROUTINE PON_PLOTSPLINE( IKNOTS, KNOTS, SPLINE, NPOLY, STATUS )
*+
*  Name:
*     PON_PLOTSPLINE

*  Purpose:
*     Plot a spline curve.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PON_PLOTSPLINE( IKNOTS, KNOTS, SPLINE, NPOLY, STATUS )

*  Description:
*      This routine plots a b-spline as produced by the PDA_CURFIT
*      routine.

*  Arguments:
*      IKNOTS = INTEGER (Given)
*         The number of knots in the spline fit.
*      KNOTS( IKNOTS ) = REAL (Given)
*         The positions of the knots.
*      SPLINE( IKNOTS ) = REAL (Given)
*         The spline coeffficients.
*      NPOLY = INTEGER (Given)
*         The order of the splines.
*      STATUS = (Given and Returned)
*         The global status.

*  Authors:
*     PDRAPER: P.W. Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     24-JUN-1992 (PDRAPER):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global parameters.
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'PONGO_PAR'       ! PONGO parameters

*  Arguments Given:
      INTEGER IKNOTS
      REAL KNOTS( IKNOTS )
      REAL SPLINE( * )
      INTEGER NPOLY
      REAL XMAX
      REAL XMIN

*  Global status:
      INTEGER STATUS

*  Local Variables:
      INTEGER I                 ! Loop index
      INTEGER NPOINTS           ! Number of points drawn
      INTEGER IFAIL             ! Failure flag
      REAL XSTEP                ! X increment
      REAL XPOS                 ! X position
      REAL X( NDATMAX )         ! X positions of spline
      REAL Y( NDATMAX )         ! Y positions of spline

*.

*  Check inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop to calculate the X positions. Make sure have enough positions to
*  to show at least a few positions per knot.
      NPOINTS = MIN( NDATMAX, MAX( 500, IKNOTS * 10 ) )
      XSTEP =  ( KNOTS( IKNOTS ) - KNOTS( 1 ) ) / REAL( NPOINTS )
      XPOS = KNOTS( 1 )
      DO 10 I = 1, NPOINTS
         X( I )  = XPOS
         XPOS = XPOS + XSTEP
 10   CONTINUE

*  Calculate the Y positions.
      CALL PDA_SPLEV( KNOTS, IKNOTS, SPLINE, NPOLY, X, Y, NPOINTS,
     :                IFAIL )

*  Plot the data.
      IF ( IFAIL .NE. 0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'PLOTSPLINE', 'Failed to plot spline, spline'//
     :        ' description must be invalid', STATUS )
      ELSE
         CALL PGLINE( NPOINTS, X, Y )
      END IF
      END
* $Id$
