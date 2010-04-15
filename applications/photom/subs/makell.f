************************************************************************

      SUBROUTINE MAKELL ( XCEN, YCEN, A, E, THETA, NE, ELLIPS )

*+
*  Name :
*     MAKELL
*
*  Purpose :
*     Make an ellipse centered on xcen, ycen, with axis a, ellipticity e,
*     and with an orientaion theta ( anti-clockwise from x axis ).
*
*  Language :
*     FORTRAN
*
*  Invocation :
*     CALL MAKELL( XCEN, YCEN, A, E, THETA, NE, ELLIPS )
*
*  Description :
*     Make an ellipse centered on xcen, ycen, with axis a, ellipticity e,
*     and with an orientaion theta ( anti-clockwise from x axis ). The
*     ellipse is made up from ne straight line sections. The vertices
*     are not evenly spaced around the ellipse, there are more points on
*     the curvier bits. This is achieved by stepping round the exscribed
*     circle in equal steps and dropping down a line perpendicular to the
*     semi-major axis of the ellipse. Where this line meets the ellipse
*     is where the vertex is calculated. If the semi-major axis of the
*     ellipse lies along the x-axis then this point corrseponds to
*     x = a * cos( u ),  y = b * sin( u ),
*     where u is the angle around the exscribed circle from the x-axis.
*     The orientation has been adjusted to be a position angle.
*
*  Arguments :
*     XCEN = REAL (Given)
*        X coordinate of centre of ellipse
*     YCEN = REAL (Given)
*        Y coordinate of centre of ellipse
*     A = REAL (Given)
*        Semi-major axis of ellipse
*     E = REAL (Given)
*        Eccentricity of ellipse
*     THETA = REAL (Given)
*        Orientation of ellipse anti-clockwise from x axis
*     NE = INTEGER (Given)
*        Number of vertices making up ellipse
*     ELLIPS( 2, NE ) = REAL (Returned)
*        List of vertices ( x and y coords ) of ellipse
*
*  Algorithm :
*     {algorithm_description}...
*
*  Deficiencies :
*     {routine_deficiencies}...
*
*  Authors :
*     NE: Nick Eaton (Durham University)
*     {enter_new_authors_here}
*
*  History :
*     10-AUG-1987 (NE):
*        Original version.
*     {enter_changes_here}
*
*  Bugs :
*     {note_any_bugs_here}
*-

*  Type Definitions :
      IMPLICIT NONE

*  Arguments Given :
      REAL XCEN
      REAL YCEN
      REAL A
      REAL E
      REAL THETA
      INTEGER NE

*  Arguments Returned :
      REAL ELLIPS( 2, NE )

*  Local Variables :
      INTEGER J

      REAL ANGLE, B, COSA, COST, PI, SECTION, SINA, SINT
*.

*   Set ups
      PI = ATAN( 1.0 ) * 4.0
      SECTION = 2.0 * PI / REAL( NE )
      ANGLE = 0.0
      COST = COS( ( THETA + 90.0 ) * PI / 180.0 )
      SINT = SIN( ( THETA + 90.0 ) * PI / 180.0 )

*   Calculate the semi-minor axis
      B = A * SQRT( 1.0 - E * E )

*   For each section calculate the vertex of the ellipse
      DO J = 1, NE
         COSA = COS( ANGLE )
         SINA = SIN( ANGLE )
         ELLIPS( 1, J ) = A * COSA * COST - B * SINA * SINT + XCEN
         ELLIPS( 2, J ) = A * COSA * SINT + B * SINA * COST + YCEN
         ANGLE = ANGLE + SECTION
      ENDDO

      END

* $Id$
