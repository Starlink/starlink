      SUBROUTINE PON_DPOLY( PROJECTION, RA0, DEC0, NP, X, Y, XW, YW,
     :                      STATUS )
*+
*  Name:
*     PON_DPOLY

*  Purpose:
*     Draw a projected polygon.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PON_DPOLY( PROJECTION, RA0, DEC0, NP, X, Y, XW, YW, STATUS )

*  Description:
*      Draws a polygon which is distorted by an astrometric projection.
*      The polygon will be filled using the current fill-hatch style
*      of PGPLOT.
*
*      Each of the vertices of the polygon is generated as an arc
*      consisting of 100 points.

*  Arguments:
*     PROJECTION = INTEGER (Given)
*        The projection number. 1 indicates no projection is in force,
*        otherwise this number is 1 + the projection numbers as
*        specified in the PROJ_ subroutine package
*     RA0 = DOUBLE PRECISION (Given)
*        The longitude centre of the projection (radians).
*     DEC0 = DOUBLE PRECISION (Given)
*        The latitude centre of the projection in (radians).
*     NP = INTEGER (Given)
*        The number of vertices of the polygon.
*     X( NP )  = DOUBLE PRECISION (Given)
*        The X co-ordinate of the centre of the ellipse.
*     Y( NP ) = DOUBLE PRECISION (Given)
*        The Y co-ordinate of the centre of the ellipse.
*     XW( 100 * NP ) = REAL (Given and Returned)
*        Workspace for holding the projected polygon.
*     YW( 100 * NP ) = REAL (Given and Returned)
*        Workspace for holding the projected polygon.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PDRAPER: Peter W. Draper (Starlink, Durham University)
*     {enter_new_authors_here}

*  History:
*     30-APR-1997 (PDRAPER):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PONGO_PAR'        ! PONGO global constants

*  Arguments Given:
      INTEGER PROJECTION
      DOUBLE PRECISION RA0
      DOUBLE PRECISION DEC0
      INTEGER NP
      DOUBLE PRECISION X( NP )
      DOUBLE PRECISION Y( NP )

*  Arguments Given and Returned:
      REAL XW( * )
      REAL YW( * )

*  Status:
      INTEGER STATUS            ! Global status

*  Local Variables:
      DOUBLE PRECISION L        ! New X position
      DOUBLE PRECISION M        ! New Y position
      DOUBLE PRECISION XE       ! X end point
      DOUBLE PRECISION XS       ! X starting point
      DOUBLE PRECISION XSTEP    ! Step in X
      DOUBLE PRECISION YE       ! Y end point
      DOUBLE PRECISION YS       ! Y starting point
      DOUBLE PRECISION YSTEP    ! Step in Y
      INTEGER I                 ! Loop variable
      INTEGER J                 ! Loop variable
      INTEGER LSTAT             ! Local status
      INTEGER NTOT              ! Total number of points to plot

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
      LSTAT = SAI__OK

*  Foreach edge of the polygon draw an arc that extends between the
*  vertices in the required projection.
      NTOT = 0
      DO 1 I = 1, NP

*  Get end points of current edge.
         XS = X( I )
         YS = Y( I )
         IF ( I .EQ. NP ) THEN

*  Special case wrap back to start
            XE = X( 1 )
            YE = Y( 1 )
         ELSE
            XE = X( I + 1 )
            YE = Y( I + 1 )
         END IF
         XSTEP = ( XE - XS ) / 100.0D0
         YSTEP = ( YE - YS ) / 100.0D0

*  Generate the points along the edge.
         DO 2 J = 1, 99
            NTOT = NTOT + 1
            CALL PROJ_CONVPTLM( PROJECTION - 1, RA0, DEC0,
     :                          XS, YS, L, M, LSTAT )
            XW( NTOT ) = REAL( L )
            YW( NTOT ) = REAL( M )
            XS = XS + XSTEP
            YS = YS + YSTEP
 2       CONTINUE

*  Make sure we get to end point.
         NTOT = NTOT + 1
         CALL PROJ_CONVPTLM( PROJECTION - 1, RA0, DEC0,
     :                       XE, YE, L, M, LSTAT )

         XW( NTOT ) = REAL( L )
         YW( NTOT ) = REAL( M )
 1    CONTINUE

*  Now plot the polygon
      CALL PGPOLY( NTOT, XW, YW )

      END
* $Id$
