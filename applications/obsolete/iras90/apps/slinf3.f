      SUBROUTINE SLINF3( IRA, SCS, LBND, UBND, MXNSCT, MXVTCE, NPOLY,
     :                   NVTCE, PLYLON, PLYLAT, STATUS )
*+
*  Name:
*     SLINF3

*  Purpose:
*     Draw polylines.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SLINF3( IRA, SCS, LBND, UBND, MXNSCT, MXVTCE, NPOLY, NVTCE,
*                  NPLYLON, PLYLAT, STATUS )

*  Description:
*     This subroutine is used to draw polylines specified by
*     their vertices position.

*  Arguments:
*     IRA = INTEGER (Given)
*        The ID of the IRA system.
*     SCS = CHARACTER*( * ) (Given)
*        Name of sky coordinate system used.
*     LBND( 2 ), UBND( 2 ) = REAL (Given)
*        The bounds of the current SGS zone in pixels.
*     NPOLY = INTEGER (Given)
*        Number of polylines to be drawn.
*     MXNSCT = INTEGER (Given)
*        The max. number of polylines.
*     MXVTCE = INTEGER (Given)
*        The max number of vertices of a polyline can have.
*     NPOLY = INTEGER (Given)
*        Number of polylines to be drawn.
*     NVTCE( NPOLY ) = INTEGER (Given and Returned)
*        The number of vertices each polyline has.
*     PLYLON( MXNSCT,  MXVTCE ) = DOUBLE PRECISION (Givne)
*        Longitude of each polyline vertex.
*     PLYLAT( MXNSCT,  MXVTCE ) = DOUBLE PRECISION (Givne)
*        Latitude of each polyline vertex.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     7-FEB-1993 (WG):
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
      INTEGER IRA
      CHARACTER*( * ) SCS
      REAL LBND( 2 ), UBND( 2 )
      INTEGER MXNSCT, MXVTCE
      INTEGER NPOLY
      INTEGER NVTCE( NPOLY )
      DOUBLE PRECISION PLYLON( MXNSCT, MXVTCE ),
     :                 PLYLAT( MXNSCT, MXVTCE )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      DOUBLE PRECISION SLA_DBEAR ! Position angle of the arc connecting

*  Local Variables:
      DOUBLE PRECISION ANG       ! Position angle from last vertex to
                                 ! present vertex
      DOUBLE PRECISION DIST      ! Distance from last to present vertex
      INTEGER I, J               ! Do loop index
      DOUBLE PRECISION LONLST, LATLST ! Longitude and latitude of the
                                      ! last vertex
      DOUBLE PRECISION LONPNT, LATPNT ! Longitude and latitude of the
                                      ! present vertex

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Draw the sections one by one.
      DO I = 1, NPOLY

*  Denote the position of the first vertex of this polyline.
         LONLST = PLYLON( I, 1 )
         LATLST = PLYLAT( I, 1 )

*  Then process the following vertices.
         DO J = 2, NVTCE( I )
            LONPNT = PLYLON( I, J )
            LATPNT = PLYLAT( I, J )

*  Find the position angle of the great circle connecting the new
*  vertice and the last vertice at the last vertice.
            ANG = SLA_DBEAR( LONLST, LATLST, LONPNT, LATPNT )

*  And the length of arc connecting these two vertices on the circle.
            CALL IRA_DIST( LONLST, LATLST, LONPNT, LATPNT, DIST,
     :                     STATUS )

*  Draw the arc connecting these two vertices.
            CALL IRA_DRGTC( IRA, LONLST, LATLST, ANG, DIST, SCS,
     :                      LBND, UBND, STATUS )

*  Denote this vertice as last, to continue drawing.
            LONLST = LONPNT
            LATLST = LATPNT
         END DO
      END DO

      END
