      SUBROUTINE CCD1_INPLY( VERTX, VERTY, NVERT, INPX, INPY, NINP, 
     :                       OUTPX, OUTPY, IND, NOUTP, STATUS )
*+
*  Name:
*     CCD1_INPLY

*  Purpose:
*     Select points from list which fall within a polygon.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL CCD1_INPLY( VERTX, VERTY, NVERT, INPX, INPY, NINP, 
*                      OUTPX, OUTPY, IND, NOUTP, STATUS )

*  Description:
*     This routine takes an input array of two-dimensional points, and
*     returns only those which fall within a specified polygon to an
*     output array.  Points on the edges or vertices are considered 
*     to be within the polygon for the purpose of this test.  It also 
*     returns an array which maps the index numbers of the output 
*     points to those of the input ones, so that it is possible to 
*     keep track of which have been selected and which rejected.
*
*     The vertices of the polygon must be supplied in clockwise or
*     anticlockwise order.  The first vertex may optionally be repeated
*     as the last one.

*  Arguments:
*     VERTX( NVERT ) = DOUBLE PRECISION (Given)
*        X coordinates of polygon vertices.
*     VERTY( NVERT ) = DOUBLE PRECISION (Given)
*        Y coordinates of polygon vertices.
*     NVERT = INTEGER (Given)
*        Number of vertices in polygon.
*     INPX( NINP ) = DOUBLE PRECISION (Given)
*        X coordinates of input list of points.
*     INPY( NINP ) = DOUBLE PRECISION (Given)
*        Y coordinates of input list of points.
*     NINP = INTEGER (Given) 
*        Number of points in input list.
*     OUTPX( NINP ) = DOUBLE PRECISION (Returned)
*        X coordinates of output list, i.e. points from input list which 
*        were found to be within the polygon.
*     OUTPY( NINP ) = DOUBLE PRECISION (Returned)
*        Y coordinates of output list, i.e. points from input list which 
*        were found to be within the polygon.
*     IND( NINP ) = INTEGER (Returned)
*        Index from input list of each point in output list.
*     NOUTP = INTEGER (Returned)
*        Number of points in output list.
*     STATUS = INTEGER (Given and returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils

*  Authors:
*     MBT: Mark Taylor (STARLINK - IoA)
*     {enter_new_authors_here}

*  History:
*     8-FEB-1999 (MBT):
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
      INTEGER NVERT
      DOUBLE PRECISION VERTX( NVERT )
      DOUBLE PRECISION VERTY( NVERT )
      INTEGER NINP

*  Arguments Returned:
      DOUBLE PRECISION INPX( NINP )
      DOUBLE PRECISION INPY( NINP )
      DOUBLE PRECISION OUTPX( NINP )
      DOUBLE PRECISION OUTPY( NINP )
      INTEGER IND( NINP )
      INTEGER NOUTP
      
*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop variable
      INTEGER INOUT              ! Inclusion status of a point in polygon
      
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise number of returned (included) points.
      NOUTP = 0

*  Go through list of input points.
      DO 1 I = 1, NINP

*  Determine whether this point is inside the polygon.
         CALL CCD1_PNPLY( INPX( I ), INPY( I ), VERTX, VERTY, NVERT,
     :                    INOUT, STATUS )

*  If inside, copy point to output list.
         IF ( INOUT .GE. 0 ) THEN
            NOUTP = NOUTP + 1
            OUTPX( NOUTP ) = INPX( I )
            OUTPY( NOUTP ) = INPY( I )
            IND( NOUTP ) = I
         END IF
 1    CONTINUE

      END
* $Id$
