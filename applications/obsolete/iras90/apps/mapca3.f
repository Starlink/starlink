      SUBROUTINE MAPCA3( IDA, SCS, LPCBND, UPCBND, WIDTH, HEIGHT,
     :                   STATUS )
*+
*  Name:
*     MAPCA3

*  Purpose:
*     Find the width and height of an image in arc-radians.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MAPCA3( IDA, SCS, LPCBND, UPCBND, WIDTH, HEIGHT, STATUS )

*  Description:
*     The arc-distance between the centre of the left and right edges
*     of the image is returned as the "width" of the image, and the
*     arc-distance between the centre of the top and bottom edges of
*     the image is returned as the "height" of the image.

*  Arguments:
*     IDA = INTEGER (Given)
*        IRA identifier for astrometry information. Note, the reference
*        point MUST have image coordinates (0.0, 0.0), i.e. P(3)=P(4)=0
*     SCS = CHARACTER * ( * ) (Given)
*        The sky coordinate system to use.
*     LPCBND( 2 ) = DOUBLE PRECISION (Given)
*        The lower bounds on pixel coordinates, which define the edges
*        of the image.
*     UPCBND( 2 ) = DOUBLE PRECISION (Given)
*        The upper bounds on pixel coordinates, which define the edges
*        of the image.
*     WIDTH = DOUBLE PRECISION (Returned)
*        The image width, in radians.
*     HEIGHT = DOUBLE PRECISION (Returned)
*        The image height, in radians.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-NOV-1991 (DSB):
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
      INTEGER IDA
      CHARACTER SCS*(*)
      DOUBLE PRECISION LPCBND( 2 )
      DOUBLE PRECISION UPCBND( 2 )

*  Arguments Returned:
      DOUBLE PRECISION WIDTH
      DOUBLE PRECISION HEIGHT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION AEDGE( 4 ) ! Sky longitude of the centre point of
                                  ! each image edge.
      DOUBLE PRECISION BEDGE( 4 ) ! Sky latitude of the centre point of
                                  ! each image edge.
      DOUBLE PRECISION XEDGE( 4 ) ! Image X coordinate at the centre
                                  ! point of each image edge.
      DOUBLE PRECISION YEDGE( 4 ) ! Image Y coordinate at the centre
                                  ! point of each image edge.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Store the image coordinates of the centre of each of the image
*  edges.
      XEDGE( 1 ) = LPCBND( 1 )
      YEDGE( 1 ) = 0.0D0
      XEDGE( 2 ) = UPCBND( 1 )
      YEDGE( 2 ) = 0.0D0
      XEDGE( 3 ) = 0.0D0
      YEDGE( 3 ) = LPCBND( 2 )
      XEDGE( 4 ) = 0.0D0
      YEDGE( 4 ) = UPCBND( 2 )

*  Convert these to sky coordinates using the supplied projection and
*  projection parameters.
      CALL IRA_TRANS( 4, XEDGE, YEDGE, .TRUE., SCS, IDA, AEDGE, BEDGE,
     :                STATUS )

*  Calculate the arc-distance between the centre of the left and right
*  edges in radians (the "width" of the image).
      CALL IRA_DIST( AEDGE( 1 ), BEDGE( 1 ), AEDGE( 2 ), BEDGE( 2 ),
     :               WIDTH, STATUS )

*  Calculate the arc-distance between the centre of the top and bottom
*  edges in radians (the "height" of the image).
      CALL IRA_DIST( AEDGE( 3 ), BEDGE( 3 ), AEDGE( 4 ), BEDGE( 4 ),
     :               HEIGHT, STATUS )

      END
