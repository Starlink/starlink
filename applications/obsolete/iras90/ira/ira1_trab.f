      SUBROUTINE IRA1_TRAB( IDA, XLO, YLO, XHI, YHI, TOL, X0, Y0,
     :                      STATUS )
*+
*  Name:
*     IRA1_TRAB

*  Purpose:
*     Trace round the bad/good boundary.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA1_TRAB( IDA, XLO, YLO, XHI, YHI, TOL, X0, Y0, STATUS )

*  Description:
*     The image is divided up into a grid of boxes with size specified
*     by TOL. A box is considered to be "bad" if the centre of the box
*     lies outside region of valid sky coordinates, or if the box is on
*     the edge of the image. The polyline representing the boundary is
*     started at the centre of the box containing the given starting
*     position. The adjacent boxes are then check to find a bad box
*     which is next to a good box, and the polyline is continued to the
*     centre of this box. This process is repeated until the starting
*     box is retruned to.
*
*     To minimise the number of transformation evaluations which need
*     to be performed, a cache is used to hold the bad/good status of
*     each box surrounding the current box. When the next box is
*     processed, the status of some of the adjacent boxes will already
*     be known from the previous box and will be available in the
*     cache.

*  Arguments:
*     IDA = INTEGER (Given)
*        An IRA identifier for the astrometry information.
*     XLO = REAL( Given)
*        The lower bound of the plotting window on the first axis of
*        the current world coordinate system.
*     XHI = REAL( Given)
*        The upper bound of the plotting window on the first axis of
*        the current world coordinate system.
*     YLO = REAL( Given)
*        The lower bound of the plotting window on the second axis of
*        the current world coordinate system.
*     YHI = REAL( Given)
*        The upper bound of the plotting window on the second axis of
*        the current world coordinate system.
*     TOL = REAL (Given)
*        The accuracy to which the curve should follow the actual
*        boundary, in pixels.
*     X0 = REAL (Given)
*        The X image coordinate of a point known to be on the bad/good
*        boundary to within the required tolerance. The point should be
*        on the bad side of the boundary.
*     Y0 = REAL (Given)
*        The Y image coordinate of a point known to be on the bad/good
*        boundary to within the required tolerance. The point should be
*        on the bad side of the boundary.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-MAR-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants

*  Arguments Given:
      INTEGER IDA
      REAL XLO
      REAL YLO
      REAL XHI
      REAL YHI
      REAL TOL
      REAL X0
      REAL Y0

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER BOX
      LOGICAL CACHE( 8 )
      INTEGER M
      INTEGER M0
      INTEGER MMAX
      LOGICAL MORE
      INTEGER N
      INTEGER N0
      INTEGER NMAX
      REAL X
      REAL XBOX
      REAL XSTART
      REAL Y
      REAL YBOX
      REAL YSTART

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  The plotting space is divided up into boxes of side TOL. Box (1,1) is
*  the bottom left box. Find the maximum box indices.
      MMAX = INT( ( XHI - XLO )/TOL ) + 1
      NMAX = INT( ( YHI - YLO )/TOL ) + 1

*  Adjust the box dimensions to get an exact number of boxes along each
*  axis.
      XBOX = ( XHI - XLO )/MMAX
      YBOX = ( YHI - YLO )/NMAX

*  Store indices of the box containing the supplied starting position.
      M0 = 1 + INT( ( X0 - XLO )/XBOX )
      N0 = 1 + INT( ( Y0 - YLO )/YBOX )

*  Find a box which is adjacent to the supplied starting position and
*  which is either bad or on the edge of the plotting space. This will
*  be used as the starting position instead of the supplied starting
*  position to ensure that the curve will is on the boundary.
      BOX = 0
      CALL IRA1_BBOX( IDA, XLO, YLO, XBOX, YBOX, MMAX, NMAX, M0, N0,
     :                BOX, CACHE, XSTART, YSTART, STATUS )

*  Start a poly line.
      CALL SGS_BPOLY( XSTART, YSTART )

*  Follow the course of the good/bad boundary.
      M = M0
      N = N0
      MORE = .TRUE.

      DO WHILE( MORE )

*  Find a box which is adjacent to the current box and which is either
*  bad or on the edge of the plotting space.
         CALL IRA1_BBOX( IDA, XLO, YLO, XBOX, YBOX, MMAX, NMAX, M, N,
     :                   BOX, CACHE, X, Y, STATUS )

*  If found OK, add the point to the poly line.
         IF( BOX .NE. 0 .AND. STATUS .EQ. SAI__OK ) THEN

*  Extend the poly line.
            CALL SGS_APOLY( X, Y )

*  If the boundary has come back to the original box, it is complete.
            IF( M .EQ. M0 .AND. N .EQ. N0 ) MORE = .FALSE.

*  IF no box could be found, the boundary cannot be continued.
         ELSE
            MORE = .FALSE.

         END IF

      END DO

*  Complete the curve.
      CALL SGS_APOLY( XSTART, YSTART )

*  Output the poly line.
      CALL SGS_OPOLY

      END
