      SUBROUTINE IRA1_BBOX( IDA, XLO, YLO, XBOX, YBOX, MMAX, NMAX, M,
     :                      N, BOX, CACHE, XC, YC, STATUS )
*+
*  Name:
*     IRA1_BBOX

*  Purpose:
*     Find an adjacent box on the good/bad boundary.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA1_BBOX( IDA, XLO, YLO, XBOX, YBOX, MMAX, NMAX, M, N, BOX,
*                     CACHE, XC, YC, STATUS )

*  Description:
*     This routine finds a bad box which is adjacent to the current bad
*     box and also to a good box (the good box must also be adjacent to
*     the current bad box). Return to the previous bad box is
*     prevented. It also updates the cache to hold the good/bad status
*     of each box adjacent to the current bad box.

*  Arguments:
*     IDA = INTEGER (Given)
*        An IRA identifier for the astrometry information.
*     XLO = REAL( Given)
*        The lower bound of the plotting window on the first axis of
*        the current world coordinate system.
*     YLO = REAL( Given)
*        The lower bound of the plotting window on the second axis of
*        the current world coordinate system.
*     XBOX = REAL (Given)
*        The size of a box in the X direction, in image coordinates.
*     YBOX = REAL (Given)
*        The size of a box in the Y direction, in image coordinates.
*     MMAX = INTEGER (Given)
*        The first index of the box on the left edge of the image.
*     NMAX = INTEGER (Given)
*        The second index of the box on the top edge of the image.
*     M = INTEGER (Given and Returned)
*        On entry, the first index of the current bad box.  On exit,
*        the first index of the next bad box.
*     N = INTEGER (Given and Returned)
*        On entry, the second index of the current bad box.  On exit,
*        the second index of the next bad box.
*     BOX = INTEGER (Given and Returned)
*        On entry, the box number of the current bad box relative to
*        the previous bad box. On exit, the box number of the next bad
*        box relative to the current bad box. Box numbers are in the
*        range 1 to 8. The box directly above the current bad box is
*        box 1, and the numbers increase clockwise round the current
*        bad box.
*     CACHE( 8 ) = LOGICAL (Given and Returned)
*        On entry, contains the bad/good status of each box adjacent to
*        the previous bad box. On exit contains the bad/good status of
*        each box adjacent to the current bax box. The array is indexed
*        by "box number" (1-8) realtive to the previous bad box (on
*        entry), or the current bad box( on exit). A true value means
*        that the centre of the box corresponds to a valid sky position
*        and is away from the edge of the image.
*     XC = REAL (Returned)
*        The image X coordinate half way between the centre of the
*        next bad box and the adjacent good box.
*     YC = REAL (Returned)
*        The image Y coordinate half way between the centre of the
*        next bad box and the adjacent good box.
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
      REAL XBOX
      REAL YBOX
      INTEGER MMAX
      INTEGER NMAX

*  Arguments Given and Returned:
      INTEGER M
      INTEGER N
      INTEGER BOX
      LOGICAL CACHE( 8 )

*  Arguments Returned:
      REAL XC
      REAL YC

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER BX
      INTEGER BXM1
      INTEGER DM( 8 )
      INTEGER DN( 8 )
      INTEGER K
      INTEGER NEXT
      REAL X0
      REAL Y0

*  Local Data:
      DATA DM / 0, 1, 1, 1, 0, -1, -1, -1 /,
     :     DN / 1, 1, 0, -1, -1, -1, 0, 1 /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set up the image coordinates of the centre of the current centre
*  box.
      X0 = XLO + ( REAL( M ) - 0.5 )*XBOX
      Y0 = YLO + ( REAL( N ) - 0.5 )*YBOX

*  The cache contains logical flags indicating if each of the 8 boxes
*  adjacent to a reference box is bad or on the edge (the flag is
*  .false. if the box is bad or on the edge).  Adjacent boxes are
*  numbered 1 to 8 in a clockwise direction round the centre
*  (reference) box, box 1 is directly above the centre box. Set up the
*  cache with respect to the supplied box (BOX becomes the box no. of
*  the previous box with respect to the current box).
      CALL IRA1_SCAC( IDA, X0, Y0, XBOX, YBOX, MMAX, NMAX, M, N, BOX,
     :                CACHE, STATUS )

*  If no previous box is defined, find the lowest numbered bad or edge
*  box and use this as the previous box.
      IF( BOX .EQ. 0 ) THEN
         DO K = 1, 8
            IF( .NOT. CACHE( K ) ) THEN
               BOX = K
               GO TO 10
            END IF
         END DO

 10      CONTINUE
      END IF

*  Find the box adjacent to the previous box in the clockwise
*  direction.
      IF( BOX .GT. 0 ) THEN
         NEXT = MOD( BOX, 8 ) + 1
         BOX = 0

*  If this adjacent box is bad or on edge, find the the first good box
*  in the clockwise direction.
         IF( .NOT. CACHE( NEXT ) ) THEN

            DO K = NEXT + 1, NEXT + 6
               BX = MOD( K - 1, 8 ) + 1

               IF( CACHE( BX ) ) THEN

*  Return the previous bad box.
                  BOX = MOD( BX + 6, 8 ) + 1
                  GO TO 20
               END IF

            END DO

*  If the adjacent box is good, find the the first bad or edge box
*  in the clockwise direction.
         ELSE

            DO K = NEXT + 1, NEXT + 6
               BX = MOD( K - 1, 8 ) + 1

               IF( .NOT. CACHE( BX ) ) THEN
                  BOX = BX
                  GO TO 20
               END IF

            END DO

         END IF

 20      CONTINUE

*  Return the image coordinates half way between the box and the
*  previous box.
         BXM1 = MOD( BX + 6, 8 ) + 1
         XC = X0 + 0.5*REAL( DM( BX ) + DM( BXM1 ) )*XBOX
         YC = Y0 + 0.5*REAL( DN( BX ) + DN( BXM1 ) )*YBOX

      END IF

*  If a box was found, return its indices.
      IF( BOX .NE. 0 ) THEN
         M = M + DM( BOX )
         N = N + DN( BOX )
      END IF

      END
