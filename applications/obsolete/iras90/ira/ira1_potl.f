      SUBROUTINE IRA1_POTL( NC, AB, XLO, YLO, XHI, YHI, MAXBRK, BREAK,
     :                      VBREAK, NBREAK, MAXLAB, LABS, NLABS,
     :                      STATUS )
*+
*  Name:
*     IRA1_POTL

*  Purpose:
*     Store end label information.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA1_POTL( NC, AB, XLO, YLO, XHI, YHI, MAXBRK, BREAK,
*                     VBREAK, NBREAK, MAXLAB, LABS, NLABS, STATUS )

*  Description:
*     "End" labels are placed at the start or end of the meridian or
*     parallel. The left edge of the image is reserved for latitude
*     labels, and the bottom edge for longitude labels. Therefore a
*     meridian cannot be end labeled if it intersects the left or right
*     edges. Similarly, a parallel cannot be end labelled if it
*     intersects the top or bottom edges. If parallels or meridians
*     start or end well away from any edge, then an end label can be
*     placed there.

*  Arguments:
*     NC = INTEGER (Given)
*        Axis index, 1 if AB is a longitude value, 2 if it is a latitude
*        value.
*     AB = DOUBLE PRECISION (Given)
*        The latitude or longitude value to label.
*     XLO = REAL (Given)
*        The low X limit of the plotting space.
*     YLO = REAL (Given)
*        The low Y limit of the plotting space.
*     XHI = REAL (Given)
*        The high X limit of the plotting space.
*     YHI = REAL (Given)
*        The high Y limit of the plotting space.
*     MAXBRK = INTEGER (Given)
*        The size of the BREAK and VBREAK arrays.
*     BREAK = INTEGER (Given)
*        The number of breaks in the meridian or parallel.
*     VBREAK( 2, MAXBRK ) = REAL (Given)
*        The unit direction vector back along the curve at each break.
*     BREAK( 2, MAXBRK ) = REAL (Given)
*        The plotted coordinates of each break in the curve.
*     MAXLAB = INTEGER (Given)
*        Size of the LABS array.
*     LABS( MAXLAB, 5 ) = DOUBLE PRECISION (Given and Returned)
*        The array into which the label information is added if an
*        "end" label can be produced.
*     NLABS = INTEGER( Given and Returned)
*        The number of labels stored in LABS.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-MAR-1992 (DSB):
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
      INTEGER NC
      DOUBLE PRECISION AB
      REAL XLO
      REAL YLO
      REAL XHI
      REAL YHI
      INTEGER MAXBRK
      REAL BREAK( 2, MAXBRK )
      REAL VBREAK( 2, MAXBRK )
      INTEGER NBREAK
      INTEGER MAXLAB

*  Arguments Given and Returned:
      DOUBLE PRECISION LABS( MAXLAB, 5 )
      INTEGER NLABS

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL EB
      LOGICAL EL
      LOGICAL ER
      LOGICAL ET
      INTEGER INDEXL
      LOGICAL LIN
      INTEGER LINDEX
      LOGICAL RIN
      INTEGER RINDEX
      LOGICAL SB
      LOGICAL SL
      LOGICAL SR
      LOGICAL ST
      REAL TOL
      REAL VX( 2 )
      REAL VY( 2 )
      REAL X( 2 )
      REAL Y( 2 )

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If fewer than two breaks occurred in the curve, no label
*  positions are saved.
      IF( NBREAK .LT. 2 ) GO TO 999

*  Decide on a tolerance for plotting positions.
      TOL = 0.001*MAX( XHI - XLO, YHI - YLO )

*  Extend the curve slightly in both directions, to produce modified
*  start and end positions for the curve.
      X( 1 ) = BREAK( 1, 1 ) - 3.0*TOL*VBREAK( 1, 1 )
      Y( 1 ) = BREAK( 2, 1 ) - 3.0*TOL*VBREAK( 2, 1 )
      VX( 1 ) = VBREAK( 1, 1 )
      VY( 1 ) = VBREAK( 2, 1 )

      X( 2 ) = BREAK( 1, NBREAK ) - 3.0*TOL*VBREAK( 1, NBREAK )
      Y( 2 ) = BREAK( 2, NBREAK ) - 3.0*TOL*VBREAK( 2, NBREAK )
      VX( 2 ) = VBREAK( 1, NBREAK )
      VY( 2 ) = VBREAK( 2, NBREAK )

*  Set up flags indicating if the first or last break is on any of the
*  edges of plotting space.
      SL = .FALSE.
      SR = .FALSE.
      ST = .FALSE.
      SB = .FALSE.

      EL = .FALSE.
      ER = .FALSE.
      ET = .FALSE.
      EB = .FALSE.

      IF( X( 1 ) .LE. XLO + TOL ) SL = .TRUE.
      IF( X( 1 ) .GE. XHI - TOL ) SR = .TRUE.
      IF( Y( 1 ) .GE. YHI - TOL ) ST = .TRUE.
      IF( Y( 1 ) .LE. YLO + TOL ) SB = .TRUE.

      IF( X( 2 ) .LE. XLO + TOL ) EL = .TRUE.
      IF( X( 2 ) .GE. XHI - TOL ) ER = .TRUE.
      IF( Y( 2 ) .GE. YHI - TOL ) ET = .TRUE.
      IF( Y( 2 ) .LE. YLO + TOL ) EB = .TRUE.

*  Set flags determining if the left and right hand ends of the line
*  are away from all edges.
      IF( X( 1 ) .LE. X( 2 ) ) THEN
         LIN = .NOT. ( SL .OR. SR .OR. ST .OR. SB )
         RIN = .NOT. ( EL .OR. ER .OR. ET .OR. EB )
         LINDEX = 1
         RINDEX = 2

      ELSE
         RIN = .NOT. ( SL .OR. SR .OR. ST .OR. SB )
         LIN = .NOT. ( EL .OR. ER .OR. ET .OR. EB )
         LINDEX = 2
         RINDEX = 1

      END IF

*  First deal with meridians.
      INDEXL = 0

      IF( NC .EQ. 1 ) THEN

*  See if this meridian started on the bottom edge of the plotting
*  zone. If it did, save the image coordinates, vector and longitude at
*  the break as a label position.
         IF( SB ) THEN
            INDEXL = 1

*  See if this meridian ended on the bottom edge of the plotting
*  zone. If it did, save the image coordinates, vector and longitude at
*  the break as a label position.
         ELSE IF( EB ) THEN
            INDEXL = 2

         END IF

*  Now deal with parallels.
      ELSE

*  See if this parallel started on the left edge of the plotting
*  zone. If it did, save the image coordinates, vector and lattude at
*  the break as a label position.
         IF( SL ) THEN
            INDEXL = 1

*  See if this parallel ended on the left edge of the plotting
*  zone. If it did, save the image coordinates, vector and latitude at
*  the break as a label position.
         ELSE IF( EL ) THEN
            INDEXL = 2

         END IF

      END IF

*  Now deal with cases where the ends of the curve are not on any edge.
*  See if the left end of this curve is away from all edges of the
*  plotting zone. If it is, save the image coordinates, vector and
*  longitude or latitude at the break as a label position.
      IF( INDEXL .EQ. 0 ) THEN

         IF( LIN ) THEN
            INDEXL = LINDEX

*  If the left end is on an edge, see if the right end is away from all
*  edges of the plotting zone. If it is, save the image coordinates,
*  vector and longitude or latitude at the break as a label position.
         ELSE IF( RIN ) THEN
            INDEXL = RINDEX

         END IF

      END IF

*  If a usable label position was found, save it as a label position .
      IF( INDEXL .NE. 0 ) THEN
         NLABS = NLABS + 1

         IF( NLABS .GT. MAXLAB ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'IRA1_POTL_ERR1',
     :           'IRA1_POTL: Too many parallels or meridians to label.',
     :                    STATUS )
            GO TO 999
         END IF

         LABS( NLABS, 1 ) = AB
         LABS( NLABS, 2 ) = DBLE( X( INDEXL ) )
         LABS( NLABS, 3 ) = DBLE( Y( INDEXL ) )
         LABS( NLABS, 4 ) = DBLE( VX( INDEXL ) )
         LABS( NLABS, 5 ) = DBLE( VY( INDEXL ) )

      END IF

 999  CONTINUE

      END
