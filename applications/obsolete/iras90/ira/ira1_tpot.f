      SUBROUTINE IRA1_TPOT( NC, XLO, YLO, XHI, YHI, MAXBRK, BREAK,
     :                      VBREAK, NBREAK, MAXTIC, TICKS, NTICKS,
     :                      STATUS )
*+
*
*  Name:
*     IRA1_TPOT

*  Purpose:
*     Store tick mark information.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA1_TPOT( NC, XLO, YLO, XHI, YHI, MAXBRK, BREAK, VBREAK,
*                     NBREAK, MAXTIC, TICKS, NTICKS, STATUS )

*  Description:
*     Tick marks are placed at the start and end of the meridian or
*     parallel. The left edge of the image is reserved for latitude
*     tick marks, and the bottom edge for longitude tick marks.
*     Therefore a meridian tick mark cannot be produced if it
*     intersects the left or right edges. Similarly, a parallel tick
*     mark cannot be produced if it intersects the top or bottom edges.
*     If parallels or meridians start or end well away from any edge,
*     then tick marks are placed there.

*  Arguments:
*     NC = INTEGER (Given)
*        Axis index, 1 if a longitude tick is to be stored, 2 if a
*        latitude tick is to be stored.
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
*     MAXTIC = INTEGER (Given)
*        Size of the TICKS array.
*     TICKS( MAXTIC, 4 ) = DOUBLE PRECISION (Given and Returned)
*        The array into which the tick mark information is added if
*        tick marks can be produced.
*     NTICKS = INTEGER( Given and Returned)
*        The number of tick marks stored in LABS.
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
      REAL XLO
      REAL YLO
      REAL XHI
      REAL YHI
      INTEGER MAXBRK
      REAL BREAK( 2, MAXBRK )
      REAL VBREAK( 2, MAXBRK )
      INTEGER NBREAK
      INTEGER MAXTIC

*  Arguments Given and Returned:
      DOUBLE PRECISION TICKS( MAXTIC, 4 )
      INTEGER NTICKS

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL EB
      LOGICAL EL
      LOGICAL ER
      LOGICAL ET
      LOGICAL SAVEEN
      LOGICAL SAVEST
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

*  If fewer than two breaks occurred in the curve, no  tick
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

*  Initialise save flags.
      SAVEST = .FALSE.
      SAVEEN = .FALSE.

*  Deal first with meridians.
      IF( NC .EQ. 1 ) THEN

*  Unless this meridian started on the left or right edge of the
*  plotting zone, save the image coordinates and vector at the start as
*  a tick position.
         IF( .NOT. ( SR .OR. SL ) ) SAVEST = .TRUE.

*  Unless this meridian ended on the left or right edge of the
*  plotting zone, save the image coordinates and vector at the end as
*  a tick position.
         IF( .NOT. ( ER .OR. EL ) ) SAVEEN = .TRUE.

*  Now deal with parallels.
      ELSE

*  Unless this parallel started on the top or bottom edge of the
*  plotting zone, save the image coordinates and vector at the start as
*  a tick position.
         IF( .NOT. ( ST .OR. SB ) ) SAVEST = .TRUE.

*  Unless this parallel ended on the top or bottom edge of the
*  plotting zone, save the image coordinates and vector at the end as
*  a tick position.
         IF( .NOT. ( ET .OR. EB ) ) SAVEEN = .TRUE.

      END IF

*  If the start was a usable tick position, save it.
      IF( SAVEST ) THEN
         NTICKS = NTICKS + 1

         IF( NTICKS .GT. MAXTIC ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'IRA1_TPOT_ERR1',
     :            'IRA1_TPOT: Too many parallels or meridians to tick.',
     :                    STATUS )
            GO TO 999
         END IF

         TICKS( NTICKS, 1 ) = DBLE( X( 1 ) )
         TICKS( NTICKS, 2 ) = DBLE( Y( 1 ) )
         TICKS( NTICKS, 3 ) = DBLE( VX( 1 ) )
         TICKS( NTICKS, 4 ) = DBLE( VY( 1 ) )

      END IF

*  If the end was a usable tick position, save it.
      IF( SAVEEN ) THEN
         NTICKS = NTICKS + 1

         IF( NTICKS .GT. MAXTIC ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'IRA1_TPOT_ERR2',
     :            'IRA1_TPOT: Too many parallels or meridians to tick.',
     :                    STATUS )
            GO TO 999
         END IF

         TICKS( NTICKS, 1 ) = DBLE( X( 2 ) )
         TICKS( NTICKS, 2 ) = DBLE( Y( 2 ) )
         TICKS( NTICKS, 3 ) = DBLE( VX( 2 ) )
         TICKS( NTICKS, 4 ) = DBLE( VY( 2 ) )

      END IF

 999  CONTINUE

      END
