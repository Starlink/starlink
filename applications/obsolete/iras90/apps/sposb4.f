      SUBROUTINE SPOSB4( X1, Y1, SCS, IDA, ANGLE, PIXSIZ, STATUS )
*+
*  Name:
*     SPOSB4

*  Purpose:
*     Find accurate orientation and pixel dimensions at a given
*     position.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPOSB4( X1, Y1, SCS, IDA, ANGLE, PIXSIZ, STATUS )

*  Description:
*     The sky coordinates at two adjacent points are found, one on the X
*     axis and one on the Y axis. The pixel dimensons are then the
*     distances between these points and the original point (assuming an
*     offset of 1 pixel along each axis). The position angle of the Y
*     axis is also found. Bad values are returned if any of the supplied
*     values are bad, or if either of the two offset positions does not
*     have a corresponding sky position.

*  Arguments:
*     X1 = REAL (Given)
*        The X coordinate of the required position, in pixels.
*     Y1 = REAL (Given)
*        The Y coordinate of the required position, in pixels.
*     SCS = CHARACTER * ( * ) (Given)
*        The sky coordinate system.
*     IDA = INTEGER (Given)
*        IRA identifier for the astrometry information.
*     ANGLE = REAL (Returned)
*        Position angle of the Y axis at the required position, in
*        degrees.
*     PIXSIZ( 2 ) = REAL (Returned)
*        The X and Y dimensions of a pixel at the required position, in
*        radians.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     22-SEP-1993 (DSB):
*        Original version.
*     10-OCT-1993 (DSB):
*        Returned value of ANGLE forced into range 0 - 360 degrees.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants
      INCLUDE 'MSG_PAR'          ! MSG_ constants
      INCLUDE 'IRA_PAR'          ! IRA_ constants

*  Arguments Given:
      REAL X1
      REAL Y1
      CHARACTER SCS*(*)
      INTEGER IDA

*  Arguments Returned:
      REAL ANGLE
      REAL PIXSIZ( 2 )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      DOUBLE PRECISION SLA_DBEAR ! Bearing of two points.
      REAL SLA_RANORM            ! Convert angle to range 0 - 2.PI

*  Local Constants:
      DOUBLE PRECISION DELTA     ! Offset in pixels
      PARAMETER ( DELTA = 1.0D-1 )

*  Local Variables:
      DOUBLE PRECISION
     :         A1,               ! Longitude of supplied position
     :         A2,               ! Longitude of offset position
     :         B1,               ! Latitude of supplied position
     :         B2,               ! Latitude of offset position
     :         DIST,             ! Arc-distance between points
     :         X2,               ! X coord. of offset position
     :         Y2                ! Y coord. of offset position

*.

*  Initialise the returned values.
      ANGLE = VAL__BADR
      PIXSIZ( 1 ) = VAL__BADR
      PIXSIZ( 2 ) = VAL__BADR

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If either of the supplied values are invalid, return invalid
*  values.
      IF( X1 .EQ. VAL__BADR .OR. Y1 .EQ. VAL__BADR ) GO TO 10

*  Find the sky coordinates corresponding to the supplied image
*  coordinates.
      CALL IRA_TRANS( 1, DBLE( X1 ), DBLE( Y1 ), .TRUE., SCS, IDA, A1,
     :                B1, STATUS )

*  If corresponding sky coordinates are bad, return bad values.
      IF( A1 .EQ. VAL__BADD .OR. B1 .EQ. VAL__BADD ) GO TO 10

*  Find the sky coordinates at a position DELTA pixels above the
*  supplied position.
      X2 = DBLE( X1 )
      Y2 = DBLE( Y1 ) + DELTA
      CALL IRA_TRANS( 1, X2, Y2, .TRUE., SCS, IDA, A2, B2, STATUS )

*  If corresponding sky coordinates are bad, return bad values.
      IF( A2 .EQ. VAL__BADD .OR. B2 .EQ. VAL__BADD ) GO TO 10

*  Find the position angle of this new sky position from the supplied
*  position (in radians).
      ANGLE = REAL( SLA_DBEAR( A1, B1, A2, B2 ) )

*  Find the arc-distance between the two points.
      CALL IRA_DIST( A1, B1, A2, B2, DIST, STATUS )

*  This is the arc-distance covered by DELTA pixels. Normalise it to one
*  pixel.
      PIXSIZ( 2 ) = REAL( DIST/DELTA )

*  Find the sky coordinates at a position DELTA pixels to the right of
*  the supplied position.
      X2 = DBLE( X1 ) + DELTA
      Y2 = DBLE( Y1 )
      CALL IRA_TRANS( 1, X2, Y2, .TRUE., SCS, IDA, A2, B2, STATUS )

*  If corresponding sky coordinates are bad, return bad values.
      IF( A2 .EQ. VAL__BADD .OR. B2 .EQ. VAL__BADD ) GO TO 10

*  Find the arc-distance between the two points.
      CALL IRA_DIST( A1, B1, A2, B2, DIST, STATUS )

*  This is the arc-distance covered by DELTA pixels. Normalise it to one
*  pixel.
      PIXSIZ( 1 ) = REAL( DIST/DELTA )

*  Arrive here if any bad values have been generated.
  10  CONTINUE

*  Check position angle is good.
      IF( ANGLE .NE. VAL__BADR ) THEN

*  Convert to degrees, in range 0 - 360.
         ANGLE = SLA_RANORM( ANGLE )*IRA__RTOD 

*  Format the value rounding to 2 decimal places.
         CALL MSG_SETR( 'ANG', 0.01*REAL( NINT( ANGLE*100.0) ) )

*  Produce the message.
         CALL MSG_OUTIF( MSG__VERB, 'SPOSB4_MSG1', '    Y axis '//
     :                   'position angle: ^ANG degrees', STATUS )

*  Produce an alternative message if the value is bad.
      ELSE
         CALL MSG_OUTIF( MSG__VERB, 'SPOSB4_MSG2', '    Y axis '//
     :                   'position angle cannot be calculated.',
     :                   STATUS )
      END IF

*  Check both pixel dimensions are good.
      IF( PIXSIZ( 1 ) .NE. VAL__BADR .AND.
     :    PIXSIZ( 2 ) .NE. VAL__BADR ) THEN

*  Convert to arc-minutes.
         PIXSIZ( 1 ) = PIXSIZ( 1 )*IRA__R2AM
         PIXSIZ( 2 ) = PIXSIZ( 2 )*IRA__R2AM

*  Format the values rounding to 2 decimal places.
         CALL MSG_SETR( 'PX', 0.01*REAL(
     :                            NINT( PIXSIZ( 1 )*100.0) ) )
         CALL MSG_SETR( 'PY', 0.01*REAL(
     :                            NINT( PIXSIZ( 2 )*100.0) ) )

*  Produce the message.
         CALL MSG_OUTIF( MSG__VERB, 'SPOSB4_MSG3', '    Pixel '//
     :                   'dimensions     : (^PX,^PY) arc-minutes',
     :                   STATUS )

*  Produce an alternative message if the values are bad.
      ELSE
         CALL MSG_OUTIF( MSG__VERB, 'SPOSB4_MSG4', '    Pixel '//
     :                   'dimensions cannot be calculated.', STATUS )
      END IF

      CALL MSG_BLANKIF( MSG__VERB, STATUS )

      END
