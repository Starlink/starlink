      SUBROUTINE IRA1_SHCAL( A0, B0, ANGLE, R0, R3, STATUS )
*+
*  Name:
*     IRA1_SHCAL

*  Purpose:
*     Calculate vectors required by IRA_SHIFT.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA1_SHCAL( A0, B0, ANGLE, R0, R3, STATUS )

*  Description:
*     This routine calculates the 3-vector R0, representing the given
*     sky position (A0,B0), and the 3-vector R3, representing the sky
*     position which is 90 degrees away from R0, along a great circle
*     passing through R0 at a position angle given by ANGLE. Each
*     3-vector holds Cartesian (X,Y,Z) values with origin at the centre
*     of the celestial sphere. The XY plane is the "equator", the Z
*     axis is in the direction of the "north pole", X is towards zero
*     longitude (A=0), and Y is towards longitude 90 degrees.
*
*     No checks are made for BAD (=VAL__BADD) input values.

*  Arguments:
*     A0 = DOUBLE PRECISION (Given)
*        The sky longitude of the given position, in radians.
*     B0 = DOUBLE PRECISION (Given)
*        The sky latitude of the given position, in radians.
*     ANGLE = DOUBLE PRECISION (Given)
*        The position angle of a great circle passing through the given
*        position.  That is, the angle from north to the required
*        direction, in radians. Positive angles are in the sense of
*        rotation from north to east.
*     R0( 3 ) = DOUBLE PRECISION (Returned)
*        3-vector R0. See above.
*     R3( 3 ) = DOUBLE PRECISION (Returned)
*        3-vector R3. See above.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-MAY-1991 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'PRM_PAR'          ! Starlink data constants.
      INCLUDE 'IRA_PAR'          ! IRA constants.
      INCLUDE 'IRA_ERR'          ! IRA errors

*  Arguments Given:
      DOUBLE PRECISION A0
      DOUBLE PRECISION B0
      DOUBLE PRECISION ANGLE

*  Arguments Returned:
      DOUBLE PRECISION R0( 3 )
      DOUBLE PRECISION R3( 3 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION COSA0     ! Cosine of A0.
      DOUBLE PRECISION COSB0     ! Cosine of B0.
      DOUBLE PRECISION COSPA     ! Cosine of ANGLE
      DOUBLE PRECISION R1(3)     ! Vector PI/2 away from R0 in meridian
                                 ! of R0.
      DOUBLE PRECISION R2(3)     ! Vector PI/2 away from R0 on equator.
      DOUBLE PRECISION SINPA     ! Sine of ANGLE
      DOUBLE PRECISION SINA0     ! Sine of A0.
      DOUBLE PRECISION SINB0     ! Sine of B0.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Store commonly used values.
      SINA0 = SIN( A0 )
      COSA0 = COS( A0 )
      SINB0 = SIN( B0 )
      COSB0 = COS( B0 )
      SINPA = SIN( ANGLE )
      COSPA = COS( ANGLE )

*  Create the vector R0 representing the given point. The XY plane
*  defines zero latitude, Z is in the direction of increasing latitude,
*  X is towards zero longitude, and Y is towards longitude 90 degrees.
      R0(1) =  COSB0*COSA0
      R0(2) =  COSB0*SINA0
      R0(3) =  SINB0

*  Create the vector R1 representing the point in the meridian of the
*  given point which has latitude 90 degrees greater than the
*  given point.
      R1(1) = -SINB0*COSA0
      R1(2) = -SINB0*SINA0
      R1(3) =  COSB0

*  Create the vector R2 representing the point on the equator (i.e. a
*  latitude of zero), which has a longitude 90 degrees to the west of
*  the given point.
      R2(1) = -SINA0
      R2(2) =  COSA0
      R2(3) =  0.0D0

*  Create the vector R3 representing the point which is 90 degrees away
*  from the given point, along the required great circle.
      R3(1) =  COSPA*R1(1) + SINPA*R2(1)
      R3(2) =  COSPA*R1(2) + SINPA*R2(2)
      R3(3) =  COSPA*R1(3) + SINPA*R2(3)

      END
