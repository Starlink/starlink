      SUBROUTINE IRA1_IDST2( A0, B0, ANGLE, A1, B1, PARDST, PRPDST,
     :                       STATUS )
*+
*  Name:
*     IRA1_IDST2

*  Purpose:
*     Resolve a sky position into distances parallel and perpendicular
*     to a given great circle.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA1_IDST2( A0, B0, ANGLE, A1, B1, PARDST, PRPDST, STATUS )

*  Description:
*     This routine provides the functionality of routine IRA_DIST2 but
*     without argument verification.

*  Arguments:
*     A0 = DOUBLE PRECISION (Given)
*        The sky longitude at the reference point, in radians.
*     B0 = DOUBLE PRECISION (Given)
*        The sky latitude at the reference point, in radians.
*     ANGLE = DOUBLE PRECISION (Given)
*        The position angle of the great circle as seen from the
*        reference point. That is, the angle from the north to the
*        required direction, in radians. Positive angles are in the
*        sense of rotation from north to east.
*     A1 = DOUBLE PRECISION (Given)
*        The sky longitude of the outlying point, in radians.
*     B1 = DOUBLE PRECISION (Given)
*        The sky latitude of the outlying point, in radians.
*     PARDST = DOUBLE PRECISION (Returned)
*        The arc-distance (in radians) from the reference point, to the
*        point of closest approach on the great circle defined by the
*        reference point and the position angle . If any of the input
*        coordinate values are equal to the Starlink "BAD" value
*        (VAL__BADD), then PRPDST is returned with the BAD value.
*     PRPDST = DOUBLE PRECISION (Returned)
*        The arc-distance (in radians) of the outlying point, from the
*        point of closest approach on the great circle defined by the
*        reference point and the position angle . If any of the input
*        coordinate values are equal to the Starlink "BAD" value
*        (VAL__BADD), then PRPDST is returned with the BAD value.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     4-FEB-1991 (DSB):
*        Original version.
*     26-APR-1991 (DSB):
*        Updated for second version of IRA
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants.
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'PRM_PAR'          ! Starlink data constants.
      INCLUDE 'IRA_PAR'          ! IRA constants.

*  Arguments Given:
      DOUBLE PRECISION A0
      DOUBLE PRECISION B0
      DOUBLE PRECISION ANGLE
      DOUBLE PRECISION A1
      DOUBLE PRECISION B1

*  Arguments Returned:
      DOUBLE PRECISION PARDST
      DOUBLE PRECISION PRPDST

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION  COSA0    ! = COS( A0 )
      DOUBLE PRECISION  COSA1    ! = COS( A1 )
      DOUBLE PRECISION  COSB     ! = COS( ANGLE )
      DOUBLE PRECISION  COSD0    ! = COS( B0 )
      DOUBLE PRECISION  COSD1    ! = COS( B1 )
      DOUBLE PRECISION  COSPRP   ! = COS( PRPDST )
      DOUBLE PRECISION  NUNIT(3) ! Unit normal to the plane containing
                                 ! the great circle.
      DOUBLE PRECISION  R0(3)    ! Vector giving supplied reference
                                 ! point.
      DOUBLE PRECISION  R3(3)    ! Vector giving point on great circle
                                 ! 90 degrees away from reference point.
      DOUBLE PRECISION  R4(3)    ! Vector giving closest point of
                                 ! approach of the great circle to the
                                 ! supplied outlying point.
      DOUBLE PRECISION  R5(3)    ! Vector giving supplied outlying point
      DOUBLE PRECISION  SINA0    ! = SIN( A0 )
      DOUBLE PRECISION  SINA1    ! = SIN( A1 )
      DOUBLE PRECISION  SINB     ! = SIN( ANGLE )
      DOUBLE PRECISION  SIND0    ! = SIN( B0 )
      DOUBLE PRECISION  SIND1    ! = SIN( B1 )
      DOUBLE PRECISION  SINPRP   ! = SIN( PRPDST )
      DOUBLE PRECISION  SLA_DVDV ! SLALIB function giving vector dot
                                 ! product.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Calculate common values.
      SINB = SIN( ANGLE )
      COSB = COS( ANGLE )
      SIND0 = SIN( B0 )
      COSD0 = COS( B0 )
      SINA0 = SIN( A0 )
      COSA0 = COS( A0 )
      SIND1 = SIN( B1 )
      COSD1 = COS( B1 )
      SINA1 = SIN( A1 )
      COSA1 = COS( A1 )

*  Find the 3D Cartesian vectors representing the reference point (R0)
*  and the outlying point (R5).
      R0(1) = COSD0*COSA0
      R0(2) = COSD0*SINA0
      R0(3) = SIND0

      R5(1) = COSD1*COSA1
      R5(2) = COSD1*SINA1
      R5(3) = SIND1

*  Find the vector giving the position of the point which is 90 degrees
*  away from the reference point, along the great circle with the given
*  position angle.
      R3(1) = -COSB*SIND0*COSA0 - SINB*SINA0
      R3(2) = -COSB*SIND0*SINA0 + SINB*COSA0
      R3(3) =  COSB*COSD0

*  Take the cross product of this vector with the vector corresponding
*  to the reference position, to get the unit normal to the plane
*  containing the great circle.
      CALL SLA_DVXV( R3, R0, NUNIT )

*  The angle between the normal and the given outlying point is 90
*  degrees minus the angle between the outlying point and the plane
*  (the required distance).  Therefore take the dot product of the
*  normal and the outlying position to get the sine of the required
*  distance.
      SINPRP = SLA_DVDV( NUNIT, R5 )
      PRPDST = ASIN( SINPRP )

*  Find R4, the vector corresponding to the position of the closest
*  approach of the great circle to the outlying point.
      COSPRP = COS( PRPDST )

      IF( COSPRP .NE. 0.0 ) THEN
         R4(1) = ( R5(1) - SINPRP*NUNIT(1) )/COSPRP
         R4(2) = ( R5(2) - SINPRP*NUNIT(2) )/COSPRP
         R4(3) = ( R5(3) - SINPRP*NUNIT(3) )/COSPRP

*  R4 can be thought of as a linear combination of R0 and R3, where the
*  coefficients are COS(PARDST) and SIN(PARDST) respectively.  Taking
*  the dot product of R4 with R0 or R3 will thus give COS(PARDST) or
*  SIN(PARDST). Use this to evaluate PARDST.
         PARDST = ATAN2( SLA_DVDV( R4, R3 ), SLA_DVDV( R4, R0 ) )

*  If the outlying point is 90 degrees away from the great circle, the
*  parallel distance (PARDST) is undefined. Set it arbitrarily to zero.
      ELSE
         PARDST = 0.0

      END IF

 999  CONTINUE

      END
