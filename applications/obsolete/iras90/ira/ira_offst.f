      SUBROUTINE IRA_OFFST( A0, B0, A1, B1, DIST, A2, B2, STATUS )
*+
*  Name:
*     IRA_OFFST

*  Purpose:
*     Find a sky position which is offset towards a given point.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA_OFFST( A0, B0, A1, B1, DIST, A2, B2, STATUS )

*  Description:
*     This routine finds the sky position which is offset away from a
*     specified "reference" position by a given arc distance, along the
*     great circle joining the reference position with a specified
*     outlying sky position. If any of the input values have the
*     Starlink "BAD" value (VAL__BADD) then both output coordinate
*     values will also be bad.

*  Arguments:
*     A0 = DOUBLE PRECISION (Given)
*        The sky longitude of the reference position, in radians.
*     B0 = DOUBLE PRECISION (Given)
*        The sky latitude of the reference position, in radians.
*     A1 = DOUBLE PRECISION (Given)
*        The sky longitude of the outlying position, in radians.
*     B1 = DOUBLE PRECISION (Given)
*        The sky longitude of the outlying position, in radians.
*     DIST = DOUBLE PRECISION (Given)
*        The arc distance to move away from the reference position,
*        towards the outlying position, in radians.
*     A2 = DOUBLE PRECISION (Returned)
*        The sky longitude of the position which is the given arc
*        distance away from the reference position in the direction of
*        the outlying position, in radians.
*     B2 = DOUBLE PRECISION (Returned)
*        The sky latitude of the position which is the given arc
*        distance away from the reference position in the direction of
*        the outlying position, in radians.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*
*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     20-SEP-1990 (DSB):
*        Original version.
*     27-APR-1991 (DSB):
*        Modified for IRA version 2.
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
      DOUBLE PRECISION A1
      DOUBLE PRECISION B1
      DOUBLE PRECISION DIST

*  Arguments Returned:
      DOUBLE PRECISION A2
      DOUBLE PRECISION B2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION ALPHA     ! Coefficient of first 3-vector.
      DOUBLE PRECISION BETA      ! Coefficient of second 3-vector.
      DOUBLE PRECISION COSB0     ! Cosine of B0
      DOUBLE PRECISION COSB1     ! Cosine of B1
      DOUBLE PRECISION D01       ! Angle between the two given points.
      DOUBLE PRECISION R0X       ! X component of reference 3-vector.
      DOUBLE PRECISION R0Y       ! Y component of reference 3-vector.
      DOUBLE PRECISION R0Z       ! Z component of reference 3-vector.
      DOUBLE PRECISION R1X       ! X component of outlying 3-vector.
      DOUBLE PRECISION R1Y       ! Y component of outlying 3-vector.
      DOUBLE PRECISION R1Z       ! Z component of outlying 3-vector.
      DOUBLE PRECISION R2X       ! X component of resultant 3-vector.
      DOUBLE PRECISION R2Y       ! Y component of resultant 3-vector.
      DOUBLE PRECISION R2Z       ! Z component of resultant 3-vector.
      DOUBLE PRECISION SIND01    ! Sine of D01.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check the input values are not bad.
      IF ( A0 .EQ. VAL__BADD .OR. B0 .EQ. VAL__BADD .OR.
     :     A1 .EQ. VAL__BADD .OR. B1 .EQ. VAL__BADD .OR.
     :     DIST .EQ. VAL__BADD ) THEN
         A2 = VAL__BADD
         B2 = VAL__BADD
         GO TO 999
      END IF

*  Calculate the 3D Cartesian vector corresponding to the reference
*  point.
      COSB0 = COS( B0 )

      R0X = COSB0*COS( A0 )
      R0Y = COSB0*SIN( A0 )
      R0Z = SIN( B0 )

*  Calculate the 3D Cartesian vector corresponding to the outlying
*  point.
      COSB1 = COS( B1 )

      R1X = COSB1*COS( A1 )
      R1Y = COSB1*SIN( A1 )
      R1Z = SIN( B1 )

*  Calculate the angle between the two given points.
      D01 = ACOS( R0X*R1X + R0Y*R1Y + R0Z*R1Z )

*  The 3-vector of the required point is a linear sum of the two input
*  3-vectors. Calculate the coefficients, alpha and beta, of the first
*  and second points which give the corect angle between the first
*  3-vector and the resultant 3-vector.
      SIND01 = SIN( D01 )
      IF( SIND01 .NE. 0.0 ) THEN
         ALPHA = SIN( D01 - DIST )/SIND01
         BETA = SIN( DIST )/SIND01

*  If the two given points are co-incident or diametrically opposite,
*  the resultant point is not defined. Give an error message.
      ELSE
         STATUS = IRA__SING
         CALL ERR_REP( 'IRA_OFFST_ERR1',
     :                 'IRA_OFFST: Given points are co-linear',
     :                 STATUS )
         GO TO 999
      END IF

*  Form the three components of the resultant 3-vector.
      R2X = ALPHA*R0X + BETA*R1X
      R2Y = ALPHA*R0Y + BETA*R1Y
      R2Z = ALPHA*R0Z + BETA*R1Z

*  Calculate the latitude of the point represented by this 3-vector.
      B2 = ASIN( MAX( -1.0D0, MIN( 1.0D0, R2Z ) ) )

*  Calculate the longitude of the point represented by this 3-vector. If
*  the resultant point is the north or south pole, set the longitude
*  arbitrarily to the longitude of the first point.
      IF( R2X .NE. 0.0 .OR. R2Y .NE. 0.0 ) THEN
         A2 = ATAN2( R2Y, R2X )

      ELSE
         A2 = A0

      END IF

*  Ensure that the output values are in their first order range.
      CALL IRA_NORM( A2, B2, STATUS )

*  Give a contextual message.
 999  IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRA_OFFST_ERR2',
     :          'IRA_OFFST: Unable to calculate an offset sky position',
     :                 STATUS )
      END IF

      END
