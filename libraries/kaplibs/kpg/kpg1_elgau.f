      SUBROUTINE KPG1_ELGAU( SIG, SIG0, AXISR, THETA, STATUS )
*+
*  Name:
*     KPG1_ELGAU

*  Purpose:
*     Calculates the axis ratio, inclination and minor-axis width of a
*     star image, given the Gaussian widths of marginal profiles at
*     45-degree intervals.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_ELGAU( SIG, SIG0, AXISR, THETA, STATUS )

*  Description:
*     The reciprocal of the square of the width varies approximately
*     like the length of an ellipse diameter as the angle of projection
*     varies.  The routine calculates the required parameters assuming
*     this relationship holds, then iterates, calculating the expected
*     deviation from this law and subtracting it from the data before
*     calculating a new estimate.  The solution of the ellipse
*     equations is analogous to using the Stokes parameters of linear
*     polarization to find the ellipse parameters.

*  Arguments:
*     SIG( 4 ) = REAL (Given)
*        The Gaussian widths of the marginal profiles of the star in
*        directions at 0, 45, 90 and 135 degrees to the x axis.
*     SIG0 = REAL (Returned)
*        The width of the minor axis of the ellipse.
*     AXISR = REAL (Returned)
*        The axis ratio of the ellipse.
*     THETA = REAL (Returned)
*        The inclination of the major axis to the x axis in radians
*        (x through y positive).
*     STATUS = INTEGER (Returned)
*        The global status.

*  Notes:
*     The routine assumes a Gaussian profile for the star.

*  Copyright:
*     Copyright (C) 1981, 1990 Science & Engineering Research Council.
*     Copyright (C) 2009 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     RFWS: R.F. Warren-Smith (Durham Univ.)
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1981 (RFWS):
*        Original version.
*     1990 September 18 (MJC):
*        Renamed from ELLIPS, re-ordered arguments, added status,
*        commented the variables, tidied, and converted the prologue.
*     28-APR-2009 (DSB):
*        Renamed from kps1_elgau to kpg1_elgau.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      REAL
     : SIG( 4 )

*  Arguments Returned:
      REAL
     :  SIG0,
     :  AXISR,
     :  THETA

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER NITER              ! Number of iterations
      PARAMETER ( NITER = 10 )
      REAL TOLLA                 ! Tolerance for the minor-axis width
      PARAMETER ( TOLLA = 1.0E-4 )
      REAL TOLLS                 ! Tolerance for the axis ratio
      PARAMETER ( TOLLS = 1.0E-4 )
      REAL TOLLT                 ! Tolerance for the inclination
      PARAMETER ( TOLLT = 1.0E-2 )
      REAL D2R                   ! Degrees to radians
      PARAMETER ( D2R = 0.0174533 )
      REAL T45                   ! 45 degrees in radians
      PARAMETER ( T45 = D2R * 45.0 )
      REAL T90                   ! 90 degrees in radians
      PARAMETER ( T90 = D2R * 90.0 )
      REAL T135                   ! 135 degrees in radians
      PARAMETER ( T135 = D2R * 135.0 )

*  Local Variables:
      REAL
     :  AXISR2,                  ! Current estimate of the axis ratio
                                 ! squared
     :  AXISRN,                  ! New estimate of the axis ratio
     :  C,                       ! Cosine of the current angle
     :  DAXISR,                  ! Difference between the new and old
                                 ! estimates of the axis ratio
     :  DSIG0,                   ! Difference between the new and old
                                 ! estimates of the minor-axis width
     :  DTHETA,                  ! Difference between the new and old
                                 ! estimates of the inclination
     :  RSIG02,                  ! Reciprocal squared of the current
                                 ! estimate of the minor-axis width
     :  RSIG2( 4 ),              ! Reciprocal squared of the marginal-
                                 ! profile widths
     :  S,                       ! Sine of the current angle
     :  SIG0N,                   ! New estimate of the minor-axis width
     :  T( 4 ),                  ! Orientations of 45-degree profiles
                                 ! with respect to the ! major axis
     :  THETAN                   ! New estimate of the inclination of
                                 ! the major axis

      DOUBLE PRECISION
     :  AMP1,                    ! Mean radius
     :  AMP2,                    ! Mean deviation
     :  D( 4 ),                  ! Deviations
     :  DELQ,                    ! Normal deviation difference
     :  DELU                     ! Normal deviation difference at 45
                                 ! degrees to the major axis

      INTEGER
     :  I,                       ! Loop counter for the four profiles
     :  ITER                     ! Iteration counter

*.

*    Check inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Work with the reciprocals of the widths squared... these vary
*    approximately elliptically with inclination angle.

      DO I = 1, 4
         RSIG2( I ) = ( 1.0 / SIG( I ) ) *  * 2
      END DO

*    Set the initial estimates of ellipse parameters.

      SIG0 = 1.0
      AXISR = 1.0
      THETA = 0.0

*    Perform iterations to find the accurate parameters.
*    ===================================================

      DO ITER = 1, NITER
         RSIG02 = ( 1.0 / SIG0 ) ** 2
         AXISR2 = AXISR ** 2

*       Define the four orientations.

         T( 1 ) = THETA
         T( 2 ) = THETA - T45
         T( 3 ) = THETA - T90
         T( 4 ) = THETA - T135

*       Make a correction to the data values which is the amount by
*       which they would deviate from a pure elliptical variation given
*       the current ellipse parameters.

         DO I = 1, 4
            C = COS( T( I ) )
            S = SIN( T( I ) )
            D( I ) = RSIG02 * ( ( C * S * ( AXISR2 - 1.0 ) ) ** 2 )
     :              / ( AXISR2 * ( ( AXISR2 * C * C ) + ( S * S ) ) )
            D( I ) = D( I ) + RSIG2( I )
         END DO

*       Now find the ellipse parameters assuming the data varies
*       elliptically.

         DELQ = D( 3 ) - D( 1 )
         DELU = D( 4 ) - D( 2 )
         AMP1 = 0.5D0 * SQRT( DELQ ** 2 + DELU ** 2 )
         AMP2 = 0.25D0 * ( D( 1 ) + D( 2 ) + D( 3 ) + D( 4 ) )
         AMP1 = MIN( AMP1, 0.9999D0 * AMP2 )

         IF ( DELQ .EQ. 0.0D0 .AND. DELU .EQ. 0.0D0 ) THEN
            THETAN = 0.0

         ELSE
            THETAN = REAL( 0.5D0 * ATAN2( DELU, DELQ ) )
         END IF

         RSIG02 = REAL( AMP1 + AMP2 )
         SIG0N = SQRT( 1.0 / MAX( RSIG02, 1.0E-10 ) )
         AXISRN = REAL( SQRT( ( AMP1 + AMP2 ) / ( AMP2 - AMP1 ) ) )

*       Calculate the changes to the parameters.

         DSIG0 = ABS( SIG0N - SIG0 )
         DAXISR = ABS( AXISRN - AXISR )
         DTHETA = ABS( THETAN - THETA )
         SIG0 = SIG0N
         AXISR = AXISRN
         THETA = THETAN

*       If the accuracy criteria are met, exit from the iteration loop.

         IF ( DSIG0 .LE. TOLLS .AND. DAXISR .LE. TOLLA .AND.
     :        DTHETA .LE. TOLLT ) GO TO 100

      END DO

  100 CONTINUE

      END
