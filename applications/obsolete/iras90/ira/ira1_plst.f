      SUBROUTINE IRA1_PLST( X0, Y0, X1, Y1, X2, Y2, X3, Y3, SQRTLM,
     :                      SHORT, DEVLIM, USEOLD, OLDVEC, PLOT,
     :                      ALLBAD, VEC, DELTAL, MAXDEV, STATUS )
*+
*  Name:
*     IRA1_PLST

*  Purpose:
*     See if a curve section can be drawn using 3 straight lines.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA1_PLST( X0, Y0, X1, Y1, X2, Y2, X3, Y3, SQRTLM, SHORT,
*                     DEVLIM, USEOLD, OLDVEC, PLOT, ALLBAD, VEC,
*                     DELTAL, MAXDEV, STATUS )

*  Description:
*     This routine checks to see if the curve joining the four supplied
*     points in image space can be reasonably represented by three
*     straight lines. The following criteria are used:
*
*     1) All four points must be valid.
*
*     2) The gaps between the points must be small enough to avoid any
*     danger of floating overflow when evaluating the distances between
*     them.
*
*     3) The angle between the line joing points 0 and 3 and the last
*     plotted line (which ended at point 0) must be less than 20
*     degrees.
*
*     4) The deviation of points 1 and 2 from the straight line through
*     points 0 and 3 must both be less than DEVLIM.
*
*     5) Points 1 and 2 must both lie on the section of the curve
*     between points 0 and 3.
*
*     6) At least one of points 1 and 2 must lie roughly half way
*     between points 0 and 3 (i.e. the mid points are not allowed to
*     cluster round the end points).
*
*     7) The section of the curve joining points 0 and 1 must be longer
*     than SHORT, unless the two intermediate points are close to the
*     two end points.

*  Arguments:
*     X0 = REAL (Given)
*        World X coordinate at point 0.
*     Y0 = REAL (Given)
*        World Y coordinate at point 0.
*     X1 = REAL (Given)
*        World X coordinate at point 1.
*     Y1 = REAL (Given)
*        World Y coordinate at point 1.
*     X2 = REAL (Given)
*        World X coordinate at point 2.
*     Y2 = REAL (Given)
*        World Y coordinate at point 2.
*     X3 = REAL (Given)
*        World X coordinate at point 3.
*     Y3 = REAL (Given)
*        World Y coordinate at point 3.
*     SQRTLM = REAL (Given)
*        The largest safe coordinate increment.
*     SHORT = REAL (Given)
*        The shortest acceptable line length.
*     DEVLIM = REAL (Given)
*        The maximum acceptable deviation from a straight line.
*     USEOLD = LOGICAL (Given)
*        True if OLDVEC contains the unit vector of the straight line
*        terminating at point 0.
*     OLDVEC( 2 ) = REAL(Given)
*        If USEOLD is true then OLDVEC should contain the unit vector
*        of the straight line terminating at point 0. Ignored
*        otherwise.
*     PLOT = LOGICAL (Returned)
*        True if straight lines can be used to represent the curve
*        joining points 0 and 1.
*     ALLBAD = LOGICAL (Returned)
*        True if all four supplied points were bad.
*     VEC( 2 ) = REAL (Returned)
*        The unit vector along the straight line joining points 0 and 3.
*        Undefined if PLOT is returned false.
*     DELTAL = REAL (Returned)
*        The length of the straight line joining points 0 and 3.
*        Undefined if PLOT is returned false.
*     MAXDEV = REAL (Returned)
*        The largest deviation of either point 1 or 2 from the straight
*        line through points 0 and 3.  Undefined if PLOT is returned
*        false.
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
      INCLUDE 'PRM_PAR'          ! Starlink data constants.

*  Arguments Given:
      REAL X0
      REAL Y0
      REAL X1
      REAL Y1
      REAL X2
      REAL Y2
      REAL X3
      REAL Y3
      REAL SQRTLM
      REAL SHORT
      REAL DEVLIM
      LOGICAL USEOLD
      REAL OLDVEC( 2 )

*  Arguments Returned:
      LOGICAL PLOT
      LOGICAL ALLBAD
      REAL VEC( 2 )
      REAL DELTAL
      REAL MAXDEV

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      REAL COSLIM                ! Criterion for lines being continuous.
      PARAMETER ( COSLIM = 0.9D0 )

*  Local Variables:
      LOGICAL          BAD0      ! True if point 0 is invalid.
      LOGICAL          BAD1      ! True if point 1 is invalid.
      LOGICAL          BAD2      ! True if point 2 is invalid.
      LOGICAL          BAD3      ! True if point 3 is invalid.
      REAL             COSANG    ! Cosine of angle between current line
                                 ! and previous line.
      REAL             D1        ! Distance from point 0 to closest
                                 ! approach to point 1.
      REAL             D2        ! Distance from point 0 to closest
                                 ! approach to point 2.
      REAL             DX        ! Increment in X between two ends of
                                 ! the current section, in pixels.
      REAL             DX1       ! Increment in X between point 0 and
                                 ! point 1, in pixels.
      REAL             DX2       ! Increment in X between point 0 and
                                 ! point 2, in pixels.
      REAL             DY        ! Increment in Y between two ends of
                                 ! the current section, in pixels.
      REAL             DY1       ! Increment in Y between point 0 and
                                 ! point 1, in pixels.
      REAL             DY2       ! Increment in Y between point 0 and
                                 ! point 2, in pixels.
      REAL             HALF      ! Half of current line.
      REAL             QUART     ! Quarter of current line.
      REAL             V1        ! Deviation of 1st intermediate point
                                 ! from line through the 2 section ends.
      REAL             V2        ! Deviation of 2nd intermediate point
                                 ! from line through the 2 section ends.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Store flags indicating if each point is valid.
      BAD0 = X0 .EQ. VAL__BADR .OR. Y0 .EQ. VAL__BADR
      BAD1 = X1 .EQ. VAL__BADR .OR. Y1 .EQ. VAL__BADR
      BAD2 = X2 .EQ. VAL__BADR .OR. Y2 .EQ. VAL__BADR
      BAD3 = X3 .EQ. VAL__BADR .OR. Y3 .EQ. VAL__BADR

*  If any of the points are invalid, then straight lines cannot be
*  used to draw the current section of the curve.
      IF( BAD0. OR. BAD1 .OR. BAD2 .OR. BAD3 ) THEN
         PLOT = .FALSE.

*  Set a flag if all four points are invalid.
         IF( BAD0 .AND. BAD1 .AND. BAD2 .AND. BAD3 ) THEN
            ALLBAD = .TRUE.
         ELSE
            ALLBAD = .FALSE.
         END IF

*  If it is still possible that a line can be plotted...
      ELSE
         ALLBAD = .FALSE.

*  Store increments between the two end points.
         DX = X3 - X0
         DY = Y3 - Y0

*  Store increments between the two intermediate points and the start
*  point.
         DX1 = X1 - X0
         DY1 = Y1 - Y0
         DX2 = X2 - X0
         DY2 = Y2 - Y0

*  If any of these increments is greater than the square root of the
*  maximum value storable in a _REAL, then there is a danger of
*  overflow. In this case the line cannot be plotted.
         IF( ABS( DX ) .GT. SQRTLM .OR. ABS( DY ) .GT. SQRTLM .OR.
     :       ABS( DX1 ) .GT. SQRTLM .OR. ABS( DY1 ) .GT. SQRTLM .OR.
     :       ABS( DX2 ) .GT. SQRTLM .OR. ABS( DY2 ) .GT. SQRTLM ) THEN
            PLOT = .FALSE.

         ELSE

*  Find the distance between points 0 and 3 in image coordinates.
            DELTAL = SQRT( DX**2 + DY**2 )

*  If the two end points are effectively coincident then check the
*  maximum distance between either of the intermediate points and the
*  start position.
            IF( DELTAL .LT. SHORT ) THEN

               V1 = SQRT( DX1*DX1 + DY1*DY1 )
               V2 = SQRT( DX2*DX2 + DY2*DY2 )
               MAXDEV = MAX( V1, V2 )

*  If the distance is too large, no line can be plotted.
               IF( MAXDEV .GT. DEVLIM ) THEN
                  PLOT = .FALSE.
               ELSE
                  PLOT = .TRUE.
               END IF

*  Otherwise, store the unit direction vector of the line.
            ELSE
               VEC( 1 ) = DX/DELTAL
               VEC( 2 ) = DY/DELTAL

*  Find the cosine of the angular deviation between this line and the
*  previous line (if one exists). If the angle is too large no line can
*  be plotted.
               IF( USEOLD ) THEN
                  COSANG =  VEC( 1 )*OLDVEC( 1 ) + VEC( 2 )*OLDVEC( 2 )

                  IF( COSANG .GT. COSLIM ) THEN
                     PLOT = .TRUE.
                  ELSE
                     PLOT = .FALSE.
                  END IF

               ELSE
                  COSANG = 1.0
                  PLOT = .TRUE.

               END IF

*  If a plot is still possible...
               IF( PLOT ) THEN

*  ...find the deviations of the two intermediate points from the line,
*  and store the maximum.
                  V1 = ABS( VEC( 1 )*DY1 - VEC( 2 )*DX1 )
                  V2 = ABS( VEC( 1 )*DY2 - VEC( 2 )*DX2 )
                  MAXDEV = MAX( V1, V2 )

*  If the deviation is too large, no line can be plotted.
                  IF( MAXDEV .GT. DEVLIM ) THEN
                     PLOT = .FALSE.

*  Otherwise, find the distances along the line from point 0 towards
*  point 3, at which points 1 and 2 are closest to the line.
                  ELSE
                     D1 = VEC( 1 )*DX1 + VEC( 2 )*DY1
                     D2 = VEC( 1 )*DX2 + VEC( 2 )*DY2

*  If the intermediate points do not lie between the two end points, no
*  line can be plotted.
                     IF( D1 .LE. 0 .OR. D1 .GE. DELTAL .OR.
     :                   D2 .LE. 0 .OR. D2 .GE. DELTAL ) THEN
                        PLOT = .FALSE.

*  Check that one of the intermediate points is within the central half
*  of the line.
                     ELSE

                        HALF= DELTAL/2.0
                        QUART = DELTAL/4.0

                        IF( ABS( HALF - D1 ) .GT. QUART .AND.
     :                      ABS( HALF - D2 ) .GT. QUART ) THEN
                           PLOT = .FALSE.
                        END IF

                     END IF

                  END IF

               END IF

            ENDIF

         END IF

      END IF

      END
