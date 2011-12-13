      LOGICAL FUNCTION ARD1_INTR( FRM, TYPE, NWCS, NPAR, PAR, UC, INIT,
     :                            STATUS )
*+
*  Name:
*     ARD1_INTR

*  Purpose:
*     Check a position is within a region.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RETURN = ARD1_INTR( FRM, TYPE, NWCS, NPAR, PAR, UC, INIT,
*                         STATUS )

*  Description:
*     Returns .TRUE. if the supplied user position (UC) is within, or on
*     the boundary of, the specified region. Only used for regions which
*     have non-zero volume/area (eg box, circle, etc, but not line,
*     column, etc).

*  Arguments:
*     FRM = INTEGER (Given)
*        A pointer to an AST Frame representing user coordinates.
*     TYPE = INTEGER (Given)
*        The identifier for the region type.
*     NWCS = INTEGER (Given)
*        The number of dimensions in the array.
*     NPAR = INTEGER (Given)
*        The size of the PAR array.
*     PAR( NPAR ) = DOUBLE PRECISION (Given)
*        The parameters defining the region.
*     UC( NWCS ) = DOUBLE PRECISION (Given)
*        The user coordinates at the position to be tested.
*     INIT = LOGICAL (Given)
*        Supplied .TRUE. if a new region is being filled.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Returned Value:
*     ARD1_INTR = LOGICAL
*        .TRUE. if the supplied user position (UC) is within, or on
*        the boundary of, the specified region, and .FALSE. otherwise.

*  Notes:
*     - No check is made that the supplied number of parameters is correct.
*     This should have been checked before calling this routine.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     8-JUN-2001 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ARD_CONST'        ! ARD private constants
      INCLUDE 'AST_PAR'          ! AST constants and functions
      INCLUDE 'ARD_ERR'          ! ARD error constants

*  Arguments Given:
      INTEGER FRM
      INTEGER TYPE
      INTEGER NWCS
      INTEGER NPAR
      DOUBLE PRECISION PAR( NPAR )
      DOUBLE PRECISION UC( NWCS )
      LOGICAL INIT

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL ARD1_INPOL

*  Local Variables:
      INTEGER
     :      I,                   ! Axis index
     :      J                    ! Axis index

      DOUBLE PRECISION
     :      A1,                  ! Angle at end of offset (unused)
     :      ANG,                 ! Total angle of rotation so far
     :      ANG0,                ! Angle of rotation for this edge
     :      HW,                  ! Half length of a box size
     :      P1( 2 ),             ! Position at end of ellipse 1st axis
     :      R,                   ! Distance from test point to ellipse centre
     :      U                    ! Angle from 1st ellipse axis to test point

      SAVE P1
*.

*  Initialize the returned value to indicate the point is within the
*  region.
      ARD1_INTR = .TRUE.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  BOX: Parameters are the supplied user co-ordinates of the box
*  centre, followed by the lengths of the box sides in user co-ordinates.
*  Box edges are always considered to be lines of constant axis values.
*  These are not necessarily geodesics. If a side length is negative, the
*  axis value must lie outside the specified range. This because (for
*  instance), and RA range from 359 degs to 1 deg *includes values less
*  than 1 and greater than 359.
      IF( TYPE .EQ. ARD__BOX ) THEN
         DO I = 1, NWCS
            IF( ABS( AST_AXDISTANCE( FRM, I, PAR( I ), UC( I ),
     :                               STATUS ) ) .GT.
     :          0.5*ABS( PAR( I + NWCS ) ) ) THEN
               ARD1_INTR = .FALSE.
               GO TO 999
            END IF
         END DO

*  POLYGON: Parameters are pairs of user co-ordinates, each being a vertex of
*  the polygon. 2-D only. Each edge is a geodesic.
      ELSE IF( TYPE .EQ. ARD__POL ) THEN

*  We use an imaginary vector from the test point to a point on the
*  polygon boundary. As the end of this vector traverses the boundary,
*  the vector rotates. If this vector does a complete revolution whilst
*  traversing the boundary the test point is inside the polygon. If the
*  test poin is outside the polygon, then the overall rotation will be
*  zero (counting clockwise rotation as positive and anticlockwise rotation
*  as negative). Initialize the total angle through which the vector has
*  rotated so far, to zero.
         ANG = 0.0

*  Loop round each edge.
         DO I = 1, NPAR - 1, 2

*  The side starts at vertex I and ends at vertex J.
            J = I + 2
            IF( J .GT. NPAR ) J = 1

*  Find the angle through which the vector rotates whilst moving from vertex
*  I to vertex J. If the angle is exactly PI, then the point is on the
*  boundary, which is considered to be within the region, so leave the loop
*  early. Otherwise, add the angle onto the running sum.
            ANG0 = AST_ANGLE( FRM, PAR( I ), UC, PAR( J ), STATUS )
            IF( ABS( ABS( ANG0 ) - ARD__PI ) .LE. 1.0D-6 ) THEN
               ANG = 6.0
               GO TO 10
            ELSE
               ANG = ANG + ANG0
            END IF

         END DO

 10      CONTINUE

*  ANG should now be 2*PI for interior points and zero for exterior points.
*  Use a value of 3 as the break point.
         ARD1_INTR = ( ABS( ANG ) .GT. 3.0 )

*  CIRCLE: Parameters are the user co-ordinates of the centre, and the
*  radius.
      ELSE IF( TYPE .EQ. ARD__CIR ) THEN

*  Find the distance from the test point to the circle centre.
*  If it is less than or equal to the radius, the point is inside.
         ARD1_INTR = ( AST_DISTANCE( FRM, PAR, UC, STATUS ) .LE.
     :                 PAR( NWCS + 1 ) )

*  ELLIPSE: Parameters are the user co-ordinates of the centre of the ellipse,
*  the half-lengths of the two axes of the ellipse, and the angle (in degrees)
*  between the first user axis and the first of the two ellipse axes.
*  Rotation from the 1st to the 2nd axis is positive. 2D only.
      ELSE IF( TYPE .EQ. ARD__ELL ) THEN

*  Do any required initialization.
         IF( INIT ) THEN

*  Find the position, P1, at the end of the 1st axis of the ellipse.
*  Do this by offsetting away from the ellipse centre by half the length of
*  the 1st axis, along a geodesic which is at the given angle to the 1st
*  axis.
            A1 = AST_OFFSET2( FRM, PAR, ARD__DTOR*( 90.0 - PAR( 5 ) ),
     :                        PAR( 3 ), P1, STATUS )
         END IF

*  Find the distance of the test point to the ellipse centre.
         R = AST_DISTANCE( FRM, UC, PAR, STATUS )

*  If co-incident, the point is inside.
         IF( R .EQ. 0.0 ) THEN
            ARD1_INTR = .TRUE.

*  Otherwise...
         ELSE

*  Find the angle from the 1st ellipse axis to the test point, measured
*  at the ellipse centre.
            U = AST_ANGLE( FRM, UC, PAR, P1, STATUS )

*  Do the test.
            ARD1_INTR = ( ( COS( U )/PAR( 3 ) )**2 +
     :                    ( SIN( U )/PAR( 4 ) )**2 .LE.
     :                    1.0/R**2 )
         END IF

*  Report an error and abort for any other keyword.
      ELSE
         STATUS = ARD__INTER
         CALL MSG_SETI( 'TYPE', TYPE )
         CALL ERR_REP( 'ARD1_INTR_ERR1', 'Illegal keyword identifier'//
     :                 ' (^TYPE) encountered in routine ARD1_INTR '//
     :                 '(programming error).', STATUS )
      END IF

 999  CONTINUE

      END
