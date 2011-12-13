      SUBROUTINE ARD1_POL( RINDEX, LBND1, UBND1, LBND2, UBND2, NPAR,
     :                     D, PAR, B, LBEXTB, UBEXTB, LBINTB, UBINTB,
     :                     STATUS )
*+
*  Name:
*     ARD1_POL

*  Purpose:
*     Initialise an array to hold a POLYGON or ROTBOX region.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_POL( RINDEX, LBND1, UBND1, LBND2, UBND2, NPAR, D, PAR,
*                    B, LBEXTB, UBEXTB, LBINTB, UBINTB, STATUS )

*  Description:
*     The array B is initialised by setting all values within the
*     supplied interior bounding box to the exterior value 0.
*     All points outside this box already hold exterior values.
*     Interior values are then assigned to the points specified by the
*     supplied parameters. The supplied parameters are a list of (x,y)
*     user co-ordinate pairs for each vertex of the polygon or rotated
*     box.

*  Arguments:
*     RINDEX = INTEGER (Given)
*        The value to use to represent interior points.
*     LBND1 = INTEGER (Given)
*        The lower pixel index bounds of the B array on the first axis.
*     UBND1 = INTEGER (Given)
*        The upper pixel index bounds of the B array on the first axis.
*     LBND2 = INTEGER (Given)
*        The lower pixel index bounds of the B array on the second axis.
*     UBND2 = INTEGER (Given)
*        The upper pixel index bounds of the B array on the second axis.
*     NPAR = INTEGER (Given)
*        The size of the PAR array.
*     D( 6 ) = DOUBLE PRECISION (Given)
*        The coefficients of the user->pixel mapping. The mapping is:
*        P1 = D0 + D1*U1 + D2*U2
*        P2 = D3 + D4*U1 + D5*U2
*     PAR( NPAR ) = DOUBLE PRECISION (Given and Returned)
*        Region parameters.
*     B( LBND1:UBND1, LBND2:UBND2 ) = INTEGER (Given and Returned)
*        The array.
*     LBEXTB( 2 ) = INTEGER (Given and Returned)
*        The lower pixel bounds of the smallest box which contains all
*        exterior points in B. A value of VAL__MAXI for element 1 is
*        returned to indicate an "infinite" box. Other elements should
*        be ignored.
*     UBEXTB( 2 ) = INTEGER (Given and Returned)
*        The upper pixel bounds of the smallest box which contains all
*        exterior points in B. The returned values should be ignored
*        since the box is "infinite".
*     LBINTB( 2 ) = INTEGER (Given and Returned)
*        The lower pixel bounds of the smallest box which contains all
*        interior points in B. A value of VAL__MAXI for element 1 is
*        used to indicate an infinite box, and a value of VAL__MINI for
*        element 1 is used to indicate a zero sized box.
*     UBINTB( 2 ) = INTEGER (Given and Returned)
*        The upper pixel bounds of the smallest box which contains all
*        interior points in B.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
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
*     23-MAR-1994 (DSB):
*        Original version.
*     26-JUN-2001 (DSB):
*        Modified for ARD version 2.0.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants
      INCLUDE 'ARD_CONST'        ! ARD_ private constants
      INCLUDE 'ARD_ERR'          ! ARD_ error constants

*  Arguments Given:
      INTEGER RINDEX
      INTEGER LBND1
      INTEGER UBND1
      INTEGER LBND2
      INTEGER UBND2
      INTEGER NPAR
      DOUBLE PRECISION D( 6 )

*  Arguments Given and Returned:
      DOUBLE PRECISION PAR( NPAR )
      INTEGER B( LBND1:UBND1, LBND2:UBND2 )
      INTEGER LBEXTB( 2 )
      INTEGER UBEXTB( 2 )
      INTEGER LBINTB( 2 )
      INTEGER UBINTB( 2 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MXCRS            ! Max. allowable no. of line crossings
      PARAMETER ( MXCRS = 100 )

*  Local Variables:
      DOUBLE PRECISION
     :  DY,                    ! Gap in y between adjacent vertices
     :  PERT,                  ! Perturbation
     :  TEST,                  ! Line-polygon intersection test
     :  XCROSS( MXCRS ),       ! Line crossings
     :  XMIN,                  ! Minimum x in the polygon
     :  XMAX,                  ! Maximum x in the polygon
     :  XT,                    ! Dummy for sorting
     :  YL,                    ! Y pixel coordinate
     :  YMIN,                  ! Minimum y in the polygon
     :  YMAX                   ! Maximum y in the polygon

      INTEGER
     :  I, J, N,               ! Loop counters
     :  LBND( 2 ),             ! Mask lower bounds
     :  MINX,                  ! Min. x in polygon to nearest element
     :  MAXX,                  ! Max. x in polygon to nearest element
     :  MAXY,                  ! Max. y in polygon to nearest element
     :  MINY,                  ! Min. y in polygon to nearest element
     :  MSKSIZ,                ! No. of elements in mask
     :  N1, N2,                ! Vertex numbers
     :  NCROSS,                ! Number of intersections
     :  NTOP,                  ! Sorting index
     :  NVERT,                 ! No. of polygon vertices
     :  UBND( 2 )              ! Mask upper bounds

      LOGICAL                  ! True if:
     :  EXIT                   ! Sorting complete

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Store the number of vertices supplied.
      NVERT = NPAR/2

*  Report an error if less than 3 vertices have been supplied.
      IF( NVERT .LT. 3 ) THEN
         STATUS = ARD__INTER
         CALL MSG_SETI( 'NP', NPAR )
         CALL ERR_REP( 'ARD1_POL_ERR1', 'Wrong no. of parameters '//
     :                 '(^NP) supplied for ARD1_POL (programming '//
     :                 'error).', STATUS )
         GO TO 999
      END IF

*  Reset all pixels within the interior bounding box so that they
*  hold exterior values. The pixels outside the interior bounding box
*  already hold exterior values.
      LBND( 1 ) = LBND1
      UBND( 1 ) = UBND1
      LBND( 2 ) = LBND2
      UBND( 2 ) = UBND2
      MSKSIZ = ( UBND1 - LBND1 + 1 )*( UBND2 - LBND2 + 1 )

      CALL ARD1_BXSET( 2, LBND, UBND, MSKSIZ, 0, LBINTB,
     :                 UBINTB, B, STATUS )

* Convert all positions from user coords to pixel coords.
      CALL ARD1_LTRAN( 2, D, NVERT, PAR, PAR, STATUS )

*  Find the maximum and minimum x and y pixel coordinates at the
*  supplied polygon vertices.
      XMIN = VAL__MAXD
      XMAX = VAL__MIND
      YMIN = VAL__MAXD
      YMAX = VAL__MIND

      DO N = 1, NVERT
         XMIN = MIN( PAR( 2*N - 1 ), XMIN )
         XMAX = MAX( PAR( 2*N - 1 ), XMAX )
         YMIN = MIN( PAR( 2*N ), YMIN )
         YMAX = MAX( PAR( 2*N ), YMAX )
      END DO

*  Convert the ranges to integer pixel index limits restricted to the
*  mask array size. Note, the value 1.0E8 is used to avoid integer
*  overflow where possible one could have used REAL( VAL__MAXI ). The
*  smaller value is used to avoid any possibility of floating point
*  rounding problems causing the value to creep above the maximum value
*  storable as an integer, and thus causing an overflow when the NINT
*  function is evaluated.
      MINX = NINT( MIN( MAX( -1.0D8, XMIN + 0.5 ), 1.0D8 ) )
      LBINTB( 1 ) = MIN( MAX( MINX, LBND1 ), UBND1 )

      MAXX = NINT( MIN( MAX( -1.0D8, XMAX + 0.5 ), 1.0D8 ) )
      UBINTB( 1 ) = MIN( MAX( MAXX, LBND1 ), UBND1 )

      MINY = NINT( MIN( MAX( -1.0D8, YMIN + 0.5 ), 1.0D8 ) )
      LBINTB( 2 ) = MIN( MAX( MINY, LBND2 ), UBND2 )

      MAXY = NINT( MIN( MAX( -1.0D8, YMAX + 0.5 ), 1.0D8 ) )
      UBINTB( 2 ) = MIN( MAX( MAXY, LBND2 ), UBND2 )

*  Scan the range of mask lines which cross the polygon.
      DO J = LBINTB( 2 ), UBINTB( 2 )

*  Store the Y pixel coordinate at the vertical centre of the mask
*  line.
         YL = DBLE( J ) - 0.5

*  Problems occur in counting the number of intersections if any array
*  line passes exactly through a polygon vertex. Therefore, the line
*  positions are shifted by a negligible amount PERT to ensure this
*  does not happen.
         PERT = 1.0D-4

*  Loop back to here with a new value for PERT if the current value
*  causes a vertex to fall exactly on the current line. Initialise the
*  number of times the current line intersects the polygon.
  20     CONTINUE
         NCROSS = 0

*  Scan through the x-y positions, testing if each polygon side
*  intersects the array line.
         DO N1 = 1, NVERT
            N2 = N1 + 1

*  Polygon vertices cycle back to the start.
            IF( N2 .GT. NVERT ) N2 = 1

*  Form the intersection test.
            TEST = ( ( PAR( 2*N1 ) - YL ) - PERT ) *
     :             ( ( YL - PAR( 2*N2 ) ) + PERT )

*  If TEST is zero, the line passes through a vertex. Therefore, change
*  PERT and start again ( "the line" refers to a line through the
*  middle of the pixel. The act of increasing PERT effectively moves the
*  line a small amount in the +ve Y direction).
            IF( ABS( TEST ) .LT. VAL__SMLD ) THEN
               PERT = PERT + 1.0D-4
               GO TO 20

*  If TEST is positive, adjacent vertices lie on opposite sides of the
*  array line. Calculate the point of intersection (as a pixel
*  coordinate) and store it.
            ELSE IF( TEST .GT. 0.0 ) THEN
               NCROSS = NCROSS + 1

               IF( NCROSS .LE. MXCRS ) THEN
                  DY = PAR( 2*N2 ) - PAR( 2*N1 )

                  IF( ABS( DY ) .LT. VAL__SMLD ) DY = SIGN( VAL__SMLD,
     :                                                      DY )
                  XCROSS( NCROSS ) = PAR( 2*N1 - 1 ) +
     :                         ( YL - PAR( 2*N1 ) ) *
     :                         ( PAR( 2*N2 - 1 ) - PAR( 2*N1 -1 ) ) / DY

*  If the storage for intersections is exceeded, return with a bad
*  status and report an error.
               ELSE
                  STATUS = ARD__BADAR
                  CALL ERR_REP ( 'ARD1_POL_ERR2', 'There were too '//
     :                           'many intersections of an ARD '//
     :                           'polygon region with array lines.',
     :                           STATUS )
                  GO TO 999
               END IF

*  End of the check for line-polygon intersections.
            END IF

*  End of the loop through the polygon vertices.
         END DO

*  If the line intersects the polygon, sort intersections into x order.
         IF( NCROSS .GT. 1 ) THEN
            EXIT = .FALSE.
            NTOP = NCROSS

*  Loop when an interchange was necessary.
            DO WHILE ( .NOT. EXIT )
               EXIT = .TRUE.
               NTOP = NTOP - 1

               DO N = 1, NTOP

*  Swap adjacent values if they are in the wrong order.
                  IF( XCROSS( N ) .GT. XCROSS( N + 1 ) ) THEN
                     XT = XCROSS( N + 1 )
                     XCROSS( N + 1 ) = XCROSS( N )
                     XCROSS( N ) = XT
                     EXIT = .FALSE.
                  END IF
               END DO
            END DO

*  Scan through the ordered intersections in pairs.
            DO N = 2, NCROSS, 2

*  Find the pixel index bounds corresponding to the current section of
*  the current mask line. The bounds are limited to the bounds of the
*  smallest rectangle enclosing the polygon. The end pixels are included
*  if their centres fall within the intersection.
               MINX = NINT( MIN( MAX( -1.0D8, XCROSS( N - 1 )  ),
     :                           1.0D8 ) ) + 1
               MINX = MAX( MINX, LBINTB( 1 ) )

               MAXX = NINT( MIN( MAX( -1.0D8, XCROSS( N ) ), 1.0D8 ) )
               MAXX = MIN( MAXX, UBINTB( 1 ) )

*  Set mask pixels lying between each pair of intersections to the
*  supplied value.
               DO I = MINX, MAXX
                  B( I, J ) = RINDEX
               END DO

*  Do the next pair of interesections of the polygon with the current
*  mask line.
            END DO

         END IF

*  Do the next mask line.
      END DO

*  If the interior bounding box is null, return the usual value
*  (VAL__MINI for LBINTB( 1 ) ).
      IF( LBINTB( 1 ) .GT. UBINTB( 1 ) .OR.
     :    LBINTB( 2 ) .GT. UBINTB( 2 ) ) LBINTB( 1 ) = VAL__MINI

*  Ensure the the exterior bounding box is returned "infinite".
      LBEXTB( 1 ) = VAL__MAXI

*  Jump to here if an error occurs.
 999  CONTINUE

      END
