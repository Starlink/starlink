      SUBROUTINE KPS1_AGNST( IPLOT, NPTS, ARDDEF, IGRP, X1, X2, Y1, Y2,
     :                       X, Y, NREG, STATUS )

*+
*  Name:
*     KPS1_AGNST

*  Purpose:
*     Stores an ARD description in a group for ARDGEN.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_AGNST( IPLOT, NPTS, ARDDEF, IGRP, X1, X2, Y1, Y2, X, Y,
*                      NREG, STATUS )

*  Description:
*     Given the current region shape and the co-ordinates/dimensions
*     specified by the user, text is created describing the region as
*     an ARD description.  The text is then appended to the supplied
*     GRP group.  It is assumed that the correct number of positions
*     are supplied to define a region (except in the case of POLYGON
*     regions for which a check is made that at least 3 positions have
*     been supplied).
*
*     Values are rounded to 1 decimal place, except for angle measures
*     which are not rounded.

*  Arguments:
*     IPLOT = INTEGER (Given)
*        An AST pointer to the Plot.
*     NPTS = INTEGER (Given)
*        Number of points defined.
*     ARDDEF = CHARACTER * ( * ) (Given)
*        User selected keyword.
*     IGRP = INTEGER (Given)
*        Identifier for the GRP group holding the ARD description.
*     X1 = REAL (Given)
*        World x co-ordinate of the bottom left-hand corner of image.
*     X2 = REAL (Given)
*        World x co-ordinate of the top right-hand corner of image.
*     Y1 = REAL (Given)
*        World y co-ordinate of the bottom left-hand corner of image.
*     Y2 = REAL (Given)
*        World y co-ordinate of the top right-hand corner of image.
*     X( NPTS ) = DOUBLE PRECISION (Given)
*        The x co-ordinate of the points and other region defining
*        information. Given in the current Frame of IPLOT.
*     Y( NPTS ) = DOUBLE PRECISION (Given)
*        The y co-ordinate of the points and other region-defining
*        information. Given in the current Frame of IPLOT.
*     NREG = INTEGER (Given and Returned)
*        The number of regions currently in the group.
*     STATUS = INTEGER(Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
*     Copyright (C) 1995-1996, 2001 Central Laboratory of the Research
*     Councils. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     GJP: Grant Privett (STARLINK)
*     DSB: David Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     6-JUN-1994 (GJP)
*        Original version
*     11-NOV-1994 (GJP)
*        Added ARD further keywords.
*     5-DEC-1994 (DSB)
*        Tidied up.  Name changed from ARDG1_STRING to KPS1_AGNST.
*        Store the ARD description in a GRP group instead of
*        writing it directly to a file.  Round values to 1 decimal
*        place.
*     1995 March 16 (MJC):
*        Corrected prologue identation and typo's.  Used modern style
*        variable declarations, and other stylistic changes for
*        consistency within KAPPA.
*     1996 March 4 (MJC):
*        Fixed bug where x co-ordinate was 10 times too big for a box.
*     18-SEP-2001 (DSB):
*        Modified for ARD V2/AST/PGPLOT.
*     2011 April 18: (MJC):
*        Fixed bug in calculation of BOX length and width.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SSE global definitions
      INCLUDE 'GRP_PAR'          ! GRP constants
      INCLUDE 'AST_PAR'          ! AST constants and functions
      INCLUDE 'NDF_PAR'          ! NDF constants

*  Arguments Given:
      INTEGER IPLOT
      INTEGER NPTS
      CHARACTER * ( * ) ARDDEF
      INTEGER IGRP
      REAL X1
      REAL X2
      REAL Y1
      REAL Y2
      DOUBLE PRECISION X( NPTS )
      DOUBLE PRECISION Y( NPTS )

*  Arguments Given and Returned:
      INTEGER NREG

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      DOUBLE PRECISION PI
      PARAMETER ( PI = 3.1415926535898D0 )

      DOUBLE PRECISION PIBY2
      PARAMETER ( PIBY2 = PI/2 )

      DOUBLE PRECISION RTOD
      PARAMETER ( RTOD = 180/PI )

*  Local Variables:
      CHARACTER LINE*(GRP__SZNAM)! Buffer for writing ARD output file
      DOUBLE PRECISION P1( NDF__MXDIM ) ! Position 1
      DOUBLE PRECISION P2( NDF__MXDIM ) ! Position 2
      DOUBLE PRECISION P3( NDF__MXDIM ) ! Position 3
      DOUBLE PRECISION P4( NDF__MXDIM ) ! Position 4
      DOUBLE PRECISION P5( NDF__MXDIM ) ! Position 5
      DOUBLE PRECISION PA1       ! Position angle
      DOUBLE PRECISION PA2       ! Position angle
      DOUBLE PRECISION PA3       ! Position angle
      DOUBLE PRECISION MD        ! Min distance to edge
      DOUBLE PRECISION D1        ! A distance
      DOUBLE PRECISION D2        ! A distance
      DOUBLE PRECISION DANG      ! Angle to turn by
      DOUBLE PRECISION RADIUS    ! Radius of the circle required
      DOUBLE PRECISION SMA       ! Semi-major axis value
      DOUBLE PRECISION SMI       ! Semi-minor axis value
      DOUBLE PRECISION T         ! Intermediate value
      DOUBLE PRECISION XC        ! Box centre
      DOUBLE PRECISION LN        ! Length of box side
      DOUBLE PRECISION YC        ! Box centre
      DOUBLE PRECISION WD        ! Length of box side
      INTEGER AXIS               ! Index of axis used to measure distances
      INTEGER I                  ! Loop variable
      INTEGER CFRM               ! Pointer to current Frame
      INTEGER NC                 ! Number of bytes of the output string occupied

*.

*  Check the inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Set the initial value for output string.
      LINE = ' '

*  Set the "characters used" counter to zero.
      NC = 0

*  Get a pointer to the Frame in which positions are supplied (the current
*  Frame of IPLOT).
      CFRM = AST_GETFRAME( IPLOT, AST__CURRENT, STATUS )

*  Determine the axis whch should be used for formatting geodesic
*  distances.
      IF( AST_ISASKYFRAME( CFRM, STATUS ) ) THEN
         AXIS = AST_GETI( CFRM, 'LATAXIS', STATUS )
      ELSE
         AXIS = 1
      END IF

*  Box.
      IF ( ARDDEF .EQ. 'BOX' ) THEN

*  Find the centre and side lengths.
         XC = X( 1 )
         YC = Y( 1 )
         LN = 2.0D0 * ABS( X( 2 ) - X( 1 ) )
         WD = 2.0D0 * ABS( Y( 2 ) - Y( 1 ) )

*  Create the string.
         CALL CHR_PUTC( 'BOX(', LINE, NC )
         NC = NC + 1
         CALL CHR_APPND( AST_FORMAT( CFRM, 1, XC, STATUS ), LINE, NC )
         CALL CHR_APPND( ',', LINE, NC )
         NC = NC + 1
         CALL CHR_APPND( AST_FORMAT( CFRM, 2, YC, STATUS ), LINE, NC )
         CALL CHR_APPND( ',', LINE, NC )
         NC = NC + 1
         CALL CHR_APPND( AST_FORMAT( CFRM, 1, LN, STATUS ), LINE, NC )
         CALL CHR_APPND( ',', LINE, NC )
         NC = NC + 1
         CALL CHR_APPND( AST_FORMAT( CFRM, 2, WD, STATUS ), LINE, NC )
         CALL CHR_APPND( ' ) ', LINE, NC )

*  Frame.
      ELSE IF ( ARDDEF .EQ. 'FRAME' ) THEN

*  Transform the given position back to graphics coords.
         CALL AST_TRAN2( IPLOT, 1, X, Y, .FALSE., XC, YC, STATUS )

*  Find the closest position on an edge of the plot.
         IF( MIN( ABS( X2 - XC ), ABS( XC - X1 ) ) .LT.
     :       MIN( ABS( Y2 - YC ), ABS( YC - Y1 ) ) ) THEN
            IF( ABS( X2 - XC ) .LT. ABS( XC - X1 ) ) THEN
               XC = X2
            ELSE
               XC = X1
            END IF
         ELSE
            IF( ABS( Y2 - YC ) .LT. ABS( YC - Y1 ) ) THEN
               YC = Y2
            ELSE
               YC = Y1
            END IF
         END IF

*  Transform this position back to the current Frame.
         CALL AST_TRAN2( IPLOT, 1, XC, YC, .TRUE., XC, YC, STATUS )

*  Find the geodesic distance from the supplied position to the nearest
*  edge position.
         P1( 1 ) = X( 1 )
         P1( 2 ) = Y( 1 )
         P2( 1 ) = XC
         P2( 2 ) = YC
         MD = AST_DISTANCE( CFRM, P1, P2, STATUS )

*  Create the string.
         CALL CHR_APPND( 'FRAME(', LINE, NC )
         NC = NC + 1
         CALL CHR_APPND( AST_FORMAT( CFRM, AXIS, MD, STATUS ), LINE,
     :                   NC )
         CALL CHR_APPND( ' ) ', LINE, NC )

*  Point.
      ELSE IF ( ARDDEF .EQ. 'POINT' ) THEN

*  Create the string.
         CALL CHR_APPND( 'POINT(', LINE, NC )
         NC = NC + 1
         CALL CHR_APPND( AST_FORMAT( CFRM, 1, X( 1 ), STATUS ), LINE,
     :                   NC )
         CALL CHR_APPND( ',', LINE, NC )
         NC = NC + 1
         CALL CHR_APPND( AST_FORMAT( CFRM, 2, Y( 1 ), STATUS ), LINE,
     :                   NC )
         CALL CHR_APPND( ' ) ', LINE, NC )

*  Row.
      ELSE IF ( ARDDEF( 1:3 ) .EQ. 'ROW' ) THEN

*  Create the string.
         CALL CHR_APPND( 'ROW(', LINE, NC )
         NC = NC + 1
         CALL CHR_APPND( AST_FORMAT( CFRM, 2, Y( 1 ), STATUS ), LINE,
     :                   NC )
         CALL CHR_APPND( ' ) ', LINE, NC )

*  Rectangle.
      ELSE IF ( ARDDEF .EQ. 'RECTANGLE' ) THEN

*  Create the string.
         CALL CHR_APPND( 'RECT(', LINE, NC )
         NC = NC + 1
         CALL CHR_APPND( AST_FORMAT( CFRM, 1, X( 1 ), STATUS ), LINE,
     :                   NC )
         CALL CHR_APPND( ',', LINE, NC )
         NC = NC + 1
         CALL CHR_APPND( AST_FORMAT( CFRM, 2, Y( 1 ), STATUS ), LINE,
     :                   NC )
         CALL CHR_APPND( ',', LINE, NC )
         NC = NC + 1
         CALL CHR_APPND( AST_FORMAT( CFRM, 1, X( 2 ), STATUS ), LINE,
     :                   NC )
         CALL CHR_APPND( ',', LINE, NC )
         NC = NC + 1
         CALL CHR_APPND( AST_FORMAT( CFRM, 2, Y( 2 ), STATUS ), LINE,
     :                   NC )
         CALL CHR_APPND( ' ) ', LINE, NC )

*  Rotated box.
      ELSE IF ( ARDDEF .EQ. 'ROTBOX' ) THEN

*  Find the width of the box.
         P1( 1 ) = X( 1 )
         P1( 2 ) = Y( 1 )
         P2( 1 ) = X( 2 )
         P2( 2 ) = Y( 2 )
         P3( 1 ) = X( 3 )
         P3( 2 ) = Y( 3 )
         CALL AST_RESOLVE( CFRM, P1, P2, P3, P4, D1, WD, STATUS )

*  Find the length of the box.
         LN = AST_DISTANCE( CFRM, P1, P2, STATUS )

*  Find the angle from axis 2 to the supplied edge at point 1.
         PA1 = AST_AXANGLE( CFRM, P1, P2, 2, STATUS )

*  Find the mid point of the supplied edge, and the position angle of the
*  edge at the mid point.
         PA2 = AST_OFFSET2( CFRM, P1, PA1, 0.5*LN, P4, STATUS )

*  Turn through 90 degrees, towards the third point.
         IF( AST_ANGLE( CFRM, P2, P1, P3, STATUS ) .GT. 0.0 ) THEN
            DANG = PIBY2
         ELSE
            DANG = - PIBY2
         END IF
         PA2 = PA2 + DANG

*  Offset in this direction for half the box width to arrive at the box
*  centre. Turn through 90 degrees to get the position angle parallel to
*  the first side at the centre.
         PA3 = AST_OFFSET2( CFRM, P4, PA2, 0.5*WD, P5, STATUS )
         PA3 = PA3 - DANG

*  Create the string.
         CALL CHR_APPND( 'ROTBOX(', LINE, NC )
         NC = NC + 1
         CALL CHR_APPND( AST_FORMAT( CFRM, 1, P5( 1 ), STATUS ), LINE,
     :                   NC )
         CALL CHR_APPND( ',', LINE, NC )
         NC = NC + 1
         CALL CHR_APPND( AST_FORMAT( CFRM, 2, P5( 2 ), STATUS ), LINE,
     :                   NC )
         CALL CHR_APPND( ',', LINE, NC )
         NC = NC + 1
         CALL CHR_APPND( AST_FORMAT( CFRM, AXIS, LN, STATUS ), LINE,
     :                   NC )
         CALL CHR_APPND( ',', LINE, NC )
         NC = NC + 1
         CALL CHR_APPND( AST_FORMAT( CFRM, AXIS, WD, STATUS ), LINE,
     :                   NC )
         CALL CHR_APPND( ',', LINE, NC )
         NC = NC + 1
         CALL CHR_PUTR( REAL( 90.0-PA3*RTOD ), LINE, NC )
         CALL CHR_APPND( ' ) ', LINE, NC )

*  Column.
      ELSE IF ( ARDDEF .EQ. 'COLUMN' ) THEN

*  Create the string.
         CALL CHR_APPND( 'COLUMN(', LINE, NC )
         NC = NC + 1
         CALL CHR_APPND( AST_FORMAT( CFRM, 1, X( 1 ), STATUS ), LINE,
     :                   NC )
         CALL CHR_APPND( ' ) ', LINE, NC )

*  Ellipse.
      ELSE IF ( ARDDEF .EQ. 'ELLIPSE' ) THEN

*  Find the length of the semi-major axis.
         P1( 1 ) = X( 1 )
         P1( 2 ) = Y( 1 )
         P2( 1 ) = X( 2 )
         P2( 2 ) = Y( 2 )
         SMA = AST_DISTANCE( CFRM, P1, P2, STATUS )

*  Find the angle from axis 1 to the semi-major axis. Invert because ARD
*  measures angles anti-clockwise (+x through +y) but AST measures them
*  clockwise (+x through -y).
         PA1 = -AST_AXANGLE( CFRM, P1, P2, 1, STATUS )

*  Resolve the line from the centre to the the third point into two
*  components parallel and perpendicular to the major axis.
         P3( 1 ) = X( 3 )
         P3( 2 ) = Y( 3 )
         CALL AST_RESOLVE( CFRM, P1, P2, P3, P4, D1, D2, STATUS )

*  Calculate the length of the semi-minor axis.
         T = SMA**2 - D1**2
         IF( T .GT. 0.0 ) THEN
            SMI = ( SMA*D2 )/SQRT( T )
         ELSE
            SMI = SMA
         END IF

*  Create the string.
         CALL CHR_APPND( 'ELLIPSE(', LINE, NC )
         NC = NC + 1
         CALL CHR_APPND( AST_FORMAT( CFRM, 1, X( 1 ), STATUS ), LINE,
     :                   NC )
         CALL CHR_APPND( ',', LINE, NC )
         NC = NC + 1
         CALL CHR_APPND( AST_FORMAT( CFRM, 2, Y( 1 ), STATUS ), LINE,
     :                   NC )
         CALL CHR_APPND( ',', LINE, NC )
         NC = NC + 1
         CALL CHR_APPND( AST_FORMAT( CFRM, AXIS, SMA, STATUS ), LINE,
     :                   NC )
         CALL CHR_APPND( ',', LINE, NC )
         NC = NC + 1
         CALL CHR_APPND( AST_FORMAT( CFRM, AXIS, SMI, STATUS ), LINE,
     :                   NC )
         CALL CHR_APPND( ',', LINE, NC )
         NC = NC + 1
         CALL CHR_PUTR( REAL( PA1*RTOD ), LINE, NC )
         CALL CHR_APPND( ' ) ', LINE, NC )

*  Circle.
      ELSE IF ( ARDDEF .EQ. 'CIRCLE' ) THEN

*  Find the circle radius.
         P1( 1 ) = X( 1 )
         P1( 2 ) = Y( 1 )
         P2( 1 ) = X( 2 )
         P2( 2 ) = Y( 2 )
         RADIUS = AST_DISTANCE( CFRM, P1, P2, STATUS )

*  Create the string.
         CALL CHR_APPND( 'CIRCLE(', LINE, NC )
         NC = NC + 1
         CALL CHR_APPND( AST_FORMAT( CFRM, 1, X( 1 ), STATUS ), LINE,
     :                   NC )
         CALL CHR_APPND( ',', LINE, NC )
         NC = NC + 1
         CALL CHR_APPND( AST_FORMAT( CFRM, 2, Y( 1 ), STATUS ), LINE,
     :                   NC )
         CALL CHR_APPND( ',', LINE, NC )
         NC = NC + 1
         CALL CHR_APPND( AST_FORMAT( CFRM, AXIS, RADIUS, STATUS ), LINE,
     :                   NC )
         CALL CHR_APPND( ' ) ', LINE, NC )

*  Line.
      ELSE IF ( ARDDEF .EQ. 'LINE' ) THEN

*  Create the string.
         CALL CHR_APPND( 'LINE(', LINE, NC )
         NC = NC + 1
         CALL CHR_APPND( AST_FORMAT( CFRM, 1, X( 1 ), STATUS ), LINE,
     :                   NC )
         CALL CHR_APPND( ',', LINE, NC )
         NC = NC + 1
         CALL CHR_APPND( AST_FORMAT( CFRM, 2, Y( 1 ), STATUS ), LINE,
     :                   NC )
         CALL CHR_APPND( ',', LINE, NC )
         NC = NC + 1
         CALL CHR_APPND( AST_FORMAT( CFRM, 1, X( 2 ), STATUS ), LINE,
     :                   NC )
         CALL CHR_APPND( ',', LINE, NC )
         NC = NC + 1
         CALL CHR_APPND( AST_FORMAT( CFRM, 2, Y( 2 ), STATUS ), LINE,
     :                   NC )
         CALL CHR_APPND( ' ) ', LINE, NC )

*  Polygon.
      ELSE IF ( ARDDEF .EQ. 'POLYGON' ) THEN

*  Issue a warning and return without further action if there are two
*  or less vertices in the polygon.
         IF ( NPTS .LE. 2 ) THEN
            CALL MSG_OUT( 'KPS1_AGNST_MSG1', 'Too few positions '//
     :                    'supplied to define a polygon.', STATUS )
            GO TO 999
         END IF

*  Create opening string.
         CALL CHR_APPND( 'POLYGON(', LINE, NC )
         NC = NC + 1

*  Add the first pair of co-ordinates to the string.
         CALL CHR_APPND( AST_FORMAT( CFRM, 1, X( 1 ), STATUS ), LINE,
     :                   NC )
         CALL CHR_APPND( ',', LINE, NC )
         NC = NC + 1
         CALL CHR_APPND( AST_FORMAT( CFRM, 2, Y( 1 ), STATUS ), LINE,
     :                   NC )
         CALL CHR_APPND( ',', LINE, NC )
         NC = NC + 1

*  Loop round the remaining pairs of co-ordinates.
         DO I = 2, NPTS

*  Store the previous line in the group.
            CALL GRP_PUT( IGRP, 1, LINE( : NC ), 0, STATUS )

*  Form the new line, padding it at the start with blanks to line the
*  co-ordinates up with the ones in the previous line.
            NC = 9
            LINE = ' '

            CALL CHR_APPND( AST_FORMAT( CFRM, 1, X( I ), STATUS ), LINE,
     :                   NC )
            CALL CHR_APPND( ',', LINE, NC )
            NC = NC + 1
            CALL CHR_APPND( AST_FORMAT( CFRM, 2, Y( I ), STATUS ), LINE,
     :                   NC )

            IF ( I .NE. NPTS) THEN
               CALL CHR_APPND( ',', LINE, NC )
               NC = NC + 1
            ELSE
               CALL CHR_APPND( ' )', LINE, NC )
            END IF

         END DO

*  Whole.
      ELSE IF ( ARDDEF .EQ. 'WHOLE' ) THEN

*  Create the string.
         CALL CHR_APPND( 'WHOLE', LINE, NC )

*  Report an error for any other shape.
      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'SH', ARDDEF )
         CALL ERR_REP( 'KPS1_AGNST_ERR1', 'Subroutine KPS1_AGNST does '/
     :                 /'not yet support ''^SH'' shapes (programming '/
     :                 /'error).', STATUS )
      END IF

*  Store the line in the group.
      CALL GRP_PUT( IGRP, 1, LINE( : NC ), 0, STATUS )

*  Increment the number of regions i the group if all went well.
      IF( STATUS .EQ. SAI__OK ) NREG = NREG + 1

 999  CONTINUE

      END
