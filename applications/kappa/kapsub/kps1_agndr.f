      SUBROUTINE KPS1_AGNDR( MXPOL, XLOW, XHIGH, YLOW, YHIGH, ARDDEF, 
     :                       X, Y, STATUS )
*+
*  Name:
*     KPS1_AGNDR

*  Purpose:
*     Draws an ARD region for ARDGEN.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_AGNDR( MXPOL, XLOW, XHIGH, YLOW, YHIGH, ARDDEF, X, Y, 
*                      STATUS )

*  Description:
*     Draws the outline of the region defined by the ARD keyword
*     contained in variable ARDDEF.  The size/shape of the region is
*     defined by the values stored in arrays X and Y.  The values are
*     the image locations chosen by the user.

*  Arguments:
*     MXPOL = INTEGER (Given)
*        Maximum number of polygon vertices.
*     XLOW = REAL (Given)
*        Lower limit of image X axis.
*     XHIGH = REAL (Given)
*        Upper limit of image X axis.
*     YLOW = REAL (Given)
*        Lower limit of image Y axis.
*     YHIGH = REAL (Given)
*        Upper limit of image Y axis.
*     ARDDEF = CHARACTER * ( * ) (Given and Returned)
*        User selected keyword.  This can be changed, for instance from
*        an ellipse to a circle if the supplied shape can be described
*        more simply by the new shape (in the case of an ellipse, if
*        the major and minor axes are the same length).  If this
*        happens the contents of the X and Y arrays will also change to
*        describe the new shape.
*     X( MXPOL ) = REAL (Given and Returned)
*        The X co-ordinate of the points and other shape defining data.
*     Y( MXPOL ) = REAL (Given and Returned)
*        The Y co-ordinate of the points and other shape defining data.
*     STATUS = INTEGER (Given and Returned)
*        The inherited status.

*  Notes:
*     The values given and returned in arrays X and Y depend on ARDDEF
*     as follows:
*
*     ARDDEF = 'BOX'
*        On entry the co-ordinates of: 1 - the centre; 2 - a corner.
*        On exit: 3 - co-ordinates of the diagonally opposite corner.
*
*     ARDDEF = 'CIRCLE'
*        On entry the co-ordinates of: 1 - the centre; 2 - a point on
*        the circle.
*        On exit: X(3) is the radius of the circle.

*     ARDDEF = 'COLUMN'
*        On entry X(1) is the column co-ordinate.

*     ARDDEF = 'ELLIPSE'
*        On entry the co-ordinates of: 1 - the centre; 2 - one end of
*        the major axis; 3 - another point on the ellipse.
*        On exit: X(4) - semi-major axis length; Y(4) - semi-minor axis
*        length; X(5) - ellipse position angle (in radians).
*
*     ARDDEF = 'FRAME'
*        On entry: 1 - the co-ordinates of a point on the frame border.
*        On exit: X(1) is the used frame width (minimum width).
*
*     ARDDEF = 'LINE'
*        On entry 1 and 2 are the start and end co-ordinates.
*
*     ARDDEF = 'POINT'
*        On entry the co-ordinates of the point.
*
*     ARDDEF = 'POLYGON'
*        Does not used X and Y.
*
*     ARDDEF = 'RECTANGLE'
*        On entry the co-ordinates of diagonally opposite corners.
*
*     ARDDEF = 'ROTBOX'
*        On entry the co-ordinates of 1,2 - two corners; 3 - a point on
*        the opposite side.
*        On exit: 3,4 - the co-ordinates of the remaining two corners;
*        5 - the co-ordinates of the centre; 6 - lengths of the sides;
*        7 - the orientation of the box in radians.
*
*     ARDDEF = 'ROW'
*        On entry Y(1) is the line co-ordinate.
*
*     ARDDEF = 'WHOLE'
*        Does not use X and Y.

*  Prior Requirements:
*     An SGS device must already be open.

*  Authors:
*     GJP: Grant Privett ( STARLINK)
*     DSB: David Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:   
*     6-JUN-1994 (GJP)
*        Original version.
*     11-NOV-1994 (GJP)
*        Added ARD further keywords.
*     5-DEC-1994 (DSB)
*        Tidied up.  Name changed from ARDG1_DRAW to KPS1_AGNDR.
*        Argument MXPOL added.  Shape abbreviations changed to full
*        names.  BOX changed to be defined by centre and one corner,
*        rather than 2 corners.  Position of ARDDEF in argument list
*        changed to reflect its returned value.  Ellipse drawing
*        corrected.
*     1995 March 15 (MJC):
*        Corrected prologue indentation and typo's, used modern style
*        of variable declarations, shortened long lines, and made other
*        stylistic changes for KAPPA.  Added Notes describing the values
*        supplied and returned in arrays X and Y.  Renamed variable LEN
*        to LENG to avoid clash with the function of the same name.
*        Defined local constants.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SSE global definitions
      INCLUDE 'PRM_PAR'          ! PRIMDAT public constants

*  Arguments Given:
      INTEGER MXPOL
      REAL XLOW, XHIGH           ! Limits of image axes
      REAL YLOW, YHIGH           ! Limits of image axes

*  Arguments Given and Returned:
      CHARACTER * ( * ) ARDDEF   ! Input ARD keyword
      REAL X( MXPOL )            ! Selected co-ordinates and keyword
      REAL Y( MXPOL )            ! data

*  Status:
      INTEGER STATUS

*  Local Constants:
      INTEGER MAXELP             ! Maximum number of points in ellipse locus
      PARAMETER ( MAXELP = 6000 )

      REAL PI                    ! Geometric constant
      PARAMETER ( PI = 3.1415926 )

*  Local Variables:
      REAL ANG2, ANG3            ! Angle from point to X axis
      REAL ANGST                 ! Increment in PHI
      REAL CONS( 5 )             ! Constants in line equations
      REAL COSPHI                ! COS( PHI )
      REAL DIRECT                ! Ellipse position angle
      REAL GRAD( 5 )             ! Gradient of a line
      INTEGER I                  ! Loop variable
      REAL LENG( 2 )             ! Distance between two points
      INTEGER NUMP               ! Number of points to be drawn 
      REAL P                     ! Constant
      REAL PHI                   ! Ellipse parameter angle
      REAL Q                     ! Constant
      REAL R2                    ! Separation of two points
      REAL R3                    ! Separation of two points
      REAL S                     ! Constant
      REAL SINPHI                ! SIN( PHI )
      REAL SMA                   ! Semi-major axis
      REAL SMI                   ! Semi-minor axis
      REAL T                     ! Constant
      REAL XX                    ! Pixel x co-ordinate of a point
      REAL YY                    ! Pixel y co-ordinate of a point

*. 

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Plot an ellipse.
*  ================
      IF ( ARDDEF .EQ. 'ELLIPSE' ) THEN

*  The first position gives the centre of the ellipse.  Convert the
*  second and third positions to be offsets from the centre.
         X( 2 ) = X( 2 ) - X( 1 ) 
         Y( 2 ) = Y( 2 ) - Y( 1 ) 

         X( 3 ) = X( 3 ) - X( 1 ) 
         Y( 3 ) = Y( 3 ) - Y( 1 ) 

*  Find the distance of each of these two points from the supplied
*  centre.
         R2 = SQRT( MAX( 0.0, X( 2 ) * X( 2 ) + Y( 2 ) * Y( 2 ) ) )
         R3 = SQRT( MAX( 0.0, X( 3 ) * X( 3 ) + Y( 3 ) * Y( 3 ) ) )

*  Find the angles from the +ve x axis to each of these radius vectors.
         IF ( R2 .GT. 0.0 ) THEN
            ANG2 = ATAN2( Y( 2 ), X( 2 ) )
         ELSE
            ANG2 = 0.0
         END IF

         IF ( R3 .GT. 0.0 ) THEN
            ANG3 = ATAN2( Y( 3 ), X( 3 ) )
         ELSE
            ANG3 = 0.0
         END IF

*  The longer of these two vectors defines the semi-major axis.  First
*  handle the case of the second supplied position marking an end of
*  the semi-major axis.
         IF ( R2 .GT. R3 ) THEN
            SMA = R2
            DIRECT = ANG2

*  We now have to find the semi-minor axis.  This is based on the
*  parameterised form of an ellipse, X=SMA.COS(PHI), Y=SMI.COS(PHI)
*  where PHI is the parameterising angle for the point (X,Y), and (X,Y)
*  are in the "natural" co-ordinate system of the ellipse (X parallel
*  to major axis and Y parallel to minor axis).  Knowing SMA and X
*  (=R3.COS(ANG3-ANG2)) we can find PHI, and then knowing Y
*  (=R3.SIN(ANG3-ANG2)) and PHI we can also find SMI.  Note R2 will not
*  be zero since we checked (R2 .GT. R3) earlier and R3 cannot be
*  negative since it is the result of a SQRT function.
            COSPHI = R3 * COS( ANG3 - ANG2 ) / R2
            SINPHI = SIN( ACOS( COSPHI ) )
            IF ( SINPHI .NE. 0.0 ) THEN
               SMI = R3 * SIN( ANG3 - ANG2 ) / SINPHI
            ELSE
               SMI = 0.0
            END IF

*  Now do the same for the case in which the third point marks the end
*  of the semi-major axis.  Ensure that R3 is non-zero.
         ELSE IF ( R3 .GT. 0.0 ) THEN
            SMA = R3
            DIRECT = ANG3

            COSPHI = R2 * COS( ANG2 - ANG3 ) / R3
            SINPHI = SIN( ACOS( COSPHI ) )
            IF ( SINPHI .NE. 0.0 ) THEN
               SMI = R2 * SIN( ANG2 - ANG3 ) / SINPHI
            ELSE
               SMI = 0.0
            END IF

*  Arrive here if all supplied points are co-incident (i.e. both R2 and
*  R3 are zero).
         ELSE
            SMI = 0.0
            SMA = 0.0
            DIRECT = 0.0
         END IF

*  Calculate the number of points required to draw the ellipse,
*  imposing sensible limits.
         NUMP = INT( 0.5 * PI * SMA ) 
         IF ( NUMP .GT. MAXELP ) NUMP = MAXELP
         IF ( NUMP .LT. 2 ) NUMP = 2

*  Find the step in the parameterising angle PHI between adjacent points.
         ANGST = 2.0 * PI / REAL( NUMP )

*  Find some constant values needed to evaluate the co-ordinates of
*  positions on the ellipse.
         S = SMA * COS( DIRECT )
         P = SMA * SIN( DIRECT )
         Q = SMI * COS( DIRECT )
         T = SMI * SIN( DIRECT )

*  Start the poly line at the PHI=0.0 (the supplied end of the
*  semi-major axis).
         PHI = 0.0
         XX = S + X( 1 )
         YY = P + Y( 1 )
         CALL SGS_BPOLY( XX, YY )

*  Do each step.  Transform the angle around around the ellipse into
*  Cartesian co-ordinates, and add the segment to the polyline.
         DO I = 2, NUMP
            PHI = PHI + ANGST
            COSPHI = COS( PHI )
            SINPHI = SIN( PHI )
            XX = S * COSPHI - T * SINPHI + X( 1 )
            YY = P * COSPHI + Q * SINPHI + Y( 1 )
            CALL SGS_APOLY( XX, YY )
         END DO

*  Close the curve by joining the last point to the first point.
         XX = S + X( 1 )
         YY = P + Y( 1 )
         CALL SGS_APOLY( XX, YY )

*  Return values defining the ellipse.
         X( 4 ) = SMA
         Y( 4 ) = SMI
         X( 5 ) = DIRECT

*  Plot the circle.
*  ================
      ELSE IF ( ARDDEF .EQ. 'CIRCLE' ) THEN

*  Find circle radius.
         X( 3 ) = SQRT( ( ABS( X( 1 ) - X( 2 ) ) )**2
     :                + ( ABS( Y( 1 ) - Y( 2 ) ) )**2 )

*  Plot the circle.
         CALL SGS_CIRCL( X( 1 ), Y( 1 ), X( 3 ) )

*  Plot the box.
*  =============
      ELSE IF ( ARDDEF .EQ. 'BOX' ) THEN

*  Find the opposite corner. ( (X(1),Y(1)) is the centre and (X(2),Y(2))
*  is a corner.)
         X( 3 ) = 2.0*X( 1 ) - X( 2 )
         Y( 3 ) = 2.0*Y( 1 ) - Y( 2 )

*  Draw the box.
         CALL SGS_BOX( X( 3 ), X( 2 ), Y( 3 ), Y( 2 ) )

*  Plot the rectangle.
*  ===================
      ELSE IF ( ARDDEF .EQ. 'RECTANGLE' ) THEN
         CALL SGS_BOX( X( 1 ), X( 2 ), Y( 1 ), Y( 2 ) )

*  Plot the point.
*  ===============
      ELSE IF ( ARDDEF .EQ. 'POINT' ) THEN
         CALL SGS_LINE( X( 1 ), Y( 1 ), X( 1 ), Y( 1 ) )

*  Plot the frame.
*  ===============
      ELSE IF ( ARDDEF .EQ. 'FRAME' ) THEN

*  Find the smallest distance to an edge along the x axis.
         XX = MIN( ABS( X( 1 ) - XLOW ), ABS( X( 1 ) - XHIGH ) )

*  Find the smallest distance to an edge along the y axis.
         YY = MIN( ABS( Y( 1 ) - YLOW ), ABS( Y( 1 ) - YHIGH ) )

*  Find which of XX and YY is smaller.
         X( 1 ) = MIN( XX, YY )

*  Draw the frame.
         CALL SGS_BOX( XLOW + X( 1 ), XHIGH - X( 1 ), YLOW + X( 1 ), 
     :                 YHIGH - X( 1 ) )

*  Plot the polygon.
*  =================
      ELSE IF ( ARDDEF .EQ. 'POLYGON' ) THEN

*  This is all handled else where since the final value for NPTS is not
*  known.

*  Plot the rotated box.
*  =====================
      ELSE IF ( ARDDEF .EQ. 'ROTBOX' ) THEN

*  Determine length of one edge.
         LENG( 1 ) = SQRT( ( ABS ( X( 1 ) - X( 2 ) ) )**2
     :                   + ( ABS ( Y( 1 ) - Y( 2 ) ) )**2 )

*  Check the edge is not vertical nor horizontal.
         IF ( ( ABS( X( 1 ) - X( 2 ) ) .GT. VAL__EPSR ) .AND.
     :        ( ABS( Y( 1 ) - Y( 2 ) ) .GT. VAL__EPSR ) ) THEN 

*  Find the linear equation of the first edge.
            GRAD( 1 ) = ( Y( 2 ) - Y( 1 ) ) / ( X( 2 ) - X( 1 ) )
            CONS( 1 ) = Y( 1 ) - GRAD( 1 ) * X( 1 )

*  Derive the linear equation of line crossing first edge at 90 degrees
*  and passing through the third point (not on the second edge).
            GRAD( 2 ) =  -1.0 / GRAD( 1 )
            CONS( 2 ) = Y( 3 ) - GRAD( 2 ) * X( 3 )

*  Determine the crossing point of the lines.
            XX = ( CONS( 2 ) - CONS( 1 ) ) / ( GRAD( 1 ) - GRAD( 2 ) )
            YY = GRAD( 1 ) * XX + CONS( 1 )

*  Use the crossing point to determine the distance between two
*  opposite faces of the box.
            LENG( 2 ) = SQRT( ( ABS( XX - X( 3 ) ) )**2 +
     :                        ( ABS( YY - Y( 3 ) ) )**2 )

*  Determine the equation of the second edge.
            GRAD( 3 ) = GRAD( 2 )
            CONS( 3 ) = Y( 2 ) - GRAD( 3 ) * X( 2 )

*  Determine the equation of the edge opposite the first.
            GRAD( 4 ) = GRAD( 1 )
            CONS( 4 ) = Y( 3 ) - GRAD( 4 ) * X( 3 )

*  Find out where they intersect.
            X( 3 ) = ( CONS( 4 ) - CONS( 3 ) ) /
     :               ( GRAD( 3 ) - GRAD( 4 ) )
            Y( 3 ) = GRAD( 3 ) * X( 3 ) + CONS( 3 )

*  Now try to find out the location of the remaining point on the
*  rectangle.
                 
*  Get equation of the remaining side.
            GRAD( 5 ) = GRAD( 2 )
            CONS( 5 ) = Y( 1 ) - GRAD( 5 ) * X( 1 )

*  Find intersection so can define the last point.
            X( 4 ) = ( CONS( 4 ) - CONS( 5 ) ) /
     :               ( GRAD( 5 ) - GRAD( 4 ) )
            Y( 4 ) = GRAD( 5 ) * X( 4 ) + CONS( 5 )

*  Retain the values for later use.
*  --------------------------------

*  Centre of box.
            X( 5 ) = ( X( 1 ) + X( 2 ) + X( 3 ) + X( 4 ) ) / 4.0
            Y( 5 ) = ( Y( 1 ) + Y( 2 ) + Y( 3 ) + Y( 4 ) ) / 4.0

*  Length of sides.
            X( 6 ) = LENG( 1 )
            Y( 6 ) = LENG( 2 )

*  Calculate the angle coping with a potential divide by zero.
            IF ( ABS( Y( 2 ) - Y( 1 ) ) .LT. 
     :           ABS( ( X( 2 ) - X( 1 ) ) * VAL__EPSR )  ) THEN
               DIRECT = PI / 2.0
            ELSE
               DIRECT = ATAN( ( X( 2 ) - X( 1 ) ) /
     :                        ( Y( 2 ) - Y( 1 ) ) )
            END IF

*  Find the position angle of the ellipse.  This lies in the range 0 to
*  180 degrees.
            X( 7 ) = -DIRECT / PI * 180.0 - 90.0
            IF ( X( 7 ) .LT. 0.0 ) X( 7 ) = X( 7 ) + 180.0

         ELSE   

*  Cope with a vertical initial line.
*  ----------------------------------
            IF ( ABS( X( 1 ) - X( 2 ) ) .LT. VAL__EPSR ) THEN

*  Find the length of the edges.
               LENG( 1 ) = ABS( Y( 2 ) - Y( 1 ) )
               LENG( 2 ) = ABS( X( 3 ) - X( 2 ) )

*  Assign values for the facing edge.
               X( 3 ) = X( 3 )
               Y( 3 ) = Y( 2 )  
               X( 4 ) = X( 3 )
               Y( 4 ) = Y( 1 )

*  Retain other values for later use.
*  ----------------------------------

*  Centre of box.
               X( 5 ) = ( X( 1 ) + X( 3 ) ) * 0.5
               Y( 5 ) = ( Y( 1 ) + Y( 2 ) ) * 0.5

*  Length of sides.
               X( 6 ) = LENG( 1 )
               Y( 6 ) = LENG( 2 )

*  Store the angle.
               X( 7 ) = -90.0
            
            ELSE                         

*  Cope with a horizontal edge.
*  ----------------------------
            
*  Find the length of the edges.
               LENG( 1 ) = ABS( X( 2 ) - X( 1 ) )
               LENG( 2 ) = ABS( Y( 3 ) - Y( 2 ) )

*  Assign values for the facing edge.
               X( 3 ) = X( 2 )
               Y( 3 ) = Y( 3 )  
               X( 4 ) = X( 1 )
               Y( 4 ) = Y( 3 )

*  Retain other values for later use.
*  ----------------------------------

*  Centre of box.
               X( 5 ) = ( X( 1 ) + X( 2 ) )/2.0
               Y( 5 ) = ( Y( 1 ) + Y( 3 ) )/2.0

*  Length of sides.
               X( 6 ) = LENG( 1 )
               Y( 6 ) = LENG( 2 )

*  Store the angle.
               X( 7 ) = 0.0

            END IF

         END IF

*  Draw the rotated box.
         CALL SGS_LINE( X( 1 ), Y( 1 ), X( 2 ), Y( 2 ) )
         CALL SGS_LINE( X( 2 ), Y( 2 ), X( 3 ), Y( 3 ) )
         CALL SGS_LINE( X( 3 ), Y( 3 ), X( 4 ), Y( 4 ) )
         CALL SGS_LINE( X( 4 ), Y( 4 ), X( 1 ), Y( 1 ) )
         
*  Plot the row.
*  =============
      ELSE IF ( ARDDEF .EQ. 'ROW' ) THEN
         CALL SGS_LINE( XLOW, Y( 1 ), XHIGH, Y( 1 ) )

*  Plot the column.
*  ================
      ELSE IF ( ARDDEF .EQ. 'COLUMN' ) THEN
         CALL SGS_LINE( X( 1 ), YLOW, X( 1 ), YHIGH)

*  Plot the line.
*  ==============
      ELSE IF ( ARDDEF .EQ. 'LINE' ) THEN
         CALL SGS_LINE( X( 1 ), Y( 1 ), X( 2 ), Y( 2 ) )

*  Plot a frame round the whole image.
*  ===================================
      ELSE IF ( ARDDEF .EQ. 'WHOLE' ) THEN
         CALL SGS_BOX( XLOW, XHIGH, YLOW, YHIGH )

*  Report an error for any other shape.
      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'SH', ARDDEF )
         CALL ERR_REP( 'KPS1_AGNDR_ERR1', 'Subroutine KPS1_AGNDR does '/
     :     /' not yet support shape ''^SH'' (programming error).',
     :     STATUS )
      END IF

*  Ensure that the object is completely plotted before returning.
      CALL SGS_FLUSH

      END
