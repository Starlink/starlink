      SUBROUTINE KPS1_AGNST( NPTS, ARDDEF, IGRP, X1, X2, Y1, Y2, 
     :                       X, Y, STATUS )

*+
*  Name:
*     KPS1_AGNST

*  Purpose:
*     Stores an ARD description in a group for ARDGEN.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_AGNST( NPTS, ARDDEF, IGRP, X1, X2, Y1, Y2, X, Y, 
*                      STATUS )

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
*     X( NPTS ) = REAL (Given)
*        The x co-ordinate of the points and other region defining 
*        information.
*     Y( NPTS ) = REAL (Given)
*        The y co-ordinate of the points and other region-defining
*        information.
*     STATUS = INTEGER(Given and Returned)
*        The global status.

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
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SSE global definitions

*  Arguments Given:
      INTEGER NPTS
      CHARACTER * ( * ) ARDDEF
      INTEGER IGRP
      REAL X1
      REAL X2
      REAL Y1
      REAL Y2
      REAL X( NPTS )
      REAL Y( NPTS )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      REAL RTOD                  ! Factor for converting radians to
      PARAMETER ( RTOD = 57.29578 ) ! degrees

*  Local Variables:
      REAL DIRECT                ! Position angle of ellipse
      INTEGER I                  ! Loop variable
      CHARACTER * ( 80 ) LINE    ! Buffer for writing ARD output file
      REAL MD                    ! Minimum distance to the edge
      INTEGER NC                 ! Number of bytes of the 
                                 ! output string occupied
      REAL RADIUS                ! Radius of the circle required
      REAL SMA                   ! Semi-major axis value
      REAL SMI                   ! Semi-minor axis value
      REAL XC                    ! Box centre
      REAL XL                    ! Length of box side
      REAL YC                    ! Box centre
      REAL YL                    ! Length of box side
   
*  Statement Functions:
      REAL ROUND, XX
      ROUND( XX ) = 0.1 * REAL( NINT( XX * 10.0 ) ) ! Round to 1
                                 ! decimal place

*.

*  Check the inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Set THE initial value for output string.
      LINE = ' ' 

*  Set the characters used counter to zero.
      NC = 0

*  Box.
      IF ( ARDDEF .EQ. 'BOX' ) THEN

*  Find the centre and side lengths.
         XC = X( 1 )
         YC = Y( 1 )
         XL = ABS( X( 3 ) - X( 2 ) )
         YL = ABS( Y( 3 ) - Y( 2 ) )

*  Create the string, rounding the co-ordinate values to 1 decimal
*  place.
         CALL CHR_PUTC( 'BOX( ', LINE, NC )
         CALL CHR_PUTR( ROUND( XC ), LINE, NC )
         CALL CHR_PUTC( ', ', LINE, NC )
         CALL CHR_PUTR( ROUND( YC ), LINE, NC )
         CALL CHR_PUTC( ', ', LINE, NC )
         CALL CHR_PUTR( ROUND( XL ), LINE, NC )
         CALL CHR_PUTC( ', ', LINE, NC )
         CALL CHR_PUTR( ROUND( YL ), LINE, NC )
         CALL CHR_PUTC( ' ) ', LINE, NC )
 
*  Frame.
      ELSE IF ( ARDDEF .EQ. 'FRAME' ) THEN

*  Find the sminimum distance to an edge.  X axis.
         MD = X( 1 )

*  Create the string.
         CALL CHR_PUTC( 'FRAME( ', LINE, NC )
         CALL CHR_PUTR( ROUND( MD ), LINE, NC )
         CALL CHR_PUTC( ' ) ', LINE, NC )

*  Point.
      ELSE IF ( ARDDEF .EQ. 'POINT' ) THEN

*  Create the string.
         CALL CHR_PUTC( 'POINT( ', LINE, NC )
         CALL CHR_PUTR( ROUND( X( 1 ) ), LINE, NC )
         CALL CHR_PUTC( ', ', LINE, NC )
         CALL CHR_PUTR( ROUND( Y( 1 ) ), LINE, NC )
         CALL CHR_PUTC( ' ) ', LINE, NC )

*  Row.
      ELSE IF ( ARDDEF( 1:3 ) .EQ. 'ROW' ) THEN

*  Create the string.
         CALL CHR_PUTC( 'ROW( ', LINE, NC )
         CALL CHR_PUTR( ROUND( Y( 1 ) ), LINE, NC )
         CALL CHR_PUTC( ' ) ', LINE, NC )

*  Rectangle.
      ELSE IF ( ARDDEF .EQ. 'RECTANGLE' ) THEN

*  Create the string.
         CALL CHR_PUTC( 'RECT( ', LINE, NC )
         CALL CHR_PUTR( ROUND( X( 1 ) ), LINE, NC )
         CALL CHR_PUTC( ', ', LINE, NC )
         CALL CHR_PUTR( ROUND( Y( 1 ) ), LINE, NC )
         CALL CHR_PUTC( ', ', LINE, NC )
         CALL CHR_PUTR( ROUND( X( 2 ) ), LINE, NC )
         CALL CHR_PUTC( ', ', LINE, NC )
         CALL CHR_PUTR( ROUND( Y( 2 ) ), LINE, NC )
         CALL CHR_PUTC( ' ) ', LINE, NC )


*  Rotated box.
      ELSE IF ( ARDDEF .EQ. 'ROTBOX' ) THEN

*  Create the string.
         CALL CHR_PUTC( 'ROTBOX( ', LINE, NC )
         CALL CHR_PUTR( ROUND( X( 5 ) ), LINE, NC )
         CALL CHR_PUTC( ', ', LINE, NC )
         CALL CHR_PUTR( ROUND( Y( 5 ) ), LINE, NC )
         CALL CHR_PUTC( ', ', LINE, NC )
         CALL CHR_PUTR( ROUND( X( 6 ) ), LINE, NC )
         CALL CHR_PUTC( ', ', LINE, NC )
         CALL CHR_PUTR( ROUND( Y( 6 ) ), LINE, NC )
         CALL CHR_PUTC( ', ', LINE, NC )
         CALL CHR_PUTR( X( 7 ), LINE, NC )
         CALL CHR_PUTC( ' ) ', LINE, NC )


*  Column.
      ELSE IF ( ARDDEF .EQ. 'COLUMN' ) THEN

*  Create the string.
         CALL CHR_PUTC( 'COLUMN( ', LINE, NC )
         CALL CHR_PUTR( ROUND( X( 1 ) ), LINE, NC )
         CALL CHR_PUTC( ' ) ', LINE, NC )

*  Ellipse.
      ELSE IF ( ARDDEF .EQ. 'ELLIPSE' ) THEN

*  Use the stored values.            
         SMA = X( 4 )
         SMI = Y( 4 )
         DIRECT = X( 5 ) * RTOD

*  Create the string.
         CALL CHR_PUTC( 'ELLIPSE( ', LINE, NC )
         CALL CHR_PUTR( ROUND( X( 1 ) ), LINE, NC )
         CALL CHR_PUTC( ', ', LINE, NC )
         CALL CHR_PUTR( ROUND( Y( 1 ) ), LINE, NC )
         CALL CHR_PUTC( ', ', LINE, NC )
         CALL CHR_PUTR( ROUND( SMA ), LINE, NC )
         CALL CHR_PUTC( ', ', LINE, NC )
         CALL CHR_PUTR( ROUND( SMI ), LINE, NC )
         CALL CHR_PUTC( ', ', LINE, NC )
         CALL CHR_PUTR( DIRECT, LINE, NC )
         CALL CHR_PUTC( ' ) ', LINE, NC )
 
*  Circle. 
      ELSE IF ( ARDDEF .EQ. 'CIRCLE' ) THEN

*  Find the circle radius. 
         RADIUS = X( 3 )

*  Create the string.
         CALL CHR_PUTC( 'CIRCLE( ', LINE, NC )
         CALL CHR_PUTR( ROUND( X( 1 ) ), LINE, NC )
         CALL CHR_PUTC( ', ', LINE, NC )
         CALL CHR_PUTR( ROUND( Y( 1 ) ), LINE, NC )
         CALL CHR_PUTC( ', ', LINE, NC )
         CALL CHR_PUTR( ROUND( RADIUS ), LINE, NC )
         CALL CHR_PUTC( ' ) ', LINE, NC )

*  Line.
      ELSE IF ( ARDDEF .EQ. 'LINE' ) THEN

*  Create the string.
         CALL CHR_PUTC( 'LINE( ', LINE, NC )
         CALL CHR_PUTR( ROUND( X( 1 ) ), LINE, NC )
         CALL CHR_PUTC( ', ', LINE, NC )
         CALL CHR_PUTR( ROUND( Y( 1 ) ), LINE, NC )
         CALL CHR_PUTC( ', ', LINE, NC )
         CALL CHR_PUTR( ROUND( X( 2 ) ), LINE, NC )
         CALL CHR_PUTC( ', ', LINE, NC )
         CALL CHR_PUTR( ROUND( Y( 2 ) ), LINE, NC )
         CALL CHR_PUTC( ' ) ', LINE, NC )

*  Polygon.
      ELSE IF ( ARDDEF .EQ. 'POLYGON' ) THEN

*  Issue a warning and return without further action if there are two
*  or less vertices in the polygon.
         IF ( NPTS .LE. 2 ) THEN
            CALL MSG_OUT( 'KPS1_AGNST_MSG1', 'Too few positions '/
     :                    /'supplied to define a polygon.', STATUS )
            GO TO 999
         END IF

*  Create opening string.
         CALL CHR_PUTC( 'POLYGON( ', LINE, NC )

*  Add the first pair of co-ordinates to the string.
         CALL CHR_PUTR( ROUND( X( 1 ) ), LINE, NC )
         CALL CHR_PUTC( ', ', LINE, NC )
         CALL CHR_PUTR( ROUND( Y( 1 ) ), LINE, NC )
         CALL CHR_PUTC( ', ', LINE, NC )

*  Loop round the remaining pairs of co-ordinates.
         DO I = 2, NPTS

*  Store the previous line in the group.
            CALL GRP_PUT( IGRP, 1, LINE( : NC ), 0, STATUS )

*  Form the new line, padding it at the start with blanks to line the
*  co-ordinates up with the ones in the previous line.
            NC = 9
            LINE = ' '

            CALL CHR_PUTR( ROUND( X( I ) ), LINE, NC )
            CALL CHR_PUTC( ', ', LINE, NC )
            CALL CHR_PUTR( ROUND( Y( I ) ), LINE, NC )

            IF ( I .NE. NPTS) THEN
               CALL CHR_PUTC( ', ', LINE, NC )
            ELSE
               CALL CHR_PUTC( ' )', LINE, NC )
            END IF

         END DO

*  Whole.
      ELSE IF ( ARDDEF .EQ. 'WHOLE' ) THEN

*  Create the string.
         CALL CHR_PUTC( 'WHOLE', LINE, NC )

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

 999  CONTINUE

      END
