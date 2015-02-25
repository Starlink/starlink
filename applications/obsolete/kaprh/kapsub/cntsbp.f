*+  CNTSBP - turbo contour an image
*
      SUBROUTINE CNTSBP( CELABL, CELABR, CELATR, CELATL, CELFRA, CLEVEL,
     :                   XOFF, YOFF, ENTER, DX, DY, DIM1, DIM2, NPTS,
     :                   XP, YP, STATUS )
*
*    Description :
*
*     The routine follows a contour in a notional sub-pixel array, via
*     linear interpolation of a full-pixel cell.  The centres of the
*     corner sub-pixels are the centres of the full-sized pixels in the
*     contour cell. The routine updates the locus of points only when
*     the increment from the last point is greater than defined x,y
*     resolutions.
*
*     The quality of output is not good, especially if a pair of corners
*     are equal to the contour height.  A smoothing algorithm is better
*     and will replace this routine at the next release.
*
*    Invocation :
*
*     CALL CNTSBP( CELABL, CELABR, CELATR, CELATL, CELFRA, CLEVEL,
*    :             XOFF, YOFF, ENTER, DX, DY, DIM1, DIM2, NPTS,
*    :             XP, YP, STATUS )
*
*    Arguments :
*
*     CELABL = REAL( READ )
*         Bottom-left pixel value of the full-sized pixel cell.
*     CELABR = REAL( READ )
*         Bottom-right pixel value of the full-sized pixel cell.
*     CELATR = REAL( READ )
*         Top-right pixel value of the full-sized pixel cell.
*     CELATL = REAL( READ )
*         Top-left pixel value of the full-sized pixel cell.
*     CELFRA = REAL( READ )
*         Fractional position along the full-sized cell side where the
*           contour enters the cell.
*     CLEVEL = REAL( READ )
*         Contour level being traced through the cell.
*     XOFF = REAL( READ )
*         x co-ordinate of the bottom-left pixel of the cell.
*     YOFF = REAL( READ )
*         y co-ordinate of the bottom-left pixel of the cell.
*     ENTER = INTEGER( READ )
*         The side from which the contour entered the cell, where 1 is
*            bottom, 2 is right, 3 is top, and 4 is left.
*     DX = REAL( READ )
*         The x resolution of the contour segments, i.e. only contour
*           segments longer than this will be plotted.
*     DY = REAL( READ )
*         The y resolution of the contour segments, i.e. only contour
*           segments longer than this will be plotted.
*     DIM1 = INTEGER( READ )
*         The first dimension of the notional 2-d sub-pixel array.
*     DIM2 = INTEGER( READ )
*         The second dimension of the notional 2-d sub-pixel array.
*     NPTS = INTEGER( READ, WRITE )
*         The number of points stored in the contour locus so far.
*     XP( * ) = REAL( READ, WRITE )
*         Work array to store the contour locus, x positions
*     YP( * ) = REAL( READ, WRITE )
*         Work array to store the contour locus, y positions
*     STATUS  = INTEGER( READ )
*         Global status value
*
*    Method :
*
*     Check inherited status
*     Find the sub-pixel cell from which to start tracing the contour
*       given the entrance cell
*     Compute the fractional co-ordinates of the sub-cell pixels
*     Linearly interpolate to compute the sub-array pixel values
*     Determine which edges of the sub-cell are crossed by a contour,
*       excluding the entrance side
*     If at least one cell edge is crossed then
*        Initialise variables
*        Loop to follow the contour
*           For each side the contour crosses compute the fractional
*             position of the intersection along that side, and store
*             possible exit sides
*           If all four edges of the cell are crossed then
*              Make sure sides connected by the contours do in fact do
*                so by swapping the co-ordinates (two cases, one caused
*                by the ordering of the searching of the sides, and the
*                second uses triangular contouring to decide)
*           Endif
*           Initialise the last-contour point stored if this is the
*             start of the following the contour within the cell
*           If the segment is longer than the plotting resolution then
*              Store the point and update the last stored point
*           Endif
*           Record that contour tracking has started and side it left
*             the sub-cell
*           For a given exit side shift the X or Y position by a
*             sub-pixel, exiting the contour-tracking loop if the
*             sub-cell lies outside the full-size cell. Compute the new
*             sub-cell pixel values by linear interpolation, then
*             determine which sides the contour crosses excluding the
*             side it entered
*        Endfor
*     Endif
*     End
*
*    Bugs :
*
*     An extra non-contour line is drawn when when diagonal pixels are
*     equal to the contour level.
*
*    Authors :
*
*     Malcolm J. Currie  STARLINK (RAL::CUR).
*
*    History :
*
*     1989 Aug 25:  Original version based on CNTTUR (RAL::CUR).
*
*    Type Definitions :

      IMPLICIT NONE            ! No implicit typing

*    Global constants :

      INCLUDE 'SAE_PAR'        ! Standard SAE constants

*    Import :

      REAL CELABL              ! Data value of bottom-left pixel in
                               ! the current full-pixel cell
      REAL CELABR              ! Ditto, bottom-right pixel
      REAL CELATR              ! Ditto, top-right pixel
      REAL CELATL              ! Ditto, top-left pixel
      REAL CELFRA              ! Fractional cell extent of contour
                               ! intersection point with the supplied
                               ! full-pixel cell
      REAL XOFF, YOFF          ! Co-ordinate of the the bottom-level
                               ! pixel in the full-pixel cell
      INTEGER DIM1, DIM2       ! Number of x,y sub pixels in full-size
                               ! cell
      REAL CLEVEL              ! Contour level
      REAL DX, DY              ! x,y resolution of the plotting
      INTEGER ENTER            ! Side of the cell from which to track the
                               ! contour

*    Import-Export :

      INTEGER NPTS             ! Number of locus points
      REAL XP( * ), YP( * )    ! Locus of the contour

*    Status :

      INTEGER STATUS           ! Inherited error status

*    Local Constants :

      INTEGER BOTTOM
      PARAMETER ( BOTTOM = 1 )

      INTEGER RIGHT
      PARAMETER ( RIGHT = 2 )

      INTEGER TOP
      PARAMETER ( TOP = 3 )

      INTEGER LEFT
      PARAMETER ( LEFT = 4 )

*    Local variables :

                               ! True if:
      LOGICAL TESTBL           ! Whether the bottom-left pixel in the
                               ! current cell exceeds (or is equal to)
                               ! the current contour level
      LOGICAL TESTBR           ! Ditto, bottom-right pixel
      LOGICAL TESTTR           ! Ditto, top right pixel
      LOGICAL TESTTL           ! Ditto, top-left pixel
      LOGICAL CROSSB           ! Whether a contour at the current level
                               ! crosses the bottom edge of the
                               ! current cell
      LOGICAL CROSSR           ! Ditto, right edge
      LOGICAL CROSST           ! Ditto, top edge
      LOGICAL CROSSL           ! Ditto, left edge
      LOGICAL FIRST            ! No contour locus stored so far in the
                               ! input full-pixel cell

      INTEGER EXIT             ! Flag to indicate through which edge
                               ! of a cell a contour line makes its
                               ! exit
      INTEGER NCROSS           ! Number of contour lines leaving a cell
      INTEGER PREXIT
      INTEGER X                ! X (2-dimensional) index of the
                               ! bottom-left pixel in a cell
      INTEGER Y                ! Ditto, Y index

      REAL CENTRE              ! Average pixel value in the cell
      REAL FRACT               ! Fractional cell extent of contour
                               ! intersection point with a cell edge
      REAL PIXBL               ! Data value of bottom-left pixel in
                               ! the current cell
      REAL PIXBR               ! Ditto, bottom-right pixel
      REAL PIXTR               ! Ditto, top-right pixel
      REAL PIXTL               ! Ditto, top-left pixel
      REAL RD1, RD2            ! Floating-point versions of the input
                               ! array dimensions
      REAL SWAP                ! Temporary variable for inter-changing
                               ! REAL values
      REAL W1, W2              ! Work varaibles to store interpolation
                               ! weights
      REAL XLAST, YLAST        ! Position of the last contour segment
                               ! plotted
      REAL XLEFT, YBOT         ! Fractional position of the bottom-left
                               ! pixel in the current cell
      REAL XRIGHT, YTOP        ! Fractional position of the top-right
                               ! pixel in the current cell
      REAL XCONT( 4 )          ! Buffer to contain contour X
                               ! coordinates in cell
      REAL YCONT( 4 )          ! Buffer to contain contour Y
                               ! coordinates in cell

*-

*   Check status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Note the sub-pixel array is notional.   The centres of its corner
*   elements are also the centres of the full-sized pixels, so that
*   contouring continues from the entrance position to the full-sized
*   array.  To achieve this the dimensions of the sub-pixel array
*   are each one more than the number spanning a full-pixel width (i.e.
*   there is a half a sub-pixel border around the full-sized cell.
*   Note this still lies within the original array. Hereafter "pixel"
*   means "sub-pixel".

      RD1 = REAL( DIM1 )
      RD2 = REAL( DIM2 )

*   Derive the cell position in which to start tracing the contour.
*   X, Y correspond to the bottom-left pixel in the cell.

      IF ( ENTER .EQ. BOTTOM ) THEN
         X = MAX( 1, MIN( DIM1, INT( DIM1 * CELFRA + 1.0 ) ) )
         Y = 1
      ELSE IF ( ENTER .EQ. TOP ) THEN
         X = MAX( 1, MIN( DIM1, INT( DIM1 * CELFRA + 1.0 ) ) )
         Y = DIM2
      ELSE IF ( ENTER .EQ. RIGHT ) THEN
         X = DIM1
         Y = MAX( 1, MIN( DIM2, INT( DIM2 * CELFRA + 1.0 ) ) )
      ELSE IF ( ENTER .EQ. LEFT ) THEN
         X = 1
         Y = MAX( 1, MIN( DIM2, INT( DIM2 * CELFRA + 1.0 ) ) )
      END IF

*   Compute the fractional co-ordinates of the cell pixels

      XLEFT = ( REAL( X ) - 1.0 ) / RD1
      XRIGHT = REAL( X ) / RD1
      YBOT = ( REAL( Y ) - 1.0 ) / RD2
      YTOP = REAL( Y ) / RD2

*   Linearly interpolate to derive pixel values of the cell

      W1 = 1.0 - XLEFT
      W2 = 1.0 - YBOT

*   ...first bottom left
      PIXBL = CELABL * W1 * W2 + CELABR * XLEFT * W2 +
     :        CELATR * XLEFT * YBOT + CELATL * W1 * YBOT

      W1 = 1.0 - XRIGHT

*   ...bottom right
      PIXBR = CELABL * W1 * W2 + CELABR * XRIGHT * W2 +
     :        CELATR * XRIGHT * YBOT + CELATL * W1 * YBOT

      W2 = 1.0 - YTOP

*   ...top right
      PIXTR = CELABL * W1 * W2 + CELABR * XRIGHT * W2 +
     :        CELATR * XRIGHT * YTOP + CELATL * W1 * YTOP

      W1 = 1.0 - XLEFT

*   ...top left
      PIXTL = CELABL * W1 * W2 + CELABR * XLEFT * W2 +
     :        CELATR * XLEFT * YTOP + CELATL * W1 * YTOP


*   Test each pixel value against the current contour level.
      TESTBL = PIXBL .GE. CLEVEL
      TESTBR = PIXBR .GE. CLEVEL
      TESTTR = PIXTR .GE. CLEVEL
      TESTTL = PIXTL .GE. CLEVEL


*   Determine which edges of the cell are crossed by the contour, by
*   intercomparing the results of the above tests for the pixels on
*   either end of each edge of the cell.  Note the entrance side is
*   excluded.
      CROSSB = ( TESTBL .NEQV. TESTBR ) .AND. ENTER .NE. BOTTOM
      CROSSR = ( TESTBR .NEQV. TESTTR ) .AND. ENTER .NE. RIGHT
      CROSST = ( TESTTR .NEQV. TESTTL ) .AND. ENTER .NE. TOP
      CROSSL = ( TESTTL .NEQV. TESTBL ) .AND. ENTER .NE. LEFT


*   Check that at least one cell edge is crossed by the contour.
      IF ( CROSSB .OR. CROSSR .OR. CROSST .OR. CROSSL ) THEN

*   Initialise variables for following the contour line.

*   ...the number of cell edges which the contour crosses --- the
*      entrance point is known:
         NCROSS = 1

*   ...the first segment of the contour in this full-pixel cell has
*      yet to be computed
         FIRST = .TRUE.

*   ...the edge through which the contour exited the current cell
*      and the previous cell:
         EXIT = 0
         IF ( ENTER .EQ. TOP ) THEN
            PREXIT = BOTTOM
         ELSE
            PREXIT = 0
         END IF

*   Loop to follow a contour line.  (Note that a
*   jump to statement 1 is used to terminate this loop.)
         DO WHILE ( .TRUE. )

*   If the contour crosses the bottom edge of the current cell, then
*   increment the count of edge intersections and remember this edge as
*   a potential contour exit point from the cell.
            IF ( CROSSB ) THEN
               NCROSS = NCROSS + 1
               EXIT = BOTTOM


*   Calculate how far along the edge the intersection occurs and add the
*   intersection point to the end of the list of contour coordinates.
               FRACT = ( CLEVEL - PIXBL ) / ( PIXBR - PIXBL )
               XCONT( NCROSS ) = ( REAL( X ) - 1.0 + FRACT )/ RD1 + XOFF
               YCONT( NCROSS ) = ( REAL( Y ) - 1.0 )/RD2 + YOFF
            END IF


*   Repeat the process of finding intersections for each edge of the
*   current cell.

*   ...the right hand edge:
            IF ( CROSSR ) THEN
               NCROSS = NCROSS + 1
               EXIT = RIGHT
               FRACT = ( CLEVEL - PIXBR ) / ( PIXTR - PIXBR )
               XCONT( NCROSS ) = REAL( X ) / RD1 + XOFF
               YCONT( NCROSS ) = ( REAL( Y ) - 1.0 + FRACT )/ RD2 + YOFF
            END IF

*   ...the top edge:
            IF ( CROSST ) THEN
               NCROSS = NCROSS + 1
               EXIT = TOP
               FRACT = ( CLEVEL - PIXTL ) / ( PIXTR - PIXTL )
               XCONT( NCROSS ) = ( REAL( X ) - 1.0 + FRACT )/ RD1 + XOFF
               YCONT( NCROSS ) = REAL( Y ) /RD2 + YOFF
            END IF

*   ...the left hand edge:
            IF ( CROSSL ) THEN
               NCROSS = NCROSS + 1
               EXIT = LEFT
               FRACT = ( CLEVEL - PIXBL ) / ( PIXTL - PIXBL )
               XCONT( NCROSS ) = ( REAL( X ) - 1.0 ) / RD1 + XOFF
               YCONT( NCROSS ) = ( REAL( Y ) - 1.0 + FRACT )/RD2 + YOFF
            END IF


*   If all four edges of the last cell were crossed by the contour, then
*   there will be two separate lines to plot within this cell.  The four
*   coordinates associated with this cell must be correctly paired so
*   that the lines do not cross. There are three possible pairing
*   combinations which leave the first coordinate (where the contour
*   first entered the cell) unchanged.  The correct pairing is achieved
*   by performing up to two coordinate interchanges.
            IF ( NCROSS .EQ. 4 )THEN

*   ...if the contour initially entered the cell through the top edge
*      (and therefore exited the previous cell through the bottom edge),
*      then the two lines will cross; this is a consequence of the
*      order in which the cell edges are considered.  If this has
*      happened, then swap the appropriate coordinates.
               IF ( PREXIT .EQ. BOTTOM ) THEN
                  SWAP = XCONT( 3 )
                  XCONT( 3 ) = XCONT( 2 )
                  XCONT( 2 ) = SWAP
                  SWAP = YCONT( 3 )
                  YCONT( 3 ) =YCONT( 2 )
                  YCONT( 2 ) = SWAP
               END IF

*   ...Check which pairs of coordinates are joined by comparing the
*      bottom-left and top-right corners with the value at the centre
*      of the cell (the mean of the four corners).  In other words
*      determine whether or not the contours pass between the top-right
*      bottom-left corners and the centre.  If they do then swap the
*      appropriate coordinates again.
               CENTRE = ( PIXBL + PIXBR + PIXTL + PIXTR ) * 0.25
               IF ( ( PIXTR .LT. CENTRE .AND. PIXBL .LT. CENTRE ) .OR.
     :              ( PIXTR .GT. CENTRE .AND. PIXBL .GT. CENTRE ) ) THEN
                  SWAP = XCONT( 4 )
                  XCONT( 4 ) = XCONT( 2 )
                  XCONT( 2 ) = SWAP
                  SWAP = YCONT( 4 )
                  YCONT( 4 ) = YCONT( 2 )
                  YCONT( 2 ) = SWAP
               END IF


*   If there are two line segments within the cell, then ignore the
*   second one.  It will be followed elsewhere.

*   End of "the contour crosses all four cell edges" condition.
            END IF

*   Initialise the last-contour point stored if none of the contour
*   line has been plotted.
            IF ( FIRST ) THEN
               XLAST = XP( NPTS )
               YLAST = YP( NPTS )
            END IF

*   Is the contour segment longer than the plotting resolution?
            IF ( ABS( XCONT( 2 ) - XLAST ) .GT. DX .OR.
     :           ABS( YCONT( 2 ) - YLAST ) .GT. DY ) THEN

*   Store the contour line.
               NPTS = NPTS + 1
               XP( NPTS ) = XCONT( 2 )
               YP( NPTS ) = YCONT( 2 )

*   Update the last position to be plotted.
               XLAST = XCONT( 2 )
               YLAST = YCONT( 2 )
            END IF


*   Note that part of the contour line has been stored.
            FIRST = .FALSE.
            NCROSS = 1


*   Remember which edge of the cell the contour exited.
            PREXIT = EXIT


*   If an exit was made through the bottom edge of the cell, then we
*   must move down.  Check that the current 2-dimensional image ARRAY
*   indices allow this (i.e. we are not already at the boundary of the
*   image region currently being contoured).  If we are, then
*   contour-following must stop at this point, so exit the contouring
*   loop.
            IF ( EXIT .EQ. BOTTOM ) THEN
               IF ( Y .LE. 1 ) GO TO 1


*   Adjust the 2-dimensional image indices of the bottom-left pixel and
*   the associated 1-dimensional (vectorised) ARRAY index to refer to
*   the bottom-left pixel of the new cell.
               Y = Y - 1

*   Copy the pixel values which are common to the old and new cells into
*   the appropriate variables for the new cell.
               PIXTL = PIXBL
               PIXTR = PIXBR

*   Compute the fractional co-ordinates of the cell pixels that have
*   changed.
               YBOT = ( REAL( Y ) - 1.0 ) / RD2
               YTOP = REAL( Y ) / RD2

*   Linearly interpolate to derive the other two pixel values of the
*   cell

               W1 = 1.0 - XLEFT
               W2 = 1.0 - YBOT

*   ...bottom left
               PIXBL = CELABL * W1 * W2 + CELABR * XLEFT * W2 +
     :                 CELATR * XLEFT * YBOT + CELATL * W1 * YBOT

               W1 = 1.0 - XRIGHT

*   ...bottom right
               PIXBR = CELABL * W1 * W2 + CELABR * XRIGHT * W2 +
     :                 CELATR * XRIGHT * YBOT + CELATL * W1 * YBOT


*   Similarly, copy the old TEST values and derive new ones where
*   needed.
               TESTTL = TESTBL
               TESTTR = TESTBR
               TESTBL = PIXBL .GE. CLEVEL
               TESTBR = PIXBR .GE. CLEVEL


*   Test whether the contour crosses each edge of the new cell.  Since
*   it entered through the top edge, the appropriate CROSS value
*   (CROSST) is set .FALSE., so that intersections with that edge will
*   not be considered (the contour must leave the cell through a
*   different edge).
               CROSSB = TESTBL .NEQV. TESTBR
               CROSSR = TESTBR .NEQV. TESTTR
               CROSST = .FALSE.
               CROSSL = TESTTL .NEQV. TESTBL


*   There is a separate section of code to handle moves in the remaining
*   three directions.  Each move follows the same pattern.

*   ...move right:
            ELSE IF ( EXIT .EQ. RIGHT ) THEN
               IF ( X .GE. DIM1 ) GO TO 1

               X = X + 1

               PIXBL = PIXBR
               PIXTL = PIXTR

               XLEFT = ( REAL( X ) - 1.0 ) / RD1
               XRIGHT = REAL( X ) / RD1

               W1 = 1.0 - XRIGHT
               W2 = 1.0 - YBOT

               PIXBR = CELABL * W1 * W2 + CELABR * XRIGHT * W2 +
     :                 CELATR * XRIGHT * YBOT + CELATL * W1 * YBOT

               W2 = 1.0 - YTOP
               PIXTR = CELABL * W1 * W2 + CELABR * XRIGHT * W2 +
     :                 CELATR * XRIGHT * YTOP + CELATL * W1 * YTOP

               TESTBL = TESTBR
               TESTTL = TESTTR
               TESTBR = PIXBR .GE. CLEVEL
               TESTTR = PIXTR .GE. CLEVEL

               CROSSB = TESTBL .NEQV. TESTBR
               CROSSR = TESTBR .NEQV. TESTTR
               CROSST = TESTTR .NEQV. TESTTL
               CROSSL = .FALSE.

*   ...move up:
            ELSE IF ( EXIT .EQ. TOP ) THEN
               IF ( Y .GE. DIM2 ) GO TO 1

               Y = Y + 1

               PIXBR = PIXTR
               PIXBL = PIXTL

               YBOT = ( REAL( Y ) - 1.0 ) / RD2
               YTOP = REAL( Y ) / RD2

               W1 = 1.0 - XRIGHT
               W2 = 1.0 - YTOP

               PIXTR = CELABL * W1 * W2 + CELABR * XRIGHT * W2 +
     :                 CELATR * XRIGHT * YTOP + CELATL * W1 * YTOP

               W1 = 1.0 - XLEFT

               PIXTL = CELABL * W1 * W2 + CELABR * XLEFT * W2 +
     :                 CELATR * XLEFT * YTOP + CELATL * W1 * YTOP

               TESTBR = TESTTR
               TESTBL = TESTTL
               TESTTR = PIXTR .GE. CLEVEL
               TESTTL = PIXTL .GE. CLEVEL

               CROSSB = .FALSE.
               CROSSR = TESTBR .NEQV. TESTTR
               CROSST = TESTTR .NEQV. TESTTL
               CROSSL = TESTTL .NEQV. TESTBL

*   ...move left:
            ELSE IF ( EXIT .EQ. LEFT ) THEN
               IF ( X .LE. 1 ) GO TO 1

               X = X - 1

               PIXTR = PIXTL
               PIXBR = PIXBL

               XLEFT = ( REAL( X ) - 1.0 ) / RD1
               XRIGHT = REAL( X ) / RD1

               W1 = 1.0 - XLEFT
               W2 = 1.0 - YBOT

               PIXBL = CELABL * W1 * W2 + CELABR * XLEFT * W2 +
     :                 CELATR * XLEFT * YBOT + CELATL * W1 * YBOT

               W2 = 1.0 - YTOP

               PIXTL = CELABL * W1 * W2 + CELABR * XLEFT * W2 +
     :                 CELATR * XLEFT * YTOP + CELATL * W1 * YTOP


               TESTTR = TESTTL
               TESTBR = TESTBL
               TESTBL = PIXBL .GE. CLEVEL
               TESTTL = PIXTL .GE. CLEVEL

               CROSSB = TESTBL .NEQV. TESTBR
               CROSSR = .FALSE.
               CROSST = TESTTR .NEQV. TESTTL
               CROSSL = TESTTL .NEQV. TESTBL
            END IF


*   End of "follow the contour" loop.
         END DO
    1    CONTINUE

*   End of "at least one edge of the initial cell is crossed by the
*   contour" condition.
      END IF

*   Exit routine.
      END
