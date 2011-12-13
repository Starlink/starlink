      SUBROUTINE ARD1_CNTDR( IPLOT, DIM1, DIM2, ARRAY, XLL, YLL, XSIZE,
     :                       YSIZE, CONT, MAXPTS, XY, DONE, STATUS )
*+
*  Name:
*     ARD1_CNTDR

*  Purpose:
*     Outlines regions of a 2-d array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_CNTDR( IPLOT, DIM1, DIM2, ARRAY, XLL, YLL, XSIZE,
*                      YSIZE, CONT, MAXPTS, XY, DONE, STATUS )

*  Description:
*     This routine outlines regions of a two-dimensional sub-array.

*  Arguments:
*     IPLOT = INTEGER (Given)
*        An AST pointer to the Plot through which the graphics will be
*        produced. The Current Frame should describe the GRID coordinates
*        of the array to be contoured.
*     DIM1 = INTEGER (Given)
*        The first dimension of the two-dimensional array.
*     DIM2 = INTEGER (Given)
*        The second dimension of the two-dimensional array.
*     ARRAY( DIM1, DIM2 ) = INTEGER (Given)
*        Two-dimensional array to be contoured.
*     XLL = INTEGER (Given)
*        The lower pixel index bound of the selected sub-array.
*     YLL = INTEGER (Given)
*        The upper pixel index bound of the selected sub-array.
*     XSIZE = INTEGER (Given)
*        The number of pixel along the x axis in the sub-array to be contoured.
*     YSIZE = INTEGER (Given)
*        The number of pixel along the y axis in the sub-array to be contoured.
*     CONT = INTEGER (Given)
*        The contour level. If positive, the contour contains only those pixels
*        with the specified pixel value. If zero, then the contour contains
*        all pixels with non-zero values. If negative, the contour
*        contains all pixels with a value greater than or equal to the
*        absolute contour value.
*     MAXPTS = INTEGER (Given)
*        Maximum number of positions in each axis that define the locus of
*        a contour.
*     XY( MAXPTS, 2 ) = DOUBLE PRECISION (Returned)
*        Workspace to hold the X and Y positions of the contour.
*     DONE( 0:XSIZE, 0:YSIZE ) = LOGICAL (Returned)
*        Workspace to store log of pixels which have already been
*        contoured.
*     STATUS = INTEGER (Given)
*        Global status value.

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
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-SEP-2001 (DSB):
*        Original version.
*     7-MAR-2011 (DSB):
*        Use AST for graphics buffering instead of direct calls to pgplot.
*        This involved renaming the file from ard1_cntdr.F to ard1_cntdr.f.
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants
      INCLUDE 'PRM_PAR'          ! VAL constants

*  Arguments Given:
      INTEGER IPLOT
      INTEGER DIM1
      INTEGER DIM2
      INTEGER ARRAY( DIM1, DIM2 )
      INTEGER XLL
      INTEGER YLL
      INTEGER XSIZE
      INTEGER YSIZE
      INTEGER CONT
      INTEGER MAXPTS

*  Arguments Returned:
      LOGICAL DONE( 0:XSIZE, 0:YSIZE )
      DOUBLE PRECISION XY( MAXPTS, 2 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop counter through sub-array columns
      INTEGER ICURR              ! Index of current Frame in IPLOT
      INTEGER II                 ! X element numbers of current pixel in sub-array
      INTEGER II0                ! X element numbers of first pixel in sub-array
      INTEGER IX                 ! X element numbers of current pixel in full-size array
      INTEGER IY                 ! Y element numbers of current pixel in full-size array
      INTEGER J                  ! Loop counter through sub-array lines
      INTEGER JJ                 ! Y element numbers of current pixel in sub-array
      INTEGER JJ0                ! Y element numbers of first pixel in sub-array
      INTEGER LSIDE              ! Index of the pixel edge to be drawn
      INTEGER M                  ! Statement function dummy argument
      INTEGER N                  ! Statement function dummy argument
      INTEGER NPTS               ! Number of points in contour's locus
      INTEGER SIDE               ! Index of the pixel edge to be drawn next
      INTEGER SIDE0              ! Index of the first drawn pixel edge
      INTEGER XUU                ! Upper X bound of plot area
      INTEGER YUU                ! Upper Y bound of plot area
      LOGICAL IN                 ! Is current pixel inside the contour?
      LOGICAL INIMG              ! Inside the sub-array?
      LOGICAL INR                ! Is right-hand pixel inside the contour?
      LOGICAL INSIDE             ! Inside the contour?
      LOGICAL INT                ! Is upper pixel inside the contour?
      LOGICAL TRACE              ! Is there a contour to be traced out?

*   Internal References:
      INIMG( M, N ) = ( M .GE. XLL .AND. M .LE. XUU .AND.
     :                  N .GE. YLL .AND. N .LE. YUU )

      INSIDE( M, N ) = ( INIMG( M, N ) .AND. (
     :                   ( ( CONT .EQ. 0  ) .AND.
     :                     ( ARRAY( M, N ) .NE. 0 ) ) .OR.
     :                   ( ( CONT .GT. 0  ) .AND.
     :                     ( ARRAY( M, N ) .EQ. CONT ) ) .OR.
     :                   ( ( CONT .LT. 0  ) .AND.
     :                     ( ARRAY( M, N ) .GE. -CONT ) )
     :                  ) )

*.

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Simplify the Plot. This adds a new Current Frame into the Plot, so note
*  the index of the original Current Frame so that it can be re-instated later.
*  This can help to speed up the drawing, and also avoids the possibility
*  of the Mapping going via a Frame in which the positions are undefined.
      ICURR = AST_GETI( IPLOT, 'CURRENT', STATUS )
      CALL ARD1_ASSIM( IPLOT, STATUS )

*  Initialise the store of cells done. The DONE flag is .TRUE. for pixels
*  which are already known to be "on" a contour which has already been
*  drawn. A pixel is "on" a contour if its right or top edge forms part
*  of the contour. "A contour" in this context is a closed segment of
*  the curve enclosing the interior pixels. Note, a pixel which is "on" a
*  contour is not necessarily "inside" the contour.
      DO J = 0, YSIZE
         DO I = 0, XSIZE
            DONE( I, J ) = .FALSE.
         END DO
      END DO

*  Store the upper bounds of the area to be done.
      XUU = XLL + XSIZE - 1
      YUU = YLL + YSIZE - 1

*  Buffer all graphics output produced while drawing the contour.
      CALL AST_BBUF( IPLOT, STATUS )

*  Scan the rows and pixels of the sub-array to be contoured.
      DO J = 0, YSIZE
         DO I = 0, XSIZE

*  If this pixel is known to be on a segment which has already been
*  drawn, skip over it (otherwise the same segment contour would be drawn
*  again).
            IF( .NOT. DONE( I, J ) ) THEN

*  Find the indices of the current pixel within the full two-dimensional array.
               IY = J + YLL - 1
               IX = I + XLL - 1

*  Assume that this pixel is not on the contour.
               TRACE = .FALSE.
               NPTS = 0

*  Store flags indicating if this pixel, the one above, and the one to
*  the right are inside the contour.
               IN = INSIDE( IX, IY )
               INR = INSIDE( IX + 1, IY )
               INT = INSIDE( IX, IY + 1 )

*  If this pixel is inside the contour, and the the one to the right is
*  not, of if this pixel is not inside the contour, and the the one to the
*  right is, the right edge of this pixel is on the contour.
               IF( IN .NEQV. INR ) THEN

*  Indicate that we should trace the contour out.
                  TRACE = .TRUE.

*  Save the indices (II,JJ) of the pixel which is "inside" the contour.
*  Also indicate which edge of pixel (II,JJ) is part of the contour, and
*  store the coords of the anti-clockwise end point (the start of the contour).
                  IF( IN ) THEN
                     II = I
                     SIDE = 3
                     XY( 1, 1 ) = DBLE( IX ) + 0.5D0
                     XY( 1, 2 ) = DBLE( IY ) + 0.5D0
                  ELSE
                     II = I + 1
                     SIDE = 1
                     XY( 1, 1 ) = DBLE( IX ) - 0.5D0
                     XY( 1, 2 ) = DBLE( IY ) - 0.5D0
                  END IF
                  JJ = J
                  NPTS = 1

*  Otherwise, if the top edge of this pixel is on the contour, indicate that we
*  should trace the contour out.
               ELSE IF( IN .NEQV. INT ) THEN
                  TRACE = .TRUE.
                  II = I
                  IF( IN ) THEN
                     JJ = J
                     SIDE = 2
                     XY( 1, 1 ) = DBLE( IX ) - 0.5D0
                     XY( 1, 2 ) = DBLE( IY ) + 0.5D0
                  ELSE
                     JJ = J + 1
                     SIDE = 4
                     XY( 1, 1 ) = DBLE( IX ) + 0.5D0
                     XY( 1, 2 ) = DBLE( IY ) - 0.5D0
                  END IF
                  NPTS = 1

               END IF

*  If this pixel is on the contour, trace out the whole contour in a
*  clockwise direction. The "current" pixel (II,JJ) is inside the
*  contour, and one if its edges (indicated by SIDE) forms part of the
*  contour.
               SIDE0 = SIDE
               II0 = II
               JJ0 = JJ
               DO WHILE( TRACE )

*  Find the indices of the current pixel in the full array.
                  IX = II + XLL - 1
                  IY = JJ + YLL - 1

*  Increment the number of positions in the polyline.
                  NPTS = NPTS + 1

*  Save the side which is about to be drawn.
                  LSIDE = SIDE

*  If the left side of this pixel is to be drawn...
                  IF( SIDE .EQ. 1 ) THEN

*  Extend the polyline to the clockwise (top) end of the left edge.
                     XY( NPTS, 1 ) = DBLE( IX ) - 0.5D0
                     XY( NPTS, 2 ) = DBLE( IY ) + 0.5D0

*  Indicate that we now know that the pixel to the left is on the contour.
                     DONE( II - 1, JJ ) = .TRUE.

*  If the pixel above this one is not inside, indicate that the top edge of
*  this pixel is to be drawn next.
                     IF( .NOT. INSIDE( IX, IY + 1 ) ) THEN
                        SIDE = 2

*  Otherwise, if the pixel above and to the left of this one is not inside,
*  indicate that the left edge of the pixel above this one is to be drawn
*  next.
                     ELSE IF( .NOT. INSIDE( IX - 1, IY + 1 ) ) THEN
                        JJ = JJ + 1
                        SIDE = 1

*  Otherwise, indicate that the bottom edge of the pixel above and to the
*  left of this one is to be drawn next.
                     ELSE
                        II = II - 1
                        JJ = JJ + 1
                        SIDE = 4
                     END IF

*  If the top edge of this pixel is to be drawn...
                  ELSE IF( SIDE .EQ. 2 ) THEN

*  Extend the polyline to the clockwise (right) end of the top edge.
                     XY( NPTS, 1 ) = DBLE( IX ) + 0.5D0
                     XY( NPTS, 2 ) = DBLE( IY ) + 0.5D0

*  Indicate that we now that the current pixel is on the contour.
                     DONE( II, JJ ) = .TRUE.

*  If the pixel to the right of this one is not inside, indicate that the
*  right edge of this pixel is to be drawn next.
                     IF( .NOT. INSIDE( IX + 1, IY ) ) THEN
                        SIDE = 3

*  Otherwise, if the pixel above and to the right of this one is not inside,
*  indicate that the top edge of the pixel to the right of this one is to be
*  drawn next.
                     ELSE IF( .NOT. INSIDE( IX + 1, IY + 1 ) ) THEN
                        II = II + 1
                        SIDE = 2

*  Otherwise, indicate that the left edge of the pixel above and to the
*  right of this one is to be drawn next.
                     ELSE
                        II = II + 1
                        JJ = JJ + 1
                        SIDE = 1
                     END IF

*  If the right edge of this pixel is to be drawn...
                  ELSE IF( SIDE .EQ. 3 ) THEN

*  Extend the polyline to the clockwise (bottom) end of the right edge.
                     XY( NPTS, 1 ) = DBLE( IX ) + 0.5D0
                     XY( NPTS, 2 ) = DBLE( IY ) - 0.5D0

*  Indicate that we now that the current pixel is on the contour.
                     DONE( II, JJ ) = .TRUE.

*  If the pixel below this one is not inside, indicate that the
*  bottom edge of this pixel is to be drawn next.
                     IF( .NOT. INSIDE( IX, IY - 1 ) ) THEN
                        SIDE = 4

*  Otherwise, if the pixel below and to the right of this one is not inside,
*  indicate that the right edge of the pixel below this one is to be
*  drawn next.
                     ELSE IF( .NOT. INSIDE( IX + 1, IY - 1 ) ) THEN
                        JJ = JJ - 1
                        SIDE = 3

*  Otherwise, indicate that the top edge of the pixel below and to the
*  right of this one is to be drawn next.
                     ELSE
                        II = II + 1
                        JJ = JJ - 1
                        SIDE = 2
                     END IF

*  If the bottom edge of this pixel is to be drawn...
                  ELSE

*  Extend the polyline to the clockwise (left) end of the bottom edge.
                     XY( NPTS, 1 ) = DBLE( IX ) - 0.5D0
                     XY( NPTS, 2 ) = DBLE( IY ) - 0.5D0

*  Indicate that we now the bottom pixel is on the contour.
                     DONE( II, JJ - 1 ) = .TRUE.

*  If the pixel to the left of this one is not inside, indicate that the
*  left edge of this pixel is to be drawn next.
                     IF( .NOT. INSIDE( IX - 1, IY ) ) THEN
                        SIDE = 1

*  Otherwise, if the pixel below and to the left of this one is not inside,
*  indicate that the bottom edge of the pixel to the left of this one is to be
*  drawn next.
                     ELSE IF( .NOT. INSIDE( IX - 1, IY - 1 ) ) THEN
                        II = II - 1
                        SIDE = 4

*  Otherwise, indicate that the right edge of the pixel below and to the
*  left of this one is to be drawn next.
                     ELSE
                        II = II - 1
                        JJ = JJ - 1
                        SIDE = 3
                     END IF

                  END IF

*  If the new side is the same as the old side, we do not need to draw
*  it since the next point will be an extension of it.
                  IF( SIDE .EQ. LSIDE ) NPTS = NPTS - 1

*  If the polyline buffer is now full, draw it and reset the buffer.
                  IF( NPTS .EQ. MAXPTS ) THEN
                     CALL AST_POLYCURVE( IPLOT, NPTS, 2, MAXPTS, XY,
     :                                   STATUS )
                     XY( 1, 1 ) = XY( NPTS, 1 )
                     XY( 1, 2 ) = XY( NPTS, 2 )
                     NPTS = 1
                  END IF

*  If we are back at the start, leave the loop. IF we have cancelled the
*  previous point because it did not change direction, reinstate it to
*  ensure that the contour is closed properly.
                  IF( II .EQ. II0 .AND. JJ .EQ. JJ0 .AND.
     :                SIDE .EQ. SIDE0 ) THEN
                     IF( SIDE .EQ. LSIDE ) NPTS = NPTS + 1
                     TRACE = .FALSE.
                  END IF

               END DO

*  Draw the polyline.
               IF( NPTS .GT. 1 ) CALL AST_POLYCURVE( IPLOT, NPTS, 2,
     :                                               MAXPTS, XY,
     :                                               STATUS )

            END IF

         END DO

      END DO

 999  CONTINUE

*  Flush the buffer holding graphics output produced while drawing this
*  contour.
      CALL AST_EBUF( IPLOT, STATUS )

*  Remove the Current Frame added by ARD1_ASSIM and re-instate the original
*  Current Frame.
      CALL AST_REMOVEFRAME( IPLOT, AST__CURRENT, STATUS )
      CALL AST_SETI( IPLOT, 'CURRENT', ICURR, STATUS )

      END
