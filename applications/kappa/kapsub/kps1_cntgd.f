      SUBROUTINE KPS1_CNTGD( OUTLIN, IPLOT, IGRP, DIM1, DIM2, ARRAY,
     :                       XLL, YLL, XSIZE, YSIZE, FAST,
     :                       CNTUSD, CNTLEN, CNTCLS, STATUS )
*+
*  Name:
*     KPS1_CNTGD

*  Purpose:
*     Draws a boundary around the good data

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_CNTGD( OUTLIN, IPLOT, IGRP, DIM1, DIM2, ARRAY, XLL, YLL,
*                      XSIZE, YSIZE, FAST, CNTUSD, CNTLEN, CNTCLS,
*                      STATUS )

*  Description:
*     This routine plots a a single pseudo-contour which encloses all the
*     good data in the supplied array.

*  Arguments:
*     OUTLIN = LOGICAL (Given)
*        If .TRUE. then the contour outlines the entire array, not just
*        the good data values.
*     IPLOT = INTEGER (Given)
*        An AST pointer to the Plot through which the graphics will be
*        produced. The Current Frame should describe the GRID coordinates
*        of the array to be contoured.
*     IGRP = INTEGER (Given)
*        A GRP identifier for a group containing descriptions of the pens
*        to be used (only the first one is actually used). If this is supplied
*        as GRP__NOID then the contour is drawn using the "Curves" attributes
*        (eg Colour(Curves), Width(Curves), etc ) in the supplied Plot. If a
*        group is supplied, then each element in the group should be a
*        comma separated list of AST attribute settings to be established
*        prior to drawing a contour. Element 1 in the group is used for
*        the contour (any other elements are ignored). Any attributes not
*        specified default to their values in the Plot.
*     DIM1 = INTEGER (Given)
*        The first dimension of the two-dimensional array.
*     DIM2 = INTEGER (Given)
*        The second dimension of the two-dimensional array.
*     ARRAY( DIM1, DIM2 ) = REAL (Given)
*        Two-dimensional array to be contoured.
*     XLL = INTEGER (Given)
*        The x co-ordinate of the lower left pixel of the selected
*        sub-array.
*     YLL = INTEGER (Given)
*        The y co-ordinate of the lower left pixel of the selected
*        sub-array.
*     XSIZE = INTEGER (Given)
*        The x size of the sub-array to be contoured.
*     YSIZE = INTEGER (Given)
*        The y size of the sub-array to be contoured.
*     FAST = LOGICAL (Given)
*        If .TRUE., then contours are drawn using straight lines as the
*        basic drawing element. Otherwise, the basic drawing element are
*        geodesic curves in the Current coordinate system of the supplied
*        Plot. This is much slower to draw, but may be useful when a
*        severly non-linear or discontinuous mapping exists between grid
*        coordinates in the array, and graphics coordinates.
*     CNTUSD = LOGICAL (Returned)
*        If an element is .TRUE., the contour was drawn (this will always
*        be the case unless the data is all bad).
*     CNTLEN = REAL (Returned)
*        This returns the total length of the contour.
*     CNTCLS = INTEGER (Returned)
*        This returns the number of closed contours
*        at each level actually used. NOT CURRENTLY IMPLEMENTED; a fixed
*        value of 1 is always returned.
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
*     useful, but WITHOUT ANY WARRANTY; without even the implied
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
*     6-FEB-2001 (DSB):
*        Original version.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants
      INCLUDE 'PRM_PAR'          ! VAL constants
      INCLUDE 'GRP_PAR'          ! GRP constants

*  Arguments Given:
      LOGICAL OUTLIN
      INTEGER IPLOT
      INTEGER IGRP
      INTEGER DIM1
      INTEGER DIM2
      REAL ARRAY( DIM1, DIM2 )
      INTEGER XLL
      INTEGER YLL
      INTEGER XSIZE
      INTEGER YSIZE
      LOGICAL FAST

*  Arguments Returned:
      LOGICAL CNTUSD
      REAL CNTLEN
      INTEGER CNTCLS

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MAXPTS             ! Maximum number of positions in each
      PARAMETER ( MAXPTS = 10000 ) ! axis that define the locus of a contour

*  Local Variables:
      CHARACTER DOMAIN*40        ! Domain of Current Frame in supplied Plot
      DOUBLE PRECISION ATTRS( 20 )! PGPLOT graphics attributes on entry
      INTEGER ICURR              ! Index of original Current Frame
      INTEGER IPLOTT             ! AST pointer to Plot with current pen set
      INTEGER IX                 ! X element numbers of current pixel in full-size array
      INTEGER IY                 ! Y element numbers of current pixel in full-size array
      INTEGER NPEN               ! No. of pens supplied by IGRP
      INTEGER NPTS               ! Number of points in contour's locus
      LOGICAL DRAWB              ! Draw bottom pixel edge?
      LOGICAL DRAWL              ! Draw left hand pixel edge?
      LOGICAL PBAD               ! Previous pixel bad?
      REAL X( MAXPTS )           ! X positions of the contour
      REAL Y( MAXPTS )           ! Y positions of the contour

*.

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Report an error if the Current Frame in the supplied Plot does not
*  have a Domain value of GRID.
      DOMAIN = AST_GETC( IPLOT, 'DOMAIN', STATUS )
      IF( DOMAIN .NE. 'GRID' )THEN

         IF( STATUS .EQ. SAI__OK ) THEN
            CALL MSG_SETC( 'DOM', DOMAIN )
            STATUS = SAI__ERROR
            CALL ERR_REP( 'KPS1_CNTGD_1', 'KPS1_CNTGD: Current Frame '//
     :                    'in supplied Plot has Domain ^DOM but '//
     :                    'should have Domain GRID (programming '//
     :                    'error).', STATUS )
         END IF

         GO TO 999

      END IF

*  Simplify the Plot. This adds a new Current Frame into the Plot, so note
*  the index of the original Current Frame so that it can be re-instated later.
*  This can help to speed up the drawing, and also avoids the possibility
*  of the Mapping going via a Frame in which the positions are undefined.
      ICURR = AST_GETI( IPLOT, 'CURRENT', STATUS )
      CALL KPG1_ASSIM( IPLOT, STATUS )

*  Store the number of pens supplied in the GRP group.
      IF( IGRP .NE. GRP__NOID ) THEN
         CALL GRP_GRPSZ( IGRP, NPEN, STATUS )
      ELSE
         NPEN = 0
      END IF

*  If different pens are being used, produce a modified Plot which draws
*  curves with the pen style supplied for this contour.
      IF( NPEN .GT. 0 .AND. STATUS .EQ. SAI__OK ) THEN

*  Take a deep copy of the supplied Plot. This Plot will be modify using the
*  supplied attribute settings. A copy is used so that the original plotting
*  attributes can be re-instated later.
         IPLOTT = AST_COPY( IPLOT, STATUS )

*  Establish the attributes for this pen.
         CALL KPS1_CNTST( IPLOTT, IGRP, 1, STATUS )

*  Issue a context message if anything went wrong setting the pen.
         IF( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'KPS1_CNTGD_1', 'Unable to set the '//
     :                    'pen for the outline curve.', STATUS )
            GO TO 999
         END IF

*  If the same pen is being used for all contours, just clone the
*  supplied Plot pointer.
      ELSE
         IPLOTT = AST_CLONE( IPLOT, STATUS )
      END IF

*  Initialise record of contour levels actually used.
      CNTUSD = .FALSE.
      CNTLEN = 0.0
      CNTCLS = 1

*  Buffer all PGPLOT output produced while drawing this contour.
      CALL PGBBUF

*  If using FAST drawing mode, establish the new PGPLOT attributes,
*  saving the old in ATTRS. If not using fast drawing mode, AST will
*  establish them when AST_CURVE is called to draw the contour.
      IF( FAST ) CALL KPG1_PGSTY( IPLOTT, 'CURVES', .TRUE., ATTRS,
     :                            STATUS )

*  If required, draw a curve outlining the whole array.
      IF( OUTLIN ) THEN
         X( 1 ) = REAL( XLL ) - 0.49
         Y( 1 ) = REAL( YLL ) - 0.49
         X( 2 ) = REAL( XLL + XSIZE ) - 0.51
         Y( 2 ) = REAL( YLL ) - 0.49
         X( 3 ) = REAL( XLL + XSIZE ) - 0.51
         Y( 3 ) = REAL( YLL + YSIZE ) - 0.51
         X( 4 ) = REAL( XLL ) - 0.49
         Y( 4 ) = REAL( YLL + YSIZE ) - 0.51
         X( 5 ) = REAL( XLL ) - 0.49
         Y( 5 ) = REAL( YLL ) - 0.49
         CNTUSD = .TRUE.
         CNTLEN = 2*( XSIZE + YSIZE )
         CALL KPG1_ASCRV( IPLOTT, FAST, 5, X, Y, STATUS )

*  Otherwise we draw a contour around just the good data...
      ELSE

*  First check the bottom edge pixels...
*  =====================================
         IY = YLL

*  If the bottom left pixel is good, start a polyline at its bottom left
*  corner (in a bit to avoid clipping).
         IF( ARRAY( XLL, IY ) .NE. VAL__BADR ) THEN
            NPTS = 1
            X( 1 ) = REAL( XLL ) - 0.49
            Y( 1 ) = REAL( IY ) - 0.49
            PBAD = .FALSE.
         ELSE
            NPTS = 0
            PBAD = .TRUE.
         END IF

*  Scan the bottom row of pixels, excluding the first.
         DO IX = XLL + 1, XLL + XSIZE - 1

*  If this is a good pixel...
            IF( ARRAY( IX, IY ) .NE. VAL__BADR ) THEN

*  If the previous pixel was bad, start a new polyline at the top left
*  corner of this pixel.
               IF( PBAD ) THEN
                  NPTS = 1
                  X( 1 ) = REAL( IX ) - 0.5
                  Y( 1 ) = REAL( IY ) + 0.5
               END IF

*  Extend the polyline to the botom left corner of this pixel (in a bit
*  from the image edge to avoid clipping).
               NPTS = NPTS + 1
               X( NPTS ) = REAL( IX ) - 0.5
               Y( NPTS ) = REAL( IY ) - 0.49

*  Save the bad pixel flag for use on the next iteration.
               PBAD = .FALSE.

*  If the current pixel is bad.
            ELSE

*  If the previous pixel was good...
               IF( .NOT. PBAD ) THEN

*  Extend the polyline to the bottom left corner of this pixel (in a bit
*  from the image edge to avoid clipping).
                  NPTS = NPTS + 1
                  X( NPTS ) = REAL( IX ) - 0.5
                  Y( NPTS ) = REAL( IY ) - 0.49

*  Extend the polyline to the top left corner of this pixel, and then
*  draw the complete polyline.
                  NPTS = NPTS + 1
                  X( NPTS ) = REAL( IX ) - 0.5
                  Y( NPTS ) = REAL( IY ) + 0.5

                  CNTUSD = .TRUE.
                  CNTLEN = CNTLEN + NPTS
                  CALL KPG1_ASCRV( IPLOTT, FAST, NPTS, X, Y, STATUS )
                  NPTS = 0
               END IF

*  Save the bad pixel flag for use on the next iteration.
               PBAD = .TRUE.

            END IF

*  Draw the current polyline if the buffers may be filled on the next pixel.
            IF( NPTS .GE. MAXPTS - 2 ) THEN
               CNTUSD = .TRUE.
               CNTLEN = CNTLEN + NPTS
               CALL KPG1_ASCRV( IPLOTT, FAST, NPTS, X, Y, STATUS )
               X( 1 ) = X( NPTS )
               Y( 1 ) = Y( NPTS )
               NPTS = 1
            END IF

         END DO

*  Finish the bottom right pixel. If it is a good pixel, extend the polyline
*  to the bottom right corner.
         IX = XLL + XSIZE - 1
         IF( ARRAY( IX, IY ) .NE. VAL__BADR ) THEN
            NPTS = NPTS + 1
            X( NPTS ) = REAL( IX ) + 0.49
            Y( NPTS ) = REAL( IY ) - 0.49
         END IF

*  Draw any remaining polyline.
         IF( NPTS .GT. 0 ) THEN
            CNTUSD = .TRUE.
            CNTLEN = CNTLEN + NPTS
            CALL KPG1_ASCRV( IPLOTT, FAST, NPTS, X, Y, STATUS )
            NPTS = 0
         END IF

*  Now check the right hand edge pixels...
*  =======================================
         IX = XLL + XSIZE - 1

*  If the bottom right pixel is good, start a polyline at its bottom right
*  corner (in a bit to avoid clipping).
         IF( ARRAY( IX, YLL ) .NE. VAL__BADR ) THEN
            NPTS = 1
            X( 1 ) = REAL( IX ) + 0.49
            Y( 1 ) = REAL( YLL ) - 0.49
            PBAD = .FALSE.
         ELSE
            NPTS = 0
            PBAD = .TRUE.
         END IF

*  Scan the right hand column of pixels, excluding the first.
         DO IY = YLL + 1, YLL + YSIZE - 1

*  If this is a good pixel...
            IF( ARRAY( IX, IY ) .NE. VAL__BADR ) THEN

*  If the previous pixel was bad, start a new polyline at the bottom left
*  corner of this pixel.
               IF( PBAD ) THEN
                  NPTS = 1
                  X( 1 ) = REAL( IX ) - 0.5
                  Y( 1 ) = REAL( IY ) - 0.5
               END IF

*  Extend the polyline to the bottom right corner of this pixel (in a bit
*  from the image edge to avoid clipping).
               NPTS = NPTS + 1
               X( NPTS ) = REAL( IX ) + 0.49
               Y( NPTS ) = REAL( IY ) - 0.5

*  Save the bad pixel flag for use on the next iteration.
               PBAD = .FALSE.

*  If the current pixel is bad.
            ELSE

*  If the previous pixel was good...
               IF( .NOT. PBAD ) THEN

*  Extend the polyline to the bottom right corner of this pixel (in a bit
*  from the image edge to avoid clipping).
                  NPTS = NPTS + 1
                  X( NPTS ) = REAL( IX ) + 0.49
                  Y( NPTS ) = REAL( IY ) - 0.5

*  Extend the polyline to the bottom left corner of this pixel, and then
*  draw the complete polyline.
                  NPTS = NPTS + 1
                  X( NPTS ) = REAL( IX ) - 0.5
                  Y( NPTS ) = REAL( IY ) - 0.5

                  CNTUSD = .TRUE.
                  CNTLEN = CNTLEN + NPTS
                  CALL KPG1_ASCRV( IPLOTT, FAST, NPTS, X, Y, STATUS )
                  NPTS = 0
               END IF

*  Save the bad pixel flag for use on the next iteration.
               PBAD = .TRUE.

            END IF

*  Draw the current polyline if the buffers may be filled on the next pixel.
            IF( NPTS .GE. MAXPTS - 2 ) THEN
               CNTUSD = .TRUE.
               CNTLEN = CNTLEN + NPTS
               CALL KPG1_ASCRV( IPLOTT, FAST, NPTS, X, Y, STATUS )
               X( 1 ) = X( NPTS )
               Y( 1 ) = Y( NPTS )
               NPTS = 1
            END IF

         END DO

*  Finish the top right pixel. If it is a good pixel, extend the polyline
*  to the top right corner.
         IY = YLL + YSIZE - 1
         IF( ARRAY( IX, IY ) .NE. VAL__BADR ) THEN
            NPTS = NPTS + 1
            X( NPTS ) = REAL( IX ) + 0.49
            Y( NPTS ) = REAL( IY ) + 0.49
         END IF

*  Draw any remaining polyline.
         IF( NPTS .GT. 0 ) THEN
            CNTUSD = .TRUE.
            CNTLEN = CNTLEN + NPTS
            CALL KPG1_ASCRV( IPLOTT, FAST, NPTS, X, Y, STATUS )
            NPTS = 0
         END IF

*  Now check the left hand edge pixels...
*  ======================================
         IX = XLL

*  If the bottom left pixel is good, start a polyline at its bottom left
*  corner (in a bit to avoid clipping).
         IF( ARRAY( IX, YLL ) .NE. VAL__BADR ) THEN
            NPTS = 1
            X( 1 ) = REAL( IX ) - 0.49
            Y( 1 ) = REAL( YLL ) - 0.49
            PBAD = .FALSE.
         ELSE
            NPTS = 0
            PBAD = .TRUE.
         END IF

*  Scan the left hand column of pixels, excluding the first.
         DO IY = YLL + 1, YLL + YSIZE - 1

*  If this is a good pixel...
            IF( ARRAY( IX, IY ) .NE. VAL__BADR ) THEN

*  If the previous pixel was bad, start a new polyline at the bottom right
*  corner of this pixel.
               IF( PBAD ) THEN
                  NPTS = 1
                  X( 1 ) = REAL( IX ) + 0.5
                  Y( 1 ) = REAL( IY ) - 0.5
               END IF

*  Extend the polyline to the bottom left corner of this pixel (in a bit
*  from the image edge to avoid clipping).
               NPTS = NPTS + 1
               X( NPTS ) = REAL( IX ) - 0.49
               Y( NPTS ) = REAL( IY ) - 0.5

*  Save the bad pixel flag for use on the next iteration.
               PBAD = .FALSE.

*  If the current pixel is bad.
            ELSE

*  If the previous pixel was good...
               IF( .NOT. PBAD ) THEN

*  Extend the polyline to the bottom left corner of this pixel (in a bit
*  from the image edge to avoid clipping).
                  NPTS = NPTS + 1
                  X( NPTS ) = REAL( IX ) - 0.49
                  Y( NPTS ) = REAL( IY ) - 0.5

*  Extend the polyline to the bottom right corner of this pixel, and then
*  draw the complete polyline.
                  NPTS = NPTS + 1
                  X( NPTS ) = REAL( IX ) + 0.5
                  Y( NPTS ) = REAL( IY ) - 0.5

                  CNTUSD = .TRUE.
                  CNTLEN = CNTLEN + NPTS
                  CALL KPG1_ASCRV( IPLOTT, FAST, NPTS, X, Y, STATUS )
                  NPTS = 0
               END IF

*  Save the bad pixel flag for use on the next iteration.
               PBAD = .TRUE.

            END IF

*  Draw the current polyline if the buffers may be filled on the next pixel.
            IF( NPTS .GE. MAXPTS - 2 ) THEN
               CNTUSD = .TRUE.
               CNTLEN = CNTLEN + NPTS
               CALL KPG1_ASCRV( IPLOTT, FAST, NPTS, X, Y, STATUS )
               X( 1 ) = X( NPTS )
               Y( 1 ) = Y( NPTS )
               NPTS = 1
            END IF

         END DO

*  Finish the top left pixel. If it is a good pixel, extend the polyline
*  to the top left corner.
         IY = YLL + YSIZE - 1
         IF( ARRAY( IX, IY ) .NE. VAL__BADR ) THEN
            NPTS = NPTS + 1
            X( NPTS ) = REAL( IX ) - 0.49
            Y( NPTS ) = REAL( IY ) + 0.49
         END IF

*  Draw any remaining polyline.
         IF( NPTS .GT. 0 ) THEN
            CNTUSD = .TRUE.
            CNTLEN = CNTLEN + NPTS
            CALL KPG1_ASCRV( IPLOTT, FAST, NPTS, X, Y, STATUS )
            NPTS = 0
         END IF

*  Now check the top edge pixels...
*  ================================
         IY = YLL + YSIZE - 1

*  If the top left pixel is good, start a polyline at its top left
*  corner (in a bit to avoid clipping).
         IF( ARRAY( XLL, IY ) .NE. VAL__BADR ) THEN
            NPTS = 1
            X( 1 ) = REAL( XLL ) - 0.49
            Y( 1 ) = REAL( IY ) + 0.49
            PBAD = .FALSE.
         ELSE
            NPTS = 0
            PBAD = .TRUE.
         END IF

*  Scan the top row of pixels, excluding the first.
         DO IX = XLL + 1, XLL + XSIZE - 1

*  If this is a good pixel...
            IF( ARRAY( IX, IY ) .NE. VAL__BADR ) THEN

*  If the previous pixel was bad, start a new polyline at the bottom left
*  corner of this pixel.
               IF( PBAD ) THEN
                  NPTS = 1
                  X( 1 ) = REAL( IX ) - 0.5
                  Y( 1 ) = REAL( IY ) - 0.5
               END IF

*  Extend the polyline to the top left corner of this pixel (in a bit
*  from the image edge to avoid clipping).
               NPTS = NPTS + 1
               X( NPTS ) = REAL( IX ) - 0.5
               Y( NPTS ) = REAL( IY ) + 0.49

*  Save the bad pixel flag for use on the next iteration.
               PBAD = .FALSE.

*  If the current pixel is bad.
            ELSE

*  If the previous pixel was good...
               IF( .NOT. PBAD ) THEN

*  Extend the polyline to the top left corner of this pixel (in a bit
*  from the image edge to avoid clipping).
                  NPTS = NPTS + 1
                  X( NPTS ) = REAL( IX ) - 0.5
                  Y( NPTS ) = REAL( IY ) + 0.49

*  Extend the polyline to the bottom left corner of this pixel, and then
*  draw the complete polyline.
                  NPTS = NPTS + 1
                  X( NPTS ) = REAL( IX ) - 0.5
                  Y( NPTS ) = REAL( IY ) - 0.5

                  CNTUSD = .TRUE.
                  CNTLEN = CNTLEN + NPTS
                  CALL KPG1_ASCRV( IPLOTT, FAST, NPTS, X, Y, STATUS )
                  NPTS = 0
               END IF

*  Save the bad pixel flag for use on the next iteration.
               PBAD = .TRUE.

            END IF

*  Draw the current polyline if the buffers may be filled on the next pixel.
            IF( NPTS .GE. MAXPTS - 2 ) THEN
               CNTUSD = .TRUE.
               CNTLEN = CNTLEN + NPTS
               CALL KPG1_ASCRV( IPLOTT, FAST, NPTS, X, Y, STATUS )
               X( 1 ) = X( NPTS )
               Y( 1 ) = Y( NPTS )
               NPTS = 1
            END IF

         END DO

*  Finish the top right pixel. If it is a good pixel, extend the polyline
*  to the top right corner.
         IX = XLL + XSIZE - 1
         IF( ARRAY( IX, IY ) .NE. VAL__BADR ) THEN
            NPTS = NPTS + 1
            X( NPTS ) = REAL( IX ) + 0.49
            Y( NPTS ) = REAL( IY ) + 0.49
         END IF

*  Draw any remaining polyline.
         IF( NPTS .GT. 0 ) THEN
            CNTUSD = .TRUE.
            CNTLEN = CNTLEN + NPTS
            CALL KPG1_ASCRV( IPLOTT, FAST, NPTS, X, Y, STATUS )
            NPTS = 0
         END IF

*  Now do the interior of the image...
*  ===================================

*  Scan all rows and columns except the bottom row and left hand column.
         DO IY = YLL + 1, YLL + YSIZE - 1
            DO IX = XLL + 1, XLL + XSIZE - 1

*  The bottom edge of this pixel will be drawn if its bad/good status
*  is different to the pixel below it. The left hand edge will be drawn
*  this pixels bad/good status is different to the pixel to the left of it.
               IF( ARRAY( IX, IY ) .NE. VAL__BADR ) THEN
                  DRAWL = ( ARRAY( IX - 1, IY ) .EQ. VAL__BADR )
                  DRAWB = ( ARRAY( IX, IY - 1 ) .EQ. VAL__BADR )
               ELSE
                  DRAWL = ( ARRAY( IX - 1, IY ) .NE. VAL__BADR )
                  DRAWB = ( ARRAY( IX, IY - 1 ) .NE. VAL__BADR )
               END IF

*  Store a polyline of 1 or 2 segments describing the edges to be drawn.
               NPTS = 0

               IF( DRAWL ) THEN
                  X( 1 ) = REAL( IX ) - 0.5
                  Y( 1 ) = REAL( IY ) + 0.5
                  X( 2 ) = REAL( IX ) - 0.5
                  Y( 2 ) = REAL( IY ) - 0.5
                  NPTS = 2
               END IF

               IF( DRAWB ) THEN
                  IF( DRAWL ) THEN
                     X( 3 ) = REAL( IX ) + 0.5
                     Y( 3 ) = REAL( IY ) - 0.5
                     NPTS = 3
                  ELSE
                     X( 1 ) = REAL( IX ) - 0.5
                     Y( 1 ) = REAL( IY ) - 0.5
                     X( 2 ) = REAL( IX ) + 0.5
                     Y( 2 ) = REAL( IY ) - 0.5
                     NPTS = 2
                  END IF
               END IF


*  Draw any polyline.
               IF( NPTS .GT. 0 ) THEN
                  CNTUSD = .TRUE.
                  CNTLEN = CNTLEN + 2
                  CALL KPG1_ASCRV( IPLOTT, FAST, NPTS, X, Y, STATUS )
               END IF

            END DO

         END DO

      END IF

*  Flush the buffers used by KPG1_ASCRV.
      CALL KPG1_ASCRV( IPLOTT, FAST, 0, 0.0, 0.0, STATUS )

*  If using FAST drawing mode, re-establish the old PGPLOT plotting attributes.
      IF( FAST ) CALL KPG1_PGSTY( IPLOTT, 'CURVES', .FALSE., ATTRS,
     :                            STATUS )

*  Flush the buffer holding PGPLOT output produced while drawing this contour.
      CALL PGEBUF

*  Annul the temporary copy of the supplied Plot which was used to do the
*  drawing.
      CALL AST_ANNUL( IPLOTT, STATUS )

 999  CONTINUE

*  Remove the Current Frame added by KPG1_ASSIM and re-instate the original
*  Current Frame.
      CALL AST_REMOVEFRAME( IPLOT, AST__CURRENT, STATUS )
      CALL AST_SETI( IPLOT, 'CURRENT', ICURR, STATUS )

      END
