      SUBROUTINE KPG1_MKPOS( NAX, POS, IPLOT, CURR, MODE, MARKER,
     :                       GEO, DONE, CLOSE, TEXT, JUST, REGION,
     :                       STATUS )
*+
*  Name:
*     KPG1_MKPOS

*  Purpose:
*     Marks a position on a graphics device.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_MKPOS( NAX, POS, IPLOT, CURR, MODE, MARKER, GEO, DONE,
*                      CLOSE, TEXT, JUST, REGION, STATUS )

*  Description:
*     This routine marks a position on a graphics device in various ways.

*  Arguments:
*     NAX = INTEGER (Given)
*        The number of co-ordinate values supplied in POS. This should be
*        equal to the number of axes in the Frame specified by CURR.
*     POS( NAX ) = DOUBLE PRECISION (Given)
*        The co-ordinates of the position, in the Frame specified by CURR.
*        No marker is drawn if a AST__BAD or VAL__BADD value is supplied
*        Ignored if DONE is .TRUE., and MODE is "POLY", "CHAIN" or "BOX".
*     IPLOT = INTEGER (Given)
*        An AST pointer to a Plot. The same Plot should be supplied for
*        all points in a polygon, chain, or set of boxes.
*     CURR = LOGICAL (Given)
*        If .TRUE., then position supplied in POS refers to the Current
*        Frame in IPLOT. Otherwise it refers to the Base Frame (which
*        should be the GRAPHICS Frame).
*     MODE = CHARACTER * ( * ) (Given)
*        The type of marker to produce (case sensitive, no abbreviations):
*
*        - "NONE" -- An immediate return is made without any graphics
*        being drawn.
*
*        - "MARK" -- Each position is marked by the symbol specified
*        by argument MARKER.
*
*        - "POLY" -- Causes each position to be joined by a line to the
*        previous position marked.  These lines may be geodesic (in the
*        Current Frame of the Plot) or straight (on the screen), as
*        specified by argument GEO.
*
*        - "CHAIN" -- This is a combination of "Mark" and "Poly". Each
*        position is marked by a symbol and joined by a line to the
*        previous position. Arguments MARKER, GEO, and CLOSE are used to
*        specify the symbols and lines to use.
*
*        -  "BOX" -- An empty rectangle with edges parallel to the axes
*        is drawn extending between the supplied position and the 
*        previous position.
*
*        -  "VLINE" -- A vertical line is drawn through the position
*        covering the entire height of the Plot.
*
*        -  "HLINE" -- A horizontal line is drawn through the position
*        covering the entire height of the Plot.
*
*        -  "CROSS" -- A full-screen cross-hair is drawn at the
*        position, i.e. a combination of Vline and Hline.
*
*        -  "TEXT" -- The text string specified by argument TEXT is
*        displayed, horizontally, and centred on the supplied position.
*
*        -  "BLANK" -- Nothing is drawn.
*
*        -  "REGION" -- The AST Region given by REGION is outlined.
*
*     MARKER = INTEGER (Given)
*        The PGPLOT marker type to use if MODE is "MARKER" or "CHAIN".
*     GEO = LOGICAL (Given)
*        Should polygon and chain line segments be drawn as geodesic curves
*        within the Current Frame of the Plot? If not they are drawn as simple
*        straight lines within the Base Frame (GRAPHICS). The same value
*        should be supplied for all points in a polygon or chain.
*     DONE = LOGICAL (Given)
*        Should be supplied .TRUE. when a polygon, chain or set of boxes has
*        been completed. The contents of POS will be ignored in this case.
*        DONE is ignored if MODE is not "PLOT", "CHAIN" or "BOX".
*     CLOSE = LOGICAL (Given)
*        If .TRUE., polygons and chains are closed by joining the first
*        position to the last position (when DONE is supplied .TRUE.).
*     TEXT = CHARACTER * ( * ) (Given)
*        The text to display if MODE is "TEXT". Trailing spaces are
*        ignored.
*     JUST = CHARACTER * ( * ) (Given)
*        A string specifying the justification to be used when displaying
*        the text supplied in TEXT (ignored if MODE is not "Text"). This
*        should be a string of two characters; the first should be "B",
*        "C" or "T", meaning bottom, centre or top. The second should be
*        "L", "C" or "R", meaning left, centre or right. The text is
*        displayed so that the position supplied in POS is at the
*        specified point within the displayed text string.
*     REGION = INTEGER (Given)
*        A two-dimensional AST Region pointer. Only used if PLOT is "REGION".
*        In order to save time calling AST_CONVERT for every Region, the
*        first Region to be plotted using this routine defines the
*        co-ordinate frame for all subsequent Regions draw by later
*        invocations of this routine. If in fact, later Regions may have a
*        different co-ordinate frame, then this routine should be called
*        with MODE=REGION and REGION=AST__NULL. This will cause the
*        current Region co-ordinate frame to be forgotten so that the next
*        non-NULL Region to be draw will define a new current co-ordinate
*        frame for subsequent Regions.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998, 2000 Central Laboratory of the Research Councils.
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
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-NOV-1998 (DSB):
*        Original version.
*     14-DEC-1998 (DSB):
*        Added BLANK option for MODE. Added argument JUST.
*     12-APR-2000 (DSB):
*        Corrected erroneous use of Y2 instead of Y1 in calls to PGQWIN.
*     3-MAY-2009 (DSB):
*        Added argument REGION.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations
      INCLUDE 'NDF_PAR'          ! NDF constants

*  Arguments Given:
      INTEGER NAX
      DOUBLE PRECISION POS( NAX )
      INTEGER IPLOT
      LOGICAL CURR
      CHARACTER MODE*(*)
      INTEGER MARKER
      LOGICAL GEO
      LOGICAL DONE
      LOGICAL CLOSE
      CHARACTER TEXT*(*)
      CHARACTER JUST*(*)
      INTEGER REGION

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION BOX( 5, 2 )        ! GRAPHICS co-ords at box corners
      DOUBLE PRECISION FPOS( NDF__MXDIM ) ! First position
      DOUBLE PRECISION LPOS( NDF__MXDIM ) ! Previous position
      DOUBLE PRECISION RPOS( NDF__MXDIM ) ! Current position
      INTEGER FS                 ! FrameSet that aligns Region with Plot
      INTEGER I                  ! Loop count
      INTEGER IBASE              ! Index of Base Frame
      INTEGER ICURR              ! Index of Current Frame
      INTEGER MAP                ! Mapping from Plot base Frame to Region
      INTEGER MAP2               ! Mapping from Plot base Frame to Region
      INTEGER NAXC               ! No. of Current Frame axes
      INTEGER RNAX               ! No. of axes in RPOS
      LOGICAL DOING              ! Has a Poly or Chain been started?
      LOGICAL GOTPOS             ! Are both ends of the line defined?
      LOGICAL JUNK               ! Unused
      REAL UP( 2 )               ! Up-vector for text
      REAL X1, X2, Y1, Y2        ! Bounds of PGPLOT window

*  Data Initialisation:
      DATA LPOS /NDF__MXDIM*AST__BAD/,
     :     FPOS /NDF__MXDIM*AST__BAD/,
     :     DOING /.FALSE./,
     :     MAP2 /AST__NULL/

*  Ensure variable values are retained between invocations of this routine.
      SAVE LPOS, FPOS, DOING, MAP2
*.

*  Check the inherited status. Also return if no graphics are required.
      IF ( STATUS .NE. SAI__OK .OR. MODE .EQ. 'NONE' ) RETURN

*  Note the index of the Current Frame in the supplied Plot so that it can
*  be re-instated later if required.
      ICURR = AST_GETI( IPLOT, 'CURRENT', STATUS )

*  Note the number of axes in the Current Frame in the supplied Plot.
      NAXC = AST_GETI( IPLOT, 'NOUT', STATUS )

*  Deal with markers.
*  ==================
      IF( MODE .EQ. 'MARK' ) THEN

*  If the supplied position is a Base Frame position, temporarily make the
*  Base Frame the Current Frame.
         IF( .NOT. CURR ) THEN
            CALL AST_SETI( IPLOT, 'CURRENT', AST_GETI( IPLOT, 'BASE',
     :                                                 STATUS ),
     :                     STATUS )
         END IF

*  Draw the marker.
         CALL AST_MARK( IPLOT, 1, NAX, 1, POS, MARKER, STATUS )

*  Re-instate the original Current Frame if necessary.
         IF( .NOT. CURR ) CALL AST_SETI( IPLOT, 'CURRENT', ICURR,
     :                                   STATUS )

*  Deal with polygons.
*  ==================
      ELSE IF( MODE .EQ. 'POLY' ) THEN

*  Save the number of axes in the required Frame. Geodesics are drawn in
*  the Current Frame, and straight lines are drawn in the Base (GRAPHICS)
*  Frame.
         IF( GEO ) THEN
            RNAX = NAXC
         ELSE
            RNAX = 2
         END IF

*  If a previous polygon has just been finished...
         IF( DONE ) THEN

*  Join the first and last vertices if required (and if a polygon has
*  been started).
            IF( CLOSE .AND. DOING ) THEN

*  See if both first and last positions are defined.
               GOTPOS = .TRUE.
               DO I = 1, RNAX
                  IF( LPOS( I ) .EQ. AST__BAD ) GOTPOS = .FALSE.
                  IF( FPOS( I ) .EQ. AST__BAD ) GOTPOS = .FALSE.
               END DO

*  If both positions are defined, draw the line joining them.
               IF( GOTPOS ) THEN

*  If we are drawing straight lines (not geodesics), temporarily make the
*  Base Frame the Current Frame in the Plot.
                  IF( .NOT. GEO ) THEN
                     CALL AST_SETI( IPLOT, 'CURRENT', AST_GETI( IPLOT,
     :                                                 'BASE', STATUS ),
     :                              STATUS )
                  END IF

*  Draw the curve.
                  CALL AST_CURVE( IPLOT, LPOS, FPOS, STATUS )

*  Re-instate the original Current Frame if necessary.
                  IF( .NOT. GEO ) CALL AST_SETI( IPLOT, 'CURRENT',
     :                                           ICURR, STATUS )
               END IF

            END IF

*  If we are starting or continuing a polygon.
         ELSE

*  If straight lines are required, we require positions in the Base
*  Frame of the Plot. For geodesics, we need positions in the Current Frame.
*  Ensure the supplied position is in the correct Frame. If not, transform
*  it into the correct Frame.
            IF( GEO ) THEN
               IF( .NOT. CURR ) THEN
                  CALL AST_TRANN( IPLOT, 1, 2, 1, POS, .TRUE., RNAX, 1,
     :                            RPOS, STATUS )
               ELSE
                  DO I = 1, RNAX
                     RPOS( I ) = POS( I )
                  END DO
               END IF

            ELSE
               IF( CURR ) THEN
                  CALL AST_TRANN( IPLOT, 1, NAXC, 1, POS, .FALSE., RNAX,
     :                            1, RPOS, STATUS )
               ELSE
                  DO I = 1, RNAX
                     RPOS( I ) = POS( I )
                  END DO
               END IF
            END IF

*  See if both previous and current positions are defined.
            IF( DOING ) THEN

               GOTPOS = .TRUE.
               DO I = 1, RNAX
                  IF( LPOS( I ) .EQ. AST__BAD ) GOTPOS = .FALSE.
                  IF( RPOS( I ) .EQ. AST__BAD ) GOTPOS = .FALSE.
               END DO

            ELSE
               GOTPOS = .FALSE.
            END IF

*  If both positions are defined, draw the line joining them.
            IF( GOTPOS ) THEN

*  If we are drawing straight lines (not geodesics), temporarily make the
*  Base Frame the Current Frame in the Plot.
               IF( .NOT. GEO ) THEN
                  CALL AST_SETI( IPLOT, 'CURRENT', AST_GETI( IPLOT,
     :                                                 'BASE', STATUS ),
     :                           STATUS )
               END IF

*  Draw the curve.
               CALL AST_CURVE( IPLOT, LPOS, RPOS, STATUS )

*  Re-instate the original Current Frame if necessary.
               IF( .NOT. GEO ) CALL AST_SETI( IPLOT, 'CURRENT', ICURR,
     :                                        STATUS )
            END IF

*  Store the new position for use next time.
            DO I = 1, RNAX
               LPOS( I ) = RPOS( I )
            END DO

*  If this is the first point in a new polygon, save it as such.
            IF( .NOT. DOING ) THEN
               DO I = 1, RNAX
                  FPOS( I ) = RPOS( I )
               END DO
               DOING = .TRUE.
            END IF

         END IF

*  Deal with chains.
*  =================
      ELSE IF( MODE .EQ. 'CHAIN' ) THEN

*  Save the number of axes in the required Frame. Geodesics are drawn in
*  the Current Frame, and straight lines are drawn in the Base (GRAPHICS)
*  Frame.
         IF( GEO ) THEN
            RNAX = NAXC
         ELSE
            RNAX = 2
         END IF

*  If a previous chain has just been finished...
         IF( DONE ) THEN

*  Join the first and last vertices if required (and if a chain has
*  been started).
            IF( CLOSE .AND. DOING ) THEN

*  See if both first and last positions are defined.
               GOTPOS = .TRUE.
               DO I = 1, RNAX
                  IF( LPOS( I ) .EQ. AST__BAD ) GOTPOS = .FALSE.
                  IF( FPOS( I ) .EQ. AST__BAD ) GOTPOS = .FALSE.
               END DO

*  If both positions are defined, draw the line joining them.
               IF( GOTPOS ) THEN

*  If we are drawing straight lines (not geodesics), temporarily make the
*  Base Frame the Current Frame in the Plot.
                  IF( .NOT. GEO ) THEN
                     CALL AST_SETI( IPLOT, 'CURRENT', AST_GETI( IPLOT,
     :                                                 'BASE', STATUS ),
     :                              STATUS )
                  END IF

*  Draw the curve.
                  CALL AST_CURVE( IPLOT, LPOS, FPOS, STATUS )

*  Re-instate the original Current Frame if necessary.
                  IF( .NOT. GEO ) CALL AST_SETI( IPLOT, 'CURRENT',
     :                                           ICURR, STATUS )
               END IF

            END IF

*  If we are starting or continuing a chain.
         ELSE

*  If straight lines are required, we require positions in the Base
*  Frame of the Plot. For geodesics, we need positions in the Current Frame.
*  Ensure the supplied position is in the currect Frame. If not, transform
*  it into the correct Frame.
            IF( GEO ) THEN
               IF( .NOT. CURR ) THEN
                  CALL AST_TRANN( IPLOT, 1, 2, 1, POS, .TRUE., RNAX, 1,
     :                            RPOS, STATUS )
               ELSE
                  DO I = 1, RNAX
                     RPOS( I ) = POS( I )
                  END DO
               END IF

            ELSE
               IF( CURR ) THEN
                  CALL AST_TRANN( IPLOT, 1, NAXC, 1, POS, .FALSE., RNAX,
     :                            1, RPOS, STATUS )
               ELSE
                  DO I = 1, RNAX
                     RPOS( I ) = POS( I )
                  END DO
               END IF

*  If we are drawing straight lines (not geodesics), temporarily make the
*  Base Frame the Current Frame in the Plot.
               CALL AST_SETI( IPLOT, 'CURRENT', AST_GETI( IPLOT,
     :                                                 'BASE', STATUS ),
     :                        STATUS )

            END IF

*  Mark the position.
            CALL AST_MARK( IPLOT, 1, RNAX, 1, RPOS, MARKER, STATUS )

*  See if both previous and current positions are defined.
            IF( DOING ) THEN

               GOTPOS = .TRUE.
               DO I = 1, RNAX
                  IF( LPOS( I ) .EQ. AST__BAD ) GOTPOS = .FALSE.
                  IF( RPOS( I ) .EQ. AST__BAD ) GOTPOS = .FALSE.
               END DO

            ELSE
               GOTPOS = .FALSE.
            END IF

*  If both positions are defined, draw the line joining them.
            IF( GOTPOS ) CALL AST_CURVE( IPLOT, LPOS, RPOS, STATUS )

*  Re-instate the original Current Frame if necessary.
            IF( .NOT. GEO ) CALL AST_SETI( IPLOT, 'CURRENT', ICURR,
     :                                     STATUS )

*  Store the new position for use next time.
            DO I = 1, RNAX
               LPOS( I ) = RPOS( I )
            END DO

*  If this is the first point in a new polygon, save it as such.
            IF( .NOT. DOING ) THEN
               DO I = 1, RNAX
                  FPOS( I ) = RPOS( I )
               END DO
               DOING = .TRUE.
            END IF

         END IF

*  Deal with boxes.
*  ================
      ELSE IF( MODE .EQ. 'BOX' ) THEN

*  If we are starting or continuing a set of boxes.
         IF( .NOT. DONE ) THEN

*  Ensure the supplied position is in the Base Frame. If not, transform it
*  into the Base Frame.
            RNAX = 2
            IF( CURR ) THEN
               CALL AST_TRANN( IPLOT, 1, NAXC, 1, POS, .FALSE., RNAX, 1,
     :                         RPOS, STATUS )
            ELSE
               DO I = 1, RNAX
                  RPOS( I ) = POS( I )
               END DO
            END IF

*  See if both previous and current positions are defined.
            IF( DOING ) THEN

               GOTPOS = .TRUE.
               DO I = 1, RNAX
                  IF( LPOS( I ) .EQ. AST__BAD ) GOTPOS = .FALSE.
                  IF( RPOS( I ) .EQ. AST__BAD ) GOTPOS = .FALSE.
               END DO

            ELSE
               GOTPOS = .FALSE.
            END IF

*  If both positions are defined, draw the box joining them.
            IF( GOTPOS ) THEN

*  Store the co-ordinates of the corners of the box.
               BOX( 1, 1 ) = LPOS( 1 )
               BOX( 1, 2 ) = LPOS( 2 )
               BOX( 2, 1 ) = LPOS( 1 )
               BOX( 2, 2 ) = RPOS( 2 )
               BOX( 3, 1 ) = RPOS( 1 )
               BOX( 3, 2 ) = RPOS( 2 )
               BOX( 4, 1 ) = RPOS( 1 )
               BOX( 4, 2 ) = LPOS( 2 )
               BOX( 5, 1 ) = LPOS( 1 )
               BOX( 5, 2 ) = LPOS( 2 )

*  Temporarily make the Base Frame the Current Frame in the Plot.
               CALL AST_SETI( IPLOT, 'CURRENT', AST_GETI( IPLOT, 'BASE',
     :                                                    STATUS ),
     :                        STATUS )

*  Draw the curves.
               CALL AST_POLYCURVE( IPLOT, 5, 2, 5, BOX, STATUS )

*  Re-instate the original Current Frame.
               CALL AST_SETI( IPLOT, 'CURRENT', ICURR, STATUS )

            END IF

*  Store the new position for use next time, and indicate we have started
*  a set of boxes.
            DO I = 1, RNAX
               LPOS( I ) = RPOS( I )
            END DO

            DOING = .TRUE.

         END IF

*  Deal with vertical lines.
*  =========================
      ELSE IF( MODE .EQ. 'VLINE' ) THEN

*  Find the bounds of the PGPLOT window.
         CALL PGQWIN( X1, X2, Y1, Y2 )

*  Ensure the supplied position is in the Base Frame. If not, transform it
*  into the Base Frame.
         RNAX = 2
         IF( CURR ) THEN
            CALL AST_TRANN( IPLOT, 1, NAXC, 1, POS, .FALSE., RNAX, 1,
     :                      RPOS, STATUS )
         ELSE
            DO I = 1, RNAX
               RPOS( I ) = POS( I )
            END DO
         END IF

*  See if the position is defined.
         GOTPOS = .TRUE.
         DO I = 1, RNAX
            IF( RPOS( I ) .EQ. AST__BAD ) GOTPOS = .FALSE.
         END DO

*  If so, draw the line.
         IF( GOTPOS ) THEN

*  Store the co-ordinates of the ends of the line. The line extends over
*  the entire PGPLOT viewport, but will be clipped by the Plot.
            BOX( 1, 1 ) = RPOS( 1 )
            BOX( 1, 2 ) = DBLE( Y1 )
            BOX( 2, 1 ) = RPOS( 1 )
            BOX( 2, 2 ) = DBLE( Y2 )

*  Temporarily make the Base Frame the Current Frame in the Plot.
            CALL AST_SETI( IPLOT, 'CURRENT', AST_GETI( IPLOT, 'BASE',
     :                                                 STATUS ),
     :                     STATUS )

*  Draw the curve.
            CALL AST_POLYCURVE( IPLOT, 2, 2, 5, BOX, STATUS )

*  Re-instate the original Current Frame.
            CALL AST_SETI( IPLOT, 'CURRENT', ICURR, STATUS )

         END IF

*  Deal with horizontal lines.
*  ===========================
      ELSE IF( MODE .EQ. 'HLINE' ) THEN

*  Find the bounds of the PGPLOT window.
         CALL PGQWIN( X1, X2, Y1, Y2 )

*  Ensure the supplied position is in the Base Frame. If not, transform it
*  into the Base Frame.
         RNAX = 2
         IF( CURR ) THEN
            CALL AST_TRANN( IPLOT, 1, NAXC, 1, POS, .FALSE., 2, 1,
     :                      RPOS, STATUS )
         ELSE
            DO I = 1, RNAX
               RPOS( I ) = POS( I )
            END DO
         END IF

*  See if the position is defined.
         GOTPOS = .TRUE.
         DO I = 1, RNAX
            IF( RPOS( I ) .EQ. AST__BAD ) GOTPOS = .FALSE.
         END DO

*  If so, draw the line.
         IF( GOTPOS ) THEN

*  Store the co-ordinates of the ends of the line. The line extends over
*  the entire PGPLOT viewport, but will be clipped by the Plot.
            BOX( 1, 1 ) = DBLE( X1 )
            BOX( 1, 2 ) = RPOS( 2 )
            BOX( 2, 1 ) = DBLE( X2 )
            BOX( 2, 2 ) = RPOS( 2 )

*  Temporarily make the Base Frame the Current Frame in the Plot.
            CALL AST_SETI( IPLOT, 'CURRENT', AST_GETI( IPLOT, 'BASE',
     :                                                 STATUS ),
     :                     STATUS )

*  Draw the curve.
            CALL AST_POLYCURVE( IPLOT, 2, 2, 5, BOX, STATUS )

*  Re-instate the original Current Frame.
            CALL AST_SETI( IPLOT, 'CURRENT', ICURR, STATUS )

         END IF

*  Deal with crossed lines.
*  ========================
      ELSE IF( MODE .EQ. 'CROSS' ) THEN

*  Find the bounds of the PGPLOT window.
         CALL PGQWIN( X1, X2, Y1, Y2 )

*  Ensure the supplied position is in the Base Frame. If not, transform it
*  into the Base Frame.
         RNAX = 2
         IF( CURR ) THEN
            CALL AST_TRANN( IPLOT, 1, NAXC, 1, POS, .FALSE., 2, 1,
     :                      RPOS, STATUS )
         ELSE
            DO I = 1, RNAX
               RPOS( I ) = POS( I )
            END DO
         END IF

*  See if the position is defined.
         GOTPOS = .TRUE.
         DO I = 1, RNAX
            IF( RPOS( I ) .EQ. AST__BAD ) GOTPOS = .FALSE.
         END DO

*  If so, draw the lines.
         IF( GOTPOS ) THEN

*  Temporarily make the Base Frame the Current Frame in the Plot.
            CALL AST_SETI( IPLOT, 'CURRENT', AST_GETI( IPLOT, 'BASE',
     :                                                 STATUS ),
     :                     STATUS )

*  Store the co-ordinates of the ends of the vertical line. The line extends
*  over the entire PGPLOT viewport, but will be clipped by the Plot.
            BOX( 1, 1 ) = RPOS( 1 )
            BOX( 1, 2 ) = DBLE( Y1 )
            BOX( 2, 1 ) = RPOS( 1 )
            BOX( 2, 2 ) = DBLE( Y2 )

*  Draw the vertical line.
            CALL AST_POLYCURVE( IPLOT, 2, 2, 5, BOX, STATUS )

*  Store the co-ordinates of the ends of the horizontal line. The line extends
*  over the entire PGPLOT viewport, but will be clipped by the Plot.
            BOX( 1, 1 ) = DBLE( X1 )
            BOX( 1, 2 ) = RPOS( 2 )
            BOX( 2, 1 ) = DBLE( X2 )
            BOX( 2, 2 ) = RPOS( 2 )

*  Draw the horizontal line.
            CALL AST_POLYCURVE( IPLOT, 2, 2, 5, BOX, STATUS )

*  Re-instate the original Current Frame.
            CALL AST_SETI( IPLOT, 'CURRENT', ICURR, STATUS )

         END IF

*  Deal with text.
*  ===============
      ELSE IF( MODE .EQ. 'TEXT' ) THEN

*  If the supplied position is a Base Frame position, temporarily make the
*  Base Frame the Current Frame.
         IF( .NOT. CURR ) THEN
            CALL AST_SETI( IPLOT, 'CURRENT', AST_GETI( IPLOT, 'BASE',
     :                                                 STATUS ),
     :                     STATUS )
         END IF

*  Set the up vector for horizontal text.
         UP( 1 ) = 0.0D0
         UP( 2 ) = 1.0D0

*  Draw the text, horizontal, and centre justified.
         CALL AST_TEXT( IPLOT, TEXT, POS, UP, JUST, STATUS )

*  Re-instate the original Current Frame if necessary.
         IF( .NOT. CURR ) CALL AST_SETI( IPLOT, 'CURRENT', ICURR,
     :                                   STATUS )


*  ------------------------------------------------------------------
*  The following code should replace the "Text" section above when
*  TextLabGap is used to specify an offset from the supplied position
*  to the text reference position. Note, for this to work, PLOTSTYLE
*  parameters should be used instead of STYLE.

c*  If the supplied position is a Current Frame position, map it into a
c*  (two-dimensional GRAPHICS) Base Frame position. Otherwise, just use the two-dimensional GRAPHICS
c*  position supplied.
c         IF( CURR ) THEN
c            CALL AST_TRANN( IPLOT, 1, NAXC, 1, POS, .FALSE., 2, 1, RPOS,
c     :                      STATUS )
c         ELSE
c            DO I = 1, 2
c               RPOS( I ) = POS( I )
c            END DO
c         END IF
c
c*  Get the minimum dimension of the the PGPLOT window. This will be in
c*  millimetres from the bottom left corner (i.e. GRAPHICS co-ordinates).
c         CALL PGQWIN( X1, X2, Y1, Y2 )
c         MINDIM = MIN( ABS( X2 - X1 ), ABS( Y2 - Y1 ) )
c
c*  If TextLabGap values have been set in the Plot, use them as the offset
c*  between the supplied position and the reference position for the text.
c         IF( AST_TEST( IPLOT, 'TEXTLABGAP(1)', STATUS ) ) THEN
c            RPOS( 1 ) = RPOS( 1 ) - MINDIM*
c     :                  AST_GETD( IPLOT, 'TEXTLABGAP(1)', STATUS )
c         END IF
c
c         IF( AST_TEST( IPLOT, 'TEXTLABGAP(2)', STATUS ) ) THEN
c            RPOS( 2 ) = RPOS( 2 ) - MINDIM*
c     :                  AST_GETD( IPLOT, 'TEXTLABGAP(2)', STATUS )
c         END IF
c
c*  Make the GRAPHICS (Base) Frame the Current Frame.
c         CALL AST_SETI( IPLOT, 'CURRENT', AST_GETI( IPLOT, 'BASE',
c     :                                              STATUS ),
c     :                  STATUS )
c
c*  Set the up vector for horizontal text.
c         UP( 1 ) = 0.0D0
c         UP( 2 ) = 1.0D0
c
c*  Draw the text.
c         CALL AST_TEXT( IPLOT, TEXT, RPOS, UP, JUST, STATUS )
c
c*  Re-instate the original Current Frame
c         CALL AST_SETI( IPLOT, 'CURRENT', ICURR, STATUS )
c
*  ------------------------------------------------------------


*  Blank (i.e. draw nothing).
*  ==========================
      ELSE IF( MODE .EQ. 'BLANK' ) THEN


*  Deal with Regions.
*  ===================
      ELSE IF( MODE .EQ. 'REGION' ) THEN

*  If no Region was supplied, just annul any cached Mapping and return.
         IF( REGION .EQ. AST__NULL ) THEN
            IF( MAP2 .NE. AST__NULL ) CALL AST_ANNUL( MAP2, STATUS )

*  If we have a Region, draw it.
         ELSE

*  If this is the first region to be drawn since a NULL Region was
*  supplied, find the Mapping that aligns the Region with the Plot, and
*  save it for use in subsequent invocations of this routine.
            IF( MAP2 .EQ. AST__NULL ) THEN

*  Save the index of the original base Frame within the Plot so it
*  can be re-instated later.
               IBASE = AST_GETI( IPLOT, 'BASE', STATUS )

*  Attempt to get a Mapping from the current Frame in the Plot to the Frame
*  represented by the Region. This will change the base Frame index in
*  the Plot.
               FS = AST_CONVERT( IPLOT, REGION, ' ', STATUS )

*  Re-instate the original base Frame
               CALL AST_SETI( IPLOT, 'BASE', IBASE, STATUS )

*  If the Region could be aligned with the Plot...
               IF( FS .NE. AST__NULL ) THEN

*  Get the Mapping from current plot Frame to Region.
                  MAP = AST_GETMAPPING( FS, AST__BASE, AST__CURRENT,
     :                                  STATUS )

*  Remove all Region effects from the Mapping.
                  MAP2 = AST_REMOVEREGIONS( MAP, STATUS )

*  Free resources.
                  CALL AST_ANNUL( MAP, STATUS )
                  CALL AST_ANNUL( FS, STATUS )
               END IF

            END IF

*  Check the Mapping was obtained.
            IF( MAP2 .NE. AST__NULL ) THEN

*  Save the index of the original current Frame within the Plot so it
*  can be re-instated later.
               ICURR = AST_GETI( IPLOT, 'CURRENT', STATUS )

*  Add the Region into the Plot, making it the current Frame.
               CALL AST_ADDFRAME( IPLOT, AST__CURRENT, MAP2, REGION,
     :                            STATUS )

*  Draw the boundary of the Region.
               JUNK = AST_BORDER( IPLOT, STATUS )

*  Remove the Region from the Plot.
               CALL AST_REMOVEFRAME( IPLOT, AST__CURRENT, STATUS )

*  Re-instate original current Frame in the Plot.
               CALL AST_SETI( IPLOT, 'CURRENT', ICURR, STATUS )

            END IF

         END IF

*  Tidy up.
*  ========

*  Report an error for an unknown MODE.
      ELSE IF( STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'MODE', MODE )
         CALL ERR_REP( 'KPG1_MKPOS_1', 'Unknown MODE value ''^MODE'' '//
     :                 'supplied (programming error).', STATUS )
      ENDIF

*  If DONE was supplied equal to .TRUE., reset the saved local variable
*  values.
      IF( DONE ) THEN
         DOING = .FALSE.
         DO I = 1, NDF__MXDIM
            LPOS( I ) = AST__BAD
            FPOS( I ) = AST__BAD
         END DO
      END IF

      END
