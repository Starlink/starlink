      SUBROUTINE KPS1_CNTDR( IPLOT, IGRP, DIM1, DIM2, ARRAY, XLL, YLL,
     :                       XSIZE, YSIZE, NCONT, CONT, STATS, FAST,
     :                       DONE, CNTUSD, CNTLEN, CNTCLS, CNTPEN,
     :                       STATUS )
*+
*  Name:
*     KPS1_CNTDR

*  Purpose:
*     Draws contours of a 2-d array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_CNTDR( IPLOT, IGRP, DIM1, DIM2, ARRAY, XLL, YLL, XSIZE,
*                      YSIZE, NCONT, CONT, STATS, FAST, DONE, CNTUSD,
*                      CNTLEN, CNTCLS, CNTPEN, STATUS )

*  Description:
*     This routine plots a contour map of a two-dimensional sub-array
*     for a set of contour levels.  For each contour level, it searches
*     the array to find a cell of four pixels containing a contour at
*     that level, and then traces the contour until it closes or ends
*     outside the image.
*
*     A log of contour heights actually plotted is made.  The pens
*     which draw the contours may be cycled modulo 3 for each height
*     used, and so assist in identification.
*
*     There is an option to compute the number of closed contours, and
*     the total contour length at each level used.

*  Arguments:
*     IPLOT = INTEGER (Given)
*        An AST pointer to the Plot through which the graphics will be
*        produced.  The Current Frame should describe the GRID
*        co-ordinates of the array to be contoured.
*     IGRP = INTEGER (Given)
*        A GRP identifier for a group containing descriptions of the
*        pens to be used to draw each contour.  If this is supplied as
*        GRP__NOID, then all contours are drawn using the "Curves"
*        attributes (e.g. Colour(Curves), Width(Curves), etc.) in the
*        supplied Plot.  If a group is supplied, then each element in
*        the group should be a comma-separated list of AST attribute
*        settings to be established prior to drawing a contour.  Element
*        1 in the group is used for the first contour, element 2 for
*        the second, etc.  If the end of the group is reached before all
*        contours have been drawn, then another pass is made through the
*        group starting at element 1 again.  Any attributes not
*        specified default to their values in the Plot.
*     DIM1 = INTEGER (Given)
*        The first dimension of the two-dimensional array.
*     DIM2 = INTEGER (Given)
*        The second dimension of the two-dimensional array.
*     ARRAY( DIM1, DIM2 ) = REAL (Given)
*        Two-dimensional array to be contoured.
*     XLL = INTEGER (Given)
*        The x co-ordinate of the lower-left pixel of the selected
*        sub-array.
*     YLL = INTEGER (Given)
*        The y co-ordinate of the lower-left pixel of the selected
*        sub-array.
*     XSIZE = INTEGER (Given)
*        The x size of the sub-array to be contoured.
*     YSIZE = INTEGER (Given)
*        The y size of the sub-array to be contoured.
*     NCONT = INTEGER (Given)
*        The number of contour levels.
*     CONT( NCONT ) = REAL (Given)
*        The contour levels.
*     STATS = LOGICAL (Given)
*        Are contour statistics required?
*     FAST = LOGICAL (Given)
*        If .TRUE., then contours are drawn using straight lines as the
*        basic drawing element.  Otherwise, the basic drawing elements
*        are geodesic curves in the Current coordinate system of the
*        supplied Plot.  This is much slower to draw, but may be useful
*        when a severly non-linear or discontinuous mapping exists
*        between grid co-ordinates in the array, and graphics
*        co-ordinates.
*     DONE( XSIZE, YSIZE ) = LOGICAL (Returned)
*        Workspace to store log of pixels which have already been
*        contoured.
*     CNTUSD( NCONT ) = LOGICAL (Returned)
*        If an element is .TRUE., the contour level corresponding to
*        that element has been used.
*     CNTLEN( NCONT ) = REAL (Returned)
*        If STATS is .TRUE., this returns the total length of contours
*        at each level actually used.
*     CNTCLS( NCONT ) = INTEGER (Returned)
*        If STATS is .TRUE., this returns the number of closed contours
*        at each level actually used.
*     CNTPEN( NCONT ) = INTEGER (Returned)
*        The pen index used for each contour.
*     STATUS = INTEGER (Given)
*        Global status value.

*  Notes:
*     -  Magic-value bad pixels are correctly processed.

*  Algorithm:
*     Check for error on entry - return if not o.k.
*     The routine makes a separate pass through the image for each
*     contour to be plotted.  The image is divided into "cells" (groups
*     of four adjacent pixels) and each is examined in turn.  Cells
*     contining "bad" pixels are ignored, but for all others the
*     minimum and maximum cell data values are found.  If the contour
*     level currently being plotted lies between these two values, then
*     a contour crosses the cell in question, otherwise the cell is
*     skipped over on this pass.
*
*     Having identified a cell containing a contour, the contour
*     following algorithm is triggered.  Each cell side (a "side" is
*     one of the lines joining pixel centres) is examined to determine
*     if the contour passes through it and, if so, at what position.
*     If the contour only passes through two cell sides, then the cell
*     is "simple" and is only crossed by a single contour line.  In
*     this case, the contour entry and exit points are put into a list
*     of positions (to be plotted), the cell is flagged as "done" and
*     the algorithm moves on to the cell adjacent to the contour exit
*     position, where the process is repeated - thereby "following" the
*     contour.
*
*     Contour following continues until the next cell is off the
*     edge of the image, has already been "done" on this pass, contains
*     a "bad" pixel or is "confused" (i.e. is crossed by more than one
*     contour line).  In "confused" cells, all four cell sides are
*     crossed by contours, and the algorithm pairs the points to form
*     two line segents to plot which do not cross and which produce the
*     shortest total length of contour line within the cell.  Contour-
*     following also terminates if the buffer containing the list of
*     points to plot becomes full.
*
*     When contour following terminates, all pending output is plotted
*     with the appropriate pen (there are two separate lines to plot if
*     the final cell was confused).  The scan through the data (looking
*     for cells which are crossed by the current contour) then resumes
*     at the cell following the one which initiated the last episode of
*     contour-following.  Cells which are already flagged as "done" do
*     not subsequently trigger further contour-following on this pass.

*  Implementation Deficiencies:
*     The contours are not smooth and the scanning algorithm can be made
*     many times faster by not examining all pixels at all heights.

*  Copyright:
*     Copyright (C) 1000, 1983, 1988-1989 Science & Engineering
*     Research Council. Copyright (C) 1997-1998, 2001 Central
*     Laboratory of the Research Councils. Copyright (C) 2006 Particle
*     Physics & Astronomy Research Council. All Rights Reserved.

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
*     RFWS: Rodney Warren-Smith (STARLINK, Durham)
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     27 July 1983 (RFWS):
*        Original version.
*     1988 August 1 (MJC):
*        Original based on RFWS's IMCONT in the EDRS package
*        but following the KAPPA style.
*     1988 August 8 (RFWS):
*        Corrected bug for confused cell, where there are not three
*        pairings of contour crossing points, formerly only two were
*        assumed; added method section.
*     1988 August 22 (MJC):
*        Extra arguments CNTUSD and PENROT added.
*     1989 August 7 (MJC):
*        Passed array dimensions as separate variables.
*     1997 May (MJC):
*        Renamed from CNTDRW.  Used modern style prologue and
*        commenting.  Added THRESH argument (for dashed lines).
*        Increased co-ordinate buffers to 10000 points.  Added
*        statistics via new arguments STATS, CNTLEN, and CNTCLS.
*     17-MAR-1998 (DSB):
*        Modified to use AST for drawing the contours.
*     20-AUG-2001 (DSB):
*        Added argument CNTPEN.
*     2006 April 12 (MJC):
*        Remove unused variable and wrapped long lines.
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
      INTEGER IPLOT
      INTEGER IGRP
      INTEGER DIM1
      INTEGER DIM2
      REAL ARRAY( DIM1, DIM2 )
      INTEGER XLL
      INTEGER YLL
      INTEGER XSIZE
      INTEGER YSIZE
      INTEGER NCONT
      REAL CONT( NCONT )
      LOGICAL STATS
      LOGICAL FAST

*  Arguments Returned:
      LOGICAL DONE( XSIZE, YSIZE )
      LOGICAL CNTUSD( NCONT )
      REAL CNTLEN( NCONT )
      INTEGER CNTCLS( NCONT )
      INTEGER CNTPEN( NCONT )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      CHARACTER CHR_NTH*2

*  Local Constants:
      INTEGER MAXPTS             ! Maximum number of positions in each
      PARAMETER ( MAXPTS = 10000 ) ! axis that define the locus of a
                                 ! contour

      INTEGER NCELL              ! Number of pixels in a cell
      PARAMETER ( NCELL = 4 )

*  Local Variables:
      CHARACTER DOMAIN*( 40 )    ! Domain of Current Frame in supplied
                                 ! Plot
      CHARACTER PENDEF*( GRP__SZNAM ) ! AST attribute settings
                                 ! for current pen
      DOUBLE PRECISION ATTRS( 20 ) ! PGPLOT graphics attributes on entry
      INTEGER DIST               ! Distance between two x,y positions
      INTEGER I                  ! Loop counter through columns
      INTEGER ICONT              ! Counter to index contour levels
      INTEGER ICURR              ! Index of original Current Frame
      INTEGER II                 ! X element numbers of current pixel in
                                 ! sub-array
      INTEGER IMOVE( NCELL )     ! X directions to move from the cell
                                 ! side where a contour leaves
      INTEGER IPEN               ! Current pen number
      INTEGER IPLOTT             ! AST pointer to Plot with current pen
                                 ! set
      INTEGER IX                 ! X element numbers of current pixel in
                                 ! full-size array
      INTEGER IY                 ! Y element numbers of current pixel in
                                 ! full-size array
      INTEGER J                  ! Loop counter through lines
      INTEGER J1                 ! Index at start of attribute setting
      INTEGER J2                 ! Index of comma at end of attribute
                                 ! setting
      INTEGER JJ                 ! Y element numbers of current pixel in
                                 ! sub-array
      INTEGER JMOVE( NCELL )     ! Y directions to move from the cell
                                 ! side where a contour leaves
      INTEGER L                  ! General variable
      INTEGER LIN                ! Current entrance side of new cell
      INTEGER LSIDE              ! Current exit side of cell
      INTEGER NEWSID( NCELL )    ! Side of entry in the new cell from
                                 ! the side of exit from the old cell
      INTEGER NEXIT              ! Number of cell exits for current cell
      INTEGER NPEN               ! No. of pens supplied by IGRP
      INTEGER NPTS               ! Number of points in contour's locus
      LOGICAL ABOVE              ! Next cell pixel's value is greater
                                 ! than contour level?
      LOGICAL ANOTE              ! Reference cell pixel's value is
                                 ! greater than contour level?
      LOGICAL BADAT              ! Was attribute setting string invalid?
      LOGICAL BADPIX             ! Cell contains one or more invalid
                                 ! pixels?
      LOGICAL CONFUS             ! Cell is confused?
      LOGICAL LINEND             ! At end of a line?
      LOGICAL OFFIMG             ! Outside the sub-array?
      LOGICAL SAME               ! Two x-y positions are the same?
      REAL B( NCELL + 1 )        ! Storage of the pixel values
      REAL BMAX                  ! Maximum pixel value in the cell
      REAL BMIN                  ! Minimum pixel value in the cell
      REAL CVAL                  ! Current contour value
      REAL CX( NCELL )           ! X co-ordinates of the cell corners
      REAL CY( NCELL )           ! Y co-ordinates of the cell corners
      REAL DX( NCELL )           ! Differential x co-ordinates of cell
                                 ! corners
      REAL DY( NCELL )           ! Differential y co-ordinates of cell
                                 ! corners
      REAL FRACT                 ! Fractional position of contour from
                                 ! Linear interpolation
      REAL RDIST                 ! Distance between two x,y positions
      REAL X( MAXPTS )           ! X positions of the contour
      REAL XTEMP                 ! Dummy for swapping x position of
                                 ! contour
      REAL Y( MAXPTS )           ! Y positions of the contour
      REAL YTEMP                 ! Dummy for swapping y position of
                                 ! contour

*   Internal References:
      BADPIX( I, J ) = ( ARRAY( I, J )     .EQ. VAL__BADR ) .OR.
     :                 ( ARRAY( I+1, J )   .EQ. VAL__BADR ) .OR.
     :                 ( ARRAY( I+1, J+1 ) .EQ. VAL__BADR ) .OR.
     :                 ( ARRAY( I, J+1 )   .EQ. VAL__BADR )

      DIST( I, J ) = SQRT( ( X( I ) - X( J ) )**2 +
     :                     ( Y( I ) - Y( J ) )**2 )

      OFFIMG( I, J ) = ( I .LT. 1 ) .OR. ( J .LT. 1 ) .OR.
     :                 ( I .GE. XSIZE ) .OR. ( J .GE. YSIZE )

      RDIST( I, J ) = SQRT( ( X( I ) - X( J ) )**2 +
     :                     ( Y( I ) - Y( J ) )**2 )

      SAME( I, J ) = ABS( X( I ) - X( J ) ) .LT. VAL__EPSR *
     :               MAX( ABS( X( I ) ), ABS( X( J ) ) ) .AND.
     :               ABS( Y( I ) - Y( J ) ) .LT. VAL__EPSR *
     :               MAX( ABS( Y( I ) ), ABS( Y( J ) ) )

*  Local Data:
      DATA CX /0.0, 1.0, 1.0, 0.0/
      DATA CY /0.0, 0.0, 1.0, 1.0/
      DATA DX /1.0, 0.0, -1.0, 0.0/
      DATA DY /0.0, 1.0, 0.0, -1.0/
      DATA IMOVE /0, 1, 0, -1/
      DATA JMOVE /-1, 0, 1, 0/
      DATA NEWSID /3, 4, 1, 2/
*.

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Report an error if the Current Frame in the supplied Plot does not
*  have a Domain value of GRID.
      DOMAIN = AST_GETC( IPLOT, 'DOMAIN', STATUS )
      IF ( DOMAIN .NE. 'GRID' ) THEN

         IF ( STATUS .EQ. SAI__OK ) THEN
            CALL MSG_SETC( 'DOM', DOMAIN )
            STATUS = SAI__ERROR
            CALL ERR_REP( 'KPS1_CNTDR_1', 'KPS1_CNTDR: Current Frame '//
     :                    'in supplied Plot has Domain ^DOM but '//
     :                    'should have Domain GRID (programming '//
     :                    'error).', STATUS )
         END IF

         GO TO 999

      END IF

*  Simplify the Plot.  This adds a new Current Frame into the Plot, so
*  note the index of the original Current Frame so that it can be
*  re-instated later.  This can help to speed up the drawing, and also
*  avoids the possibility of the Mapping going via a Frame in which the
*  positions are undefined.
      ICURR = AST_GETI( IPLOT, 'CURRENT', STATUS )
      CALL KPG1_ASSIM( IPLOT, STATUS )

*  Store the number of pens supplied in the GRP group.
      IF ( IGRP .NE. GRP__NOID ) THEN
         CALL GRP_GRPSZ( IGRP, NPEN, STATUS )
      ELSE
         NPEN = 0
      END IF

*  Initialise the pen number.
      IPEN = 1

*  Scan through each contour level.
      DO ICONT = 1, NCONT
         CVAL = CONT( ICONT )

*  If different pens are being used, produce a modified Plot which draws
*  curves with the pen style supplied for this contour.
         IF ( NPEN .GT. 0 ) THEN

*  Take a deep copy of the supplied Plot. This Plot will be modify using
*  the supplied attribute settings.  A copy is used so that the original
*  plotting attributes can be re-instated later.
            IPLOTT = AST_COPY( IPLOT, STATUS )

*  Get the next list of AST Attribute settings from the group.
            CALL GRP_GET( IGRP, IPEN, 1, PENDEF, STATUS )

*  Abort if an error has occurred.
            IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Loop round each comma-delimited attribute in the definitions,
*  translating colour names and any defined synonyms, and storing it in
*  the Plot.
            IF ( PENDEF .NE. ' ' ) THEN
               J1 = 1
               DO WHILE( J1 .LE. GRP__SZNAM )
                  J2 = J1
                  CALL CHR_FIND( PENDEF, ',', .TRUE., J2 )
                  CALL KPG1_ASSTS( PENDEF( J1 : J2 - 1 ), .TRUE.,
     :                             .TRUE., IPLOTT, BADAT, STATUS )
                  J1 = J2 + 1
               END DO

*  Issue a context message if anything went wrong setting the pen.
               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL MSG_SETI( 'I', ICONT )
                  CALL MSG_SETC( 'I', CHR_NTH( ICONT ) )
                  CALL ERR_REP( 'KPS1_CNTDR_1', 'Unable to set the '//
     :                         'pen for the ^I contour level.', STATUS )
                  GO TO 999
               END IF

            END IF

*  Increment the index of the next pen to use, cycling back to the start
*  when the end is reached.
            IPEN = IPEN + 1
            IF ( IPEN .GT. NPEN ) IPEN = 1

*  If the same pen is being used for all contours, just clone the
*  supplied Plot pointer.
         ELSE
            IPLOTT = AST_CLONE( IPLOT, STATUS )
         END IF

*  Return the colour used for this contour.
         CNTPEN( ICONT ) = AST_GETI( IPLOTT, 'COLOUR(CURVES)', STATUS )

*  Buffer all PGPLOT output produced while drawing this contour.
         CALL PGBBUF

*  If using FAST drawing mode, establish the new PGPLOT attributes,
*  saving the old in ATTRS. If not using fast drawing mode, AST will
*  establish them when AST_CURVE is called to draw the contour.
         IF ( FAST ) CALL KPG1_PGSTY( IPLOTT, 'CURVES', .TRUE., ATTRS,
     :                               STATUS )

*  Initialise record of contour levels actually used.
         CNTUSD( ICONT ) = .FALSE.
         IF ( STATS ) THEN
            CNTLEN( ICONT ) = 0.0
            CNTCLS( ICONT ) = 0
         END IF

*  Initialise the store of cells done.
         DO J = 1, YSIZE - 1
            DO I = 1, XSIZE - 1
               DONE( I, J ) = .FALSE.
            END DO
         END DO

*  Initialise counter for number of x-y co-ordinates to plot.
         NPTS = 0

*  Scan the image, looking for a cell containing the current contour
*  level.
         DO J = 1, YSIZE - 1
            DO I = 1, XSIZE - 1

*  If he cell has already been contoured, omit it.
               IF ( .NOT. DONE( I, J ) ) THEN

*  Note this cell has been looked at.
                  DONE( I, J ) = .TRUE.

*  Find the position of the current pixel in the full two-dimensional
*  array.
                  IX = I + XLL - 1
                  IY = J + YLL - 1

*  Don't use this cell if there is a bad pixel adjacent.
                  IF ( .NOT. BADPIX( IX, IY ) ) THEN

*  Extract data values and test if they contain the contour.
                     B( 1 ) = ARRAY( IX, IY )
                     B( 2 ) = ARRAY( IX+1, IY )
                     B( 3 ) = ARRAY( IX+1, IY+1 )
                     B( 4 ) = ARRAY( IX, IY+1 )
                     BMAX = MAX( B( 1 ), B( 2 ), B( 3 ), B( 4 ) )
                     BMIN = MIN( B( 1 ), B( 2 ), B( 3 ), B( 4 ) )

                     IF ( CVAL .LT. BMAX .AND. CVAL .GT. BMIN ) THEN
                        B( 5 ) = B( 1 )

*  Initialise the pointers to the cells on this contour.
                        II = I
                        JJ = J

*  Initialise the cell side where the contour enters the cell.
                        LIN = 0
                        LINEND = .FALSE.
                        DO WHILE ( .NOT. LINEND )
                           NEXIT = 0

*  Scan the cell sides, searching for intersections with the contour.
                           ANOTE = B( 1 ) .GE. CVAL

                           DO L = 1, 4
                              ABOVE = B( L+1 ) .GE. CVAL

*  Don't count contour exits from the same side as it entered.
                               IF ( ( ABOVE .NEQV. ANOTE ) .AND.
     :                             ( L .NE. LIN ) ) THEN
                                 LSIDE = L
                                 NEXIT = NEXIT+1
                                 NPTS = NPTS+1

*  Calculate the co-ordinates of the contour exit point from the cell
*  by linear interpolation, and store them in X and Y.
                                 FRACT = ( CVAL - B( L ) ) / ( B( L+1 )
     :                                   -B( L ) )
                                 X( NPTS ) = IX + CX( L ) + DX( L ) *
     :                                       FRACT
                                 Y( NPTS ) = IY + CY( L ) + DY( L ) *
     :                                       FRACT

                              END IF

                              ANOTE = ABOVE
                           END DO

*  The cell is confused if the number of contour exits does not match
*  the number of entries.
                           IF ( LIN .EQ. 0 ) THEN
                              CONFUS = NEXIT .NE. 2

                           ELSE
                              CONFUS = NEXIT .NE. 1
                           END IF

*  Find the co-ordinates of the next cell which the contour enters.
                           II = II + IMOVE( LSIDE )
                           JJ = JJ + JMOVE( LSIDE )
                           IX = IX + IMOVE( LSIDE )
                           IY = IY + JMOVE( LSIDE )

*  Find the side of the new cell through which it enters.
                           LIN = NEWSID( LSIDE )

*  It is the end of current contour line if the:
*     o  contour goes off edge of the image,
*     o  hits an invalid pixel,
*     o  enters a cell already contoured,
*     o  leaves a confused cell, or
*     o  exceeds the storage space for the X and Y arrays.
                           IF ( OFFIMG( II, JJ ) ) THEN
                              LINEND = .TRUE.

                           ELSE
                              LINEND = BADPIX( IX, IY ) .OR. CONFUS
     :                                 .OR. DONE( II, JJ ) .OR.
     :                                 ( NPTS .GE. MAXPTS )
                           END IF

*  If we are continuing on this contour, extract the data for next cell
*  and mark the cell done.
                           IF ( .NOT. LINEND ) THEN
                              B( 1 ) = ARRAY( IX, IY )
                              B( 2 ) = ARRAY( IX+1, IY )
                              B( 3 ) = ARRAY( IX+1, IY+1 )
                              B( 4 ) = ARRAY( IX, IY+1 )
                              B( 5 ) = B( 1 )
                              DONE( II, JJ ) = .TRUE.
                           END IF

*  Return to analyse the new cell.
                        END DO

*  If the last cell on a contour was confused, all four cell sides will
*  be crossed by a contour.  The crossing points must be correctly
*  paired.  There are three possible pairing combinations which leave
*  the first point in its original position.
                        IF ( CONFUS ) THEN

*  Check if the current pairing causes contour lines to cross.  If so,
*  swap the appropriate pair of points so they no longer cross.
                           IF ( ( MAX( X(NPTS), X(NPTS-1) ) .GT.
     :                            MAX( X(NPTS-2), X(NPTS-3) ) .AND.
     :                            MIN( X(NPTS), X(NPTS-1) ) .LT.
     :                            MIN( X(NPTS-2), X(NPTS-3) ) ) .OR.
     :                          ( MAX( X(NPTS), X(NPTS-1) ) .LT.
     :                            MAX( X(NPTS-2), X(NPTS-3) ) .AND.
     :                            MIN( X(NPTS), X(NPTS-1) ) .GT.
     :                            MIN( X(NPTS-2), X(NPTS-3) ) ) ) THEN

                              XTEMP = X( NPTS-1 )
                              YTEMP = Y( NPTS-1 )
                              X( NPTS-1 ) = X( NPTS-2 )
                              Y( NPTS-1 ) = Y( NPTS-2 )
                              X( NPTS-2 ) = XTEMP
                              Y( NPTS-2 ) = YTEMP
                           END IF

*  Make a further swap if necessary, to find the pairing (out of the
*  two which remain) which produces the shorter total length of contour
*  line.
                           IF ( DIST( NPTS, NPTS-1 ) +
     :                          DIST( NPTS-2, NPTS-3 ) .GT.
     :                          DIST( NPTS-1, NPTS-2 ) +
     :                          DIST( NPTS-3, NPTS ) ) THEN

*  Swap the pairing if necessary.
                              XTEMP = X( NPTS )
                              YTEMP = Y( NPTS )
                              X( NPTS ) = X( NPTS-2 )
                              Y( NPTS ) = Y( NPTS-2 )
                              X( NPTS-2 ) = XTEMP
                              Y( NPTS-2 ) = YTEMP
                           END IF
                           NPTS = NPTS - 2

*  End of confusion check.
                        END IF

*  Indicate contour level has been used.
                        CNTUSD( ICONT ) = .TRUE.

*  Add the length of the new section of contour.  Allow for a spearate
*  section arising from a confused contour.
                        IF ( STATS .AND. NPTS .GT. 1 ) THEN
                           DO L = 1, NPTS - 1
                              CNTLEN( ICONT ) = CNTLEN( ICONT ) +
     :                                          RDIST( L, L + 1 )
                           END DO
                           IF ( CONFUS ) THEN
                              CNTLEN( ICONT ) = CNTLEN( ICONT ) +
     :                                          RDIST( NPTS, NPTS + 1 )
                           END IF

*  Count the number of closed contours by seeing if the first and last
*  points are the same.
                           IF ( SAME( 1, NPTS ) ) THEN
                              CNTCLS( ICONT ) = CNTCLS( ICONT ) + 1
                           END IF
                        END IF

*  Plot the stored contour.
                        CALL KPG1_ASCRV( IPLOTT, FAST, NPTS, X, Y,
     :                                   STATUS )

*  Plot the segment of the other contour found in the confused cell.
                        IF ( CONFUS ) THEN
                           CALL KPG1_ASCRV( IPLOTT, FAST, 2,
     :                                     X( NPTS + 1 ), Y( NPTS + 1 ),
     :                                     STATUS )
                        END IF

*  Reset the number of points to plot.
                        NPTS = 0

*  End of contour-lies-between-pixels check.
                     END IF

*  End of bad-pixel check.
                  END IF

*  End of already contoured-pixel check.
               END IF

*  End of the loop through the columns.
            END DO

*  End of the loop through the lines.
         END DO

*  Flush the buffers used by KPG1_ASCRV.
         CALL KPG1_ASCRV( IPLOTT, FAST, 0, 0.0, 0.0, STATUS )

*  If using FAST drawing mode, re-establish the old PGPLOT plotting
*  attributes.
         IF ( FAST ) CALL KPG1_PGSTY( IPLOTT, 'CURVES', .FALSE., ATTRS,
     :                               STATUS )

*  Flush the buffer holding PGPLOT output produced while drawing this
*  contour.
         CALL PGEBUF

*  Annul the temporary copy of the supplied Plot which was used to do
*  the drawing.
         CALL AST_ANNUL( IPLOTT, STATUS )

*  End of the contour-level loop.
      END DO

 999  CONTINUE

*  Remove the Current Frame added by KPG1_ASSIM and re-instate the
*  original Current Frame.
      CALL AST_REMOVEFRAME( IPLOT, AST__CURRENT, STATUS )
      CALL AST_SETI( IPLOT, 'CURRENT', ICURR, STATUS )

      END
