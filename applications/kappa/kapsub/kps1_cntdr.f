      SUBROUTINE KPS1_CNTDR( DIM1, DIM2, ARRAY, XLL, YLL, XSIZE, YSIZE,
     :                       NCONT, CONT, PENROT, THRESH, STATS, DONE,
     :                       CNTUSD, CNTLEN, CNTCLS, STATUS )
*+
*  Name:
*     KPS1_CNTDR

*  Purpose:
*     Draws contours of a 2-d array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_CNTDR( DIM1, DIM2, ARRAY, XLL, YLL, XSIZE, YSIZE, NCONT,
*                      CONT, PENROT, THRESH, STATS, DONE, CNTUSD,
*                      CNTLEN, CNTCLS, STATUS )

*  Description:
*     This routine plots a contour map of a two-dimensional sub-array
*     for a set of contour levels.  For each contour level, it searches
*     the array to find a cell of four pixels containing a contour at
*     that level, and then traces the contour until it closes or ends
*     outside the image.

*     A log of contour heights actually plotted is made.  The SGS pens
*     which draw the contours may be cycled modulo 3 for each height
*     used, and so assist in identification.

*     There is an option to compute the number of closed contours, and
*     the total contour length at each level used.

*     This application uses SGS and GKS graphics.

*  Arguments:
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
*     NCONT = INTEGER (Given)
*        The number of contour levels.
*     CONT( NCONT ) = REAL (Given)
*        The contour levels.
*     PENROT = LOGICAL (Given)
*        If .TRUE., the three pens drawing the contours will be cycled.
*     THRESH = REAL (Given)
*        The threshold below which pen 5 will be used for plotting, or
*        pens 5 to 7 if PENROT = .TRUE..  A bad value means that pen 2,
*        or pens 2 to 4 respectively are used.
*     DONE( XSIZE, YSIZE ) = LOGICAL (Returned)
*        Workspace to store log of pixels which have already been
*        contoured.
*     CNTUSD( NCONT ) = LOGICAL (Returned)
*        If an element is .TRUE., the contour level corresponding to
*        that element has been used.
*     CNTLEN( NCONT ) = REAL (Returned)
*        If STATS is .TRUE., this returns the total length of contours at
*        each level actually used.
*     CNTCLS( NCONT ) = INTEGER (Returned)
*        If STATS is .TRUE., this returns the number of closed contours
*        at each level actually used.
*     STATUS = INTEGER (Given)
*        Global status value.

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

*     Contour following continues until the next cell is off the
*     edge of the image, has already been "done" on this pass, contains
*     a "bad" pixel or is "confused" (i.e. is crossed by more than one
*     contour line).  In "confused" cells, all four cell sides are
*     crossed by contours, and the algorithm pairs the points to form
*     two line segents to plot which do not cross and which produce the
*     shortest total length of contour line within the cell.  Contour-
*     following also terminates if the buffer containing the list of
*     points to plot becomes full.

*     When contour following terminates, all pending output is plotted
*     with the appropriate pen (there are two separate lines to plot if
*     the final cell was confused).  The scan through the data (looking
*     for cells which are crossed by the current contour) then resumes
*     at the cell following the one which initiated the last episode of
*     contour-following.  Cells which are already flagged as "done" do
*     not subsequently trigger further contour-following on this pass.

*     Finally the graphics pen is reset if pen cycling was requested.
*     End

*  Notes:
*     -  Magic-value bad pixels are correctly processed.
*     -  Uses pen 2 for plotting.  5 also is used when there is a
*     threshold set.  For pen rotation these become pens 2 to 4 without
*     a threshold, and 5 to 7 with a threshold.

*  Prior Requirements:
*     -  This routine uses SGS and GKS graphics.  The GKS device must
*     already be open.
*     -  If the threshold is set, pen 5 should be a dashed form of pen
*     2.  When pen rotation is also requested pens 6 and 7 are dashed
*     forms of pens 3 and 4.

*  Implementation Deficiencies:
*     The contours are not smooth and the scanning algorithm can be made
*     many times faster by not examining all pixels at all heights.

*  Authors:
*     RFWS: Rodney Warren-Smith (STARLINK, Durham)
*     MJC: Malcolm J. Currie (STARLINK)
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
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SSE global definitions
      INCLUDE 'PRM_PAR'          ! VAL__ public constants

*  Arguments Given:
      INTEGER DIM1
      INTEGER DIM2
      REAL ARRAY( DIM1, DIM2 )
      INTEGER XLL
      INTEGER YLL
      INTEGER XSIZE
      INTEGER YSIZE
      INTEGER NCONT
      REAL CONT( NCONT )
      LOGICAL PENROT
      REAL THRESH
      LOGICAL STATS

*  Arguments Returned:
      LOGICAL DONE( XSIZE, YSIZE )
      LOGICAL CNTUSD( NCONT )
      REAL CNTLEN( NCONT )
      INTEGER CNTCLS( NCONT )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MAXPTS             ! Maximum number of positions in each
                                 ! axis that define the locus of a
                                 ! contour
      PARAMETER ( MAXPTS = 10000 )

      INTEGER NCELL              ! Number of pixels in a cell
      PARAMETER ( NCELL = 4 )


*  Local Variables:
      LOGICAL ABOVE              ! Next cell pixel's value is greater
                                 ! than contour level?
      LOGICAL ANOTE              ! Reference cell pixel's value is
                                 ! greater than contour level?
      REAL B( NCELL + 1 )        ! Storage of the pixel values
      LOGICAL BADPIX             ! Cell contains one or more invalid
                                 ! pixels?
      REAL BMAX                  ! Maximum pixel value in the cell
      REAL BMIN                  ! Minimum pixel value in the cell
      LOGICAL CONFUS             ! Cell is confused?
      REAL CX( NCELL )           ! X co-ordinates of the cell corners
      REAL CY( NCELL )           ! Y co-ordinates of the cell corners
      REAL CVAL                  ! Current contour value
      INTEGER DIST               ! Distance between two x,y positions
      LOGICAL DSHTHR             ! Is there a threshold?
      REAL DX( NCELL )           ! Differential x co-ordinates of cell
                                 ! corners
      REAL DY( NCELL )           ! Differential y co-ordinates of cell
                                 ! corners
      REAL FRACT                 ! Fractional position of contour from
                                 ! Linear interpolation
      INTEGER I                  ! Loop counter through columns
      INTEGER ICONT              ! Counter to index contour levels
      INTEGER II                 ! X element numbers of current pixel in
                                 ! sub-array
      INTEGER IMOVE( NCELL )     ! X directions to move from the cell
                                 ! side where a contour leaves
      INTEGER INPEN              ! Input SGS pen number
      INTEGER IX                 ! X element numbers of current pixel in
                                 ! full-size array
      INTEGER IY                 ! Y element numbers of current pixel in
                                 ! full-size array
      INTEGER J                  ! Loop counter through lines
      INTEGER JJ                 ! Y element numbers of current pixel in
                                 ! sub-array
      INTEGER JMOVE( NCELL )     ! Y directions to move from the cell
                                 ! side where a contour leaves
      INTEGER L                  ! General variable
      INTEGER LIN                ! Current entrance side of new cell
      LOGICAL LINEND             ! At end of a line?
      INTEGER LSIDE              ! Current exit side of cell
      INTEGER NEXIT              ! Number of cell exits for current cell
      INTEGER NEWSID( NCELL )    ! Side of entry in the new cell from
                                 ! the side of exit from the old cell
      INTEGER NPTS               ! Number of points in contour's locus
      LOGICAL OFFIMG             ! Outside the sub-array?
      REAL RDIST                 ! Distance between two x,y positions
      INTEGER PENNO              ! SGS pen number
      LOGICAL SAME               ! Two x-y positions are the same?
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

*  Inquire the current pen number.
      CALL SGS_IPEN( INPEN )

*  See if a threshold is required.
      DSHTHR = THRESH .NE. VAL__BADR

*  Initialise the pen colour for the first contour level.
      CALL SGS_SPEN( 2 )
      IF ( DSHTHR .AND. CONT( 1 ) .LT. THRESH ) CALL SGS_SPEN( 5 )

*  Scan through each contour level.
      DO ICONT = 1, NCONT
         CVAL = CONT( ICONT )

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

*  Find the position of the current pixel in the full two-dimensional array.
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

*  Count the number of closed contours by seeing tif the first and last
*  points are the same.
                           IF ( SAME( 1, NPTS ) ) THEN
                              CNTCLS( ICONT ) = CNTCLS( ICONT ) + 1
                           END IF
                        END IF

*  Plot the stored contour and reset the number of co-ordinates.
                        CALL GPL( NPTS, X, Y )

*  Plot the segment of the other contour found in the confused cell.
                        IF ( CONFUS )
     :                     CALL GPL( 2, X( NPTS+1 ), Y( NPTS+1 ) )

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

*  Set the pen if required.
         IF ( PENROT .AND. CNTUSD( ICONT ) ) THEN
            PENNO = MOD( ICONT, 3 ) + 2

*  Check whether next contour is below the threshold.  If so use the
*  higher pens.
            IF ( DSHTHR .AND.
     :           CONT( MIN( NCONT, ICONT+1 ) ) .LT. THRESH ) THEN
               PENNO = PENNO + 3
            END IF
            CALL SGS_SPEN( PENNO )

*  No cycling but have to decide which of two pens to use depending on
*  the next contour level.
         ELSE IF ( DSHTHR .AND.
     :             CONT( MIN( NCONT, ICONT+1 ) ) .LT. THRESH ) THEN
            CALL SGS_SPEN( 5 )

         ELSE IF ( DSHTHR ) THEN
            CALL SGS_SPEN( 2 )

         END IF

*  End of the contour-level loop.
      END DO

*  Reset the pen to the input value.
      CALL SGS_SPEN( INPEN )

      END
