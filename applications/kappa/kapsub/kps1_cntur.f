      SUBROUTINE KPS1_CNTUR( DIM1, DIM2, ARRAY, XSTART, YSTART, XSIZE,
     :                       YSIZE, WKDIM1, WKDIM2, NCONT, CONT, DX, DY,
     :                       ANNOTA, NOISY, LABFRQ, PENROT, THRESH,
     :                       MAXRES, NLOCUS, XP, YP, SLIST, LINK, FLAG,
     :                       CNTUSD, STATUS )
*+
*  Name:
*     KPS1_CNTUR

*  Purpose:
*     Contour an image quickly.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_CNTUR( DIM1, DIM2, ARRAY, XSTART, YSTART, XSIZE, YSIZE,
*                      WKDIM1, WKDIM2, NCONT, CONT, DX, DY, ANNOTA,
*                      NOISY, LABFRQ, PENROT, THRESH, MAXRES, NLOCUS,
*                      XP, YP, SLIST, LINK, FLAG, CNTUSD, STATUS )

*  Description:
*     The routine plots a contour map of a two-dimensional sub-array.
*     It is optimised for speed of execution.  It uses a linked list to
*     keep track of which cells of pixels have already been contoured
*     to avoid unnecessary searching of all pixels at each contour
*     level.  All line segments are drawn as straight lines without
*     smoothing.

*     There is an option to draw to the plotting resolution, i.e.
*     sub-pixel segments, otherwise it is only single pixel resolution.
*     This slows down the contouring.  If speed is of the essence, then
*     this option should not be used.  (Subroutine J06GBF in application
*     CONTOUR has better sub-pixel plotting algorithms that smooth the
*     contours giving rounded images.)

*     A log of contour heights actually plotted is made.   Contours may
*     be annotated, the frequency with contour level may be defined.
*     If no annotations are used then the SGS pens which draw the
*     contours may be cycled modulo 3 for each height used, and so
*     assist in identification.  A threshold may be set below which
*     contours are drawn with different pens (usually dashed).

*  Arguments:
*     DIM1 = INTEGER (Given)
*        The first dimension of the two-dimensional array.
*     DIM2 = INTEGER (Given)
*        The second dimension of the two-dimensional array.
*     ARRAY( DIM1, DIM2 ) = REAL (Given)
*        Input array of data.
*     XSTART = INTEGER (Given)
*        X start co-ordinate of the sub-array to be contoured.
*     YSTART = INTEGER (Given)
*        Y start co-ordinate of the sub-array to be contoured.
*     XSIZE = INTEGER (Given)
*        X size of the sub-array to be contoured.
*     YSIZE = INTEGER (Given)
*        Y size of the sub-array to be contoured.
*     WKDIM1 = INTEGER (Given)
*        The first dimension of the LINK and FLAG arrays.
*     WKDIM2 = INTEGER (Given)
*        The second dimension of the LINK and FLAG arrays.
*     NCONT = INTEGER (Given)
*        Number of contour levels.
*     CONT( NCONT ) = REAL (Given)
*        The contour levels. These should be in increasing order.
*     DX = REAL (Given)
*        The x resolution of the contour segments, i.e. only contour
*        segments longer than this will be plotted.
*     DY = REAL (Given)
*        The y resolution of the contour segments, i.e. only contour
*        segments longer than this will be plotted.
*     ANNOTA = LOGICAL (Given)
*        If .TRUE., the contours are to be annotated with contour
*        numbers.
*     NOISY = LOGICAL (Given)
*        If .TRUE., annotations are written to a given contour at twice
*        the frequency.
*     LABFRQ = INTEGER (Given)
*        The frequency of annotated contours as the contour level
*        changes, thus 1 means all contour levels will be annotated.
*     PENROT = LOGICAL (Given)
*        If .TRUE., and no annotation is being performed the three pens
*        drawing the contours will be cycled.
*     THRESH = REAL (Given)
*        The threshold below which pen 5 will be used for plotting, or
*        pens 5 to 7 if PENROT = .TRUE..  A bad value means that pen 2,
*        or pens 2 to 4 respectively are used.
*     MAXRES = LOGICAL (Given)
*        If .TRUE., contours will be interpolated between pixels to the
*        resolutions DX and DY.
*     NLOCUS = INTEGER (Given)
*        The maximum number of segments that can be stored, and should
*        be comfortably large enough to hold the longest contour
*        allowing for sub-pixel resolution if that option is selected.
*     XP( NLOCUS ) = REAL (Returned)
*        Work array to store the contour locus, x positions.
*     YP( NLOCUS ) = REAL (Returned)
*        Work array to store the contour locus, y positions.
*     SLIST( NCONT ) = INTEGER (Returned)
*        Work space for starting cells  of linked lists.
*     LINK( WKDIM1 * WKDIM2 ) = INTEGER (Returned)
*        Work space for the pointers linking contourable array cells in
*        a series of linked lists.
*     FLAG( WKDIM1 * WKDIM2 ) = INTEGER (Returned)
*        Work space to flag which array cells have been contoured.
*     CNTUSD( NCONT ) = LOGICAL (Returned)
*        If an element is .TRUE., the contour level corresponding to
*        that element has been used.
*     STATUS  = INTEGER (Given)
*        Global status value.

*  Algorithm:
*     Check inherited status
*     Initialise line patterns for annotated contours, or store the pen
*       number for cyclic pens
*     Compute the size of the notional sub-pixel array when required
*     Scan through the rectangular region of the array specified,
*       dividing it into panels.
*     For each panel
*        Locate the contourable cells and asociate them in linked lists
*        For each contour
*           Initialise a flag for all the cells in the crowded-cell list
*           For simple then crowded cells
*              Loop through all cells linked to the starting cell that
*                have not already been contoured
*                 Extract the pixel values for the four corners of the
*                   cell
*                 Determine which edges of the cell are crossed by the
*                   contour
*                 If at least one cell is crossed by the contour then
*                    Note that the contour level has been used
*                    Loop to follow the contour line
*                       Compute the fractional intersections along the
*                         crossed edges
*                       Allow for the special case when all four edges
*                         are crossed, switching the connnections
*                         between sides if the previous cell's exit side
*                         was the bottom, or by dividing the cell into
*                         triangles
*                       Initialise last-contour plotted if the contour
*                         tracking has just begun and store the first
*                         point in the contour locus
*                       If sub-pixel resolution required compute the
*                         locus within the cell and update the last
*                         position
*                       Store the exit point from the cell if it is
*                         longer than the resolution or there is only
*                         one point stored so far
*                       If the cell has two line segments then
*                          Plot the second one, with sub-pixel
*                            resolution if requested and to the plotting
*                            resolution, via NCAR for annotated contours
*                            otherwise by GKS directly
*                          Exit the contour-tracing loop
*                       Endif
*                       Record that contour tracking has started and
*                         side it left the cell
*                       For a given exit side shift the X or Y position
*                         by a pixel, exiting the contour-tracking loop
*                         if the cell lies outside the panel or the
*                         cell has already been contoured. Compute which
*                         sides the contour crosses excluding the side
*                         it entered
*                    Endfor
*                    If there is more than one point to plot use NCAR
*                      curve-drawing routine to plot annotated contours,
*                      or GKS polyline with option pen rotation
*                      otherwise
*                 Endif
*                 Find next cell which may contain the current contour
*                   level
*              Endfor
*           Endfor
*        Endfor
*     Endfor
*     End

*  Notes:
*     -  Magic-value bad pixels are correctly processed.
*     -  Uses pen 2 for plotting.  5 also is used when there is a
*     threshold set.  For pen rotation these become pens 2 to 4 without
*     a threshold, and 5 to 7 with a threshold.

*  Prior Requirements:
*     -  This routine uses SGS, NCAR and GKS graphics. The GKS device
*     must already be open.
*     -  If the threshold is set, pen 5 should be a dashed form of pen
*     2.  When pen rotation is also requested pens 6 and 7 are dashed
*     forms of pens 3 and 4.

*  Implementation Deficiencies:
*     Does not handle contours that pass through the cell corners well.
*     The sub-pixel resolution is only partially successful, and
*     requires a smoothing technique.  The number of segments is not
*     checked that it does not exceed the buffer size, because of
*     efficiency reasons.  Needs to handle smoothing options for
*     sub-pixel resolution.

*  Authors:
*     R.F. Warren-Smith (STARLINK, University of Durham)
*     Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26-SEP-1988 (RFWS):
*        Original version.
*     1989 Aug 26 (MJC):
*        Completed prologue; added many extra arguments and options
*        (e.g. annotation, pen rotation, sub-pixel plotting to the
*        resolution); improved checks to prevent crossing contours.
*     1989 Oct 20 (MJC):
*        Somehow an old version crept in which no longer drew the
*        correct annotations --- now corrected.
*     1991 Oct 22 (MJC):
*        Looks like it's happened again.  Corrected the annotation
*        dashed-line selection (ignores number of points).  Also
*        decreased length of dashed patterns by 50 per cent.
*     1997 May (MJC):
*        Renamed from CNTTUR.  Used modern style prologue and
*        commenting.  Added THRESH argument (for dashed lines).
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants

*  Arguments Given:
      INTEGER DIM1, DIM2         ! ARRAY dimension sizes
      REAL ARRAY( DIM1 * DIM2 )  ! Image to be contoured
      INTEGER XSTART, YSTART     ! Start element in the sub-array
      INTEGER XSIZE, YSIZE       ! Dimensions of the sub-array
      INTEGER WKDIM1, WKDIM2     ! Dimension sizes of the LINK and FLAG
                                 ! workspace arrays
      INTEGER NCONT              ! Number of contour levels
      REAL CONT( NCONT )         ! Array of contour levels
      REAL DX                    ! X resolution of the plotting
      REAL DY                    ! Y resolution of the plotting
      LOGICAL ANNOTA             ! Contour lines will alternately be
                                 ! annotated with a contour number?
      LOGICAL NOISY              ! Annotations will be written twice as
                                 ! frequently as for smooth contours?
      INTEGER LABFRQ             ! Frequency of contour-level
                                 ! annotations
      LOGICAL PENROT             ! Pens are cycled as the
                                 ! contour level increases?
      REAL THRESH                ! Pen-change threshold
      LOGICAL MAXRES             ! If true sub-pixel contouring should be
                                 ! attempted, plotting detail to the
                                 ! x-y resolutions
      INTEGER NLOCUS             ! The maximum number of contour segments
                                 ! that can be stored

*  Arguments Returned:
      REAL XP( NLOCUS )          ! Contour locus, ositions
      REAL YP( NLOCUS )          ! Ditto y positions
      INTEGER SLIST( NCONT )     ! Workspace
      INTEGER LINK( WKDIM1 * WKDIM2 ) ! Workspace
      INTEGER FLAG( WKDIM1 * WKDIM2 ) ! Workspace
      LOGICAL CNTUSD( NCONT )    ! If an element is true the contour
                                 ! height corresponding to that element
                                 ! has been used

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      CHARACTER * ( 16 ) AGDSHN  ! NCAR dash-pattern name generator
      EXTERNAL AGDSHN

*  Local Constants:
      INTEGER MXSBPX             ! Maximum number of sub-pixels
      PARAMETER( MXSBPX = 200 )

      INTEGER NPATRN             ! Maximum number of NCAR line
      PARAMETER( NPATRN = 26 )   ! patterns

      INTEGER BOTTOM
      PARAMETER ( BOTTOM = 1 )

      INTEGER RIGHT
      PARAMETER ( RIGHT = 2 )

      INTEGER TOP
      PARAMETER ( TOP = 3 )

      INTEGER LEFT
      PARAMETER ( LEFT = 4 )

*  Local Variables:
      INTEGER CELL               ! Cell number: vectorised array index
                                 ! for the cell entry in the FLAG array
      REAL CENTRE                ! Average pixel value in the cell
      REAL CLEVEL                ! Current contour level
      INTEGER CLIST              ! Pointer into the linked list of
                                 ! "crowded" cells
      INTEGER CONTYP             ! Type of cell being used to initiate
                                 ! a contour-following: 1 => simple,
                                 ! 2 => crowded
      LOGICAL CROSSB             ! Contour at the current level crosses
                                 ! the bottom edge of the current cell?
      LOGICAL CROSSL             ! Ditto, left edge
      LOGICAL CROSSR             ! Ditto, right edge
      LOGICAL CROSST             ! Ditto, top edge
      INTEGER DIMPL( 2 )         ! Lower dimension bounds of sub-array
      INTEGER DIMPU( 2 )         ! Upper dimension bounds of sub-array
      CHARACTER * ( 10 ) DSHPAT( NPATRN ) ! NCAR dashed line pattern - noisy case
      CHARACTER * ( 20 ) DSHPAS( NPATRN ) ! NCAR dashed line pattern
      LOGICAL DSHTHR             ! Is there a threshold?
      INTEGER EXIT               ! Flag to indicate through which edge
                                 ! of a cell a contour line makes its
                                 ! exit
      REAL FRACT                 ! Fractional cell extent of contour
                                 ! intersection point with a cell edge
      INTEGER I                  ! Loop counter for solid line
      INTEGER ICELL              ! Cell number of initial cell on a
                                 ! contour which initiates contour
                                 ! following
      INTEGER ICONT              ! Counter to index contour levels
      INTEGER INPEN              ! Input SGS pen number
      INTEGER IPIX               ! Vectorised array index for the top
                                 ! right hand pixel in a cell
      INTEGER ISWAP              ! Temporary variable for inter-changing
                                 ! INTEGER values
      INTEGER K                  ! Loop counter for line pattern
      INTEGER LINO               ! Dashed-line number offset
      INTEGER LINTYP             ! Line type (0=continuous, +ve=user
                                 ! defined
      INTEGER MINPTS             ! Minimum number of points in dashed
                                 ! line
      INTEGER NCROSS             ! Number of contour lines leaving a
                                 ! cell
      INTEGER NPTS               ! Number of points in contour's locus
      INTEGER NPTSC              ! Number of points in second contour's
                                 ! locus in a confused cell
      INTEGER PENNO              ! SGS pen number
      REAL PIXBL                 ! Data value of bottom-left pixel in
                                 ! the current cell
      REAL PIXBR                 ! Ditto, bottom-right pixel
      REAL PIXTR                 ! Ditto, top-right pixel
      REAL PIXTL                 ! Ditto, top-left pixel
      LOGICAL PLOT               ! Contour polyline has been stored
                                 ! awaiting plotting?
      INTEGER PREXIT             ! Side from which the contour exited in
                                 ! the previous cell
      INTEGER SIDES( 4 )         ! The cell sides corresponding to the
                                 ! XCONT, YCONT arrays
      INTEGER SBDIM1             ! x dimension of sub-pixel array
      INTEGER SBDIM2             ! y dimension of sub-pixel array
      INTEGER SOLID              ! Pattern number for solid line
      REAL SWAP                  ! Temporary variable for inter-changing
                                 ! REAL values
      LOGICAL SUBPIX             ! Perform sub-pixel contouring?
      LOGICAL TESTBL             ! Bottom-left pixel in the current cell
                                 ! exceeds (or is equal to) the current
                                 ! contour level?
      LOGICAL TESTBR             ! Ditto, bottom-right pixel
      LOGICAL TESTTL             ! Ditto, top-left pixel
      LOGICAL TESTTR             ! Ditto, top right pixel
      INTEGER X                  ! X (two-dimensional) index of the
                                 ! bottom-left pixel in a cell
      REAL XCONT( 4 )            ! Buffer to contain contour X
                                 ! co-ordinates
      REAL XLAST                 ! X position of the last contour
                                 ! segment plotted
      INTEGER XMAX               ! Upper limit for the X extent of an
                                 ! image region
      INTEGER XMIN               ! Lower limit for the X extent of an
                                 ! image region
      REAL XPC( MXSBPX + 2 )     ! X contour locus for second contour in
                                 ! confused area
      INTEGER Y                  ! Y (two-dimensional) index of the
                                 ! bottom-left pixel in a cell
      REAL YCONT( 4 )            ! Buffer to contain contour Y
                                 ! co-ordinates
      REAL YLAST                 ! Y position of the last contour
                                 ! segment plotted
      INTEGER YMAX               ! Upper limit for the Y extent of an
                                 ! image region
      INTEGER YMIN               ! Lower limit for the Y extent of an
                                 ! image region
      REAL YPC( MXSBPX + 2 )     ! Ditto y contour

*.

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  See if a threshold is required.
      DSHTHR = THRESH .NE. VAL__BADR

*  If annotated contours are required use a naive attempt to label
*  the contours via NCAR
      IF ( ANNOTA ) THEN
         IF ( LABFRQ .EQ. 1 ) THEN
            LINO = 0
         ELSE
            LINO = 1
         END IF

*  Find pattern number of the solid line for contours or contour
*  segments that are not to be annotated.
         SOLID = MIN( NPATRN, ( NCONT - 1 ) / LABFRQ + 2 )

*  Noisy data require more frequent annotations, so the dashed-line
*  pattern is shorter.
         IF ( NOISY ) THEN
            MINPTS = 10

*  First define some patterns.
            DO K = 1, NCONT, LABFRQ
               IF ( K/LABFRQ .LT. NPATRN - 1 + LINO )  THEN
                  IF ( K .LT. 10 ) THEN

*  Format statements used to make apostrophes (pen up) clearer and
*  easier to support.
                     WRITE( DSHPAT( K/LABFRQ + LINO ), 100 ) K
  100                FORMAT('$$$$$$$''', I1, '''' )
                  ELSE
                     WRITE( DSHPAT( K/LABFRQ + LINO ), 110 ) K
  110                FORMAT('$$$$$$''', I2, '''' )
                  END IF
               END IF
            END DO

*  Create the solid line.  Cannot rely on NCAR initialisation.
            DO I = 1, MINPTS
               DSHPAT( SOLID )( I:I ) = ''''
            END DO

*  Smoother contours so have fewer annotations.
         ELSE
            MINPTS = 20

*  First define some patterns.
            DO K = 1, NCONT, LABFRQ
               IF ( K/LABFRQ .LT. NPATRN - 1 + LINO )  THEN
                  IF ( K .LT. 10 ) THEN

*  Format statements used to make apostrophes (pen up) clearer and
*  easier to support.
                     WRITE( DSHPAS( K/LABFRQ + LINO ), 120 ) K
  120                FORMAT('$$$$$$$$$$$$$$$$$''', I1, '''' )
                  ELSE
                     WRITE( DSHPAS( K/LABFRQ + LINO ), 130 ) K
  130                FORMAT('$$$$$$$$$$$$$$$$''', I2, '''' )
                  END IF
               END IF
            END DO

*  Create the solid line.  Cannot rely on NCAR initialisation.
            DO I = 1, MINPTS
               DSHPAS( SOLID )( I:I ) = ''''
            END DO

*  End of noisy-contours check
         END IF

*  Store dash patterns, creating an identifier for later reference.
         CALL AGSETI( 'DASH/SELECTOR.', SOLID )
         CALL AGSETI( 'DASH/LENGTH.', MINPTS )
         IF ( NOISY ) THEN

*  First the solid line followed by the noisy patterns.
            CALL AGSETC( AGDSHN( SOLID ), DSHPAT( SOLID ) )
            DO K = 1, NCONT, LABFRQ
               CALL AGSETC( AGDSHN( K/LABFRQ + LINO ),
     :                      DSHPAT( K/LABFRQ + LINO ) )
            END DO
         ELSE

*  Now the solid line followed by the noisy patterns.
            CALL AGSETC( AGDSHN( SOLID ), DSHPAS( SOLID ) )
            DO K = 1, NCONT, LABFRQ
               CALL AGSETC( AGDSHN( K/LABFRQ + LINO ),
     :                      DSHPAS( K/LABFRQ + LINO ) )
            END DO
         END IF
      ELSE

*  Inquire the current pen number.
         CALL SGS_IPEN( INPEN )

*  End of annotations-required check
      END IF

*  Compute the size of the sub-pixel array. Note this is a notional
*  array, since the value of its elements can be computed from the
*  corners of the cell when required.  Note there is some constraint to
*  prevent ridiculous cases.
      IF ( MAXRES ) THEN
         IF ( DX .GT. 1.E-6 ) THEN
            SBDIM1 = MIN( INT( 1.0 / DX ), MXSBPX )
         ELSE
            SBDIM1 = 0
         END IF
         IF ( DY .GT. 1.E-6 ) THEN
            SBDIM2 = MIN( INT( 1.0 / DY ), MXSBPX )
         ELSE
            SBDIM2 = 0
         END IF

*  Set up the flag which says whether or not sub-pixel contouring
*  should be performed.
         SUBPIX = SBDIM1 .GE. 2 .AND. SBDIM2 .GE. 2
      ELSE
         SUBPIX = .FALSE.
      END IF

*  Assume none of the contour levels exist within the data until proven
*  otherwise.
      DO  ICONT = 1, NCONT
         CNTUSD( ICONT ) = .FALSE.
      END DO

*  Scan through the image, dividing it into rectangular regions of
*  maximum size WKDIM1, WKDIM2.  For each region, derive the lower and
*  upper bounds on the two-dimensional ARRAY indices of the bottom-left
*  pixel of each cell which lies within the region.
      DO YMIN = YSTART + 1, YSTART + YSIZE, WKDIM2
         YMAX = MIN( YMIN + ( WKDIM2 - 1 ), DIM2 - 1 )

         DO XMIN = XSTART + 1, XSTART + XSIZE, WKDIM1
            XMAX = MIN( XMIN + ( WKDIM1 - 1 ), DIM1 - 1 )

*  Locate the contourable cells within this region of the image and
*  associate them together into linked lists.
            DIMPL( 1 ) = XMIN
            DIMPL( 2 ) = YMIN
            DIMPU( 1 ) = XMAX
            DIMPU( 2 ) = YMAX
            CALL LCCELL( DIM1, DIM2, ARRAY, DIMPL, DIMPU, WKDIM1,
     :                   WKDIM2, NCONT, CONT, SLIST, CLIST, LINK, FLAG,
     :                   STATUS )

*  Initialise the pen colour for the first contour level.
            CALL SGS_SPEN( 2 )
            IF ( .NOT. ANNOTA .AND. ( DSHTHR .AND.
     :           CLEVEL .LT. THRESH ) ) CALL SGS_SPEN( 5 )

*  Loop to plot contours within the current image region at each
*  contour level.
            DO ICONT = 1, NCONT
               CLEVEL = CONT( ICONT )

*  Initialise a FLAG value for all the cells in the "crowded cell"
*  list, to indicate that they have not yet been contoured at this
*  level.
               CELL = CLIST
               DO WHILE( CELL .NE. 0 )
                  FLAG( CELL ) = 0
                  CELL = LINK( CELL )
               END DO

*  Consider contour lines starting on simple and crowded cells in turn
*  (according to the value of CONTYP) and obtain an initial cell of the
*  appropriate type.
               DO CONTYP = 1, 2
                  IF ( CONTYP .EQ. 1 ) THEN
                     ICELL = SLIST( ICONT )
                  ELSE
                     ICELL = CLIST
                  END IF

*  Loop through all the cells which are linked to it, looking for one
*  in which to start following a contour.  Check that the FLAG value is
*  zero, indicating that the cell has not already been contoured.
                  DO WHILE ( ICELL .NE. 0 )
                     IF ( FLAG( ICELL ) .EQ. 0 ) THEN

*  Calculate the two-dimensional image ARRAY indices of the bottom-left
*  pixel in the cell.
                        Y = ( ICELL - 1 ) / WKDIM1
                        X = ( ICELL - 1 ) - ( Y * WKDIM1 )
                        Y = Y + YMIN
                        X = X + XMIN

*  Derive the 1-dimensional (vectorised) image ARRAY index of the
*  bottom-left pixel.
                        IPIX = DIM1 * ( Y - 1 ) + X


*  Extract the pixel values from the four corners of the cell.
                        PIXBL = ARRAY( IPIX )
                        PIXBR = ARRAY( IPIX + 1 )
                        PIXTR = ARRAY( IPIX + DIM1 + 1 )
                        PIXTL = ARRAY( IPIX + DIM1 )

*  Test each pixel value against the current contour level.
                        TESTBL = PIXBL .GE. CLEVEL
                        TESTBR = PIXBR .GE. CLEVEL
                        TESTTR = PIXTR .GE. CLEVEL
                        TESTTL = PIXTL .GE. CLEVEL

*  Determine which edges of the cell are crossed by the contour, by
*  intercomparing the results of the above tests for the pixels on
*  either end of each edge of the cell.
                        CROSSB = TESTBL .NEQV. TESTBR
                        CROSSR = TESTBR .NEQV. TESTTR
                        CROSST = TESTTR .NEQV. TESTTL
                        CROSSL = TESTTL .NEQV. TESTBL

*  Check that at least one cell edge is crossed by the contour.
                        IF ( CROSSB .OR. CROSSR .OR.
     :                       CROSST .OR. CROSSL ) THEN

*  There is a contour at this level.
                        CNTUSD( ICONT ) = .TRUE.

*  Initialise variables for following the contour line.

*  ...the current cell:
                        CELL = ICELL

*  ...the number of cell edges which the contour crosses:
                        NCROSS = 0

*  ...the edge through which the contour exited the current cell:
                        EXIT = 0
                        PREXIT = 0

*  ...the number of points stored awaiting to be plotted:
                        NPTS = 0

*  ...whether part of the contour line has been stored for plotting:
                        PLOT = .FALSE.

*  Loop to follow a contour line, flagging each cell on the line as it
*  is contoured to prevent it being considered again.  (Note that a
*  jump to statement 1 is used to terminate this loop.)
                        DO WHILE ( .TRUE. )
                           FLAG( CELL ) = - 1

*  If the contour crosses the bottom edge of the current cell, then
*  increment the count of edge intersections and remember this edge as
*  a potential contour exit point from the cell.
                           IF ( CROSSB ) THEN
                              NCROSS = NCROSS + 1
                              EXIT = BOTTOM
                              SIDES( NCROSS ) = BOTTOM

*  Calculate how far along the edge the intersection occurs and add the
*  intersection point to the end of the list of contour co-ordinates.
                              FRACT = ( CLEVEL - PIXBL ) /
     :                                ( PIXBR - PIXBL )
                              XCONT( NCROSS ) = REAL( X ) - 0.5 + FRACT
                              YCONT( NCROSS ) = REAL( Y ) - 0.5
                           END IF

*  Repeat the process of finding intersections for each edge of the
*  current cell.

*  ...the right hand edge:
                           IF ( CROSSR ) THEN
                              NCROSS = NCROSS + 1
                              EXIT = RIGHT
                              SIDES( NCROSS ) = RIGHT
                              FRACT = ( CLEVEL - PIXBR ) /
     :                                ( PIXTR - PIXBR )
                              XCONT( NCROSS ) = REAL( X ) + 0.5
                              YCONT( NCROSS ) = REAL( Y ) - 0.5 + FRACT
                           END IF

*   ...the top edge:
                           IF ( CROSST ) THEN
                              NCROSS = NCROSS + 1
                              EXIT = TOP
                              SIDES( NCROSS ) = TOP
                              FRACT = ( CLEVEL - PIXTL ) /
     :                                ( PIXTR - PIXTL )
                              XCONT( NCROSS ) = REAL( X ) - 0.5 + FRACT
                              YCONT( NCROSS ) = REAL( Y ) + 0.5
                           END IF

*   ...the left hand edge:
                           IF ( CROSSL ) THEN
                              NCROSS = NCROSS + 1
                              EXIT = LEFT
                              SIDES( NCROSS ) = LEFT
                              FRACT = ( CLEVEL - PIXBL ) /
     :                                ( PIXTL - PIXBL )
                              XCONT( NCROSS ) = REAL( X ) - 0.5
                              YCONT( NCROSS ) = REAL( Y ) - 0.5 + FRACT
                           END IF

*  If all four edges of the last cell were crossed by the contour, then
*  there will be two separate lines to plot within this cell.  The four
*  co-ordinates associated with this cell must be correctly paired so
*  that the lines do not cross. There are three possible pairing
*  combinations which leave the first co-ordinate (where the contour
*  first entered the cell) unchanged.  The correct pairing is achieved
*  by performing up to two co-ordinate interchanges.
                           IF ( NCROSS .EQ. 4 )THEN

*  ...if the contour initially entered the cell through the top edge
*  (and therefore exited the previous cell through the bottom edge),
*  then the two lines will cross; this is a consequence of the order in
*  which the cell edges are considered.  If this has happened, then
*  swap the appropriate co-ordinates and entrance sides.
                              IF ( PREXIT .EQ. BOTTOM ) THEN
                                 SWAP = XCONT( 3 )
                                 XCONT( 3 ) = XCONT( 2 )
                                 XCONT( 2 ) = SWAP
                                 SWAP = YCONT( 3 )
                                 YCONT( 3 ) =YCONT( 2 )
                                 YCONT( 2 ) = SWAP
                                 ISWAP = SIDES( 3 )
                                 SIDES( 3 ) = SIDES( 2 )
                                 SIDES( 2 ) = ISWAP
                              END IF

*  ...Check which pairs of co-ordinates are joined by comparing the
*  bottom-left and top-right corners with the value at the centre of
*  the cell (the mean of the four corners).  In other words determine
*  whether or not the contours pass between the top-right bottom-left
*  corners and the centre.  If they do then swap the appropriate
*  co-ordinates again.
                              CENTRE = ( PIXBL + PIXBR + PIXTL + PIXTR )
     :                                 * 0.25
                              IF ( ( PIXTR .LT. CENTRE .AND.
     :                               PIXBL .LT. CENTRE ) .OR.
     :                             ( PIXTR .GT. CENTRE .AND.
     :                               PIXBL .GT. CENTRE ) ) THEN
                                 SWAP = XCONT( 4 )
                                 XCONT( 4 ) = XCONT( 2 )
                                 XCONT( 2 ) = SWAP
                                 SWAP = YCONT( 4 )
                                 YCONT( 4 ) = YCONT( 2 )
                                 YCONT( 2 ) = SWAP
                                 ISWAP = SIDES( 4 )
                                 SIDES( 4 ) = SIDES( 2 )
                                 SIDES( 2 ) = ISWAP
                              END IF

*  End of "the contour crosses all four cell edges" condition.
                           END IF

*  Initialise the last-contour point plotted if none of the contour
*  line has been plotted.
                           IF ( .NOT. PLOT ) THEN
                              XLAST = XCONT( 1 )
                              YLAST = YCONT( 1 )
                              NPTS = NPTS + 1
                              XP( NPTS ) = XCONT( 1 )
                              YP( NPTS ) = YCONT( 1 )
                           END IF

*  Sub-pixel resolution required.
                           IF ( SUBPIX ) THEN

*  Calculate fractional position within the cell for the entry point.
                              IF ( SIDES( 1 ) .EQ. BOTTOM .OR.
     :                             SIDES( 1 ) .EQ. TOP ) THEN
                                 FRACT = XCONT( 1 ) - REAL( X ) + 0.5
                              ELSE
                                 FRACT = YCONT( 1 ) - REAL( Y ) + 0.5
                              END IF

*  Now compute the locus of the contour within the cell, using a grid
*  of SBDIM1 by SBDIM2 of interpolated sub pixels.
                              CALL CNTSBP( PIXBL, PIXBR, PIXTR, PIXTL,
     :                                     FRACT, CLEVEL, REAL( X )-0.5,
     :                                     REAL( Y ) - 0.5, SIDES( 1 ),
     :                                     DX, DY, SBDIM1, SBDIM2, NPTS,
     :                                     XP, YP, STATUS )

*  Update the last position.
                              XLAST = XP( NPTS )
                              YLAST = YP( NPTS )
                           END IF

*  Is the contour segment longer than the plotting resolution?  Note
*  that we also have to allow for the case where the cell is confused,
*  otherwise the polyline might attempt to output only one point, which
*  is not allowed.
                           IF ( ABS( XCONT( 2 ) - XLAST ) .GT. DX .OR.
     :                          ABS( YCONT( 2 ) - YLAST ) .GT. DY .OR.
     :                        ( NCROSS .EQ. 4 .AND. NPTS .EQ. 1 ) ) THEN

*  Store the contour line.
                              NPTS = NPTS + 1
                              XP( NPTS ) = XCONT( 2 )
                              YP( NPTS ) = YCONT( 2 )

*  Update the last position to be plotted.
                              XLAST = XCONT( 2 )
                              YLAST = YCONT( 2 )
                           END IF

*  If there are two line segments within the cell, then plot the second
*  one.  Contour-following must be broken at this point, since there
*  will be two exit points from the cell and it is not possible to
*  follow both.
                           IF ( NCROSS .EQ. 4 ) THEN

*  Store the initial point in the locus.
                              XPC( 1 ) = XCONT( 3 )
                              YPC( 1 ) = YCONT( 3 )
                              NPTSC = 1

*  Sub-pixel resolution IS required.
                              IF ( SUBPIX ) THEN

*  Calculate fractional position within the cell for the entry point.
                                 IF ( SIDES( 3 ) .EQ. BOTTOM .OR.
     :                                SIDES( 3 ) .EQ. TOP ) THEN
                                    FRACT = XCONT( 3 ) - REAL( X ) + 0.5
                                 ELSE
                                    FRACT = YCONT( 3 ) - REAL( Y ) + 0.5
                                 END IF

*  Now compute the locus of the contour within the cell, using a grid
*  of SBDIM1 by SBDIM2 of interpolated sub pixels.
                                 CALL CNTSBP( PIXBL, PIXBR, PIXTR,
     :                                        PIXTL, FRACT, CLEVEL,
     :                                        REAL( X ) - 0.5,
     :                                        REAL( Y ) - 0.5,
     :                                        SIDES( 3 ), DX, DY,
     :                                        SBDIM1, SBDIM2, NPTSC,
     :                                        XPC, YPC, STATUS )

*  Is the contour segment longer than the plotting resolution?
                                 IF ( ABS( XCONT( 4 ) - XPC( NPTSC ) )
     :                                .GT. DX .OR. ABS( YCONT( 2 ) -
     :                                YPC( NPTSC ) ) .GT. DY .OR.
     :                                NPTSC .EQ. 1 ) THEN

*  Store the contour line.
                                     NPTSC = NPTSC + 1
                                     XPC( NPTSC ) = XCONT( 4 )
                                     YPC( NPTSC ) = YCONT( 4 )
                                 END IF
                              ELSE

*  Locus is just the two points. Ignore check for segment length.
                                 XPC( 2 ) = XCONT( 4 )
                                 YPC( 2 ) = YCONT( 4 )
                                 NPTSC = 2
                              END IF

*  Plot the locus.
                              IF ( ANNOTA ) THEN

*  Only annotate contours every LABFRQth level.
                                 IF ( MOD( ICONT - 1, LABFRQ ) .EQ. 0 )
     :                                THEN
                                    LINTYP = ( ICONT - 1 ) / LABFRQ + 1
                                 ELSE

*  This is the last user line type and because it is undefined, the
*  line type is solid.
                                    LINTYP = SOLID
                                 END IF

*  Plot the stored contour and reset the number of co-ordinates
*  ...dashed line:
                                 CALL AGCURV( XPC, 1, YPC, 1, NPTSC,
     :                                        LINTYP )
                                 CALL PLOTIT( 0, 0, 2 )
                              ELSE

*  ...solid line:
                                 CALL GPL( NPTSC, XPC, YPC )

*  Check for an error.
                                 CALL GKS_GSTAT( STATUS )
                                 IF ( STATUS .NE. SAI__OK ) THEN
                                    CALL MSG_SETI( 'N', NPTSC )
                                    CALL MSG_SETI( 'X', X )
                                    CALL MSG_SETI( 'Y', Y )
                                    CALL MSG_SETI( 'I', ICONT )
                                    CALL ERR_REP( 'KPS1_CNTUR_GPLC',
     :                                'CTNTUR: Error plotting confused'/
     :                                /' cell ^X, ^Y at level ^I --- '/
     :                                /'^N points', STATUS )
                                 END IF
                              END IF

*  Cannot follow both contours so here we must exit the loop.
                              GO TO 1
                           END IF

*  Note that part of the contour line has been stored.
                           PLOT = .TRUE.
                           NCROSS = 1

*  Remember which edge of the cell the contour exited.
                           PREXIT = EXIT

*  If an exit was made through the bottom edge of the cell, then we
*  must move down.  Check that the current two-dimensional image ARRAY
*  indices allow this (i.e. we are not already at the boundary of the
*  image region currently being contoured).  If we are, then
*  contour-following must stop at this point, so exit the contouring
*  loop.
                           IF ( EXIT .EQ. BOTTOM ) THEN
                              IF ( Y .LE. YMIN ) GO TO 1

*  Find the new cell which the contour enters, and check that it is
*  free for contouring.  If not, then it may be a "bad" cell
*  (containing one or more "bad" pixels), or we may have joined up with
*  part of a contour line which has already been contoured.  In either
*  case, contour-following must stop at this point.
                              CELL = CELL - WKDIM1
                              IF ( FLAG( CELL ) .NE. 0 ) GO TO 1

*  Adjust the two-dimensional image indices of the bottom-left pixel
*  and the associated 1-dimensional (vectorised) ARRAY index to refer
*  to the bottom-left pixel of the new cell.
                               Y = Y - 1
                               IPIX = IPIX - DIM1

*  Copy the pixel values which are common to the old and new cells into
*  the appropriate variables for the new cell.  Obtain new pixel values
*  from the image ARRAY where needed.
                               PIXTL = PIXBL
                               PIXTR = PIXBR
                               PIXBL = ARRAY( IPIX )
                               PIXBR = ARRAY( IPIX + 1 )

*  Similarly, copy the old TEST values and derive new ones where
*  needed.
                              TESTTL = TESTBL
                              TESTTR = TESTBR
                              TESTBL = PIXBL .GE. CLEVEL
                              TESTBR = PIXBR .GE. CLEVEL

*  Test whether the contour crosses each edge of the new cell.  Since
*  it entered through the top edge, the appropriate CROSS value
*  (CROSST) is set .FALSE., so that intersections with that edge will
*  not be considered (the contour must leave the cell through a
*  different edge).
                              CROSSB = TESTBL .NEQV. TESTBR
                              CROSSR = TESTBR .NEQV. TESTTR
                              CROSST = .FALSE.
                              CROSSL = TESTTL .NEQV. TESTBL

*  There is a separate section of code to handle moves in the remaining
*  three directions.  Each move follows the same pattern.

*  ...move right:
                           ELSE IF ( EXIT .EQ. RIGHT ) THEN
                              IF ( X .GE. XMAX ) GO TO 1

                              CELL = CELL + 1
                              IF ( FLAG( CELL ) .NE. 0 ) GO TO 1

                              X = X + 1
                              IPIX = IPIX + 1

                              PIXBL = PIXBR
                              PIXTL = PIXTR
                              PIXBR = ARRAY( IPIX + 1 )
                              PIXTR = ARRAY( IPIX + DIM1 + 1 )

                              TESTBL = TESTBR
                              TESTTL = TESTTR
                              TESTBR = PIXBR .GE. CLEVEL
                              TESTTR = PIXTR .GE. CLEVEL

                              CROSSB = TESTBL .NEQV. TESTBR
                              CROSSR = TESTBR .NEQV. TESTTR
                              CROSST = TESTTR .NEQV. TESTTL
                              CROSSL = .FALSE.

*  ...move up:
                           ELSE IF ( EXIT .EQ. TOP ) THEN
                              IF ( Y .GE. YMAX ) GO TO 1

                              CELL = CELL + WKDIM1
                              IF ( FLAG( CELL ) .NE. 0 ) GO TO 1

                              Y = Y + 1
                              IPIX = IPIX + DIM1

                              PIXBR = PIXTR
                              PIXBL = PIXTL
                              PIXTR = ARRAY( IPIX + DIM1 + 1 )
                              PIXTL = ARRAY( IPIX + DIM1 )

                              TESTBR = TESTTR
                              TESTBL = TESTTL
                              TESTTR = PIXTR .GE. CLEVEL
                              TESTTL = PIXTL .GE. CLEVEL

                              CROSSB = .FALSE.
                              CROSSR = TESTBR .NEQV. TESTTR
                              CROSST = TESTTR .NEQV. TESTTL
                              CROSSL = TESTTL .NEQV. TESTBL

*  ...move left:
                           ELSE IF ( EXIT .EQ. LEFT ) THEN
                              IF ( X .LE. XMIN ) GO TO 1

                              CELL = CELL - 1
                              IF ( FLAG( CELL ) .NE. 0 ) GO TO 1

                              X = X - 1
                              IPIX = IPIX - 1

                              PIXTR = PIXTL
                              PIXBR = PIXBL
                              PIXBL = ARRAY( IPIX )
                              PIXTL = ARRAY( IPIX + DIM1 )

                              TESTTR = TESTTL
                              TESTBR = TESTBL
                              TESTBL = PIXBL .GE. CLEVEL
                              TESTTL = PIXTL .GE. CLEVEL

                              CROSSB = TESTBL .NEQV. TESTBR
                              CROSSR = .FALSE.
                              CROSST = TESTTR .NEQV. TESTTL
                              CROSSL = TESTTL .NEQV. TESTBL
                           END IF

*  End of "follow the contour" loop.
                        END DO
    1                   CONTINUE

                        IF ( NPTS .GT. 1 ) THEN
                           IF ( ANNOTA ) THEN

*  Only annotate contours every LABFRQth level.
                              IF ( MOD( ICONT - 1, LABFRQ ) .EQ. 0 )
     :                             THEN
                                 LINTYP = ( ICONT - 1 ) / LABFRQ + 1
                              ELSE

*  This is the last user line type and because it is undefined, the the
*  line type is solid.
                                 LINTYP = SOLID
                              END IF

*  Plot the stored contour and flush it.
                              CALL AGCURV( XP, 1, YP, 1, NPTS, LINTYP )
                              CALL PLOTIT( 0, 0, 2 )
                           ELSE

                              CALL GPL( NPTS, XP, YP )

*  Check for an error.
                              CALL GKS_GSTAT( STATUS )
                              IF ( STATUS .NE. SAI__OK ) THEN
                                 CALL MSG_SETI( 'N', NPTS )
                                 CALL MSG_SETI( 'X', X )
                                 CALL MSG_SETI( 'Y', Y )
                                 CALL MSG_SETI( 'I', ICONT )
                                 CALL ERR_REP( 'KPS1_CNTUR_GPLC',
     :                             'KPS1_CNTUR: Error plotting '/
     :                             /'cell ^X, ^Y at level ^I --- ^N '/
     :                             /'points', STATUS )
                              END IF
                           END IF

*  End of "points to plot" check.
                        END IF

*  Reset the number of co-ordinates for the next contour.
                        NPTS = 0

*  End of "at least one edge of the initial cell is crossed by the
*  contour" condition.
                        END IF

*  End of "the initial cell has not already been contoured" condition.
                     END IF

*  End of "consider all possible starting points for a contour" loop.
*  Find the next cell which may contain the current contour level.
                     ICELL = LINK( ICELL )
                  END DO

*  End of "consider simple & crowded cells" loop.
               END DO

*  Set pen if required.
               IF ( .NOT. ANNOTA. AND. PENROT .AND.
     :              CNTUSD( ICONT ) ) THEN
                  PENNO = MOD( ICONT, 3 ) + 2

*  Check whether next contour is below the threshold.  If so use the
*  higher pens.
                  IF ( DSHTHR .AND.
     :                 CONT( MIN( NCONT, ICONT+1 ) ) .LT. THRESH ) THEN
                     PENNO = PENNO + 3
                  END IF
                  CALL SGS_SPEN( PENNO )

*  No cycling but have to decide which of two pens to use depending on
*  the next contour level.
               ELSE IF ( DSHTHR .AND. CONT( MIN( NCONT, ICONT+1 ) ) .LT.
     :                   THRESH ) THEN
                  CALL SGS_SPEN( 5 )

               ELSE IF ( DSHTHR ) THEN
                  CALL SGS_SPEN( 2 )

               END IF

*  End of "contour each level" loop.
            END DO

*  Reset the pen to the default.
*            IF ( .NOT. ANNOTA .AND. PENROT ) CALL SGS_SPEN( 2 )

*  End of "scan through the image, dividing it into regions" loops.
         END DO
      END DO

*  Reset the pen to the input value.
      CALL SGS_SPEN( INPEN )

*  Exit routine.
      END
