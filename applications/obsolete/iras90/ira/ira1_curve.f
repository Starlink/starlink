      SUBROUTINE IRA1_CURVE( MAP, XLO, XHI, YLO, YHI, INK, TOL, MAXBRK,
     :                       OUT, BREAK, VBREAK, NBREAK, LENGTH,
     :                       STATUS )
*+
*  Name:
*     IRA1_CURVE

*  Purpose:
*     Draw a section of a curve.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA1_CURVE( MAP, XLO, XHI, YLO, YHI, INK, TOL, MAXBRK, OUT,
*                      BREAK, VBREAK, NBREAK, LENGTH, STATUS )

*  Description:
*     This routine uses the EXTERNAL routine specified by argument MAP
*     to define a curve in two dimensions. The routine specified by MAP
*     is called to map scalar values between zero and one to positions
*     along the curve. The scalar value corresponds to a "distance"
*     along the curve from some specified starting position. The
*     calling routine will normally use common blocks to pass
*     information about the start and end of the curve (etc) to the MAP
*     routine.
*
*     IRA1_CURVE tries to minimise the number of calls to MAP by
*     monitoring the curvature of the curve; nearly straight curves need
*     less points to define the curve than do highly curved curves.
*     IRA1_CURVE also deals with discontinuities along the curve, and
*     returns information about such breaks in the curve, together with
*     breaks caused by the curve going outside the plotting zone.
*
*     The algorithm first considers drawing the whole curve as a
*     straight line. This is done by dividing the curve into three
*     equal sections, and calling the MAP routine to find the (X,y)
*     coordinates of the two mid-points. If they are reasonably close
*     to the straight line through the two end points, then straight
*     lines are used to draw the three sections (other criteria are
*     imposed as well as closeness to a straight line, such as all four
*     points being valid, etc). The tolerance allowed between the mid
*     points and the straight line is determined by the TOL argument.
*     If straight lines cannot be used, then the algorithm next
*     considers splitting each of the three sections up in the same
*     way. This is done up to a maximum number of levels determined by
*     the TOL argument. If this fails to produce a set of sections
*     which can be joined with straight lines, then the curve is
*     considered to be discontinuous, and the algorithm puts a break in
*     the curve.
*
*     Plotting is done in the current SGS zone, using the current worl
*     coordinate system, and the current pen.

*  Arguments:
*   MAP = SUBROUTINE (Given)
*      The name of a subroutine to use to transform distances along the
*      curve into (X,Y) positions. Data can be passed between the
*      calling routine and the map routine using common blocks. The
*      subroutine must be declared EXTERNAL within the calling routine,
*      and must have the following specification:
*
*      SUBROUTINE <map>( N, DIST, X, Y, STATUS )
*
*      where
*
*         N = INTEGER (Given)
*            The size of the DIST, X and Y arrays.
*         DIST( N ) = REAL (Given)
*            A set of values between zero and one representing distances
*            along the curve.
*         X( N ) = REAL (Returned)
*            The X coordinates (within the current SGS world coordinate
*            system) of the points corresponding to the positions along
*            the curve specified by DIST. These may be set equal to the
*            bad value (VAL__BADR) if the corresponding distances do not
*            map to a valid position in world coordinates.
*         Y( N ) = REAL (Returned)
*            As for X but holding the Y coordinates.
*         STATUS = INTEGER (Given and Returned)
*            The global status.
*
*     XLO = REAL( Given)
*        The lower bound of the plotting window on the first axis of
*        the current world coordinate system.
*     XHI = REAL( Given)
*        The upper bound of the plotting window on the first axis of
*        the current world coordinate system.
*     YLO = REAL( Given)
*        The lower bound of the plotting window on the second axis of
*        the current world coordinate system.
*     YHI = REAL( Given)
*        The upper bound of the plotting window on the second axis of
*        the current world coordinate system.
*     INK = LOGICAL (Given)
*        If .false., then no curve is plotted by the routine but the
*        returned information is still produced. If .true. then the
*        curve is plotted using the current SGS pen.
*     TOL = INTEGER (Given)
*        A measure of the accuracy required for the curve. The value
*        should be between zero and ten. Values outside this range are
*        taken as being equal to the nearest end point of the range. A
*        value of zero gives maximum accuracy, at the cost of
*        increased plotting time. A value of ten gives poor accuracy
*        but is faster.
*     MAXBRK = INTEGER (Given)
*        The size of the arguments VBREAK and BREAK. The symbolic
*        value IRA__MXBRK can be used. An error is reported if the
*        supplied arrays are too small.
*     OUT = LOGICAL (Returned)
*        If .true. then the curve was completely outside the plotting
*        zone, and so no part of it could be plotted.
*     BREAK( 2, MAXBRK ) = REAL (Returned)
*        Contains the world coordinates at which each break in the
*        plotted curve occurred. These breaks may be caused by
*        discontinuities in the curve, or simply by the fact that the
*        curve went outside the plotting window. The start and end of
*        the curve are also considered to be "breaks" even when they
*        occur within the plotting window (unless they are coincident).
*        BREAK( 1, I ) holds the X world coordinate value at the I'th
*        break, and BREAK( 2, I ) holds the Y world coordinate value.
*     VBREAK( 2, MAXBRK ) = REAL (Returned)
*        Contains the unit vector (within the world coordinate system)
*        parallel to the tangent to the curve at each break. The sense
*        is such that the vector always points back along the plotted
*        section of the curve.
*     NBREAK = INTEGER (Returned)
*        The number of breaks for which information is returned in BREAK
*        and VBREAK.
*     LENGTH = REAL (Returned)
*        The plotted length of the curve (in world coordinates).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     The algorithm is most easily understood in the context of a
*     language in which a subroutine can recursively call it self:
*
*     -  Find the world coordinates of both end points of the curve.
*     -  Find the world coordinates of two points evenly spaced between
*        the two end points.
*     -  If( all four points are nearly co-linear ) then
*     -     draw straight lines connecting the four points.
*     -  else
*     -     Re-call the subroutine to draw the first of the three
*           sub-sections defined by the four points.
*     -     Re-call the subroutine to draw the second sub-section.
*     -     Re-call the subroutine to draw the third sub-section.
*     -  End if
*
*     Unfortuanely, Fortran does not allow a subroutine to call itself.
*     For this reason, a DO loop is used to implement the re-entries,
*     and the information needed by each level is stored in arrays
*     indexed by the number of times the subroutine has been
*     "re-entered" (called the "level"). The processing at each level
*     has up to four stages:
*
*     0) See if the current section of the curve can be plotted using
*     straight lines. If so, do it, and go up a level to plot the next
*     section of the curve.
*
*     1) If the current section could not be plotted using straight
*     lines, set up the data required by the next level down to plot the
*     first of the three sub-sections of the current section, and then
*     re-enter the algorithm at the next level down.
*
*     2) Do the same for the second of the three sub-sections of the
*     current section.
*
*     3) Do the same for the third of the three sub-sections of the
*     current section.
*
*     4) Once all three sub-sections have been drawn, go up a section to
*     plot the next section of the curve.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-MAR-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'PRM_PAR'          ! Starlink data constants.

*  Arguments Given:
      EXTERNAL MAP
      REAL XLO
      REAL XHI
      REAL YLO
      REAL YHI
      LOGICAL INK
      INTEGER TOL
      INTEGER MAXBRK

*  Arguments Returned:
      LOGICAL OUT
      REAL BREAK( 2, MAXBRK )
      REAL VBREAK( 2, MAXBRK )
      INTEGER NBREAK
      REAL LENGTH

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MAXLEV             ! Maximum no. of levels.
      PARAMETER ( MAXLEV = 14 )

*  Local Variables:
      LOGICAL ALLBAD      ! True if all four points are bad.
      INTEGER CURSTG      ! Stage being performed at current level.
      REAL    D0( MAXLEV )! Distance along curve at start of current
                          ! section.
      REAL    D1( MAXLEV )! Distance along curve one third of way into
                          ! current section.
      REAL    D2( MAXLEV )! Distance along curve two thirds of way
                          ! into current section.
      REAL    D3( MAXLEV )! Distance along curve at end of current
                          ! section.
      REAL    DD( 2 )     ! Offsets of two points on the curve.
      REAL    DELTA       ! Gap between intermediate points.
      REAL    DELTAL      ! Distance between two ends of the current
                          ! section, in world coordinates.
      REAL    DEVLIM      ! Max. allowed deviation from a
                          ! straight line, in world coordinates.
      INTEGER I           ! Loop count.
      INTEGER LEVEL       ! Current level.
      REAL    LNF         ! Plotted length fraction.
      INTEGER LTOL        ! Local copy of TOL, limited to [0,10].
      REAL    MAXDEV      ! Maximum of V1 and V2.
      INTEGER NLEVS       ! Maximum no. of levels to use.
      INTEGER NXTLEV      ! Level at which the next sub-section will be
                          ! drawn.
      REAL    OLDVEC( 2 ) ! Unit direction vector of line previously
                          ! plotted.
      LOGICAL PLOT        ! True if a line is to be plotted.
      REAL    SHORT       ! Shortest length line considered non-zero.
      REAL    SQRTLM      ! Max _REAL value without danger of overflow.
      INTEGER STAGE( MAXLEV )! Stage reached at each level.
      LOGICAL USEOLD      ! True if previous line drawn joins up with
                          ! the line currently being considered.
      REAL    VEC( 2 )    ! Unit direction vector of line currently
                          ! being considered.
      REAL    VMOD        ! Vector modulus.
      REAL    XX( 2 )     ! X image coordinates from
                          ! transformation.
      REAL    X0( MAXLEV )! X coord. at start of current section.
      REAL    X1( MAXLEV )! X coord. 1/3 of the way into the current
                          ! section.
      REAL    X2( MAXLEV )! X coord. 2/3 of the way into the current
                          ! section.
      REAL    X3( MAXLEV )! X coord. at end of current section.
      REAL    YY( 2 )     ! Y image coordinates from transformation.
      REAL    Y0( MAXLEV )! Y coord. at start of current section.
      REAL    Y1( MAXLEV )! Y coord. 1/3 of the way into the current
                          ! section.
      REAL    Y2( MAXLEV )! Y coord. 2/3 of the way into the current
                          ! section.
      REAL    Y3( MAXLEV )! Y coord. at end of current section.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Store the largest number which can safely be squared without causing
*  a floating overflow.
      SQRTLM = 0.9*SQRT( VAL__MAXR )

*  Store the maximum number of levels to use, dependant on the value of
*  TOL.
      LTOL = MIN( 10, MAX( 0, TOL ) )
      NLEVS = MAXLEV - LTOL

*  Store the maximum deviation from a straight line allowed for a
*  section of the curve to be plotted as a straight line, dependant on
*  TOL.
      DEVLIM = REAL( LTOL**3 + 1 )*MIN( XHI - XLO, YHI - YLO )/7500.0

*  Store the minimum plottable line length.
      SHORT = 1.0E-3*DEVLIM

*  Initialise a flag to indicate that none of the curve falls within
*  the SGS zone.
      OUT = .TRUE.

*  Initialise the plotted length of the curve to zero.
      LENGTH = 0.0

*  Initialise a flag to indicate that there is no usable direction
*  vector for the previously plotted line.
      USEOLD = .FALSE.

*  Map the start and end positions into world coordinates.
      DD( 1 ) = 0.0
      DD( 2 ) = 1.0
      CALL  MAP( 2, DD, XX, YY, STATUS )

*  Initialise values for level 1.
      LEVEL = 1
      STAGE( LEVEL ) = 0
      D0( LEVEL ) = 0.0
      D3( LEVEL ) = 1.0
      X0( LEVEL ) = XX( 1 )
      Y0( LEVEL ) = YY( 1 )
      X3( LEVEL ) = XX( 2 )
      Y3( LEVEL ) = YY( 2 )

*  Loop round until level zero is reached.
      DO WHILE( LEVEL .GT. 0 )

*  If an error has been reported, abort.
         IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Save the stage reached at the current level.
         CURSTG = STAGE( LEVEL )

*  If nothing has been done yet at this level...
         IF( CURSTG .EQ. 0 ) THEN

*  Find the offsets along the curve of two points (points 1 and 2 )
*  evenly spaced between points 0 and 3 (supplied by the previous
*  level).
            DELTA = ( D3( LEVEL ) - D0( LEVEL ) )/3.0
            D1( LEVEL ) = D0( LEVEL ) + DELTA
            D2( LEVEL ) = D0( LEVEL ) + 2.0*DELTA

*  Find the corresponding world coordinates.
            DD( 1 ) = D1( LEVEL )
            DD( 2 ) = D2( LEVEL )

            CALL  MAP( 2, DD, XX, YY, STATUS )

            X1( LEVEL ) = XX( 1 )
            Y1( LEVEL ) = YY( 1 )
            X2( LEVEL ) = XX( 2 )
            Y2( LEVEL ) = YY( 2 )

*  See if this section of the curve can be plotted as a straight line.
*  If it can, VEC is returned holding the unit vector along the straight
*  line.
            CALL IRA1_PLST( X0( LEVEL ), Y0( LEVEL ), X1( LEVEL ),
     :                      Y1( LEVEL ), X2( LEVEL ), Y2( LEVEL ),
     :                      X3( LEVEL ), Y3( LEVEL ), SQRTLM, SHORT,
     :                      DEVLIM, USEOLD, OLDVEC, PLOT, ALLBAD, VEC,
     :                      DELTAL, MAXDEV, STATUS )

*  Do any plotting of straight lines of non-zero length.
            IF( PLOT .AND. STATUS .EQ. SAI__OK ) THEN
               IF( DELTAL .GT. SHORT ) THEN

*  If the deviation of the current section from a straight line is very
*  low, use a single line through the two end points.
                  IF( MAXDEV .LT. 0.1*DEVLIM ) THEN

                     CALL IRA1_PLOT( INK, XLO, YLO, XHI, YHI,
     :                               X0( LEVEL ), Y0( LEVEL ),
     :                               X3( LEVEL ), Y3( LEVEL ), MAXBRK,
     :                               OUT, BREAK, VBREAK, NBREAK, LNF,
     :                               STATUS )

*  If the deviation of the current section from a straight line is not
*  quite so low, use three lines.
                  ELSE

                     CALL IRA1_PLOT( INK, XLO, YLO, XHI, YHI,
     :                               X0( LEVEL ), Y0( LEVEL ),
     :                               X1( LEVEL ), Y1( LEVEL ), MAXBRK,
     :                               OUT, BREAK, VBREAK, NBREAK, LNF,
     :                               STATUS )

                     CALL IRA1_PLOT( INK, XLO, YLO, XHI, YHI,
     :                               X1( LEVEL ), Y1( LEVEL ),
     :                               X2( LEVEL ), Y2( LEVEL ), MAXBRK,
     :                               OUT, BREAK, VBREAK, NBREAK, LNF,
     :                               STATUS )

                     CALL IRA1_PLOT( INK, XLO, YLO, XHI, YHI,
     :                               X2( LEVEL ), Y2( LEVEL ),
     :                               X3( LEVEL ), Y3( LEVEL ), MAXBRK,
     :                               OUT, BREAK, VBREAK, NBREAK, LNF,
     :                               STATUS )

                  END IF

*  Save the unit direction vector of the plotted line, and indicate that
*  it can be used to estimate the angle between the current line and the
*  next line to be plotted.
                  OLDVEC( 1 ) = VEC( 1 )
                  OLDVEC( 2 ) = VEC( 2 )
                  USEOLD = .TRUE.

*  Increment the total plotted length.
                  LENGTH = LENGTH + DELTAL*LNF

               END IF

*  Now that the current section has been drawn, go up a level to draw
*  the next section.
               LEVEL = LEVEL - 1

*  If a straight line could not be used to approximate the current
*  section of the curve...
            ELSE

*  If all four points in the current section are bad, there are probably
*  no good points anywhere in the section. Abondon this section and go
*  on to the next.
               IF( ALLBAD ) THEN
                  LEVEL = LEVEL - 1
                  USEOLD = .FALSE.

*  If the maximum number of levels has been used, the curve is probably
*  discontinuous. In this case abandon the attempt to draw the current
*  section of the curve, and go up a level to try the next section.
               ELSE IF( LEVEL .EQ. NLEVS ) THEN
                  LEVEL = LEVEL - 1
                  USEOLD = .FALSE.

*  If more levels remain to be used, indicate that the next stage is
*  to be entered at the current level.
               ELSE
                  STAGE( LEVEL ) = 1

               END IF

            END IF

*  If the section bounded by points 0 and 3 was not straight enough
*  to be approximated by a straight line, plot each of the three
*  sub-section formed by the two intermediate positions.
         ELSE

*  Once all three sub-sections have been drawn, go up a level to draw
*  the next section.
            IF( CURSTG .GT. 3 ) THEN
               LEVEL = LEVEL - 1

*  If any sub-sections remain to be drawn, store information for the
*  next lower level.
            ELSE
               NXTLEV = LEVEL + 1

*  Indicate that the next level is at stage 0.
               STAGE( NXTLEV ) = 0

*  Stage 1 is to plot the section between points 0 and 1. Therefore
*  point 0 for the current level becomes point 0 for the next level, and
*  point 1 for the current level becomes point 3 for the next level.
               IF( CURSTG .EQ. 1 ) THEN
                  D0( NXTLEV ) = D0( LEVEL )
                  D3( NXTLEV ) = D1( LEVEL )
                  X0( NXTLEV ) = X0( LEVEL )
                  Y0( NXTLEV ) = Y0( LEVEL )
                  X3( NXTLEV ) = X1( LEVEL )
                  Y3( NXTLEV ) = Y1( LEVEL )

*  Stage 2 is to plot the section between points 1 and 2. Therefore
*  point 1 for the current level becomes point 0 for the next level, and
*  point 2 for the current level becomes point 3 for the next level.
               ELSE IF( CURSTG .EQ. 2 ) THEN
                  D0( NXTLEV ) = D1( LEVEL )
                  D3( NXTLEV ) = D2( LEVEL )
                  X0( NXTLEV ) = X1( LEVEL )
                  Y0( NXTLEV ) = Y1( LEVEL )
                  X3( NXTLEV ) = X2( LEVEL )
                  Y3( NXTLEV ) = Y2( LEVEL )

*  Stage 3 is to plot the section between points 2 and 3. Therefore
*  point 2 for the current level becomes point 0 for the next level, and
*  point 3 for the current level becomes point 3 for the next level.
               ELSE
                  D0( NXTLEV ) = D2( LEVEL )
                  D3( NXTLEV ) = D3( LEVEL )
                  X0( NXTLEV ) = X2( LEVEL )
                  Y0( NXTLEV ) = Y2( LEVEL )
                  X3( NXTLEV ) = X3( LEVEL )
                  Y3( NXTLEV ) = Y3( LEVEL )

               END IF

*  Indicate that the current level is ready for the next stage.
               STAGE( LEVEL ) = CURSTG + 1

*  Move on to the new level.
               LEVEL = NXTLEV

            END IF

         END IF

*  Go round to draw the next section (or sub-section) of the curve.

      END DO

*  If the supplied start position is within the plotting space...
      IF( X0( 1 ) .GE. XLO .AND. X0( 1 ) .LE. XHI .AND.
     :    Y0( 1 ) .GE. YLO .AND. Y0( 1 ) .LE. YHI ) THEN

*  ...and the start and end are nearly coincident...
         IF( ABS( X0( 1 ) - X3( 1 ) ) .LE. DEVLIM .AND.
     :       ABS( Y0( 1 ) - Y3( 1 ) ) .LE. DEVLIM ) THEN

*  ...and the plotted line is of significant length...
            IF( LENGTH .GT. 2.0*DEVLIM ) THEN

*  ...The curve has looped back on itself. Remove the starting and
*  ending positions from the list of breaks.
               NBREAK = NBREAK - 2

               DO I = 1, NBREAK
                  BREAK( 1, I ) = BREAK( 1, I + 1 )
                  BREAK( 2, I ) = BREAK( 2, I + 1 )
                  VBREAK( 1, I ) = VBREAK( 1, I + 1 )
                  VBREAK( 2, I ) = VBREAK( 2, I + 1 )
               END DO

            END IF

         END IF

      END IF

*  Ensure that all break vectors are normalised.
      DO I = 1, NBREAK
         IF( VBREAK( 1, I ) .NE. VAL__BADR ) THEN
            VMOD = SQRT( VBREAK( 1, I )**2 + VBREAK( 2, I )**2 )
            IF( VMOD .GT. 0.0 ) THEN
               VBREAK( 1, I ) = VBREAK( 1, I )/VMOD
               VBREAK( 2, I ) = VBREAK( 2, I )/VMOD
            ELSE
               VBREAK( 1, I ) = VAL__BADR
               VBREAK( 2, I ) = VAL__BADR
            END IF
         END IF
      END DO

*  If no part of the curve could be drawn, set the number of breaks to
*  zero.
 999  CONTINUE
      IF( OUT ) NBREAK = 0

      END
