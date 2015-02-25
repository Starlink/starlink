*+  LINPLT - Draws a line plot, connecting a series x-y values, with
*            labelled and annotated axes

      SUBROUTINE LINPLT( XDAT, YDAT, NELM, XLIMIT, YLIMIT, XLOW, YLOW,
     :                   XHIGH, YHIGH, TITLE, XLAB, YLAB, MINTIC,
     :                   MAJTIC, XLOG, YLOG, OUTTIC, THICK, STATUS )
*
*    Description :
*
*     This subroutine uses NCAR and SNX to draw annotated and labelled
*     axes for a 'line plot' (a continuous line connecting a series of
*     x and y values) within the current SGS zone.
*
*     NCAR has unfortunate defaults for many of its plotting parameters.
*     These have been changed to obtain a clearer plot on a variety of
*     devices. Also one changes (a) was made for the special
*     requirements of a line plot.  The changes are:
*       a) start and end the axes at the start and end pixels rather
*          than at major tick marks;
*       b) increase the top margin to accommodate a larger typeface;
*       c) increase the width of the enumerations, labels and title;
*       d) reduce the maximum number of major tick marks to prevent
*          shrinkage; and
*       e) increase the size of axis tick marks.
*
*     There are other options controlled by input arguments:
*       f) control the extents of the x and y axes, rather than letting
*          NCAR decide;
*       g) plot the tick marks outside the grid (default is inside);
*       h) have either or both axes logarithmic;
*       i) plot with thick lines
*
*     Note before using the routine an SGS device must be opened, a call
*     to SNX_AGWV made and an SGS zone selected or created.
*
*    Invocation :
*
*     CALL LINPLT( XDAT, YDAT, NELM, XLIMIT, YLIMIT, XLOW, YLOW, XHIGH,
*    :             YHIGH, TITLE, XLAB, YLAB, MINTIC, MAJTIC, XLOG, YLOG,
*    :             OUTTIC, THICK, STATUS )
*
*    Arguments :
*
*     XDAT( NELM ) = REAL( READ )
*         The x data
*     YDAT( NELM ) = REAL( READ )
*         The y data
*     NELM = INTEGER( READ )
*         The number of x,y data pairs
*     XLIMIT = LOGICAL( READ )
*         If true the specified x-axis limits (%XLOW and %XHIGH) are
*         used, otherwise they are ignored and AUTOGRAPH computes the x
*         limits.
*     YLIMIT = LOGICAL( READ )
*         If true the specified y-axis limits (%YLOW and %YHIGH) are
*         used, otherwise they are ignored and AUTOGRAPH computes the
*         y limits.
*     XLOW = REAL( READ )
*         Co-ordinate of start of x axis
*     YLOW = REAL( READ )
*         Co-ordinate of start of y axis
*     XHIGH = REAL( READ )
*         Co-ordinate of end of x axis
*     YHIGH = REAL( READ )
*         Co-ordinate of end of y axis
*     TITLE = CHAR( READ )
*         Title for the plot
*     XLAB = CHAR( READ )
*         Label for the x axis
*     YLAB = CHAR( READ )
*         Label for the y axis
*     XLOG = LOGICAL( READ )
*         If true the x axis is logarithmic
*     YLOG = LOGICAL( READ )
*         If true the y axis is logarithmic
*     MINTIC( 2 ) = REAL( READ )
*         The number of minor tick marks between each major tick mark
*           for the x and y axes.  A negative value forces the graphics
*           package to compute appropriate values.
*     MAJTIC( 2 ) = REAL( READ )
*         The parameter controlling the numbers of major tick marks
*           for the x and y axes.  (Number used is between MAJTIC+2 and
*           5*MAJTIC/2+4.) A negative value forces the graphics package
*           to compute appropriate values.
*     OUTTIC = LOGICAL( READ )
*         If true the axis tick marks are drawn outside the box
*     THICK = REAL( READ )
*         The line thickness in units of the default
*     STATUS = INTEGER( UPDATE )
*         This is the global status, if this variable has an error
*           value on entry then an immediate return will occur.
*
*    Method :
*
*     If no error on entry then return
*     Store input values of all AUTOGRAPH parameters to be changed.
*     Reset AUTOGRAPH parameters as required
*     Draw labelled and enumerated axes
*     Flush AUTOGRAPH buffers
*     Restore altered AUTOGRAPH parameters to their values on input to this
*       routine
*     Return
*
*    Authors :
*
*     Malcolm J. Currie  STARLINK ( RAL::CUR )
*
*    History :
*
*     1988 Oct 25 : Original based on CNTAXS ( RAL::CUR )
*     1991 Jun 21 : Allow for reversed axes. (RAL::CUR).
*
*    Type Definitions :

      IMPLICIT NONE            ! no default typing allowed

*    Global constants :

      INCLUDE 'SAE_PAR'        ! Global SSE definitions

*    Import :

      INTEGER
     :  NELM

      CHARACTER*(*)
     :  TITLE,
     :  XLAB,
     :  YLAB

      REAL
     :  XLOW,
     :  XHIGH,
     :  YLOW,
     :  YHIGH,
     :  XDAT( NELM ),
     :  YDAT( NELM ),
     :  THICK,
     :  MINTIC( 2 ),
     :  MAJTIC( 2 )

      LOGICAL
     :  XLIMIT,
     :  YLIMIT,
     :  OUTTIC,
     :  XLOG,
     :  YLOG

*    Status :

      INTEGER STATUS

*    Local variables :

      REAL
                               ! storage for input AUTOGRAPH parameter:
     :  XORDER,                ! x-axis polarity
     :  YORDER,                ! y-axis polarity
     :  GRID,                  ! fractional height of top edge of grid
     :  MAJCOU( 4 ),           ! "number" of major tick marks
     :  MINCOU( 4 ),           ! "number" of minor tick marks
     :  NICE( 2 ),             ! nice axes
     :  OMITIC( 4 ),           ! outward-minor-tick-mark sizes
     :  OMATIC( 4 ),           ! outward-major-tick-mark sizes
     :  IMITIC( 4 ),           ! inward-minor-tick-mark sizes
     :  IMATIC( 4 ),           ! inward-major-tick-mark sizes
     :  LIMITS( 4 ),           ! x-y axis maxima and minima
     :  NLOFF( 2 ),            ! bottom and left numeric-label offsets
     :  NLW( 2 ),              ! bottom and left numeric-label widths
     :  XLILO,                 ! x-axis type
     :  YLILO,                 ! y-axis type
     :  LABW( 3 )              ! bottom, left and top label widths

      LOGICAL                  ! True if:
     :  XNEG,                  ! X axis is inverted
     :  YNEG                   ! Y axis is inverted

      INTEGER
     :  LINWDT                 ! NCAR default line width

*-
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Plot graph with AUTOGRAPH

      CALL SNX_AGCS

*    Store current nice values

      CALL AGGETF( 'X/NICE.', NICE(1) )
      CALL AGGETF( 'Y/NICE.', NICE(2) )

*    Do not necessarily want major tick marks at vertices

      CALL AGSETF( 'X/NICE.', 0. )
      CALL AGSETF( 'Y/NICE.', 0. )

*    Determine wether or not either axis is inverted.

      XNEG = XHIGH .LT. XLOW .AND. XLIMIT .AND. .NOT. XLOG
      YNEG = YHIGH .LT. YLOW .AND. YLIMIT .AND. .NOT. YLOG

*    Record and modify the x axis polarity.  1.0 means it increases from
*    right to left.

      IF ( XNEG ) THEN
         CALL AGGETF( 'X/ORDER.', XORDER )
         CALL AGSETF( 'X/ORDER.', 1.0 )
      END IF

*    Record and modify the y axis polarity.  1.0 means it increases from
*    top to bottom.

      IF ( YNEG ) THEN
         CALL AGGETF( 'Y/ORDER.', YORDER )
         CALL AGSETF( 'Y/ORDER.', 1.0 )
      END IF

*    Store current top-grid value

      CALL AGGETF( 'GRID/TOP.', GRID )

*    Create a larger border at the top

      CALL AGSETF( 'GRID/TOP.', 0.91 )

*    Store axis types and set to logarithmic where required

      IF ( XLOG ) THEN
         CALL AGGETF( 'X/LOGARITHMIC.', XLILO )
         CALL AGSETF( 'X/LOGARITHMIC.', -1. )
      END IF

      IF ( YLOG ) THEN
         CALL AGGETF( 'Y/LOGARITHMIC.', YLILO )
         CALL AGSETF( 'Y/LOGARITHMIC.', -1. )
      END IF

      IF ( XLIMIT ) THEN

*       Store current extrema.

         CALL AGGETF( 'X/MINIMUM.', LIMITS(1) )
         CALL AGGETF( 'X/MAXIMUM.', LIMITS(2) )

*       Set axis limits because a dummy point is to be plotted.  Allow
*       for inverted axes.

         CALL AGSETF( 'X/MINIMUM.', MIN( XLOW, XHIGH ) )
         CALL AGSETF( 'X/MAXIMUM.', MAX( XLOW, XHIGH ) )
      END IF

      IF ( YLIMIT ) THEN
         CALL AGGETF( 'Y/MINIMUM.', LIMITS(3) )
         CALL AGGETF( 'Y/MAXIMUM.', LIMITS(4) )

*       Set axis limits because a dummy point is to be plotted.  Allow
*       for inverted axes.

         CALL AGSETF( 'Y/MINIMUM.', MIN( YLOW, YHIGH ) )
         CALL AGSETF( 'Y/MAXIMUM.', MAX( YLOW, YHIGH ) )
      END IF

*    Store current major-ticks parameters

      CALL AGGETF( 'AXIS/BOTTOM/TICKS/MAJOR/SPACING/COUNT.', MAJCOU(1) )
      CALL AGGETF( 'AXIS/LEFT/TICKS/MAJOR/SPACING/COUNT.', MAJCOU(2) )
      CALL AGGETF( 'AXIS/TOP/TICKS/MAJOR/SPACING/COUNT.', MAJCOU(3) )
      CALL AGGETF( 'AXIS/RIGHT/TICKS/MAJOR/SPACING/COUNT.', MAJCOU(4) )

*    Get requested number of major tick marks

      CALL AGSETF( 'AXIS/BOTTOM/TICKS/MAJOR/SPACING/COUNT.', MAJTIC(1) )
      CALL AGSETF( 'AXIS/LEFT/TICKS/MAJOR/SPACING/COUNT.', MAJTIC(2) )
      CALL AGSETF( 'AXIS/TOP/TICKS/MAJOR/SPACING/COUNT.', MAJTIC(1) )
      CALL AGSETF( 'AXIS/RIGHT/TICKS/MAJOR/SPACING/COUNT.', MAJTIC(2) )

*    Store current minor-ticks parameters

      CALL AGGETF( 'AXIS/BOTTOM/TICKS/MINOR/SPACING.', MINCOU(1) )
      CALL AGGETF( 'AXIS/TOP/TICKS/MINOR/SPACING.', MINCOU(3) )
      CALL AGGETF( 'AXIS/LEFT/TICKS/MINOR/SPACING.', MINCOU(2) )
      CALL AGGETF( 'AXIS/RIGHT/TICKS/MINOR/SPACING.', MINCOU(4) )

*    Set the requested number of minor tick marks per major tick. Note
*    logarithmic plots should have the fixed value of 8.
*    First the abscissa...

      IF ( MINTIC( 1 ) .GT. 0.999999 .AND. .NOT. XLOG ) THEN
         CALL AGSETF( 'AXIS/BOTTOM/TICKS/MINOR/SPACING.', MINTIC(1) )
         CALL AGSETF( 'AXIS/TOP/TICKS/MINOR/SPACING.', MINTIC(1) )
      ELSE IF ( XLOG ) THEN
         CALL AGSETF( 'AXIS/BOTTOM/TICKS/MINOR/SPACING.', 8. )
         CALL AGSETF( 'AXIS/TOP/TICKS/MINOR/SPACING.', 8. )
      END IF

*    now the ordinate

      IF ( MINTIC( 2 ) .GT. 0.999999 .AND. .NOT. YLOG ) THEN
         CALL AGSETF( 'AXIS/LEFT/TICKS/MINOR/SPACING.', MINTIC(2) )
         CALL AGSETF( 'AXIS/RIGHT/TICKS/MINOR/SPACING.', MINTIC(2) )
      ELSE IF ( YLOG ) THEN
         CALL AGSETF( 'AXIS/LEFT/TICKS/MINOR/SPACING.', 8. )
         CALL AGSETF( 'AXIS/RIGHT/TICKS/MINOR/SPACING.', 8. )
      END IF

      IF ( OUTTIC ) THEN

*       Store current outward tick mark sizes

         CALL AGGETF( 'AXIS/LEFT/TICKS/MINOR/LENGTH/OUTWARD.',
     :                OMITIC(2) )
         CALL AGGETF( 'AXIS/RIGHT/TICKS/MINOR/LENGTH/OUTWARD.',
     :                OMITIC(4) )
         CALL AGGETF( 'AXIS/TOP/TICKS/MINOR/LENGTH/OUTWARD.',
     :                OMITIC(3) )
         CALL AGGETF( 'AXIS/BOTTOM/TICKS/MINOR/LENGTH/OUTWARD.',
     :                OMITIC(1) )

         CALL AGGETF( 'AXIS/LEFT/TICKS/MAJOR/LENGTH/OUTWARD.',
     :                OMATIC(2) )
         CALL AGGETF( 'AXIS/RIGHT/TICKS/MAJOR/LENGTH/OUTWARD.',
     :                OMATIC(4) )
         CALL AGGETF( 'AXIS/TOP/TICKS/MAJOR/LENGTH/OUTWARD.',
     :                OMATIC(3) )
         CALL AGGETF( 'AXIS/BOTTOM/TICKS/MAJOR/LENGTH/OUTWARD.',
     :                OMATIC(1) )

*       Set outward-pointing axis tick marks so they do no intersect the
*       contours

         CALL AGSETF( 'AXIS/LEFT/TICKS/MINOR/LENGTH/OUTWARD.', 0.012 )
         CALL AGSETF( 'AXIS/RIGHT/TICKS/MINOR/LENGTH/OUTWARD.', 0.012 )
         CALL AGSETF( 'AXIS/TOP/TICKS/MINOR/LENGTH/OUTWARD.', 0.012 )
         CALL AGSETF( 'AXIS/BOTTOM/TICKS/MINOR/LENGTH/OUTWARD.', 0.012 )

         CALL AGSETF( 'AXIS/LEFT/TICKS/MAJOR/LENGTH/OUTWARD.', 0.02 )
         CALL AGSETF( 'AXIS/RIGHT/TICKS/MAJOR/LENGTH/OUTWARD.', 0.02 )
         CALL AGSETF( 'AXIS/TOP/TICKS/MAJOR/LENGTH/OUTWARD.', 0.02 )
         CALL AGSETF( 'AXIS/BOTTOM/TICKS/MAJOR/LENGTH/OUTWARD.', 0.02 )
      END IF

*    Store current inward tick mark sizes

      CALL AGGETF( 'AXIS/LEFT/TICKS/MINOR/LENGTH/INWARD.', IMITIC(2) )
      CALL AGGETF( 'AXIS/RIGHT/TICKS/MINOR/LENGTH/INWARD.', IMITIC(4) )
      CALL AGGETF( 'AXIS/TOP/TICKS/MINOR/LENGTH/INWARD.', IMITIC(3) )
      CALL AGGETF( 'AXIS/BOTTOM/TICKS/MINOR/LENGTH/INWARD.', IMITIC(1) )

      CALL AGGETF( 'AXIS/LEFT/TICKS/MAJOR/LENGTH/INWARD.', IMATIC(2) )
      CALL AGGETF( 'AXIS/RIGHT/TICKS/MAJOR/LENGTH/INWARD.', IMATIC(4) )
      CALL AGGETF( 'AXIS/TOP/TICKS/MAJOR/LENGTH/INWARD.', IMATIC(3) )
      CALL AGGETF( 'AXIS/BOTTOM/TICKS/MAJOR/LENGTH/INWARD.', IMATIC(1) )

      IF ( OUTTIC ) THEN

*       Remove the inward-pointing tick marks because only outward are
*       required

         CALL AGSETF( 'AXIS/LEFT/TICKS/MINOR/LENGTH/INWARD.', 0.0 )
         CALL AGSETF( 'AXIS/RIGHT/TICKS/MINOR/LENGTH/INWARD.', 0.0 )
         CALL AGSETF( 'AXIS/TOP/TICKS/MINOR/LENGTH/INWARD.', 0.0 )
         CALL AGSETF( 'AXIS/BOTTOM/TICKS/MINOR/LENGTH/INWARD.', 0.0 )

         CALL AGSETF( 'AXIS/LEFT/TICKS/MAJOR/LENGTH/INWARD.', 0.0 )
         CALL AGSETF( 'AXIS/RIGHT/TICKS/MAJOR/LENGTH/INWARD.', 0.0 )
         CALL AGSETF( 'AXIS/TOP/TICKS/MAJOR/LENGTH/INWARD.', 0.0 )
         CALL AGSETF( 'AXIS/BOTTOM/TICKS/MAJOR/LENGTH/INWARD.', 0.0 )
      ELSE

*       Lengthen the interior tick marks

         CALL AGSETF( 'AXIS/LEFT/TICKS/MINOR/LENGTH/INWARD.', 0.012 )
         CALL AGSETF( 'AXIS/RIGHT/TICKS/MINOR/LENGTH/INWARD.', 0.012 )
         CALL AGSETF( 'AXIS/TOP/TICKS/MINOR/LENGTH/INWARD.', 0.012 )
         CALL AGSETF( 'AXIS/BOTTOM/TICKS/MINOR/LENGTH/INWARD.', 0.012 )

         CALL AGSETF( 'AXIS/LEFT/TICKS/MAJOR/LENGTH/INWARD.', 0.02 )
         CALL AGSETF( 'AXIS/RIGHT/TICKS/MAJOR/LENGTH/INWARD.', 0.02 )
         CALL AGSETF( 'AXIS/TOP/TICKS/MAJOR/LENGTH/INWARD.', 0.02 )
         CALL AGSETF( 'AXIS/BOTTOM/TICKS/MAJOR/LENGTH/INWARD.', 0.02 )
      END IF

      IF ( OUTTIC ) THEN

*       Store numeric-label offsets

         CALL AGGETF( 'AXIS/LEFT/OFFSET.', NLOFF(2) )
         CALL AGGETF( 'AXIS/BOTTOM/OFFSET.', NLOFF(1) )

*       As a by-product, need to shift numeric labels outward

         CALL AGSETF( 'AXIS/LEFT/OFFSET.', 0.02 )
         CALL AGSETF( 'AXIS/BOTTOM/OFFSET.', 0.02 )
      END IF

*    Store the axis annotation sizes

      CALL AGGETF( 'AXIS/LEFT/NUMERIC/WIDTH.', NLW(2) )
      CALL AGGETF( 'AXIS/BOTTOM/NUMERIC/WIDTH.', NLW(1) )

*    Now the store the labels

      CALL AGSETC( 'LABEL/NAME.', 'L' )
      CALL AGSETI( 'LINE/NUMBER.', 100 )
      CALL AGGETF( 'LINE/CHARACTER.', LABW(2) )

      CALL AGSETC( 'LABEL/NAME.', 'B' )
      CALL AGSETI( 'LINE/NUMBER.', -100 )
      CALL AGGETF( 'LINE/CHARACTER.', LABW(1) )

      CALL AGSETC( 'LABEL/NAME.', 'T' )
      CALL AGSETI( 'LINE/NUMBER.', 100 )
      CALL AGGETF( 'LINE/CHARACTER.', LABW(3) )


*    Make the axis annotations larger
*    First the numbers

      CALL AGSETF( 'AXIS/LEFT/NUMERIC/WIDTH.', 0.025 )
      CALL AGSETF( 'AXIS/BOTTOM/NUMERIC/WIDTH.', 0.025 )

*    Now the labels

      CALL AGSETC( 'LABEL/NAME.', 'L' )
      CALL AGSETI( 'LINE/NUMBER.', 100 )
      CALL AGSETF( 'LINE/CHARACTER.', 0.03 )

      CALL AGSETC( 'LABEL/NAME.', 'B' )
      CALL AGSETI( 'LINE/NUMBER.', -100 )
      CALL AGSETF( 'LINE/CHARACTER.', 0.03 )

      CALL AGSETC( 'LABEL/NAME.', 'T' )
      CALL AGSETI( 'LINE/NUMBER.', 100 )
      CALL AGSETF( 'LINE/CHARACTER.', 0.036 )

*    Store and reset the line width

      IF ( THICK .GT. 0.000999 ) THEN
         CALL GETUSV( 'LW', LINWDT )
         CALL SETUSV( 'LW', NINT( THICK*1000 ) )
      END IF

*    Draw labelled axes.

      CALL SNX_EZRXY( XDAT, YDAT, NELM, XLAB, YLAB, TITLE )

*    Flush the plot.

      CALL PLOTIT( 0, 0, 2 )


*    Restore input parameters which have been altered

*    axis nice values

      CALL AGSETF( 'X/NICE.', NICE(1) )
      CALL AGSETF( 'Y/NICE.', NICE(2) )

*    x-axis polarity

      IF ( XNEG ) CALL AGSETF( 'X/ORDER.', XORDER )

*    y-axis polarity

      IF ( YNEG ) CALL AGSETF( 'Y/ORDER.', YORDER )

*    top-grid value

      CALL AGSETF( 'GRID/TOP.', GRID )

*    axis types

      IF ( XLOG ) CALL AGSETF( 'X/LOGARITHMIC.', XLILO )
      IF ( YLOG ) CALL AGSETF( 'Y/LOGARITHMIC.', YLILO )

      IF ( XLIMIT ) THEN

*       x extrema

         CALL AGSETF( 'X/MINIMUM.', LIMITS(1) )
         CALL AGSETF( 'X/MAXIMUM.', LIMITS(2) )
      END IF

      IF ( YLIMIT ) THEN

*       y extrema

         CALL AGSETF( 'Y/MINIMUM.', LIMITS(3) )
         CALL AGSETF( 'Y/MAXIMUM.', LIMITS(4) )
      END IF

*    major-ticks parameters

      CALL AGSETF( 'AXIS/BOTTOM/TICKS/MAJOR/SPACING/COUNT.', MAJCOU(1) )
      CALL AGSETF( 'AXIS/LEFT/TICKS/MAJOR/SPACING/COUNT.', MAJCOU(2) )
      CALL AGSETF( 'AXIS/TOP/TICKS/MAJOR/SPACING/COUNT.', MAJCOU(3) )
      CALL AGSETF( 'AXIS/RIGHT/TICKS/MAJOR/SPACING/COUNT.', MAJCOU(4) )

*    minor-ticks parameters

      CALL AGSETF( 'AXIS/BOTTOM/TICKS/MINOR/SPACING.', MINCOU(1) )
      CALL AGSETF( 'AXIS/LEFT/TICKS/MINOR/SPACING.', MINCOU(2) )
      CALL AGSETF( 'AXIS/TOP/TICKS/MINOR/SPACING.', MINCOU(3) )
      CALL AGSETF( 'AXIS/RIGHT/TICKS/MINOR/SPACING.', MINCOU(4) )

      IF ( OUTTIC ) THEN

*       outward tick mark sizes

         CALL AGSETF( 'AXIS/LEFT/TICKS/MINOR/LENGTH/OUTWARD.',
     :                OMITIC(2) )
         CALL AGSETF( 'AXIS/RIGHT/TICKS/MINOR/LENGTH/OUTWARD.',
     :                OMITIC(4) )
         CALL AGSETF( 'AXIS/TOP/TICKS/MINOR/LENGTH/OUTWARD.',
     :                OMITIC(3) )
         CALL AGSETF( 'AXIS/BOTTOM/TICKS/MINOR/LENGTH/OUTWARD.',
     :                OMITIC(1) )

         CALL AGSETF( 'AXIS/LEFT/TICKS/MAJOR/LENGTH/OUTWARD.',
     :                OMATIC(2) )
         CALL AGSETF( 'AXIS/RIGHT/TICKS/MAJOR/LENGTH/OUTWARD.',
     :                OMATIC(4) )
         CALL AGSETF( 'AXIS/TOP/TICKS/MAJOR/LENGTH/OUTWARD.',
     :                OMATIC(3) )
         CALL AGSETF( 'AXIS/BOTTOM/TICKS/MAJOR/LENGTH/OUTWARD.',
     :                OMATIC(1) )
      END IF

*    inward tick mark sizes

      CALL AGSETF( 'AXIS/LEFT/TICKS/MINOR/LENGTH/INWARD.', IMITIC(2) )
      CALL AGSETF( 'AXIS/RIGHT/TICKS/MINOR/LENGTH/INWARD.', IMITIC(4) )
      CALL AGSETF( 'AXIS/TOP/TICKS/MINOR/LENGTH/INWARD.', IMITIC(3) )
      CALL AGSETF( 'AXIS/BOTTOM/TICKS/MINOR/LENGTH/INWARD.', IMITIC(1) )

      CALL AGSETF( 'AXIS/LEFT/TICKS/MAJOR/LENGTH/INWARD.', IMATIC(2) )
      CALL AGSETF( 'AXIS/RIGHT/TICKS/MAJOR/LENGTH/INWARD.', IMATIC(4) )
      CALL AGSETF( 'AXIS/TOP/TICKS/MAJOR/LENGTH/INWARD.', IMATIC(3) )
      CALL AGSETF( 'AXIS/BOTTOM/TICKS/MAJOR/LENGTH/INWARD.', IMATIC(1) )

      IF ( OUTTIC ) THEN

*       numeric-label offsets

         CALL AGSETF( 'AXIS/LEFT/OFFSET.', NLOFF(2) )
         CALL AGSETF( 'AXIS/BOTTOM/OFFSET.', NLOFF(1) )
      END IF

*    axis annotation sizes

      CALL AGSETF( 'AXIS/LEFT/NUMERIC/WIDTH.', NLW(2) )
      CALL AGSETF( 'AXIS/BOTTOM/NUMERIC/WIDTH.', NLW(1) )

*    labels

      CALL AGSETC( 'LABEL/NAME.', 'L' )
      CALL AGSETI( 'LINE/NUMBER.', 100 )
      CALL AGSETF( 'LINE/CHARACTER.', LABW(2) )

      CALL AGSETC( 'LABEL/NAME.', 'B' )
      CALL AGSETI( 'LINE/NUMBER.', -100 )
      CALL AGSETF( 'LINE/CHARACTER.', LABW(1) )

      CALL AGSETC( 'LABEL/NAME.', 'T' )
      CALL AGSETI( 'LINE/NUMBER.', 100 )
      CALL AGSETF( 'LINE/CHARACTER.', LABW(3) )

*    Store and reset the line width

      IF ( THICK .GT. 0.000999 ) THEN
         CALL SETUSV( 'LW', LINWDT )
      END IF

      END
