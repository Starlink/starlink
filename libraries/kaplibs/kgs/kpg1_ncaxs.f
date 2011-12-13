      SUBROUTINE KPG1_NCAXS( XLOW, YLOW, XHIGH, YHIGH, GRID, TITLE,
     :                       ABSLAB, ORDLAB, XLOG, YLOG, MINTIC, MAJTIC,
     :                       OUTTIC, THICK, NICE, STATUS )
*+
*  Name:
*     KPG1_NCAXS

*  Purpose:
*     Draws NCAR annotated axes in the current SGS zone.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_NCAXS( XLOW, YLOW, XHIGH, YHIGH, GRID, TITLE, ABSLAB,
*                      ORDLAB, XLOG, YLOG, MINTIC, MAJTIC, OUTTIC,
*                      THICK, NICE, STATUS )

*  Description:
*     This subroutine uses NCAR and SNX to plot labelled and
*     annotated axes with a title within the current SGS zone.

*     NCAR has unfortunate defaults for many of its plotting parameters.
*     These have been changed to obtain a clearer plot on a variety of
*     devices. The changes are:
*       a) increase the width of the enumerations, labels and title;
*       b) disable the rotation and squashing of the axis ticks;
*       c) increase the size of axis tick marks; and
*       d) increase the maximum label length to 50 characters.

*     There are options controlled by input arguments:
*       e) specify the location of the grid (say to allow for larger
*          text size for the title;
*       f) plot the tick marks outside the grid so they do not cross
*          the graph (useful for contour plots);
*       g) start and end the axes at the start and end pixels rather
*          than at major tick marks (also useful for contour plots);
*       h) control the maximum number of major tick marks to prevent
*          shrinkage,
*       i) control the number of minor ticks per major tick;
*       j) have either or both axes logarithmic; and
*       k) plot with thick lines.

*     Also the terminator of character strings is a dollar.  This is
*     changed to a <NUL>.

*     Note before using the routine an SGS device must be opened, a
*     call to SNX_AGWV made and an SGS zone selected or created.  For
*     image displays and contour plots the zone will normally have the
*     aspect ratio of the array (of data values that are to be
*     presented) to get square pixels.
*

*  Arguments:
*     XLOW = REAL (Given)
*        Co-ordinate of start of x axis.
*     YLOW = REAL (Given)
*        Co-ordinate of start of y axis.
*     XHIGH = REAL (Given)
*        Co-ordinate of end of x axis.
*     YHIGH = REAL (Given)
*        Co-ordinate of end of y axis.
*     GRID( 4 ) = REAL (Given)
*        The grid location in the order left, right, bottom, top.  A
*        negative value gives the corresponding default: 0.15, 0.95,
*        0.15, 0.95 respectively.  All values should lie in the range
*        0.0 to 1.0 with left less than right and top greater than
*        bottom, however, in practice left and bottom should be in the
*        range 0.15 to 0.2, and top and right in the range 0.9 to 0.95.
*     TITLE = CHARARACTER * ( * ) (Given)
*        Title for the plot.
*     ABSLAB = CHARACTER * ( * ) (Given)
*        Label for the abscissa, in which NCAR fancy fonts may be
*        embedded.
*     ORDLAB = CHARACTER * ( * ) (Given)
*        Label for the ordinate, in which NCAR fancy fonts may be
*        embedded.
*     XLOG = LOGICAL (Given)
*         If .TRUE ., the x axis is logarithmic.
*     YLOG = LOGICAL (Given)
*         If .TRUE ., the y axis is logarithmic.
*     MINTIC( 2 ) = REAL (Given)
*        The number of minor tick marks between each major tick mark
*        for the x and y axes.  A negative value forces the graphics
*        package to compute appropriate values.
*     MAJTIC( 2 ) = REAL (Given)
*        The parameter controlling the numbers of major tick marks for
*        the x and y axes.  (Number used is between MAJTIC+2 and
*        5*MAJTIC/2+4.) A negative value forces the graphics package to
*        compute appropriate values.
*     OUTTIC = LOGICAL (Given)
*        If true the axis tick marks are drawn outside the box.
*     THICK = REAL (Given)
*        The line thickness in units of the default.
*     NICE = LOGICAL (Given)
*        If .TRUE., a major tick mark will appear at the ends of each
*        axis.
*     STATUS = INTEGER (Given & Returned)
*        This is the global status, if this variable has an error value
*        on entry then an immediate return will occur.

*  Algorithm:
*     If no error on entry then return
*     Store input values of all AUTOGRAPH parameters to be changed.
*     Reset AUTOGRAPH parameters as required
*     Draw labelled ( abslab, ordlab, title ) and enumerated axes
*     Flush AUTOGRAPH buffers
*     Restore altered AUTOGRAPH parameters to their values on input to
*       this routine
*     Return

*  Copyright:
*     Copyright (C) 1996 Central Laboratory of the Research Councils.
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
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1996 September 27 (MJC):
*        Original version based on NCRAXS and parts of LINPLT.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Global SSE definitions

*  Arguments Given:
      REAL XLOW
      REAL XHIGH
      REAL YLOW
      REAL YHIGH
      REAL GRID( 4 )
      CHARACTER * ( * ) TITLE
      CHARACTER * ( * ) ABSLAB
      CHARACTER * ( * ) ORDLAB
      LOGICAL XLOG
      LOGICAL YLOG
      REAL MINTIC( 2 )
      REAL MAJTIC( 2 )
      LOGICAL OUTTIC
      REAL THICK
      LOGICAL NICE

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! String length ignoring trailing
                                 ! blanks
*  Local Variables:
      REAL XD( 2 )               ! Dummy x co-ordinates
      LOGICAL XNEG               ! X axis is inverted?
      REAL YD( 2 )               ! Dummy y co-ordinates
      LOGICAL YNEG               ! Y axis is inverted?

                                 ! Storage for input AUTOGRAPH
                                 ! parameter in order of appearance:
      REAL OGRID( 4 )            ! Fraction of the frame zone in which the
                                 ! plot, i.e. grid bounds
      REAL XORDER                ! x-axis polarity
      REAL YORDER                ! y-axis polarity
      REAL XLILO                 ! x-axis type
      REAL YLILO                 ! y-axis type
      REAL CNTROL( 4 )           ! Input AUTOGRAPH grid parameters
      REAL MAJCOU( 4 )           ! "number" of major tick marks
      REAL MINCOU( 4 )           ! "number" of minor tick marks
      REAL NICEA( 2 )            ! Nice axes
      REAL OMITIC( 4 )           ! Outward-minor-tick-mark sizes
      REAL OMATIC( 4 )           ! Outward-major-tick-mark sizes
      REAL IMITIC( 4 )           ! Inward-minor-tick-mark sizes
      REAL IMATIC( 4 )           ! Inward-major-tick-mark sizes
      REAL LIMITS( 4 )           ! x,y axis maxima and minima
      REAL NLOFF( 2 )            ! Bottom and left numeric-label offsets
      REAL NLW( 2 )              ! Bottom and left numeric-label widths
      REAL LINMAX                ! Maximum number of characters plotted in
                                 ! a label
      REAL LABW( 3 )             ! Bottom, left and top label widths
      CHARACTER * ( 1 ) TERMST   ! Default string terminator
      INTEGER LINWDT             ! NCAR default line width

*.

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Store dummy data at location very unlikely to be needed for actual
*  data.  If the location of the axes is known, use the lower bounds.
      IF ( NICE ) THEN
         XD( 1 ) = XHIGH + 0.5 * ( XHIGH - XLOW )
         YD( 1 ) = YHIGH + 0.5 * ( YHIGH - YLOW )
      ELSE
         XD( 1 ) = XHIGH
         YD( 1 ) = YHIGH
      END IF

*  Store the current NCAR grid values.
      IF ( GRID( 1 ) .GE. 0.0 ) CALL AGGETF( 'GRID/LEFT.', OGRID( 1 ) )
      IF ( GRID( 2 ) .GE. 0.0 ) CALL AGGETF( 'GRID/RIGHT.', OGRID( 2 ) )
      IF ( GRID( 3 ) .GE. 0.0 ) CALL AGGETF( 'GRID/BOTTOM.', OGRID( 3 ))
      IF ( GRID( 4 ) .GE. 0.0 ) CALL AGGETF( 'GRID/TOP.', OGRID( 4 ) )

*  Set the current NCAR grid values.
      IF ( GRID( 1 ) .GE. 0.0 ) CALL AGSETF( 'GRID/LEFT.', GRID( 1 ) )
      IF ( GRID( 2 ) .GE. 0.0 ) CALL AGSETF( 'GRID/RIGHT.', GRID( 2 ) )
      IF ( GRID( 3 ) .GE. 0.0 ) CALL AGSETF( 'GRID/BOTTOM.', GRID( 3 ) )
      IF ( GRID( 4 ) .GE. 0.0 ) CALL AGSETF( 'GRID/TOP.', GRID( 4 ) )

*  Plot graph with AUTOGRAPH.
      CALL SNX_AGCS

*  Store current nice values
      IF ( .NOT. NICE ) THEN
         CALL AGGETF( 'X/NICE.', NICEA( 1 ) )
         CALL AGGETF( 'Y/NICE.', NICEA( 2 ) )

*  Do not necessarily want major tick marks at vertices.
         CALL AGSETF( 'X/NICE.', 0. )
         CALL AGSETF( 'Y/NICE.', 0. )
      END IF

*  Determine whether or not either axis is inverted.
      XNEG = XHIGH .LT. XLOW .AND. .NOT. XLOG
      YNEG = YHIGH .LT. YLOW .AND. .NOT. YLOG

*  Record and modify the x axis polarity.  1.0 means it increases from
*  right to left.
      IF ( XNEG ) THEN
         CALL AGGETF( 'X/ORDER.', XORDER )
         CALL AGSETF( 'X/ORDER.', 1.0 )
      END IF

*  Record and modify the y axis polarity.  1.0 means it increases from
*  top to bottom.
      IF ( YNEG ) THEN
         CALL AGGETF( 'Y/ORDER.', YORDER )
         CALL AGSETF( 'Y/ORDER.', 1.0 )
      END IF

*  Store axis types and set to logarithmic where required.
      IF ( XLOG ) THEN
         CALL AGGETF( 'X/LOGARITHMIC.', XLILO )
         CALL AGSETF( 'X/LOGARITHMIC.', -1. )
      END IF

      IF ( YLOG ) THEN
         CALL AGGETF( 'Y/LOGARITHMIC.', YLILO )
         CALL AGSETF( 'Y/LOGARITHMIC.', -1. )
      END IF

*  Store current axis control values.
      CALL AGGETF( 'AXIS/LEFT/CONTROL.', CNTROL(2) )
      CALL AGGETF( 'AXIS/BOTTOM/CONTROL.', CNTROL(1) )
      CALL AGGETF( 'AXIS/RIGHT/CONTROL.', CNTROL(4) )
      CALL AGGETF( 'AXIS/TOP/CONTROL.', CNTROL(3) )

*  Disable squashing and rotating of the ticks.
      CALL AGSETF( 'AXIS/BOTTOM/CONTROL.', 1. )
      CALL AGSETF( 'AXIS/TOP/CONTROL.', 1. )

*  Disable rotating of the ticks.
      CALL AGSETF( 'AXIS/LEFT/CONTROL.', 2. )
      CALL AGSETF( 'AXIS/RIGHT/CONTROL.', 2. )

*  Store current extrema
      CALL AGGETF( 'X/MINIMUM.', LIMITS(1) )
      CALL AGGETF( 'X/MAXIMUM.', LIMITS(2) )
      CALL AGGETF( 'Y/MINIMUM.', LIMITS(3) )
      CALL AGGETF( 'Y/MAXIMUM.', LIMITS(4) )

*  Set axis limits because a dummy point is to be plotted.  Allow for
*  inverted axes.
      CALL AGSETF( 'X/MINIMUM.', MIN( XLOW, XHIGH ) )
      CALL AGSETF( 'X/MAXIMUM.', MAX( XLOW, XHIGH ) )
      CALL AGSETF( 'Y/MINIMUM.', MIN( YLOW, YHIGH ) )
      CALL AGSETF( 'Y/MAXIMUM.', MAX( YLOW, YHIGH ) )

*  Store current major-ticks parameters
      CALL AGGETF( 'AXIS/BOTTOM/TICKS/MAJOR/SPACING/COUNT.', MAJCOU(1) )
      CALL AGGETF( 'AXIS/LEFT/TICKS/MAJOR/SPACING/COUNT.', MAJCOU(2) )
      CALL AGGETF( 'AXIS/TOP/TICKS/MAJOR/SPACING/COUNT.', MAJCOU(3) )
      CALL AGGETF( 'AXIS/RIGHT/TICKS/MAJOR/SPACING/COUNT.', MAJCOU(4) )

*  Get requested number of major tick marks
      CALL AGSETF( 'AXIS/BOTTOM/TICKS/MAJOR/SPACING/COUNT.', MAJTIC(1) )
      CALL AGSETF( 'AXIS/LEFT/TICKS/MAJOR/SPACING/COUNT.', MAJTIC(2) )
      CALL AGSETF( 'AXIS/TOP/TICKS/MAJOR/SPACING/COUNT.', MAJTIC(1) )
      CALL AGSETF( 'AXIS/RIGHT/TICKS/MAJOR/SPACING/COUNT.', MAJTIC(2) )

*  Store current minor-ticks parameters.
      CALL AGGETF( 'AXIS/BOTTOM/TICKS/MINOR/SPACING.', MINCOU(1) )
      CALL AGGETF( 'AXIS/TOP/TICKS/MINOR/SPACING.', MINCOU(3) )
      CALL AGGETF( 'AXIS/LEFT/TICKS/MINOR/SPACING.', MINCOU(2) )
      CALL AGGETF( 'AXIS/RIGHT/TICKS/MINOR/SPACING.', MINCOU(4) )

*  Set the requested number of minor tick marks per major tick.  Note
*  logarithmic plots should have the fixed value of 8.  First the
*  abscissa...
      IF ( MINTIC( 1 ) .GT. 0.999999 .AND. .NOT. XLOG ) THEN
         CALL AGSETF( 'AXIS/BOTTOM/TICKS/MINOR/SPACING.', MINTIC(1) )
         CALL AGSETF( 'AXIS/TOP/TICKS/MINOR/SPACING.', MINTIC(1) )
      ELSE IF ( XLOG ) THEN
         CALL AGSETF( 'AXIS/BOTTOM/TICKS/MINOR/SPACING.', 8. )
         CALL AGSETF( 'AXIS/TOP/TICKS/MINOR/SPACING.', 8. )
      END IF

*  now the ordinate.
      IF ( MINTIC( 2 ) .GT. 0.999999 .AND. .NOT. YLOG ) THEN
         CALL AGSETF( 'AXIS/LEFT/TICKS/MINOR/SPACING.', MINTIC(2) )
         CALL AGSETF( 'AXIS/RIGHT/TICKS/MINOR/SPACING.', MINTIC(2) )
      ELSE IF ( YLOG ) THEN
         CALL AGSETF( 'AXIS/LEFT/TICKS/MINOR/SPACING.', 8. )
         CALL AGSETF( 'AXIS/RIGHT/TICKS/MINOR/SPACING.', 8. )
      END IF

      IF ( OUTTIC ) THEN

*  Store current outward tick-mark sizes.
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

*  Set outward-pointing axis tick marks so they do no intersect the
*  plot.
         CALL AGSETF( 'AXIS/LEFT/TICKS/MINOR/LENGTH/OUTWARD.', 0.012 )
         CALL AGSETF( 'AXIS/RIGHT/TICKS/MINOR/LENGTH/OUTWARD.', 0.012 )
         CALL AGSETF( 'AXIS/TOP/TICKS/MINOR/LENGTH/OUTWARD.', 0.012 )
         CALL AGSETF( 'AXIS/BOTTOM/TICKS/MINOR/LENGTH/OUTWARD.', 0.012 )

         CALL AGSETF( 'AXIS/LEFT/TICKS/MAJOR/LENGTH/OUTWARD.', 0.02 )
         CALL AGSETF( 'AXIS/RIGHT/TICKS/MAJOR/LENGTH/OUTWARD.', 0.02 )
         CALL AGSETF( 'AXIS/TOP/TICKS/MAJOR/LENGTH/OUTWARD.', 0.02 )
         CALL AGSETF( 'AXIS/BOTTOM/TICKS/MAJOR/LENGTH/OUTWARD.', 0.02 )
      END IF

*  Store current inward tick-mark sizes.
      CALL AGGETF( 'AXIS/LEFT/TICKS/MINOR/LENGTH/INWARD.', IMITIC(2) )
      CALL AGGETF( 'AXIS/RIGHT/TICKS/MINOR/LENGTH/INWARD.', IMITIC(4) )
      CALL AGGETF( 'AXIS/TOP/TICKS/MINOR/LENGTH/INWARD.', IMITIC(3) )
      CALL AGGETF( 'AXIS/BOTTOM/TICKS/MINOR/LENGTH/INWARD.', IMITIC(1) )

      CALL AGGETF( 'AXIS/LEFT/TICKS/MAJOR/LENGTH/INWARD.', IMATIC(2) )
      CALL AGGETF( 'AXIS/RIGHT/TICKS/MAJOR/LENGTH/INWARD.', IMATIC(4) )
      CALL AGGETF( 'AXIS/TOP/TICKS/MAJOR/LENGTH/INWARD.', IMATIC(3) )
      CALL AGGETF( 'AXIS/BOTTOM/TICKS/MAJOR/LENGTH/INWARD.', IMATIC(1) )

      IF ( OUTTIC ) THEN

*  Remove the inward-pointing tick marks because only outward are
*  required.
         CALL AGSETF( 'AXIS/LEFT/TICKS/MINOR/LENGTH/INWARD.', 0.0 )
         CALL AGSETF( 'AXIS/RIGHT/TICKS/MINOR/LENGTH/INWARD.', 0.0 )
         CALL AGSETF( 'AXIS/TOP/TICKS/MINOR/LENGTH/INWARD.', 0.0 )
         CALL AGSETF( 'AXIS/BOTTOM/TICKS/MINOR/LENGTH/INWARD.', 0.0 )

         CALL AGSETF( 'AXIS/LEFT/TICKS/MAJOR/LENGTH/INWARD.', 0.0 )
         CALL AGSETF( 'AXIS/RIGHT/TICKS/MAJOR/LENGTH/INWARD.', 0.0 )
         CALL AGSETF( 'AXIS/TOP/TICKS/MAJOR/LENGTH/INWARD.', 0.0 )
         CALL AGSETF( 'AXIS/BOTTOM/TICKS/MAJOR/LENGTH/INWARD.', 0.0 )
      ELSE

*  Lengthen the interior tick marks.
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

*  Store numeric-label offsets.
         CALL AGGETF( 'AXIS/LEFT/OFFSET.', NLOFF(2) )
         CALL AGGETF( 'AXIS/BOTTOM/OFFSET.', NLOFF(1) )

*  As a by-product, there is a need to shift numeric labels outward.
         CALL AGSETF( 'AXIS/LEFT/OFFSET.', 0.02 )
         CALL AGSETF( 'AXIS/BOTTOM/OFFSET.', 0.02 )
      END IF

*  Store the axis annotation sizes
      CALL AGGETF( 'AXIS/LEFT/NUMERIC/WIDTH.', NLW(2) )
      CALL AGGETF( 'AXIS/BOTTOM/NUMERIC/WIDTH.', NLW(1) )

*  Store the maximum label length in characters.
      CALL AGGETF( 'LINE/MAXIMUM.', LINMAX )

*  Set maximum label length to 50 characters.
      CALL AGSETF( 'LINE/MAXIMUM.', 50.0 )

*  Store and reset the character terminator string.
      CALL AGGETC( 'LINE/END.', TERMST )
      CALL AGSETC( 'LINE/END.', CHAR( 0 ) )

*  Now the store the labels.
      CALL AGSETC( 'LABEL/NAME.', 'L' )
      CALL AGSETI( 'LINE/NUMBER.', 100 )
      CALL AGGETF( 'LINE/CHARACTER.', LABW(2) )

      CALL AGSETC( 'LABEL/NAME.', 'B' )
      CALL AGSETI( 'LINE/NUMBER.', -100 )
      CALL AGGETF( 'LINE/CHARACTER.', LABW(1) )

      CALL AGSETC( 'LABEL/NAME.', 'T' )
      CALL AGSETI( 'LINE/NUMBER.', 100 )
      CALL AGGETF( 'LINE/CHARACTER.', LABW(3) )


*  Make the axis annotations larger.
*
*  First the numbers,
      CALL AGSETF( 'AXIS/LEFT/NUMERIC/WIDTH.', 0.025 )
      CALL AGSETF( 'AXIS/BOTTOM/NUMERIC/WIDTH.', 0.025 )

*  and now the labels.
      CALL AGSETC( 'LABEL/NAME.', 'L' )
      CALL AGSETI( 'LINE/NUMBER.', 100 )
      CALL AGSETF( 'LINE/CHARACTER.', 0.03 )

      CALL AGSETC( 'LABEL/NAME.', 'B' )
      CALL AGSETI( 'LINE/NUMBER.', -100 )
      CALL AGSETF( 'LINE/CHARACTER.', 0.03 )

      CALL AGSETC( 'LABEL/NAME.', 'T' )
      CALL AGSETI( 'LINE/NUMBER.', 100 )
      CALL AGSETF( 'LINE/CHARACTER.', 0.036 )

*  Store and reset the line width.
      IF ( THICK .GT. 0.000999 ) THEN
         CALL GETUSV( 'LW', LINWDT )
         CALL SETUSV( 'LW', NINT( THICK*1000 ) )
      END IF

*  Draw labelled axes.
      CALL SNX_EZRXY( XD, YD, 1, ABSLAB( :CHR_LEN( ABSLAB ) ),
     :                ORDLAB( :CHR_LEN( ORDLAB ) ),
     :                TITLE( :CHR_LEN( TITLE ) ) )

*  Flush the NCAR buffers.
      CALL PLOTIT( 0, 0, 2 )

*  Restore input parameters which have been altered:
*  axis nice values,
      IF ( .NOT. NICE ) THEN
         CALL AGSETF( 'X/NICE.', NICEA( 1 ) )
         CALL AGSETF( 'Y/NICE.', NICEA( 2 ) )
      END IF

*  the current NCAR grid values,
      IF ( GRID( 1 ) .GE. 0.0 ) CALL AGSETF( 'GRID/LEFT.', OGRID( 1 ) )
      IF ( GRID( 2 ) .GE. 0.0 ) CALL AGSETF( 'GRID/RIGHT.', OGRID( 2 ) )
      IF ( GRID( 3 ) .GE. 0.0 ) CALL AGSETF( 'GRID/BOTTOM.', OGRID( 3 ))
      IF ( GRID( 4 ) .GE. 0.0 ) CALL AGSETF( 'GRID/TOP.', OGRID( 4 ) )

*  x-axis polarity,
      IF ( XNEG ) CALL AGSETF( 'X/ORDER.', XORDER )

*  y-axis polarity,
      IF ( YNEG ) CALL AGSETF( 'Y/ORDER.', YORDER )

*  axis-control values,
      CALL AGSETF( 'AXIS/LEFT/CONTROL.', CNTROL(2) )
      CALL AGSETF( 'AXIS/BOTTOM/CONTROL.', CNTROL(1) )
      CALL AGSETF( 'AXIS/RIGHT/CONTROL.', CNTROL(4) )
      CALL AGSETF( 'AXIS/TOP/CONTROL.', CNTROL(3) )

*  axis types,
      IF ( XLOG ) CALL AGSETF( 'X/LOGARITHMIC.', XLILO )
      IF ( YLOG ) CALL AGSETF( 'Y/LOGARITHMIC.', YLILO )

*  extrema,
      CALL AGSETF( 'X/MINIMUM.', LIMITS(1) )
      CALL AGSETF( 'X/MAXIMUM.', LIMITS(2) )
      CALL AGSETF( 'Y/MINIMUM.', LIMITS(3) )
      CALL AGSETF( 'Y/MAXIMUM.', LIMITS(4) )

*  major-ticks parameters,
      CALL AGSETF( 'AXIS/BOTTOM/TICKS/MAJOR/SPACING/COUNT.', MAJCOU(1) )
      CALL AGSETF( 'AXIS/LEFT/TICKS/MAJOR/SPACING/COUNT.', MAJCOU(2) )
      CALL AGSETF( 'AXIS/TOP/TICKS/MAJOR/SPACING/COUNT.', MAJCOU(3) )
      CALL AGSETF( 'AXIS/RIGHT/TICKS/MAJOR/SPACING/COUNT.', MAJCOU(4) )

*    minor-ticks parameters,
      CALL AGSETF( 'AXIS/BOTTOM/TICKS/MINOR/SPACING.', MINCOU(1) )
      CALL AGSETF( 'AXIS/LEFT/TICKS/MINOR/SPACING.', MINCOU(2) )
      CALL AGSETF( 'AXIS/TOP/TICKS/MINOR/SPACING.', MINCOU(3) )
      CALL AGSETF( 'AXIS/RIGHT/TICKS/MINOR/SPACING.', MINCOU(4) )

      IF ( OUTTIC ) THEN

*  outward tick-mark sizes,
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

*  inward tick-mark sizes,
      CALL AGSETF( 'AXIS/LEFT/TICKS/MINOR/LENGTH/INWARD.', IMITIC(2) )
      CALL AGSETF( 'AXIS/RIGHT/TICKS/MINOR/LENGTH/INWARD.', IMITIC(4) )
      CALL AGSETF( 'AXIS/TOP/TICKS/MINOR/LENGTH/INWARD.', IMITIC(3) )
      CALL AGSETF( 'AXIS/BOTTOM/TICKS/MINOR/LENGTH/INWARD.', IMITIC(1) )

      CALL AGSETF( 'AXIS/LEFT/TICKS/MAJOR/LENGTH/INWARD.', IMATIC(2) )
      CALL AGSETF( 'AXIS/RIGHT/TICKS/MAJOR/LENGTH/INWARD.', IMATIC(4) )
      CALL AGSETF( 'AXIS/TOP/TICKS/MAJOR/LENGTH/INWARD.', IMATIC(3) )
      CALL AGSETF( 'AXIS/BOTTOM/TICKS/MAJOR/LENGTH/INWARD.', IMATIC(1) )

      IF ( OUTTIC ) THEN

*  numeric-label offsets,
         CALL AGSETF( 'AXIS/LEFT/OFFSET.', NLOFF(2) )
         CALL AGSETF( 'AXIS/BOTTOM/OFFSET.', NLOFF(1) )
      END IF

*  axis annotation sizes,
      CALL AGSETF( 'AXIS/LEFT/NUMERIC/WIDTH.', NLW(2) )
      CALL AGSETF( 'AXIS/BOTTOM/NUMERIC/WIDTH.', NLW(1) )

*  maximum label length,
      CALL AGSETF( 'LINE/MAXIMUM.', LINMAX )

*  and the character terminator string.

      CALL AGSETC( 'LINE/END.', TERMST )

*  Now the store the labels.
      CALL AGSETC( 'LABEL/NAME.', 'L' )
      CALL AGSETI( 'LINE/NUMBER.', 100 )
      CALL AGSETF( 'LINE/CHARACTER.', LABW(2) )

      CALL AGSETC( 'LABEL/NAME.', 'B' )
      CALL AGSETI( 'LINE/NUMBER.', -100 )
      CALL AGSETF( 'LINE/CHARACTER.', LABW(1) )

      CALL AGSETC( 'LABEL/NAME.', 'T' )
      CALL AGSETI( 'LINE/NUMBER.', 100 )
      CALL AGSETF( 'LINE/CHARACTER.', LABW(3) )

*  Store and reset the line width.
      IF ( THICK .GT. 0.000999 ) THEN
         CALL SETUSV( 'LW', LINWDT )
      END IF

      END
